#include "MessageClient.hpp"

#include <stdexcept>
#include <vector>
#include <algorithm>

#include <QUdpSocket>
#include <QHostInfo>
#include <QTimer>
#include <QQueue>
#include <QByteArray>
#include <QHostAddress>

#include "NetworkMessage.hpp"

#include "pimpl_impl.hpp"

#include "moc_MessageClient.cpp"

class MessageClient::impl
  : public QUdpSocket
{
  Q_OBJECT;

public:
  impl (QString const& id, QString const& version,
        port_type server_port, MessageClient * self)
    : self_ {self}
    , id_ {id}
    , version_ {version}
    , server_port_ {server_port}
    , schema_ {2}  // use 2 prior to negotiation not 1 which is broken
    , force_ {false}
    , heartbeat_timer_ {new QTimer {this}}
  {
    connect (heartbeat_timer_, &QTimer::timeout, this, &impl::heartbeat);
    connect (this, &QIODevice::readyRead, this, &impl::pending_datagrams);

    heartbeat_timer_->start (NetworkMessage::pulse * 1000);

    // bind to an ephemeral port
    bind ();
  }

  ~impl ()
  {
    closedown ();
  }

  enum StreamStatus {Fail, Short, OK};

  void parse_message (QByteArray const& msg);
  void pending_datagrams ();
  void heartbeat ();
  void closedown ();
  StreamStatus check_status (QDataStream const&) const;
  void send_message (QByteArray const&);
  void send_message (QDataStream const& out, QByteArray const& message)
  {
      if (OK == check_status (out))
        {
          send_message (message);
        }
      else
        {
          Q_EMIT self_->error ("Error creating UDP message");
        }
  }

  Q_SLOT void host_info_results (QHostInfo);

  MessageClient * self_;
  QString id_;
  QString version_;
  QString server_string_;
  port_type server_port_;
  QHostAddress server_;
  quint32 schema_;
  bool force_;
  QTimer * heartbeat_timer_;
  std::vector<QHostAddress> blocked_addresses_;

  // hold messages sent before host lookup completes asynchronously
  QQueue<QByteArray> pending_messages_;
  QByteArray last_message_;
};

#include "MessageClient.moc"

void MessageClient::impl::host_info_results (QHostInfo host_info)
{
  if (QHostInfo::NoError != host_info.error ())
    {
      Q_EMIT self_->error ("UDP server lookup failed:\n" + host_info.errorString ());
      pending_messages_.clear (); // discard
    }
  else if (host_info.addresses ().size ())
    {
      auto server = host_info.addresses ()[0];
      if (blocked_addresses_.end () == std::find (blocked_addresses_.begin (), blocked_addresses_.end (), server))
        {
          server_ = server;

          // send initial heartbeat which allows schema negotiation
          heartbeat ();

          // clear any backlog
          while (pending_messages_.size ())
            {
              send_message (pending_messages_.dequeue ());
            }
        }
      else
        {
          Q_EMIT self_->error ("UDP server blocked, please try another");
          pending_messages_.clear (); // discard
        }
    }
}

void MessageClient::impl::pending_datagrams ()
{
  while (hasPendingDatagrams ())
    {
      QByteArray datagram;
      datagram.resize (pendingDatagramSize ());
      QHostAddress sender_address;
      port_type sender_port;
      if (0 <= readDatagram (datagram.data (), datagram.size (), &sender_address, &sender_port))
        {
          parse_message (datagram);
        }
    }
}

void MessageClient::impl::parse_message (QByteArray const& msg)
{
  try
    {
      // 
      // message format is described in NetworkMessage.hpp
      // 
      NetworkMessage::Reader in {msg};
      if (OK == check_status (in) && id_ == in.id ()) // OK and for us
        {
          if (schema_ < in.schema ()) // one time record of server's
                                      // negotiated schema
            {
              schema_ = in.schema ();
            }

          //
          // message format is described in NetworkMessage.hpp
          //
          switch (in.type ())
            {
            case NetworkMessage::Reply:
              {
                // unpack message
                QTime time;
                qint32 snr;
                float delta_time;
                quint32 delta_frequency;
                QByteArray mode;
                QByteArray message;
                bool low_confidence {false};
                quint8 modifiers {0};
                in >> time >> snr >> delta_time >> delta_frequency >> mode >> message
                   >> low_confidence >> modifiers;
                if (check_status (in) != Fail)
                  {
                    Q_EMIT self_->reply (time, snr, delta_time, delta_frequency
                                         , QString::fromUtf8 (mode), QString::fromUtf8 (message)
                                         , low_confidence, modifiers);
                  }
              }
              break;

            case NetworkMessage::Replay:
              if (check_status (in) != Fail)
                {
                  last_message_.clear ();
                  Q_EMIT self_->replay ();
                }
              break;

            case NetworkMessage::HaltTx:
              {
                bool enableTx_only {false};
                in >> enableTx_only;
                if (check_status (in) != Fail)
                  {
                    Q_EMIT self_->halt_tx (enableTx_only);
                  }
              }
              break;

            case NetworkMessage::FreeText:
              {
                QByteArray message;
                bool send {true};
                in >> message >> send;
                if (check_status (in) != Fail)
                  {
                    Q_EMIT self_->free_text (QString::fromUtf8 (message), send);
                  }
              }
              break;

            case NetworkMessage::SetTxDeltaFreq:
              {
                quint32 tx_delta_frequency;
                in >> tx_delta_frequency;
                if (check_status (in) != Fail)
                  {
                    Q_EMIT self_->set_tx_deltafreq (tx_delta_frequency);
                  }
              }
              break;

            case NetworkMessage::TriggerCQ:
              {
                QByteArray direction;
                bool tx_period {false};
                bool send {true};
                in >> direction >> send;
                if (check_status (in) != Fail)
                  {
                    Q_EMIT self_->trigger_CQ (QString::fromUtf8 (direction),tx_period,send);
                  }
              }
              break;

            default:
              // Ignore
              break;
            }
        }
    }
  catch (std::exception const& e)
    {
      Q_EMIT self_->error (QString {"MessageClient exception: %1"}.arg (e.what ()));
    }
  catch (...)
    {
      Q_EMIT self_->error ("Unexpected exception in MessageClient");
    }
}

void MessageClient::impl::heartbeat ()
{
   if (server_port_ && !server_.isNull ())
    {
      QByteArray message;
      NetworkMessage::Builder hb {&message, NetworkMessage::Heartbeat, id_, schema_};
      hb << NetworkMessage::Builder::schema_number // maximum schema number accepted
         << version_.toUtf8 ();
      if (OK == check_status (hb))
        {
          writeDatagram (message, server_, server_port_);
        }
    }
}

void MessageClient::impl::closedown ()
{
   if (server_port_ && !server_.isNull ())
    {
      QByteArray message;
      NetworkMessage::Builder out {&message, NetworkMessage::Close, id_, schema_};
      if (OK == check_status (out))
        {
          writeDatagram (message, server_, server_port_);
        }
    }
}

void MessageClient::impl::send_message (QByteArray const& message)
{
  if (server_port_)
    {
      if (!server_.isNull ())
        {
          if (force_ || message != last_message_) // avoid duplicates, force status dupe for same callsign UDP reply
            {
              force_=false;
              writeDatagram (message, server_, server_port_);
              last_message_ = message;
            }
        }
      else
        {
          pending_messages_.enqueue (message);
        }
    }
}

auto MessageClient::impl::check_status (QDataStream const& stream) const -> StreamStatus
{
  auto stat = stream.status ();
  StreamStatus result {Fail};
  switch (stat)
    {
    case QDataStream::ReadPastEnd:
      result = Short;
      break;

    case QDataStream::ReadCorruptData:
      Q_EMIT self_->error ("Message serialization error: read corrupt data");
      break;

    case QDataStream::WriteFailed:
      Q_EMIT self_->error ("Message serialization error: write error");
      break;

    default:
      result = OK;
      break;
    }
  return result;
}

MessageClient::MessageClient (QString const& id, QString const& version,
                              QString const& server, port_type server_port, QObject * self)
  : QObject {self}
  , m_ {id, version, server_port, this}
{
#if QT_VERSION < QT_VERSION_CHECK(5, 15, 0)
#if defined (Q_OS_WIN)
  connect (&*m_, static_cast<void (impl::*) (impl::SocketError)> (&impl::error), [this] (impl::SocketError e)
            {
              if (e != impl::NetworkError // take this out when Qt 5.5 stops doing this spuriously
                  && e != impl::ConnectionRefusedError) // not interested in this with UDP socket
                  { Q_EMIT error (m_->errorString ()); }
            });
#else
  connect (&*m_, static_cast<void (impl::*) (impl::SocketError)> (&impl::error), [this] (impl::SocketError e)
            {
              { Q_UNUSED (e); Q_EMIT error (m_->errorString ()); }
            });
#endif
#else
#if defined (Q_OS_WIN)
  connect (&*m_, &impl::errorOccurred, [this] (impl::SocketError e)
            {
              if (e != impl::NetworkError // take this out when Qt 5.5 stops doing this spuriously
                  && e != impl::ConnectionRefusedError) // not interested in this with UDP socket
                  { Q_EMIT error (m_->errorString ()); }
            });
#else
  connect (&*m_, &impl::errorOccurred, [this] (impl::SocketError e)
            {
              if (e != impl::TemporaryError
                  && e != impl::ConnectionRefusedError) // not interested in this with UDP socket
              { Q_UNUSED (e); Q_EMIT error (m_->errorString ()); }
            });
#endif
#endif
  set_server (server);
}

QHostAddress MessageClient::server_address () const
{
  return m_->server_;
}

auto MessageClient::server_port () const -> port_type
{
  return m_->server_port_;
}

void MessageClient::set_server (QString const& server)
{
  m_->server_.clear ();
  m_->server_string_ = server;
  if (!server.isEmpty ())
    {
      // queue a host address lookup
      QHostInfo::lookupHost (server, &*m_, SLOT (host_info_results (QHostInfo)));
    }
}

void MessageClient::set_server_port (port_type server_port)
{
  m_->server_port_ = server_port;
}

void MessageClient::send_raw_datagram (QByteArray const& message, QHostAddress const& dest_address
                                       , port_type dest_port)
{
  if (dest_port && !dest_address.isNull ())
    {
      m_->writeDatagram (message, dest_address, dest_port);
    }
}

void MessageClient::add_blocked_destination (QHostAddress const& a)
{
  m_->blocked_addresses_.push_back (a);
  if (a == m_->server_)
    {
      m_->server_.clear ();
      Q_EMIT error ("UDP server blocked, please try another");
      m_->pending_messages_.clear (); // discard
    }
}

void MessageClient::status_update (Frequency f, QString const& mode, QString const& dx_call
                                   , QString const& report, QString const& tx_mode
                                   , bool tx_enabled, bool transmitting, bool decoding
                                   , qint32 rx_df, qint32 tx_df, QString const& de_call
                                   , QString const& de_grid, QString const& dx_grid
                                   , bool watchdog_timeout, QString const& sub_mode
                                   , bool fast_mode, bool tx_first, bool force)
{
  if (m_->server_port_ && !m_->server_string_.isEmpty ())
    {
      QByteArray message;
      NetworkMessage::Builder out {&message, NetworkMessage::Status, m_->id_, m_->schema_};
      out << f << mode.toUtf8 () << dx_call.toUtf8 () << report.toUtf8 () << tx_mode.toUtf8 ()
          << tx_enabled << transmitting << decoding << rx_df << tx_df << de_call.toUtf8 ()
          << de_grid.toUtf8 () << dx_grid.toUtf8 () << watchdog_timeout << sub_mode.toUtf8 ()
          << fast_mode << tx_first;
      if(force) m_->force_=true;
      m_->send_message (out, message);
    }
}

void MessageClient::decode (bool is_new, QTime time, qint32 snr, float delta_time, quint32 delta_frequency
                            , QString const& mode, QString const& message_text, bool low_confidence
                            , bool off_air)
{
   if (m_->server_port_ && !m_->server_string_.isEmpty ())
    {
      QByteArray message;
      NetworkMessage::Builder out {&message, NetworkMessage::Decode, m_->id_, m_->schema_};
      out << is_new << time << snr << delta_time << delta_frequency << mode.toUtf8 ()
          << message_text.toUtf8 () << low_confidence << off_air;
      m_->send_message (out, message);
    }
}

void MessageClient::WSPR_decode (bool is_new, QTime time, qint32 snr, float delta_time, Frequency frequency
                                 , qint32 drift, QString const& callsign, QString const& grid, qint32 power
                                 , bool off_air)
{
   if (m_->server_port_ && !m_->server_string_.isEmpty ())
    {
      QByteArray message;
      NetworkMessage::Builder out {&message, NetworkMessage::WSPRDecode, m_->id_, m_->schema_};
      out << is_new << time << snr << delta_time << frequency << drift << callsign.toUtf8 ()
          << grid.toUtf8 () << power << off_air;
      m_->send_message (out, message);
    }
}

void MessageClient::clear_decodes ()
{
   if (m_->server_port_ && !m_->server_string_.isEmpty ())
    {
      QByteArray message;
      NetworkMessage::Builder out {&message, NetworkMessage::Clear, m_->id_, m_->schema_};
      m_->send_message (out, message);
    }
}

void MessageClient::qso_logged (QDateTime time_off, QString const& dx_call, QString const& dx_grid
                                , Frequency dial_frequency, QString const& mode, QString const& report_sent
                                , QString const& report_received, QString const& tx_power
                                , QString const& comments, QString const& name, QDateTime time_on
                                , QString const& operator_call, QString const& my_call
                                , QString const& my_grid)
{
   if (m_->server_port_ && !m_->server_string_.isEmpty ())
    {
      QByteArray message;
      NetworkMessage::Builder out {&message, NetworkMessage::QSOLogged, m_->id_, m_->schema_};
      out << time_off << dx_call.toUtf8 () << dx_grid.toUtf8 () << dial_frequency << mode.toUtf8 ()
          << report_sent.toUtf8 () << report_received.toUtf8 () << tx_power.toUtf8 () << comments.toUtf8 ()
          << name.toUtf8 () << time_on << operator_call.toUtf8 () << my_call.toUtf8 () << my_grid.toUtf8 ();
      m_->send_message (out, message);
    }
}

void MessageClient::logged_ADIF (QByteArray const& ADIF_record)
{
   if (m_->server_port_ && !m_->server_string_.isEmpty ())
    {
      QByteArray message;
      NetworkMessage::Builder out {&message, NetworkMessage::LoggedADIF, m_->id_, m_->schema_};
      QByteArray ADIF {"\n<adif_ver:5>3.1.0\n<programid:4>JTDX\n<EOH>\n" + ADIF_record};
      out << ADIF;
      m_->send_message (out, message);
    }
}