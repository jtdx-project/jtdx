#include "MessageServer.hpp"

#include <stdexcept>

#include <QUdpSocket>
#include <QString>
#include <QTimer>
#include <QHash>

#include "NetworkMessage.hpp"
#include "qt_helpers.hpp"

#include "pimpl_impl.hpp"

#include "moc_MessageServer.cpp"

class MessageServer::impl
  : public QUdpSocket
{
  Q_OBJECT;

public:
  impl (MessageServer * self, QString const& version)
    : self_ {self}
    , version_ {version}
    , port_ {0u}
    , clock_ {new QTimer {this}}
  {
    connect (this, &QIODevice::readyRead, this, &MessageServer::impl::pending_datagrams);
#if QT_VERSION < QT_VERSION_CHECK(5, 15, 0)
    connect (this, static_cast<void (impl::*) (SocketError)> (&impl::error), [this] (SocketError /* e */)
             { Q_EMIT self_->error (errorString ()); });
#else
    connect (this, static_cast<void (impl::*) (SocketError)> (&impl::errorOccurred), [this] (SocketError /* e */)
             { Q_EMIT self_->error (errorString ()); });
#endif
    connect (clock_, &QTimer::timeout, this, &impl::tick);
    clock_->start (NetworkMessage::pulse * 1000);
  }

  enum StreamStatus {Fail, Short, OK};

  void leave_multicast_group ();
  void join_multicast_group ();
  void parse_message (QHostAddress const& sender, port_type sender_port, QByteArray const& msg);
  void tick ();
  void pending_datagrams ();
  StreamStatus check_status (QDataStream const&) const;
  void send_message (QDataStream const& out, QByteArray const& message, QHostAddress const& address, port_type port)
  {
      if (OK == check_status (out))
        {
          writeDatagram (message, address, port);
        }
      else
        {
          Q_EMIT self_->error ("Error creating UDP message");
        }
  }

  MessageServer * self_;
  QString version_;
  port_type port_;
  QHostAddress multicast_group_address_;
  static BindMode const bind_mode_;
  struct Client
  {
    Client () = default;
    Client (QHostAddress const& sender_address, port_type const& sender_port)
      : sender_address_ {sender_address}
      , sender_port_ {sender_port}
      , negotiated_schema_number_ {2} // not 1 because it's broken
      , last_activity_ {QDateTime::currentDateTime ()}
    {
    }
    Client (Client const&) = default;
    Client& operator= (Client const&) = default;

    QHostAddress sender_address_;
    port_type sender_port_;
    quint32 negotiated_schema_number_;
    QDateTime last_activity_;
  };
  QHash<QString, Client> clients_; // maps id to Client
  QTimer * clock_;
};

#include "MessageServer.moc"

MessageServer::impl::BindMode const MessageServer::impl::bind_mode_ = ShareAddress | ReuseAddressHint;

void MessageServer::impl::leave_multicast_group ()
{
  if (!multicast_group_address_.isNull () && BoundState == state ())
    {
      leaveMulticastGroup (multicast_group_address_);
    }
}

void MessageServer::impl::join_multicast_group ()
{
  if (BoundState == state ()
      && !multicast_group_address_.isNull ())
    {
      if (IPv4Protocol == multicast_group_address_.protocol ()
          && IPv4Protocol != localAddress ().protocol ())
        {
          close ();
          bind (QHostAddress::AnyIPv4, port_, bind_mode_);
        }
      if (!joinMulticastGroup (multicast_group_address_))
        {
          multicast_group_address_.clear ();
        }
    }
}

void MessageServer::impl::pending_datagrams ()
{
  while (hasPendingDatagrams ())
    {
      QByteArray datagram;
      datagram.resize (pendingDatagramSize ());
      QHostAddress sender_address;
      port_type sender_port;
      if (0 <= readDatagram (datagram.data (), datagram.size (), &sender_address, &sender_port))
        {
          parse_message (sender_address, sender_port, datagram);
        }
    }
}

void MessageServer::impl::parse_message (QHostAddress const& sender, port_type sender_port, QByteArray const& msg)
{
  try
    {
      //
      // message format is described in NetworkMessage.hpp
      //
      NetworkMessage::Reader in {msg};

      auto id = in.id ();
      if (OK == check_status (in))
        {
          if (!clients_.contains (id))
            {
              auto& client = (clients_[id] = {sender, sender_port});
              QByteArray client_version;

              if (NetworkMessage::Heartbeat == in.type ())
                {
                  // negotiate a working schema number
                  in >> client.negotiated_schema_number_;
                  if (OK == check_status (in))
                    {
                      auto sn = NetworkMessage::Builder::schema_number;
                      client.negotiated_schema_number_ = std::min (sn, client.negotiated_schema_number_);

                      // reply to the new client informing it of the
                      // negotiated schema number
                      QByteArray message;
                      NetworkMessage::Builder hb {&message, NetworkMessage::Heartbeat, id, client.negotiated_schema_number_};
                      hb << NetworkMessage::Builder::schema_number // maximum schema number accepted
                         << version_.toUtf8 ();
                      if (impl::OK == check_status (hb))
                        {
                          writeDatagram (message, client.sender_address_, client.sender_port_);
                        }
                      else
                        {
                          Q_EMIT self_->error ("Error creating UDP message");
                        }
                    }
                  // we don't care if this fails to read
                  in >> client_version;
                }
              Q_EMIT self_->client_opened (id, QString::fromUtf8 (client_version));
            }
          clients_[id].last_activity_ = QDateTime::currentDateTime ();
  
          //
          // message format is described in NetworkMessage.hpp
          //
          switch (in.type ())
            {
            case NetworkMessage::Heartbeat:
              //nothing to do here as time out handling deals with lifetime
              break;

            case NetworkMessage::Clear:
              Q_EMIT self_->clear_decodes (id);
              break;

            case NetworkMessage::Status:
              {
                // unpack message
                Frequency f;
                QByteArray mode;
                QByteArray dx_call;
                QByteArray report;
                QByteArray tx_mode;
                bool tx_enabled {false};
                bool transmitting {false};
                bool decoding {false};
                qint32 rx_df {-1};
                qint32 tx_df {-1};
                QByteArray de_call;
                QByteArray de_grid;
                QByteArray dx_grid;
                bool watchdog_timeout {false};
                QByteArray sub_mode;
                bool fast_mode {false};
                bool tx_first {false};
                bool force {false};
                in >> f >> mode >> dx_call >> report >> tx_mode >> tx_enabled >> transmitting >> decoding
                   >> rx_df >> tx_df >> de_call >> de_grid >> dx_grid >> watchdog_timeout >> sub_mode
                   >> fast_mode >> tx_first;

                if (check_status (in) != Fail)
                  {
                    Q_EMIT self_->status_update (id, f, QString::fromUtf8 (mode), QString::fromUtf8 (dx_call)
                                                 , QString::fromUtf8 (report), QString::fromUtf8 (tx_mode)
                                                 , tx_enabled, transmitting, decoding, rx_df, tx_df
                                                 , QString::fromUtf8 (de_call), QString::fromUtf8 (de_grid)
                                                 , QString::fromUtf8 (dx_grid), watchdog_timeout
                                                 , QString::fromUtf8 (sub_mode), fast_mode, tx_first, force);
                  }
              }
              break;

            case NetworkMessage::Decode:
              {
                // unpack message
                bool is_new {true};
                QTime time;
                qint32 snr;
                float delta_time;
                quint32 delta_frequency;
                QByteArray mode;
                QByteArray message;
                in >> is_new >> time >> snr >> delta_time >> delta_frequency >> mode >> message;
                if (check_status (in) != Fail)
                  {
                    Q_EMIT self_->decode (is_new, id, time, snr, delta_time, delta_frequency
                                          , QString::fromUtf8 (mode), QString::fromUtf8 (message));
                  }
              }
              break;

            case NetworkMessage::WSPRDecode:
              {
                // unpack message
                bool is_new {true};
                QTime time;
                qint32 snr;
                float delta_time;
                Frequency frequency;
                qint32 drift;
                QByteArray callsign;
                QByteArray grid;
                qint32 power;
                in >> is_new >> time >> snr >> delta_time >> frequency >> drift >> callsign >> grid >> power;
                if (check_status (in) != Fail)
                  {
                    Q_EMIT self_->WSPR_decode (is_new, id, time, snr, delta_time, frequency, drift
                                          , QString::fromUtf8 (callsign), QString::fromUtf8 (grid), power);
                  }
              }
              break;

            case NetworkMessage::QSOLogged:
              {
                QDateTime time;
                QByteArray dx_call;
                QByteArray dx_grid;
                Frequency dial_frequency;
                QByteArray mode;
                QByteArray report_sent;
                QByteArray report_received;
                QByteArray tx_power;
                QByteArray comments;
                QByteArray name;
                QDateTime time_on; // Note: LOTW uses TIME_ON for their +/- 30-minute time window
                QByteArray operator_call;
                QByteArray my_call;
                QByteArray my_grid;
                in >> time >> dx_call >> dx_grid >> dial_frequency >> mode >> report_sent >> report_received
                   >> tx_power >> comments >> name >> time_on >> operator_call >> my_call >> my_grid;
                if (check_status (in) != Fail)
                  {
                    Q_EMIT self_->qso_logged (id, time, QString::fromUtf8 (dx_call), QString::fromUtf8 (dx_grid)
                                              , dial_frequency, QString::fromUtf8 (mode), QString::fromUtf8 (report_sent)
                                              , QString::fromUtf8 (report_received), QString::fromUtf8 (tx_power)
                                              , QString::fromUtf8 (comments), QString::fromUtf8 (name), time_on
                                              , QString::fromUtf8 (operator_call), QString::fromUtf8 (my_call)
                                              , QString::fromUtf8 (my_grid));
                  }
              }
              break;

            case NetworkMessage::Close:
              Q_EMIT self_->client_closed (id);
              clients_.remove (id);
              break;

            case NetworkMessage::LoggedADIF:
              {
                QByteArray ADIF;
                in >> ADIF;
                if (check_status (in) != Fail)
                  {
                    Q_EMIT self_->logged_ADIF (id, ADIF);
                  }
              }
              break;

            default:
              // Ignore
              break;
            }
        }
      else
        {
          Q_EMIT self_->error ("MessageServer warning: invalid UDP message received");
        }
    }
  catch (std::exception const& e)
    {
      Q_EMIT self_->error (QString {"MessageServer exception: %1"}.arg (e.what ()));
    }
  catch (...)
    {
      Q_EMIT self_->error ("Unexpected exception in MessageServer");
    }
}

void MessageServer::impl::tick ()
{
  auto now = QDateTime::currentDateTime ();
  auto iter = std::begin (clients_);
  while (iter != std::end (clients_))
    {
      if (now > (*iter).last_activity_.addSecs (NetworkMessage::pulse))
        {
          Q_EMIT self_->clear_decodes (iter.key ());
          Q_EMIT self_->client_closed (iter.key ());
          iter = clients_.erase (iter); // safe while iterating as doesn't rehash
        }
      else
        {
          ++iter;
        }
    }
}

auto MessageServer::impl::check_status (QDataStream const& stream) const -> StreamStatus
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

MessageServer::MessageServer (QObject * parent, QString const& version)
  : QObject {parent}
  , m_ {this, version}
{
}

void MessageServer::start (port_type port, QHostAddress const& multicast_group_address)
{
  if (port != m_->port_
      || multicast_group_address != m_->multicast_group_address_)
    {
      m_->leave_multicast_group ();
      if (impl::BoundState == m_->state ())
        {
          m_->close ();
        }
      m_->multicast_group_address_ = multicast_group_address;
      auto address = m_->multicast_group_address_.isNull ()
        || impl::IPv4Protocol != m_->multicast_group_address_.protocol () ? QHostAddress::Any : QHostAddress::AnyIPv4;
      if (port && m_->bind (address, port, m_->bind_mode_))
        {
          m_->port_ = port;
          m_->join_multicast_group ();
        }
      else
        {
          m_->port_ = 0;
        }
    }
}

void MessageServer::reply (QString const& id, QTime time, qint32 snr, float delta_time, quint32 delta_frequency, QString const& mode, QString const& message_text)
{
  auto iter = m_->clients_.find (id);
  if (iter != std::end (m_->clients_))
    {
      QByteArray message;
      NetworkMessage::Builder out {&message, NetworkMessage::Reply, id, (*iter).negotiated_schema_number_};
      out << time << snr << delta_time << delta_frequency << mode.toUtf8 () << message_text.toUtf8 ();
      m_->send_message (out, message, iter.value ().sender_address_, (*iter).sender_port_);
    }
}

void MessageServer::replay (QString const& id)
{
  auto iter = m_->clients_.find (id);
  if (iter != std::end (m_->clients_))
    {
      QByteArray message;
      NetworkMessage::Builder out {&message, NetworkMessage::Replay, id, (*iter).negotiated_schema_number_};
      m_->send_message (out, message, iter.value ().sender_address_, (*iter).sender_port_);
    }
}

void MessageServer::halt_tx (QString const& id, bool auto_only)
{
  auto iter = m_->clients_.find (id);
  if (iter != std::end (m_->clients_))
    {
      QByteArray message;
      NetworkMessage::Builder out {&message, NetworkMessage::HaltTx, id, (*iter).negotiated_schema_number_};
      out << auto_only;
      m_->send_message (out, message, iter.value ().sender_address_, (*iter).sender_port_);
    }
}

void MessageServer::free_text (QString const& id, QString const& text, bool send)
{
  auto iter = m_->clients_.find (id);
  if (iter != std::end (m_->clients_))
    {
      QByteArray message;
      NetworkMessage::Builder out {&message, NetworkMessage::FreeText, id, (*iter).negotiated_schema_number_};
      out << text.toUtf8 () << send;
      m_->send_message (out, message, iter.value ().sender_address_, (*iter).sender_port_);
    }
}

void MessageServer::set_tx_deltafreq (QString const& id, quint32 tx_delta_frequency)
{
  auto iter = m_->clients_.find (id);
  if (iter != std::end (m_->clients_))
    {
      QByteArray message;
      NetworkMessage::Builder out {&message, NetworkMessage::FreeText, id, (*iter).negotiated_schema_number_};
      out << tx_delta_frequency;
      m_->send_message (out, message, iter.value ().sender_address_, (*iter).sender_port_);
    }
}

void MessageServer::trigger_CQ (QString const& id, QString const& direction, bool tx_period, bool send)
{
  auto iter = m_->clients_.find (id);
  if (iter != std::end (m_->clients_))
    {
      QByteArray message;
      NetworkMessage::Builder out {&message, NetworkMessage::FreeText, id, (*iter).negotiated_schema_number_};
      out << direction.toUtf8 () << tx_period << send;
      m_->send_message (out, message, iter.value ().sender_address_, (*iter).sender_port_);
    }
}
