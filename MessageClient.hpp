#ifndef MESSAGE_CLIENT_HPP__
#define MESSAGE_CLIENT_HPP__

#include <QObject>
#include <QTime>
#include <QString>

#include "Radio.hpp"
#include "pimpl_h.hpp"

class QByteArray;
class QHostAddress;

//
// MessageClient - Manage messages sent and replies received from a
//                 matching server (MessageServer) at the other end of
//                 the wire
//
//
// Each outgoing message type is a Qt slot
//
class MessageClient
  : public QObject
{
  Q_OBJECT;

public:
  using Frequency = Radio::Frequency;
  using port_type = quint16;

  // instantiate and initiate a host lookup on the server
  //
  // messages will be silently dropped until a server host lookup is complete
  MessageClient (QString const& id, QString const& version,
                 QString const& server, port_type server_port, QObject * parent = nullptr);

  // query server details
  QHostAddress server_address () const;
  port_type server_port () const;

  // initiate a new server host lookup or is the server name is empty
  // the sending of messages is disabled
  Q_SLOT void set_server (QString const& server = QString {});

  // change the server port messages are sent to
  Q_SLOT void set_server_port (port_type server_port = 0u);

  // outgoing messages
  Q_SLOT void status_update (Frequency, QString const& mode, QString const& dx_call, QString const& report
                             , QString const& tx_mode, bool tx_enabled, bool transmitting, bool decoding
                             , qint32 rx_df, qint32 tx_df, QString const& de_call, QString const& de_grid
                             , QString const& dx_grid, bool watchdog_timeout, QString const& sub_mode
                             , bool fast_mode, bool tx_first, bool force);
  Q_SLOT void decode (bool is_new, QTime time, qint32 snr, float delta_time, quint32 delta_frequency
                      , QString const& mode, QString const& message, bool low_confidence
                      , bool off_air);
  Q_SLOT void WSPR_decode (bool is_new, QTime time, qint32 snr, float delta_time, Frequency
                           , qint32 drift, QString const& callsign, QString const& grid, qint32 power
                           , bool off_air);
  Q_SLOT void clear_decodes ();
  Q_SLOT void qso_logged (QDateTime time_off, QString const& dx_call, QString const& dx_grid
                          , Frequency dial_frequency, QString const& mode, QString const& report_sent
                          , QString const& report_received, QString const& tx_power, QString const& comments
                          , QString const& name, QDateTime time_on, QString const& operator_call
                          , QString const& my_call, QString const& my_grid);

  // ADIF_record argument should be valid ADIF excluding any <EOR> end
  // of record marker
  Q_SLOT void logged_ADIF (QByteArray const& ADIF_record);

  // this slot may be used to send arbitrary UDP datagrams to and
  // destination allowing the underlying socket to be used for general
  // UDP messaging if desired
  Q_SLOT void send_raw_datagram (QByteArray const&, QHostAddress const& dest_address, port_type dest_port);

  // disallowed message destination (does not block datagrams sent
  // with send_raw_datagram() above)
  Q_SLOT void add_blocked_destination (QHostAddress const&);

  // this signal is emitted if the server sends us a reply, the only
  // reply supported is reply to a prior CQ or QRZ message
  Q_SIGNAL void reply (QTime, qint32 snr, float delta_time, quint32 delta_frequency, QString const& mode
                       , QString const& message_text, bool low_confidence, quint8 modifiers);

  // this signal is emitted if the server has requested a replay of
  // all decodes
  Q_SIGNAL void replay ();

  // this signal is emitted if the server has requested immediate (or
  // Enable Tx if enableTx_only is true) transmission to halt
  Q_SIGNAL void halt_tx (bool enableTx_only);

  // this signal is emitted if the server has requested a new free
  // message text
  Q_SIGNAL void free_text (QString const&, bool send);

  // this signal is emitted if the server has requested to change
  // TX delta frequency
  Q_SIGNAL void set_tx_deltafreq (quint32 tx_delta_frequency);

  // this signal is emitted if the server has requested to set CQ message for transmission,
  // CQ direction and TX period
  // and optionally trigger CQ or directional CQ message transmission
  Q_SIGNAL void trigger_CQ (QString const& direction, bool tx_period, bool send);

  // this signal is emitted when network errors occur or if a host
  // lookup fails
  Q_SIGNAL void error (QString const&) const;

private:
  class impl;
  pimpl<impl> m_;
};

#endif
