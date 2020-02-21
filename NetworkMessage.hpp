// last time modified by Igor UA3DJY on 20200205

#ifndef NETWORK_MESSAGE_HPP__
#define NETWORK_MESSAGE_HPP__

/*
 * JTDX Message Formats
 * ======================
 *
 * All messages are written or  read using the QDataStream derivatives
 * defined below, note that we are using the default for floating
 * point precision which means all are double precision i.e. 64-bit
 * IEEE format.
 *
 *  Message is big endian format
 *
 *   Header format:
 *
 *      32-bit unsigned integer magic number 0xadbccbda
 *      32-bit unsigned integer schema number
 *
 *   Payload format:
 *
 *      As per  the QDataStream format,  see below for version  used and
 *      here:
 *
 *        http://doc.qt.io/qt-5/datastreamformat.html
 *
 *      for the serialization details for each type, at the time of
 *      writing the above document is for Qt_5_0 format which is buggy
 *      so we use Qt_5_4 format, differences are:
 *
 *      QDateTime:
 *           QDate      qint64    Julian day number
 *           QTime      quint32   Milli-seconds since midnight
 *           timespec   quint8    0=local, 1=UTC, 2=Offset from UTC
 *                                                 (seconds)
 *                                3=time zone
 *           offset     qint32    only present if timespec=2
 *           timezone   several-fields only present if timespec=3
 *
 *      we will avoid using QDateTime fields with time zones for simplicity.
 *
 * Type utf8  is a  utf-8 byte  string formatted  as a  QByteArray for
 * serialization purposes  (currently a quint32 size  followed by size
 * bytes, no terminator is present or counted).
 *
 * The QDataStream format document linked above is not complete for
 * the QByteArray serialization format, it is similar to the QString
 * serialization format in that it differentiates between empty
 * strings and null strings. Empty strings have a length of zero
 * whereas null strings have a length field of 0xffffffff.
 *
 * Schema Negotiation
 * ------------------
 *
 * The NetworkMessage::Builder  class specifies a schema  number which
 * may be  incremented from time to  time. It represents a  version of
 * the underlying encoding schemes used to store data items. Since the
 * underlying  encoding  is   defined  by  the  Qt   project  in  it's
 * QDataStream  stream operators,  it  is essential  that clients  and
 * servers  of  this protocol  can  agree  on  a common  scheme.   The
 * NetworkMessage  utility classes  below exchange  the schema  number
 * actually used.  The handling of  the schema is backwards compatible
 * to  an  extent,  so  long   as  clients  and  servers  are  written
 * correctly. For  example a server  written to any  particular schema
 * version can communicate with a client written to a later schema.
 *
 * Schema Version 1:- this schema used the QDataStream::Qt_5_0 version
 *  which is broken.
 *
 * Schema Version 2:- this schema uses the QDataStream::Qt_5_2 version.
 *
 * Schema Version 3:- this schema uses the QDataStream::Qt_5_4 version.

 *
 *
 *
 * Message       Direction Value                  Type
 * ------------- --------- ---------------------- -----------
 * Heartbeat     Out/In    0                      quint32
 *                         Id (unique key)        utf8
 *                         Maximum schema number  quint32
 *                         version                utf8
 *
 *    The heartbeat  message shall be  sent on a periodic  basis every
 *    NetworkMessage::pulse   seconds   (see    below),   the   WSJT-X
 *    application  does  that  using the  MessageClient  class.   This
 *    message is intended to be used by servers to detect the presence
 *    of a  client and also  the unexpected disappearance of  a client
 *    and  by clients  to learn  the schema  negotiated by  the server
 *    after it receives  the initial heartbeat message  from a client.
 *    The message_aggregator reference server does just that using the
 *    MessageServer class. Upon  initial startup a client  must send a
 *    heartbeat message as soon as  is practical, this message is used
 *    to negotiate the maximum schema  number common to the client and
 *    server. Note  that the  server may  not be  able to  support the
 *    client's  requested maximum  schema  number, in  which case  the
 *    first  message received  from the  server will  specify a  lower
 *    schema number (never a higher one  as that is not allowed). If a
 *    server replies  with a lower  schema number then no  higher than
 *    that number shall be used for all further outgoing messages from
 *    either clients or the server itself.
 *
 *    Note: the  "Maximum schema number"  field was introduced  at the
 *    same time as schema 3, therefore servers and clients must assume
 *    schema 2 is the highest schema number supported if the Heartbeat
 *    message does not contain the "Maximum schema number" field.
 *
 *
 * Status        Out       1                      quint32
 *                         Id (unique key)        utf8
 *                         Dial Frequency (Hz)    quint64
 *                         Mode                   utf8
 *                         DX call                utf8
 *                         Report                 utf8
 *                         Tx Mode                utf8
 *                         Tx Enabled             bool
 *                         Transmitting           bool
 *                         Decoding               bool
 *                         Rx DF                  qint32
 *                         Tx DF                  qint32
 *                         DE call                utf8
 *                         DE grid                utf8
 *                         DX grid                utf8
 *                         Tx Watchdog            bool
 *                         Sub-mode               utf8
 *                         Fast mode              bool
 *                         Tx first               bool
 *
 *    WSJT-X  sends this  status message  when various  internal state
 *    changes to allow the server to  track the relevant state of each
 *    client without the need for  polling commands. The current state
 *    changes that generate status messages are:
 *
 *      Application start up,
 *      "Enable Tx" button status changes,
 *      Dial frequency changes,
 *      Changes to the "DX Call" field,
 *      Operating mode changes,
 *      Transmit mode changed (in dual JT9+JT65 mode),
 *      Changes to the "Rpt" spinner,
 *      After an old decodes replay sequence (see Replay below),
 *      When switching between Tx and Rx mode,
 *      At the start and end of decoding.
 *
 *    TX first shows selected  transmission  period where 'true' value
 *    corresponds  to  'TX 00/30' second in  FT8  mode  and  'TX even' 
 *    minute in JT65/JT9/T10 modes.
 *
 *
 * Decode        Out       2                      quint32
 *                         Id (unique key)        utf8
 *                         New                    bool
 *                         Time                   QTime
 *                         snr                    qint32
 *                         Delta time (S)         float (serialized as double)
 *                         Delta frequency (Hz)   quint32
 *                         Mode                   utf8
 *                         Message                utf8
 *                         Low confidence         bool
 *                         Off air                bool
 *
 *      The decode message is sent when  a new decode is completed, in
 *      this case the 'New' field is true. It is also used in response
 *      to  a "Replay"  message where  each  old decode  in the  "Band
 *      activity" window, that  has not been erased, is  sent in order
 *      as a one of these messages with the 'New' field set to  false.
 *      See the "Replay"  message  below  for  details  of  usage. Low
 *      confidence decodes are flagged for decoders where a decode has 
 *      a higher than normal probability of being false. 
 *      Off  air  decodes  are  those  that  result  from playing back 
 *      a .WAV file.
 *
 *
 * Clear         Out       3                      quint32
 *                         Id (unique key)        utf8
 *
 *      This message is  send when all prior "Decode"  messages in the
 *      "Band activity"  window have been discarded  and therefore are
 *      no long available for actioning  with a "Reply" message. It is
 *      sent when the user erases  the "Band activity" window and when
 *      WSJT-X  closes down  normally. The  server should  discard all
 *      decode messages upon receipt of this message.
 *
 *
 * Reply         In        4                      quint32
 *                         Id (target unique key) utf8
 *                         Time                   QTime
 *                         snr                    qint32
 *                         Delta time (S)         float (serialized as double)
 *                         Delta frequency (Hz)   quint32
 *                         Mode                   utf8
 *                         Message                utf8
 *
 *      In order for a server  to provide a useful cooperative service
 *      to JTDX it  is possible for it to initiate  a  QSO  by sending
 *      this message to a client. JTDX filters this   message and only
 *      acts upon it  if the message exactly describes a prior decode.
 *      The action taken is  exactly  equivalent  to  the user  double
 *      clicking the message in the "Band activity" window. The intent
 *      of  this message  is  for  servers  to  be  able to provide an
 *      advanced  look  up  of  potential QSO  partners,  for  example
 *      determining if they have been worked before or if working them
 *      may advance some objective like award progress.  
 *
 *
 * QSO Logged    Out       5                      quint32
 *                         Id (unique key)        utf8
 *                         Date & Time            QDateTime
 *                         DX call                utf8
 *                         DX grid                utf8
 *                         Dial frequency (Hz)    quint64
 *                         Mode                   utf8
 *                         Report send            utf8
 *                         Report received        utf8
 *                         Tx power               utf8
 *                         Comments               utf8
 *                         Name                   utf8
 *                         Date & Time On         QDateTime
 *                         Operator call          utf8
 *                         My call                utf8
 *                         My grid                utf8
 *
 *      The  QSO logged  message is  sent  to the  server(s) when  the
 *      JTDX  user  accepts the "Log  QSO" dialog by clicking the "OK"
 *      button.
 *
 *
 * Close         Out       6                      quint32
 *                         Id (unique key)        utf8
 *
 *      Close is sent by a client immediately prior to it shutting
 *      down gracefully.
 *
 *
 * Replay        In        7                      quint32
 *                         Id (unique key)        utf8
 *
 *      When a server starts it may  be useful for it to determine the
 *      state  of preexisting  clients. Sending  this message  to each
 *      client as it is discovered  will cause that client (WSJT-X) to
 *      send a "Decode" message for each decode currently in its "Band
 *      activity"  window. Each  "Decode" message  sent will  have the
 *      "New" flag set to false so that they can be distinguished from
 *      new decodes. After  all the old decodes have  been broadcast a
 *      "Status" message  is also broadcast.  If the server  wishes to
 *      determine  the  status  of  a newly  discovered  client;  this
 *      message should be used.
 *
 *
 * Halt Tx       In        8
 *                         Id (unique key)        utf8
 *                         Auto Tx Only           bool
 *
 *      The server may stop a client from transmitting messages either
 *      immediately or at  the end of the  current transmission period
 *      using this message.
 *
 *
 * Free Text     In        9
 *                         Id (unique key)        utf8
 *                         Text                   utf8
 *                         Send                   bool
 *
 *      This message  allows the server  to set the current  free text
 *      message content. Sending this  message with a non-empty "Text"
 *      field is equivalent to typing  a new message (old contents are
 *      discarded) in to  the WSJT-X free text message  field or "Tx5"
 *      field (both  are updated) and if  the "Send" flag is  set then
 *      clicking the "Now" radio button for the "Tx5" field if tab one
 *      is current or clicking the "Free  msg" radio button if tab two
 *      is current.
 *
 *      It is the responsibility of the  sender to limit the length of
 *      the  message   text  and   to  limit   it  to   legal  message
 *      characters. Despite this,  it may be difficult  for the sender
 *      to determine the maximum message length without reimplementing
 *      the complete message encoding protocol. Because of this is may
 *      be better  to allow any  reasonable message length and  to let
 *      the WSJT-X application encode and possibly truncate the actual
 *      on-air message.
 *
 *      If the  message text is  empty the  meaning of the  message is
 *      refined  to send  the  current free  text  unchanged when  the
 *      "Send" flag is set or to  clear the current free text when the
 *      "Send" flag is  unset.  Note that this API does  not include a
 *      command to  determine the  contents of  the current  free text
 *      message.
 *
 *
 * WSPRDecode    Out       10                     quint32
 *                         Id (unique key)        utf8
 *                         New                    bool
 *                         Time                   QTime
 *                         snr                    qint32
 *                         Delta time (S)         float (serialized as double)
 *                         Frequency (Hz)         quint64
 *                         Drift (Hz)             qint32
 *                         Callsign               utf8
 *                         Grid                   utf8
 *                         Power (dBm)            qint32
 *
 *      The decode message is sent when  a new decode is completed, in
 *      this case the 'New' field is true. It is also used in response
 *      to  a "Replay"  message where  each  old decode  in the  "Band
 *      activity" window, that  has not been erased, is  sent in order
 *      as  a one  of  these  messages with  the  'New'  field set  to
 *      false. See the "Replay" message below for details of usage.
 *
 * 
 * Logged ADIF    Out      12                     quint32
 *                         Id (unique key)        utf8
 *                         ADIF text              utf8
 *
 *      The  logged ADIF  message is  sent to  the server(s)  when the
 *      JTDX user accepts the "Log  QSO" dialog by clicking the "OK"
 *      button. The  "ADIF text" field  consists of a valid  ADIF file
 *      such that  the JTDX  UDP header information  is encapsulated
 *      into a valid ADIF header. E.g.:
 *
 *          <magic-number><schema-number><type><id><32-bit-count>  # binary encoded fields
 *          # the remainder is the contents of the ADIF text field
 *          <adif_ver:5>3.0.7
 *          <programid:4>JTDX
 *          <EOH>
 *          ADIF log data fields ...<EOR>
 *
 *      Note that  receiving applications can treat  the whole message
 *      as a valid ADIF file with one record without special parsing.
 *
 *
 * SetTxDeltaFreq  In      50                     quint32
 *                         Id (unique key)        utf8
 *                         TX delta frequency     quint32
 *
 *      Setting TX delta frequency in JTDX.  Received  value  will  be
 *      checked against widegraph frequency range,  it will be ignored
 *      if it does not fit there.
 * 
 *
 * TriggerCQ    In         51                     quint32
 *                         Id (unique key)        utf8
 *                         Direction              utf8
 *                         Tx period              bool
 *                         Send                   bool
 *
 *      The  triggerCQ   message  is  dedicated  to  set CQ direction,
 *      TX period  and  optionally  trigger  CQ  message  transmission 
 *      in  JTDX  from  external  software    through    the   network
 *      connection.  Directional  CQ  is  also  being  supported where 
 *      direction is two-char combination in the range AA..ZZ.
 *      TX period is equivalent to TX first in the Status UDP message,
 *      where  'true'  value  shall  correspond  to  'TX 00/30' second 
 *      in FT8 mode and 'TX even' minute in JT65/JT9/T10 modes.
 *
 *      If the "Send" flag is set  then  CQ message  will be generated 
 *      and Enable Tx button will be switched on.
 */

#include <QDataStream>

#include "pimpl_h.hpp"

class QIODevice;
class QByteArray;
class QString;

namespace NetworkMessage
{
  // NEVER DELETE MESSAGE TYPES
  enum Type
    {
      Heartbeat,
      Status,
      Decode,
      Clear,
      Reply,
      QSOLogged,
      Close,
      Replay,
      HaltTx,
      FreeText,
      WSPRDecode,
      reserved11, //WSJT-X Location
      LoggedADIF,
      reserved13, //WSJT-X HighlightCallsign
      reserved14, //any future WSJT-X messages
      reserved15,
      reserved16,
      reserved17,
      reserved18,
      reserved19,
      reserved20,
      reserved21,
      reserved22,
      reserved23,
      reserved24,
      reserved25,
      reserved26,
      reserved27,
      reserved28,
      reserved29,
      reserved30,
      reserved31,
      reserved32,
      reserved33,
      reserved34,
      reserved35,
      reserved36,
      reserved37,
      reserved38,
      reserved39,
      reserved40,
      reserved41,
      reserved42,
      reserved43,
      reserved44,
      reserved45,
      reserved46,
      reserved47,
      reserved48,
      reserved49,
      SetTxDeltaFreq,
      TriggerCQ,
      maximum_message_type_     // ONLY add new message types
                                // immediately before here
    };

  quint32 constexpr pulse {15}; // seconds

  //
  // NetworkMessage::Builder - build a message containing serialized Qt types
  //
  class Builder
    : public QDataStream
  {
  public:
    static quint32 constexpr magic {0xadbccbda}; // never change this

    // increment this if a newer Qt schema is required and add decode
    // logic to the Builder and Reader class implementations
#if QT_VERSION >= 0x050400
    static quint32 constexpr schema_number {3};
#elif QT_VERSION >= 0x050200
    static quint32 constexpr schema_number {2};
#else
    // Schema 1 (Qt_5_0) is broken
#error "Qt version 5.2 or greater required"
#endif

    explicit Builder (QIODevice *, Type, QString const& id, quint32 schema);
    explicit Builder (QByteArray *, Type, QString const& id, quint32 schema);
    Builder (Builder const&) = delete;
    Builder& operator = (Builder const&) = delete;

  private:
    void common_initialization (Type type, QString const& id, quint32 schema);
  };

  //
  // NetworkMessage::Reader - read a message containing serialized Qt types
  //
  // Message  is as  per NetworkMessage::Builder  above, the  schema()
  // member  may be  used  to  determine the  schema  of the  original
  // message.
  //
  class Reader
    : public QDataStream
  {
  public:
    explicit Reader (QIODevice *);
    explicit Reader (QByteArray const&);
    Reader (Reader const&) = delete;
    Reader& operator = (Reader const&) = delete;
    ~Reader ();

    quint32 schema () const;
    Type type () const;
    QString id () const;

  private:
    class impl;
    pimpl<impl> m_;
  };
}

#endif
