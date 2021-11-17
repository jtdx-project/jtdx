#ifndef TRANSCEIVER_HPP__
#define TRANSCEIVER_HPP__

#include <QObject>

#include "qt_helpers.hpp"
#include "Radio.hpp"
#include "JTDXDateTime.h"

class QString;

//
// Abstract Transceiver Interface
//
//  This is  the minimal  generic interface  to a  rig as  required by
//  wsjtx.
//
// Responsibilities
//
//  Provides a  Qt slot  to set  the frequency, mode  and PTT  of some
//  transceiver. This is a Qt slot so  that it may be invoked across a
//  thread boundary.
//
//  Provides a synchronisation Qt slot  which should be implemented in
//  sub-classes in such a way that  normal operation of the rig is not
//  disturbed.  This  is  intended  to   be  use  to  poll  rig  state
//  periodically and changing  VFO to read the other  VFO frequency or
//  mode for  example should  not be  done since  the operator  may be
//  tuning the VFO at the time and would be surprised by an unprompted
//  VFO change.
//
//  Provides a control interface using Qt  slots to start and stop the
//  rig control and PTT connections.
//
//  These  are Qt  slots rather  than the  constructor and  destructor
//  because   it   is   expected   that   the   concrete   Transceiver
//  implementations will run in a  separate thread from where they are
//  constructed.
//
//  Qt signals are defined to notify clients of asynchronous rig state
//  changes and failures.  These can and are expected  to cross thread
//  boundaries.
//
//  A  signal   finished()  is   defined  that   concrete  Transceiver
//  implementations must emit when they are ripe for destruction. This
//  is  intended to  be  used  by clients  that  move the  Transceiver
//  instance to  a thread  and need  to use  QObject::deleteLater() to
//  safely dispose of the Transceiver instance. Implementations should
//  expect Qt  slot calls  after emitting  finished, it  is up  to the
//  implementation whether these slot invocations are ignored.
//
class Transceiver
  : public QObject
{
  Q_OBJECT

public:
  using Frequency = Radio::Frequency;

protected:
  Transceiver (QObject * parent) : QObject {parent} {}

public:
  virtual ~Transceiver () {}

  enum MODE {UNK, CW, CW_R, USB, LSB, FSK, FSK_R, DIG_U, DIG_L, AM, FM, DIG_FM};
  Q_ENUM (MODE)

  //
  // Aggregation of all of the rig and PTT state accessible via this
  // interface.
  //
  class TransceiverState
  {
  public:
    TransceiverState ()
      : online_ {false}
      , rx_frequency_ {0}
      , tx_frequency_ {0}
      , mode_ {UNK}
      , split_ {Split::unknown}
      , ptt_ {false}
      , ft4_mode_ {false}
      , audio_ {false}
      , tx_audio_ {false}
      , tune_ {false}
      , quick_ {false}
      , period_ {120.0}
      , blocksize_ {3456}
      , symbolslength_ {79}
      ,	framespersymbol_ {1920.0}
      ,	trfrequency_ {1500.0}
      ,	tonespacing_ {-3.0}
      ,	synchronize_ {true}
      ,	dbsnr_ {99.}
      ,	trperiod_ {60.0}
      ,	spread_ {0.0}
      ,	nsym_ {79}
      , volume_ {0}
      , level_ {0}
      , power_ {0}
      , swr_ {0}
    {
    }

    bool online () const {return online_;}
    Frequency frequency () const {return rx_frequency_;}
    Frequency tx_frequency () const {return tx_frequency_;}
    bool split () const {return Split::on == split_;}
    MODE mode () const {return mode_;}
    bool ptt () const {return ptt_;}
    bool ft4_mode () const {return ft4_mode_;}
    bool audio () const {return audio_;}
    bool tx_audio () const {return tx_audio_;}
    bool tune () const {return tune_;}
    bool quick () const {return quick_;}
    double period () const {return period_;}
    qint32 blocksize () const {return blocksize_;}
    unsigned symbolslength () const {return symbolslength_;}    
    double framespersymbol () const {return framespersymbol_;}
    double trfrequency () const {return trfrequency_;}
    double tonespacing () const {return tonespacing_;}
    bool synchronize () const {return synchronize_;}
    double dbsnr () const {return dbsnr_;}
    double trperiod () const {return trperiod_;}
    double spread () const {return spread_;}
    int nsym () const {return nsym_;}
    qreal volume () const {return volume_;}
    int level () const {return level_;}
    unsigned int power () const {return power_;}
    unsigned int swr () const {return swr_;}

    void online (bool state) {online_ = state;}
    void frequency (Frequency f) {rx_frequency_ = f;}
    void tx_frequency (Frequency f) {tx_frequency_ = f;}
    void split (bool state) {split_ = state ? Split::on : Split::off;}
    void mode (MODE m) {mode_ = m;}
    void ptt (bool state) {ptt_ = state;}
    void ft4_mode (bool state) {ft4_mode_ = state;}
    void audio (bool state) {audio_ = state;}
    void tx_audio (bool state) {tx_audio_ = state;}
    void tune (bool state) {tune_ = state;}
    void quick (bool state) {quick_ = state;}
    void period (double period) {period_ = period;}
    void blocksize (qint32 blocksize) {blocksize_ = blocksize;}
    void symbolslength (unsigned symbolslength) {symbolslength_ = symbolslength;}
    void framespersymbol (double framespersymbol) {framespersymbol_ = framespersymbol;}
    void trfrequency (double trfrequency) {trfrequency_ = trfrequency;}
    void tonespacing (double tonespacing) {tonespacing_ = tonespacing;}
    void synchronize (bool synchronize) {synchronize_ = synchronize;}
    void dbsnr (double dbsnr) {dbsnr_ = dbsnr;}
    void trperiod (double trperiod) {trperiod_ = trperiod;}
    void spread (double spread) {spread_ = spread;}
    void nsym (int nsym) {nsym_ = nsym;}
    void volume (qreal volume) {volume_ = volume;}
    void level (int strength) {level_ = strength;}
    void power (unsigned int mwpower) {power_ = mwpower;}
    void swr (unsigned int mswr) {swr_ = mswr;}

  private:
    bool online_;
    Frequency rx_frequency_;
    Frequency tx_frequency_;    // 0 means use Rx
    MODE mode_;
    enum class Split {unknown, off, on} split_;
    bool ptt_;
    bool ft4_mode_;
    bool audio_;
    bool tx_audio_;
    bool tune_;
    bool quick_;
    double period_;
    qint32 blocksize_;
    unsigned symbolslength_;
    double framespersymbol_;
    double trfrequency_;
    double tonespacing_;
    bool synchronize_;
    double dbsnr_;
    double trperiod_;
    double spread_;
    int nsym_;
    qreal volume_;
    int level_;
    unsigned int power_;
    unsigned int swr_;
    
    // Don't forget to update the debug print and != operator if you
    // add more members here

    friend QDebug operator << (QDebug, TransceiverState const&);
    friend bool operator != (TransceiverState const&, TransceiverState const&);
  };

  //
  // The following  slots and signals are  expected to all run  in the
  // same thread which  is not necessarily the main GUI  thread. It is
  // up  to  the client  of  the  Transceiver  class to  organise  the
  // allocation to a thread and the lifetime of the object instances.
  //

  // Apply  state changes  to the  rig. The  sequence_number parameter
  // will  be included  in  any status  updates  generated after  this
  // transaction  is processed.  The sequence  number may  be used  to
  // ignore any status  updates until the results  of this transaction
  // have been processed thus avoiding any unwanted "ping-pong" due to
  // signals crossing in transit.
  Q_SLOT virtual void set (Transceiver::TransceiverState const&,
                           unsigned sequence_number) noexcept = 0;

  // Connect and disconnect.
  Q_SLOT virtual void start (unsigned sequence_number,JTDXDateTime* jtdxtime) noexcept = 0;
  Q_SLOT virtual void stop () noexcept = 0;

  //
  // asynchronous status updates
  //

  // 0 - 1Hz
  // 1 - 10Hz rounded
  // -1 - 10Hz truncated
  // 2 - 100Hz rounded
  // -2 - 100Hz truncated
  Q_SIGNAL void resolution (int);

  // rig audio data transfer
  Q_SIGNAL void tciframeswritten (qint64);

  // rig audio data transfer
  Q_SIGNAL void tci_mod_active (bool);

  // rig state changed
  Q_SIGNAL void update (Transceiver::TransceiverState const&,
                        unsigned sequence_number) const;
  // something went wrong - not recoverable, start new instance
  Q_SIGNAL void failure (QString const& reason) const;

  // Ready to be destroyed.
  Q_SIGNAL void finished () const;
};

Q_DECLARE_METATYPE (Transceiver::TransceiverState);

#if !defined (QT_NO_DEBUG_STREAM)
QDebug operator << (QDebug, Transceiver::TransceiverState const&);
#endif

ENUM_QDATASTREAM_OPS_DECL (Transceiver, MODE);
ENUM_CONVERSION_OPS_DECL (Transceiver, MODE);

bool operator != (Transceiver::TransceiverState const&, Transceiver::TransceiverState const&);
bool operator == (Transceiver::TransceiverState const&, Transceiver::TransceiverState const&);

#endif
