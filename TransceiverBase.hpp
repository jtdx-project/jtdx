#ifndef TRANSCEIVER_BASE_HPP__
#define TRANSCEIVER_BASE_HPP__

#include <stdexcept>

#include <QString>
#include <QDir>
#include <QStandardPaths>
#include "Transceiver.hpp"
#include "JTDXDateTime.h"

//
// Base Transceiver Implementation
//
//  Behaviour common to all Transceiver implementations.
//
// Collaborations
//
//  Implements the Transceiver abstract  interface as template methods
//  and provides  a new abstract interface  with similar functionality
//  (do_XXXXX operations). Provides and  calls abstract interface that
//  gets  called post  the above  operations (do_post_XXXXX)  to allow
//  caching implementation etc.
//
//  A  key factor  is  to  catch all  exceptions  thrown by  sub-class
//  implementations where  the template method  is a Qt slot  which is
//  therefore  likely  to  be  called   by  Qt  which  doesn't  handle
//  exceptions. Any exceptions are converted to Transceiver::failure()
//  signals.
//
//  Sub-classes update the stored state via a protected interface.
//
// Responsibilities:
//
//  Wrap incoming  Transceiver messages catching all  exceptions in Qt
//  slot driven  messages and converting  them to Qt signals.  This is
//  done because exceptions  make concrete Transceiver implementations
//  simpler  to   write,  but  exceptions  cannot   cross  signal/slot
//  boundaries  (especially across  threads).  This  also removes  any
//  requirement for the client code to handle exceptions.
//
//  Maintain the  state of the  concrete Transceiver instance  that is
//  passed back via  the Transceiver::update(TransceiverState) signal,
//  it   is  still   the   responsibility   of  concrete   Transceiver
//  implementations to emit  the state_change signal when  they have a
//  status update.
//
//  Maintain    a   go/no-go    status   for    concrete   Transceiver
//  implementations  ensuring only  a valid  sequence of  messages are
//  passed. A concrete Transceiver instance  must be started before it
//  can receive  messages, any exception thrown  takes the Transceiver
//  offline.
//
//  Implements methods  that concrete Transceiver  implementations use
//  to update the Transceiver state.  These do not signal state change
//  to  clients  as  this  is   the  responsibility  of  the  concrete
//  Transceiver implementation, thus allowing multiple state component
//  updates to be signalled together if required.
//
class TransceiverBase
  : public Transceiver
{
  Q_OBJECT;

protected:
  TransceiverBase (QObject * parent)
    : Transceiver {parent}
    , jtdxtime_ {nullptr}
    , last_sequence_number_ {0}
    , debug_file_ {QDir(QStandardPaths::writableLocation (QStandardPaths::DataLocation)).absoluteFilePath ("jtdx_debug.txt").toStdString()}
  {}

public:
  //
  // Implement the Transceiver abstract interface.
  //
  void start (unsigned sequence_number, JTDXDateTime* jtdxdatetime) noexcept override final;
  void set (TransceiverState const&,
            unsigned sequence_number) noexcept override final;
  void stop () noexcept override final;

  //
  // Query operations
  //
  TransceiverState const& state () const {return actual_;}

protected:
  //
  // Error exception which is thrown to signal unexpected errors.
  //
  struct error
    : public std::runtime_error
  {
    explicit error (char const * const msg) : std::runtime_error (msg) {}
    explicit error (QString const& msg) : std::runtime_error (msg.toStdString ()) {}
  };

  // Template methods that sub classes implement to do what they need to do.
  //
  // These methods may throw exceptions to signal errors.
  virtual int do_start (JTDXDateTime*) = 0;  // returns resolution, See Transceiver::resolution
  virtual void do_post_start (JTDXDateTime*) {}

  virtual void do_stop () = 0;
  virtual void do_post_stop () {}

  virtual void do_frequency (Frequency, MODE, bool no_ignore) = 0;
  virtual void do_post_frequency (Frequency, MODE) {}

  virtual void do_tx_frequency (Frequency, MODE, bool no_ignore) = 0;
  virtual void do_post_tx_frequency (Frequency, MODE) {}

  virtual void do_mode (MODE) = 0;
  virtual void do_post_mode (MODE) {}

  virtual void do_ptt (bool = true) = 0;
  virtual void do_post_ptt (bool = true) {}

  virtual void do_post_ft4_mode (bool = false) {}

  virtual void do_audio (bool = false) {}
  virtual void do_tune (bool = false) {}
  virtual void do_period (double) {}
  virtual void do_blocksize (qint32) {}
  virtual void do_spread(double) {}
  virtual void do_nsym(int) {}
  virtual void do_trfrequency(double) {}
  virtual void do_txvolume (qreal) {}
  virtual void do_modulator_start(unsigned, double, double, double, bool, double, double) {}
  virtual void do_modulator_stop(bool) {}

  virtual bool do_pre_update () {return true;}

  // sub classes report rig state changes with these methods
  void update_rx_frequency (Frequency);
  void update_other_frequency (Frequency = 0);
  void update_split (bool);
  void update_mode (MODE);
  void update_PTT (bool = true);
  void update_level (int = 0);
  void update_power (unsigned int = 0);
  void update_swr (unsigned int = 0);
  // Calling this eventually triggers the Transceiver::update(State) signal.
  void update_complete (bool force_signal = false);

  // sub class may asynchronously take the rig offline by calling this
  void offline (QString const& reason);

private:
  void startup ();
  void shutdown ();
  bool maybe_low_resolution (Frequency low_res, Frequency high_res);
  JTDXDateTime * jtdxtime_;
  // use this convenience class to notify in update methods
  class may_update
  {
  public:
    explicit may_update (TransceiverBase * self, bool force_signal = false)
      : self_ {self}
      , force_signal_ {force_signal}
    {}
    ~may_update () {self_->update_complete (force_signal_);}
  private:
    TransceiverBase * self_;
    bool force_signal_;
  };

  TransceiverState requested_;
  TransceiverState actual_;
  TransceiverState last_;
  unsigned last_sequence_number_;    // from set state operation
  std::string debug_file_;
};

// some trace macros
#if WSJT_TRACE_CAT
#define TRACE_CAT(FAC, MSG) qDebug () << QString {"%1::%2:"}.arg ((FAC)).arg (__func__) << MSG
#else
#define TRACE_CAT(FAC, MSG)
#endif

#if WSJT_TRACE_CAT && WSJT_TRACE_CAT_POLLS
#define TRACE_CAT_POLL(FAC, MSG) qDebug () << QString {"%1::%2:"}.arg ((FAC)).arg (__func__) << MSG
#else
#define TRACE_CAT_POLL(FAC, MSG)
#endif

#endif
