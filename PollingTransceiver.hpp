#ifndef POLLING_TRANSCEIVER_HPP__
#define POLLING_TRANSCEIVER_HPP__

#include <QObject>

#include "TransceiverBase.hpp"

class QTimer;

//
// Polling Transceiver
//
//  Helper base  class that  encapsulates the emulation  of continuous
//  update and caching of a transceiver state.
//
// Collaborations
//
//  Implements the TransceiverBase post  action interface and provides
//  the abstract  poll() operation  for sub-classes to  implement. The
//  poll operation is invoked every poll_interval seconds.
//
// Responsibilities
//
//  Because some rig interfaces don't immediately update after a state
//  change request; this  class allows a rig a few  polls to stabilise
//  to the  requested state before  signalling the change.  This means
//  that  clients don't  see  intermediate states  that are  sometimes
//  inaccurate,  e.g. changing  the split  TX frequency  on Icom  rigs
//  requires a  VFO switch  and polls while  switched will  return the
//  wrong current frequency.
//
class PollingTransceiver
  : public TransceiverBase
{
  Q_OBJECT;                     // for translation context

protected:
  explicit PollingTransceiver (int poll_interval, // in seconds
                               QObject * parent);

protected:
  // Sub-classes implement this and fetch what they can from the rig
  // in a non-intrusive manner.
  virtual void do_poll () = 0;

  void do_post_start (JTDXDateTime*) override final;
  void do_post_stop () override final;
  void do_post_frequency (Frequency, MODE) override final;
  void do_post_tx_frequency (Frequency, MODE) override final;
  void do_post_mode (MODE) override final;
  void do_post_ptt (bool = true) override final;
  void do_post_ft4_mode (bool = false) override final;
  bool do_pre_update () override final;

private:
  void start_timer ();
  void stop_timer ();

  Q_SLOT void handle_timeout ();

  int interval_;    // polling interval in milliseconds
  int set_interval_;
  QTimer * poll_timer_;
  bool ft4_mode_;
  bool fast_mode_;
  // keep a record of the last state signalled so we can elide
  // duplicate updates
  Transceiver::TransceiverState last_signalled_state_;

  // keep a record of expected state so we can compare with actual
  // updates to determine when state changes have bubbled through
  Transceiver::TransceiverState next_state_;

  unsigned retries_;            // number of incorrect polls left
  std::string debug_file_;
  JTDXDateTime * m_jtdxtime;

};

#endif
