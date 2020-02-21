#ifndef EMULATE_SPLIT_TRANSCEIVER_HPP__
#define EMULATE_SPLIT_TRANSCEIVER_HPP__

#include <memory>

#include "Transceiver.hpp"

//
// Emulate Split Transceiver
//
// Helper decorator class that encapsulates  the emulation of split TX
// operation.
//
// Responsibilities
//
//  Delegates  all  but setting  of  other  (split) frequency  to  the
//  wrapped Transceiver instance. Also routes failure signals from the
//  wrapped Transceiver instance to this instances failure signal.
//
//  Intercepts status  updates from  the wrapped  Transceiver instance
//  and re-signals it with the emulated status.
//
//  Generates a status update signal if the other (split) frequency is
//  changed, this is necessary  since the wrapped transceiver instance
//  never receives other frequency changes.
//
class EmulateSplitTransceiver final
  : public Transceiver
{
public:
  // takes ownership of wrapped Transceiver
  explicit EmulateSplitTransceiver (std::unique_ptr<Transceiver> wrapped,
                                    QObject * parent = nullptr);

  void set (TransceiverState const&,
            unsigned sequence_number) noexcept override;

  // forward everything else to wrapped Transceiver
  void start (unsigned sequence_number) noexcept override {wrapped_->start (sequence_number);}
  void stop () noexcept override {wrapped_->stop ();}

private:
  void handle_update (TransceiverState const&, unsigned seqeunce_number);

  std::unique_ptr<Transceiver> wrapped_;
  Frequency rx_frequency_;        // requested Rx frequency
  Frequency tx_frequency_;        // requested Tx frequency
  bool split_; // requested split state
};

#endif
