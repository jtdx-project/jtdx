#ifndef DX_LAB_SUITE_COMMANDER_TRANSCEIVER_HPP__
#define DX_LAB_SUITE_COMMANDER_TRANSCEIVER_HPP__

#include <memory>

#include "TransceiverFactory.hpp"
#include "PollingTransceiver.hpp"

class QTcpSocket;
class QByteArray;
class QString;

//
// DX Lab Suite Commander Interface
//
// Implemented as a Transceiver decorator  because we may want the PTT
// services of another Transceiver  type such as the HamlibTransceiver
// which can  be enabled by wrapping  a HamlibTransceiver instantiated
// as a "Hamlib Dummy" transceiver in the Transceiver factory method.
//
class DXLabSuiteCommanderTransceiver final
  : public PollingTransceiver
{
  Q_OBJECT;                     // for translation context

public:
  static void register_transceivers (TransceiverFactory::Transceivers *, int id);

  // takes ownership of wrapped Transceiver
  explicit DXLabSuiteCommanderTransceiver (std::unique_ptr<TransceiverBase> wrapped,
                                           QString const& address, bool use_for_ptt,
                                           int poll_interval, QObject * parent = nullptr);

protected:
  int do_start () override;
  void do_stop () override;
  void do_frequency (Frequency, MODE, bool no_ignore) override;
  void do_tx_frequency (Frequency, MODE, bool no_ignore) override;
  void do_mode (MODE) override;
  void do_ptt (bool on) override;

  void poll () override;

private:
  MODE get_mode (bool no_debug = false);
  void simple_command (QString const&, bool no_debug = false);
  QString command_with_reply (QString const&, bool no_debug = false);
  bool write_to_port (QString const&);
  QString frequency_to_string (Frequency) const;
  Frequency string_to_frequency (QString) const;

  std::unique_ptr<TransceiverBase> wrapped_; // may be null
  bool use_for_ptt_;
  QString server_;
  QTcpSocket * commander_;
  QLocale locale_;
};

#endif
