#include "OmniRigTransceiver.hpp"

#include <QDebug>
#include <objbase.h>
#include <QThread>
#include <QEventLoop>

#include "qt_helpers.hpp"

#include "moc_OmniRigTransceiver.cpp"

namespace
{
  auto constexpr OmniRig_transceiver_one_name = "OmniRig Rig 1";
  auto constexpr OmniRig_transceiver_two_name = "OmniRig Rig 2";
}

auto OmniRigTransceiver::map_mode (OmniRig::RigParamX param) -> MODE
{
  if (param & OmniRig::PM_CW_U)
    {
      return CW_R;
    }
  else if (param & OmniRig::PM_CW_L)
    {
      return CW;
    }
  else if (param & OmniRig::PM_SSB_U)
    {
      return USB;
    }
  else if (param & OmniRig::PM_SSB_L)
    {
      return LSB;
    }
  else if (param & OmniRig::PM_DIG_U)
    {
      return DIG_U;
    }
  else if (param & OmniRig::PM_DIG_L)
    {
      return DIG_L;
    }
  else if (param & OmniRig::PM_AM)
    {
      return AM;
    }
  else if (param & OmniRig::PM_FM)
    {
      return FM;
    }
  TRACE_CAT ("OmniRigTransceiver", "unrecognized mode");
  throw_qstring (tr ("OmniRig: unrecognized mode"));
  return UNK;
}

OmniRig::RigParamX OmniRigTransceiver::map_mode (MODE mode)
{
  switch (mode)
    {
    case AM: return OmniRig::PM_AM;
    case CW: return OmniRig::PM_CW_L;
    case CW_R: return OmniRig::PM_CW_U;
    case USB: return OmniRig::PM_SSB_U;
    case LSB: return OmniRig::PM_SSB_L;
    case FSK: return OmniRig::PM_DIG_L;
    case FSK_R: return OmniRig::PM_DIG_U;
    case DIG_L: return OmniRig::PM_DIG_L;
    case DIG_U: return OmniRig::PM_DIG_U;
    case FM: return OmniRig::PM_FM;
    case DIG_FM: return OmniRig::PM_FM;
    default: break;
    }
  return OmniRig::PM_SSB_U; // quieten compiler grumble
}

void OmniRigTransceiver::register_transceivers (TransceiverFactory::Transceivers * registry, unsigned id1, unsigned id2)
{
  (*registry)[OmniRig_transceiver_one_name] = TransceiverFactory::Capabilities {
    id1
    , TransceiverFactory::Capabilities::none // COM isn't serial or network
    , true             // does PTT
    , false            // doesn't select mic/data (use OmniRig config file)
    , true             // can remote control RTS nd DTR
    , true             // asynchronous interface
  };
  (*registry)[OmniRig_transceiver_two_name] = TransceiverFactory::Capabilities {
    id2
    , TransceiverFactory::Capabilities::none // COM isn't serial or network
    , true             // does PTT
    , false            // doesn't select mic/data (use OmniRig config file)
    , true             // can remote control RTS nd DTR
    , true             // asynchronous interface
  };
}

OmniRigTransceiver::OmniRigTransceiver (std::unique_ptr<TransceiverBase> wrapped,
                                        RigNumber n, TransceiverFactory::PTTMethod ptt_type,
                                        QString const& ptt_port, QObject * parent)
  : TransceiverBase {parent}
  , wrapped_ {std::move (wrapped)}
  , use_for_ptt_ {TransceiverFactory::PTT_method_CAT == ptt_type || ("CAT" == ptt_port && (TransceiverFactory::PTT_method_RTS == ptt_type || TransceiverFactory::PTT_method_DTR == ptt_type))}
  , ptt_type_ {ptt_type}
  , rig_number_ {n}
  , readable_params_ {0}
  , writable_params_ {0}
  , send_update_signal_ {false}
  , reversed_ {false}
  , m_jtdxtime {nullptr}
{
}

// returns false on time out
bool OmniRigTransceiver::await_notification_with_timeout (int timeout)
{
  QEventLoop el;
  connect (this, &OmniRigTransceiver::notified, &el, [&el] () {el.exit (1);});
  QTimer::singleShot (timeout, Qt::CoarseTimer, &el, [&el] () {el.exit (0);});
  return 1 == el.exec ();       // wait for notify or timer
}

int OmniRigTransceiver::do_start (JTDXDateTime * jtdxtime)
{
  TRACE_CAT ("OmniRigTransceiver", "starting");
  m_jtdxtime = jtdxtime;
  if (wrapped_) wrapped_->start (0,m_jtdxtime);

  CoInitializeEx (nullptr, 0 /*COINIT_APARTMENTTHREADED*/); // required because Qt only does this for GUI thread

  omni_rig_.reset (new OmniRig::OmniRigX {this});
  if (omni_rig_->isNull ())
    {
      TRACE_CAT ("OmniRigTransceiver", "failed to start COM server");
      throw_qstring (tr ("Failed to start OmniRig COM server"));
    }

  // COM/OLE exceptions get signaled
  connect (&*omni_rig_, SIGNAL (exception (int, QString, QString, QString)), this, SLOT (handle_COM_exception (int, QString, QString, QString)));

  // IOmniRigXEvent interface signals
  connect (&*omni_rig_, SIGNAL (VisibleChange ()), this, SLOT (handle_visible_change ()));
  connect (&*omni_rig_, SIGNAL (RigTypeChange (int)), this, SLOT (handle_rig_type_change (int)));
  connect (&*omni_rig_, SIGNAL (StatusChange (int)), this, SLOT (handle_status_change (int)));
  connect (&*omni_rig_, SIGNAL (ParamsChange (int, int)), this, SLOT (handle_params_change (int, int)));
  connect (&*omni_rig_
           , SIGNAL (CustomReply (int, QVariant const&, QVariant const&))
           , this, SLOT (handle_custom_reply (int, QVariant const&, QVariant const&)));

  TRACE_CAT ("OmniRigTransceiver", "OmniRig s/w version:" << QString::number (omni_rig_->SoftwareVersion ()).toLocal8Bit ()
             << "i/f version:" << QString::number (omni_rig_->InterfaceVersion ()).toLocal8Bit ());

  // fetch the interface of the RigX CoClass and instantiate a proxy object
  switch (rig_number_)
    {
    case One: rig_.reset (new OmniRig::RigX (omni_rig_->Rig1 ())); break;
    case Two: rig_.reset (new OmniRig::RigX (omni_rig_->Rig2 ())); break;
    }

  Q_ASSERT (rig_);
  Q_ASSERT (!rig_->isNull ());

  offline_timer_.reset (new QTimer); // instantiate here as
                                     // constructor runs in wrong
                                     // thread
  offline_timer_->setSingleShot (true);
  connect (offline_timer_.data (), &QTimer::timeout, [this] () {offline ("Rig went offline");});

  if (use_for_ptt_ && (TransceiverFactory::PTT_method_DTR == ptt_type_ || TransceiverFactory::PTT_method_RTS == ptt_type_))
    {
      // fetch the interface for the serial port if we need it for PTT
      port_.reset (new OmniRig::PortBits (rig_->PortBits ()));

      Q_ASSERT (port_);
      Q_ASSERT (!port_->isNull ());
      TRACE_CAT ("OmniRigTransceiver", "OmniRig RTS state:" << port_->Rts ());

      if (!port_->Lock ()) // try to take exclusive use of the OmniRig serial port for PTT
        {
          TRACE_CAT ("OmniRigTransceiver", "Failed to get exclusive use of serial port for PTT from OmniRig");
        }

      // start off so we don't accidentally key the radio
      if (TransceiverFactory::PTT_method_DTR == ptt_type_)
        {
          port_->SetDtr (false);
        }
      else      // RTS
        {
          port_->SetRts (false);
        }
    }

  rig_type_ = rig_->RigType ();
  readable_params_ = rig_->ReadableParams ();
  writable_params_ = rig_->WriteableParams ();

  TRACE_CAT ("OmniRigTransceiver", QString {"OmniRig initial rig type: %1 readable params = 0x%2 writable params = 0x%3 for rig %4"}
    .arg (rig_type_)
    .arg (readable_params_, 8, 16, QChar ('0'))
    .arg (writable_params_, 8, 16, QChar ('0'))
    .arg (rig_number_).toLocal8Bit ());
  for (int i = 0; i < 5; ++i)
    {
      if (OmniRig::ST_ONLINE == rig_->Status ())
        {
          break;
        }
      await_notification_with_timeout (1000);
    }
  if (OmniRig::ST_ONLINE != rig_->Status ())
    {
      throw_qstring ("OmniRig: " + rig_->StatusStr ());
    }
  QThread::msleep (500);        // leave some time for Omni-Rig to get
                                // the rig status for the 1st. time
  auto f = rig_->GetRxFrequency ();
  for (int i = 0; (f == 0) && (i < 5); ++i)
    {
      await_notification_with_timeout (1000);
      f = rig_->GetRxFrequency ();
    }
  update_rx_frequency (f);
  int resolution {0};
  if (OmniRig::PM_UNKNOWN == rig_->Vfo ()
      && (writable_params_ & (OmniRig::PM_VFOA | OmniRig::PM_VFOB))
      == (OmniRig::PM_VFOA | OmniRig::PM_VFOB))
    {
      // start with VFO A (probably MAIN) on rigs that we
      // can't query VFO but can set explicitly
      rig_->SetVfo (OmniRig::PM_VFOA);
    }
  f = state ().frequency ();
  if (f % 10) return resolution; // 1Hz resolution
  auto test_frequency = f - f % 100 + 55;
  if (OmniRig::PM_FREQ & writable_params_)
    {
      rig_->SetFreq (test_frequency);
    }
  else if (reversed_ && (OmniRig::PM_FREQB & writable_params_))
    {
      rig_->SetFreqB (test_frequency);
    }
  else if (!reversed_ && (OmniRig::PM_FREQA & writable_params_))
    {
      rig_->SetFreqA (test_frequency);
    }
  else
    {
      throw_qstring (tr ("OmniRig: don't know how to set rig frequency"));
    }
  if (!await_notification_with_timeout (1000))
    {
      TRACE_CAT ("OmniRigTransceiver", "do_start 1: wait timed out");
      throw_qstring (tr ("OmniRig: timeout waiting for update from rig"));
    }
  switch (rig_->GetRxFrequency () - test_frequency)
    {
    case -5: resolution = -1; break;  // 10Hz truncated
    case 5: resolution = 1; break;    // 10Hz rounded
    case -15: resolution = -2; break; // 20Hz truncated
    case -55: resolution = -2; break; // 100Hz truncated
    case 45: resolution = 2; break;   // 100Hz rounded
    }
  if (1 == resolution)  // may be 20Hz rounded
    {
      test_frequency = f - f % 100 + 51;
      if (OmniRig::PM_FREQ & writable_params_)
        {
          rig_->SetFreq (test_frequency);
        }
      else if (reversed_ && (OmniRig::PM_FREQB & writable_params_))
        {
          rig_->SetFreqB (test_frequency);
        }
      else if (!reversed_ && (OmniRig::PM_FREQA & writable_params_))
        {
          rig_->SetFreqA (test_frequency);
        }
      if (!await_notification_with_timeout (2000))
        {
          TRACE_CAT ("OmniRigTransceiver", "do_start 2: wait timed out");
          throw_qstring (tr ("OmniRig: timeout waiting for update from rig"));
        }
      if (9 == rig_->GetRxFrequency () - test_frequency)
        {
          resolution = 2;   // 20Hz rounded
        }
    }
  if (OmniRig::PM_FREQ & writable_params_)
    {
      rig_->SetFreq (f);
    }
  else if (reversed_ && (OmniRig::PM_FREQB & writable_params_))
    {
      rig_->SetFreqB (f);
    }
  else if (!reversed_ && (OmniRig::PM_FREQA & writable_params_))
    {
      rig_->SetFreqA (f);
    }
  update_rx_frequency (f);
  return resolution;
}

void OmniRigTransceiver::do_stop ()
{
  QThread::msleep (200);        // leave some time for pending
                                // commands at the server end

  offline_timer_.reset ();      // destroy here rather than in
                                // destructor as destructor runs in
                                // wrong thread

  if (port_)
    {
      port_->Unlock ();   // release serial port
      port_->clear ();
      port_.reset ();
    }
  if (omni_rig_)
    {
      if (rig_)
        {
          rig_->clear ();
          rig_.reset ();
        }
      omni_rig_->clear ();
      omni_rig_.reset ();
      CoUninitialize ();
    }
  if (wrapped_) wrapped_->stop ();
  TRACE_CAT ("OmniRigTransceiver", "stopped");
}

void OmniRigTransceiver::handle_COM_exception (int code, QString source, QString desc, QString help)
{
  TRACE_CAT ("OmniRigTransceiver", QString::number (code) + " at " + source + ": " + desc + " (" + help + ')');
  throw_qstring (tr ("OmniRig COM/OLE error: %1 at %2: %3 (%4)").arg (QString::number (code)).arg (source). arg (desc). arg (help));
}

void OmniRigTransceiver::handle_visible_change ()
{
  if (!omni_rig_ || omni_rig_->isNull ()) return;
  TRACE_CAT ("OmniRigTransceiver", "visibility change: visibility =" << omni_rig_->DialogVisible ());
}

void OmniRigTransceiver::handle_rig_type_change (int rig_number)
{
  if (!omni_rig_ || omni_rig_->isNull ()) return;
  TRACE_CAT ("OmniRigTransceiver", "rig type change: rig =" << rig_number);
  if (rig_number_ == rig_number)
    {
      if (!rig_ || rig_->isNull ()) return;
      readable_params_ = rig_->ReadableParams ();
      writable_params_ = rig_->WriteableParams ();
      TRACE_CAT ("OmniRigTransceiver", QString {"rig type change to: %1 readable params = 0x%2 writable params = 0x%3 for rig %4"}
        .arg (rig_->RigType ())
        .arg (readable_params_, 8, 16, QChar ('0'))
        .arg (writable_params_, 8, 16, QChar ('0'))
        .arg (rig_number).toLocal8Bit ());
    }
}

void OmniRigTransceiver::handle_status_change (int rig_number)
{
  if (!omni_rig_ || omni_rig_->isNull ()) return;
  TRACE_CAT ("OmniRigTransceiver", QString {"status change for rig %1"}.arg (rig_number).toLocal8Bit ());
  if (rig_number_ == rig_number)
    {
      if (!rig_ || rig_->isNull ()) return;
      auto const& status = rig_->StatusStr ().toLocal8Bit ();
      TRACE_CAT ("OmniRigTransceiver", "OmniRig status change: new status = " << status);
      if (OmniRig::ST_ONLINE != rig_->Status ())
        {
          if (!offline_timer_->isActive ())
            {
              // Omni-Rig is prone to reporting the rig offline and
              // then recovering autonomously, so we will give it a
              // few seconds to make its mind up
              offline_timer_->start (10000);
            }
        }
      else
        {
          offline_timer_->stop (); // good to go again
          Q_EMIT notified ();
        }
      // else
      //   {
      //     update_rx_frequency (rig_->GetRxFrequency ());
      //     update_complete ();
      //     TRACE_CAT ("OmniRigTransceiver", "frequency:" << state ().frequency ());
      //   }
    }
}

void OmniRigTransceiver::handle_params_change (int rig_number, int params)
{
  if (!omni_rig_ || omni_rig_->isNull ()) return;
  TRACE_CAT ("OmniRigTransceiver", QString {"params change: params = 0x%1 for rig %2"}
        .arg (params, 8, 16, QChar ('0'))
        .arg (rig_number).toLocal8Bit ()
        << "state before:" << state ());
  if (rig_number_ == rig_number)
    {
      if (!rig_ || rig_->isNull ()) return;
      //      starting_ = false;
      TransceiverState old_state {state ()};
      auto need_frequency = false;

      if (params & OmniRig::PM_VFOAA)
        {
          TRACE_CAT ("OmniRigTransceiver", "VFOAA");
          update_split (false);
          reversed_ = false;
          update_rx_frequency (rig_->FreqA ());
          update_other_frequency (rig_->FreqB ());
        }
      if (params & OmniRig::PM_VFOAB)
        {
          TRACE_CAT ("OmniRigTransceiver", "VFOAB");
          update_split (true);
          reversed_ = false;
          update_rx_frequency (rig_->FreqA ());
          update_other_frequency (rig_->FreqB ());
        }
      if (params & OmniRig::PM_VFOBA)
        {
          TRACE_CAT ("OmniRigTransceiver", "VFOBA");
          update_split (true);
          reversed_ = true;
          update_other_frequency (rig_->FreqA ());
          update_rx_frequency (rig_->FreqB ());
        }
      if (params & OmniRig::PM_VFOBB)
        {
          TRACE_CAT ("OmniRigTransceiver", "VFOBB");
          update_split (false);
          reversed_ = true;
          update_other_frequency (rig_->FreqA ());
          update_rx_frequency (rig_->FreqB ());
        }
      if (params & OmniRig::PM_VFOA)
        {
          TRACE_CAT ("OmniRigTransceiver", "VFOA");
          reversed_ = false;
          need_frequency = true;
        }
      if (params & OmniRig::PM_VFOB)
        {
          TRACE_CAT ("OmniRigTransceiver", "VFOB");
          reversed_ = true;
          need_frequency = true;
        }

      if (params & OmniRig::PM_FREQ)
        {
          TRACE_CAT ("OmniRigTransceiver", "FREQ");
          need_frequency = true;
        }
      if (params & OmniRig::PM_FREQA)
        {
          auto f = rig_->FreqA ();
          TRACE_CAT ("OmniRigTransceiver", "FREQA = " << f);
          if (reversed_)
            {
              update_other_frequency (f);
            }
          else
            {
              update_rx_frequency (f);
            }
        }
      if (params & OmniRig::PM_FREQB)
        {
          auto f = rig_->FreqB ();
          TRACE_CAT ("OmniRigTransceiver", "FREQB = " << f);
          if (reversed_)
            {
              update_rx_frequency (f);
            }
          else
            {
              update_other_frequency (f);
            }
        }
      if (need_frequency)
        {
          if (readable_params_ & OmniRig::PM_FREQA)
            {
              auto f = rig_->FreqA ();
              if (f)
                {
                  TRACE_CAT ("OmniRigTransceiver", "FREQA = " << f);
                  if (reversed_)
                    {
                      update_other_frequency (f);
                    }
                  else
                    {
                      update_rx_frequency (f);
                    }
                }
            }
          if (readable_params_ & OmniRig::PM_FREQB)
            {
              auto f = rig_->FreqB ();
              if (f)
                {
                  TRACE_CAT ("OmniRigTransceiver", "FREQB = " << f);
                  if (reversed_)
                    {
                      update_rx_frequency (f);
                    }
                  else
                    {
                      update_other_frequency (f);
                    }
                }
            }
          if (readable_params_ & OmniRig::PM_FREQ && !state ().ptt ())
            {
              auto f = rig_->Freq ();
              if (f)
                {
                  TRACE_CAT ("OmniRigTransceiver", "FREQ = " << f);
                  update_rx_frequency (f);
                }
            }
        }
      if (params & OmniRig::PM_PITCH)
        {
          TRACE_CAT ("OmniRigTransceiver", "PITCH");
        }
      if (params & OmniRig::PM_RITOFFSET)
        {
          TRACE_CAT ("OmniRigTransceiver", "RITOFFSET");
        }
      if (params & OmniRig::PM_RIT0)
        {
          TRACE_CAT ("OmniRigTransceiver", "RIT0");
        }
      if (params & OmniRig::PM_VFOEQUAL)
        {
          auto f = readable_params_ & OmniRig::PM_FREQA ? rig_->FreqA () : rig_->Freq ();
          auto m = map_mode (rig_->Mode ());
          TRACE_CAT ("OmniRigTransceiver", QString {"VFOEQUAL f=%1 m=%2"}.arg (f).arg (m));
          update_rx_frequency (f);
          update_other_frequency (f);
          update_mode (m);
        }
      if (params & OmniRig::PM_VFOSWAP)
        {
          TRACE_CAT ("OmniRigTransceiver", "VFOSWAP");
          auto f = state ().tx_frequency ();
          update_other_frequency (state ().frequency ());
          update_rx_frequency (f);
          update_mode (map_mode (rig_->Mode ()));
        }
      if (params & OmniRig::PM_SPLITON)
        {
          TRACE_CAT ("OmniRigTransceiver", "SPLITON");
          update_split (true);
        }
      if (params & OmniRig::PM_SPLITOFF)
        {
          TRACE_CAT ("OmniRigTransceiver", "SPLITOFF");
          update_split (false);
        }
      if (params & OmniRig::PM_RITON)
        {
          TRACE_CAT ("OmniRigTransceiver", "RITON");
        }
      if (params & OmniRig::PM_RITOFF)
        {
          TRACE_CAT ("OmniRigTransceiver", "RITOFF");
        }
      if (params & OmniRig::PM_XITON)
        {
          TRACE_CAT ("OmniRigTransceiver", "XITON");
        }
      if (params & OmniRig::PM_XITOFF)
        {
          TRACE_CAT ("OmniRigTransceiver", "XITOFF");
        }
      if (params & OmniRig::PM_RX)
        {
          TRACE_CAT ("OmniRigTransceiver", "RX");
          update_PTT (false);
        }
      if (params & OmniRig::PM_TX)
        {
          TRACE_CAT ("OmniRigTransceiver", "TX");
          update_PTT ();
        }
      if (params & OmniRig::PM_CW_U)
        {
          TRACE_CAT ("OmniRigTransceiver", "CW-R");
          update_mode (CW_R);
        }
      if (params & OmniRig::PM_CW_L)
        {
          TRACE_CAT ("OmniRigTransceiver", "CW");
          update_mode (CW);
        }
      if (params & OmniRig::PM_SSB_U)
        {
          TRACE_CAT ("OmniRigTransceiver", "USB");
          update_mode (USB);
        }
      if (params & OmniRig::PM_SSB_L)
        {
          TRACE_CAT ("OmniRigTransceiver", "LSB");
          update_mode (LSB);
        }
      if (params & OmniRig::PM_DIG_U)
        {
          TRACE_CAT ("OmniRigTransceiver", "DATA-U");
          update_mode (DIG_U);
        }
      if (params & OmniRig::PM_DIG_L)
        {
          TRACE_CAT ("OmniRigTransceiver", "DATA-L");
          update_mode (DIG_L);
        }
      if (params & OmniRig::PM_AM)
        {
          TRACE_CAT ("OmniRigTransceiver", "AM");
          update_mode (AM);
        }
      if (params & OmniRig::PM_FM)
        {
          TRACE_CAT ("OmniRigTransceiver", "FM");
          update_mode (FM);
        }

      if (old_state != state () || send_update_signal_)
        {
          update_complete ();
          send_update_signal_ = false;
        }
      TRACE_CAT ("OmniRigTransceiver", "OmniRig params change: state after:" << state ());
    }
  Q_EMIT notified ();
}

void OmniRigTransceiver::handle_custom_reply (int rig_number, QVariant const& command, QVariant const& reply)
{
  (void)command;
  (void)reply;

  if (!omni_rig_ || omni_rig_->isNull ()) return;
  if (rig_number_ == rig_number)
    {
      if (!rig_ || rig_->isNull ()) return;
      TRACE_CAT ("OmniRigTransceiver", "custom command" << command.toString ().toLocal8Bit ()
                 << "with reply" << reply.toString ().toLocal8Bit ()
                 << QString ("for rig %1").arg (rig_number).toLocal8Bit ());
      TRACE_CAT ("OmniRigTransceiver", "rig number:" << rig_number_ << ':' << state ());
    }
}

void OmniRigTransceiver::do_ptt (bool on)
{
  TRACE_CAT ("OmniRigTransceiver", on << state ());
  if (use_for_ptt_ && TransceiverFactory::PTT_method_CAT == ptt_type_)
    {
      TRACE_CAT ("OmniRigTransceiver", "set PTT");
      rig_->SetTx (on ? OmniRig::PM_TX : OmniRig::PM_RX);
    }
  else
    {
      if (port_)
        {
          if (TransceiverFactory::PTT_method_RTS == ptt_type_)
            {
              TRACE_CAT ("OmniRigTransceiver", "set RTS");
              port_->SetRts (on);
            }
          else      // "DTR"
            {
              TRACE_CAT ("OmniRigTransceiver", "set DTR");
              port_->SetDtr (on);
            }
        }
      else if (wrapped_)
        {
          TRACE_CAT ("OmniRigTransceiver", "set PTT using basic transceiver");
          TransceiverState new_state {wrapped_->state ()};
          new_state.ptt (on);
          wrapped_->set (new_state, 0);
        }
    }
  update_PTT (on);
}

void OmniRigTransceiver::do_frequency (Frequency f, MODE m, bool /*no_ignore*/)
{
  TRACE_CAT ("OmniRigTransceiver", f << state ());
  if (UNK != m)
    {
      do_mode (m);
    }
  if (OmniRig::PM_FREQ & writable_params_)
    {
      rig_->SetFreq (f);
      update_rx_frequency (f);
    }
  else if (reversed_ && (OmniRig::PM_FREQB & writable_params_))
    {
      rig_->SetFreqB (f);
      update_rx_frequency (f);
    }
  else if (!reversed_ && (OmniRig::PM_FREQA & writable_params_))
    {
      rig_->SetFreqA (f);
      update_rx_frequency (f);
    }
  else
    {
      throw_qstring (tr ("OmniRig: don't know how to set rig frequency"));
    }
}

void OmniRigTransceiver::do_tx_frequency (Frequency tx, MODE m, bool /*no_ignore*/)
{
  TRACE_CAT ("OmniRigTransceiver", tx << state ());
  bool split {tx != 0};
  if (split)
    {
      if (UNK != m)
        {
          do_mode (m);
          if (OmniRig::PM_UNKNOWN == rig_->Vfo ())
            {
              if (writable_params_ & OmniRig::PM_VFOEQUAL)
                {
                  // nothing to do here because OmniRig will use VFO
                  // equalize to set the mode of the Tx VFO for us
                }
              else if ((writable_params_ & (OmniRig::PM_VFOA | OmniRig::PM_VFOB))
                   == (OmniRig::PM_VFOA | OmniRig::PM_VFOB))
                {
                  rig_->SetVfo (OmniRig::PM_VFOB);
                  do_mode (m);
                  rig_->SetVfo (OmniRig::PM_VFOA);
                }
              else if (writable_params_ & OmniRig::PM_VFOSWAP)
                {
                  rig_->SetVfo (OmniRig::PM_VFOSWAP);
                  do_mode (m);
                  rig_->SetVfo (OmniRig::PM_VFOSWAP);
                }
            }
        }
      TRACE_CAT ("OmniRigTransceiver", "set SPLIT mode on");
      rig_->SetSplitMode (state ().frequency (), tx);
      update_other_frequency (tx);
      update_split (true);
    }
  else
    {
      TRACE_CAT ("OmniRigTransceiver", "set SPLIT mode off");
      rig_->SetSimplexMode (state ().frequency ());
      update_split (false);
    }
  bool notify {false};
  if (readable_params_ & OmniRig::PM_FREQ || !(readable_params_ & (OmniRig::PM_FREQA | OmniRig::PM_FREQB)))
    {
      update_other_frequency (tx); // async updates won't return this
      // so just store it and hope
      // operator doesn't change the
      // "back" VFO on rig
      notify = true;
    }
  if (!((OmniRig::PM_VFOAB | OmniRig::PM_VFOBA | OmniRig::PM_SPLITON) & readable_params_))
    {
      TRACE_CAT ("OmniRigTransceiver", "setting SPLIT manually");
      update_split (split); // we can't read it so just set and
      // hope op doesn't change it
      notify = true;
    }
  if (notify)
    {
      update_complete ();
    }
}

void OmniRigTransceiver::do_mode (MODE mode)
{
  TRACE_CAT ("OmniRigTransceiver", mode << state ());
  // TODO: G4WJS OmniRig doesn't seem to have any capability of tracking/setting VFO B mode
  auto mapped = map_mode (mode);
  if (mapped & writable_params_)
    {
      rig_->SetMode (mapped);
      update_mode (mode);
    }
  else
    {
      offline ("OmniRig invalid mode");
    }
}
