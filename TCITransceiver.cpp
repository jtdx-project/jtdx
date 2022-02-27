#include "TCITransceiver.hpp"

#include <QRegularExpression>
#include <QLocale>
#include <QThread>
#include <qmath.h>
#if QT_VERSION >= QT_VERSION_CHECK(5, 15, 0)
#include <QRandomGenerator>
#endif
#include "commons.h"

#include "NetworkServerLookup.hpp"

#include "moc_TCITransceiver.cpp"

namespace
{
  char const * const TCI_transceiver_1_name {"TCI Client RX1"};
  char const * const TCI_transceiver_2_name {"TCI Client RX2"};

  QString map_mode (Transceiver::MODE mode)
  {
    switch (mode)
      {
      case Transceiver::AM: return "am";
      case Transceiver::CW: return "cw";
//      case Transceiver::CW_R: return "CW-R";
      case Transceiver::USB: return "usb";
      case Transceiver::LSB: return "lsb";
//      case Transceiver::FSK: return "RTTY";
//      case Transceiver::FSK_R: return "RTTY-R";
      case Transceiver::DIG_L: return "digl";
      case Transceiver::DIG_U: return "digu";
      case Transceiver::FM: return "wfm";
      case Transceiver::DIG_FM:
        return "nfm";
      default: break;
      }
    return "";
  }
static const QString SmTZ(";");
static const QString SmDP(":");
static const QString SmCM(",");
static const QString SmTrue("true");
static const QString SmFalse("false");

// Command maps
static const QString CmdDevice("device");
static const QString CmdReceiveOnly("receive_only");
static const QString CmdTrxCount("trx_count");
static const QString CmdChannelCount("channels_count");
static const QString CmdVfoLimits("vfo_limits");
static const QString CmdIfLimits("if_limits");
static const QString CmdModeList("modulations_list");
static const QString CmdMode("modulation");
static const QString CmdReady("ready");
static const QString CmdStop("stop");
static const QString CmdStart("start");
static const QString CmdPreamp("preamp");
static const QString CmdDds("dds");
static const QString CmdIf("if");
static const QString CmdTrx("trx");
static const QString CmdRxEnable("rx_enable");
static const QString CmdTxEnable("tx_enable");
static const QString CmdRxChannelEnable("rx_channel_enable");
static const QString CmdRitEnable("rit_enable");
static const QString CmdRitOffset("rit_offset");
static const QString CmdXitEnable("xit_enable");
static const QString CmdXitOffset("xit_offset");
static const QString CmdSplitEnable("split_enable");
static const QString CmdIqSR("iq_samplerate");
static const QString CmdIqStart("iq_start");
static const QString CmdIqStop("iq_stop");
static const QString CmdCWSpeed("cw_macros_speed");
static const QString CmdCWDelay("cw_macros_delay");
static const QString CmdSpot("spot");
static const QString CmdSpotDelete("spot_delete");
static const QString CmdFilterBand("rx_filter_band");
static const QString CmdVFO("vfo");
static const QString CmdVersion("protocol"); //protocol:esdr,1.2;
static const QString CmdTune("tune");
static const QString CmdRxMute("rx_mute");
static const QString CmdSmeter("rx_smeter");
static const QString CmdPower("tx_power");
static const QString CmdSWR("tx_swr");
static const QString CmdECoderRX("ecoder_switch_rx");
static const QString CmdECoderVFO("ecoder_switch_channel");
static const QString CmdAudioSR("audio_samplerate");
static const QString CmdAudioStart("audio_start");
static const QString CmdAudioStop("audio_stop");
static const QString CmdAppFocus("app_focus");
static const QString CmdVolume("volume");
static const QString CmdSqlEnable("sql_enable");
static const QString CmdSqlLevel("sql_level");
static const QString CmdDrive("drive");
static const QString CmdTuneDrive("tune_drive");
static const QString CmdMute("mute");
static const QString CmdRxSensorsEnable("rx_sensors_enable");
static const QString CmdTxSensorsEnable("tx_sensors_enable");
static const QString CmdRxSensors("rx_sensors");
static const QString CmdTxSensors("tx_sensors");
static const QString CmdAgcMode("agc_mode");
static const QString CmdAgcGain("agc_gain");
static const QString CmdLock("lock");

}

extern "C" {
  void   fil4_(qint16*, qint32*, qint16*, qint32*, float*);
}
extern dec_data dec_data;

extern float gran();		// Noise generator (for tests only)

#define RAMP_INCREMENT 64  // MUST be an integral factor of 2^16

#if defined (WSJT_SOFT_KEYING)
# define SOFT_KEYING WSJT_SOFT_KEYING
#else
# define SOFT_KEYING 1
#endif

double constexpr TCITransceiver::m_twoPi;

void TCITransceiver::register_transceivers (TransceiverFactory::Transceivers * registry, unsigned id1, unsigned id2)
{
  (*registry)[TCI_transceiver_1_name] = TransceiverFactory::Capabilities {id1, TransceiverFactory::Capabilities::tci, true};
  (*registry)[TCI_transceiver_2_name] = TransceiverFactory::Capabilities {id2, TransceiverFactory::Capabilities::tci, true};
}

static constexpr quint32 AudioHeaderSize = 16u*sizeof(quint32);

TCITransceiver::TCITransceiver (std::unique_ptr<TransceiverBase> wrapped,QString const& rignr,
                                                                QString const& address, bool use_for_ptt,
                                                                int poll_interval, QObject * parent)
  : PollingTransceiver {poll_interval, parent}
  , wrapped_ {std::move (wrapped)}
  , rx_ {rignr}
  , server_ {address}
  , use_for_ptt_ {use_for_ptt}
  , errortable {tr("ConnectionRefused"),
  tr("RemoteHostClosed"),
  tr("HostNotFound"),
  tr("SocketAccess"),
  tr("SocketResource"),
  tr("SocketTimeout"),
  tr("DatagramTooLarge"),
  tr("Network"),
  tr("AddressInUse"),
  tr("SocketAddressNotAvailable"),
  tr("UnsupportedSocketOperation"),
  tr("UnfinishedSocketOperation"),
  tr("ProxyAuthenticationRequired"),
  tr("SslHandshakeFailed"),
  tr("ProxyConnectionRefused"),
  tr("ProxyConnectionClosed"),
  tr("ProxyConnectionTimeout"),
  tr("ProxyNotFound"),
  tr("ProxyProtocol"),
  tr("Operation"),
  tr("SslInternal"),
  tr("SslInvalidUserData"),
  tr("Temporary"),
  tr("UnknownSocket") }
  , error_ {""}
  , do_snr_ {(poll_interval & do__snr) == do__snr}
  , do_pwr_ {(poll_interval & do__pwr) == do__pwr}
  , rig_power_ {(poll_interval & rig__power) == rig__power}
  , rig_power_off_ {(poll_interval & rig__power_off) == rig__power_off}
  , tci_audio_ {(poll_interval & tci__audio) == tci__audio}
  , commander_ {nullptr}
  , tci_timer1_ {nullptr}
  , tci_loop1_ {nullptr}
  , tci_timer2_ {nullptr}
  , tci_loop2_ {nullptr}
  , tci_timer3_ {nullptr}
  , tci_loop3_ {nullptr}
  , wavptr_ {nullptr} 
  , m_jtdxtime {nullptr}
  , m_downSampleFactor {4}
  , m_buffer ((m_downSampleFactor > 1) ?
              new short [max_buffer_size * m_downSampleFactor] : nullptr)
  , m_quickClose {false}
  , m_phi {0.0}
  , m_toneSpacing {0.0}
  , m_fSpread {0.0}
  , m_state {Idle}
  , m_tuning {false}
  , m_cwLevel {false}
  , m_j0 {-1}
  , m_toneFrequency0 {1500.0}
  , debug_file_ {QDir(QStandardPaths::writableLocation (QStandardPaths::DataLocation)).absoluteFilePath ("jtdx_debug.txt").toStdString()}
  , wav_file_ {QDir(QStandardPaths::writableLocation (QStandardPaths::DataLocation)).absoluteFilePath ("tx.wav").toStdString()}
{
    m_samplesPerFFT = 6912 / 2;
    tci_Ready = false;
    trxA = 0;
    trxB = 0;
    cntIQ = 0;
    bIQ = false;
    inConnected = false;
    audioSampleRate = 48000u;
    mapCmd_[CmdDevice]       = Cmd_Device;
    mapCmd_[CmdReceiveOnly]  = Cmd_ReceiveOnly;
    mapCmd_[CmdTrxCount]     = Cmd_TrxCount;
    mapCmd_[CmdChannelCount] = Cmd_ChannelCount;
    mapCmd_[CmdVfoLimits]    = Cmd_VfoLimits;
    mapCmd_[CmdIfLimits]     = Cmd_IfLimits;
    mapCmd_[CmdModeList]     = Cmd_ModeList;
    mapCmd_[CmdMode]         = Cmd_Mode;
    mapCmd_[CmdReady]        = Cmd_Ready;
    mapCmd_[CmdStop]         = Cmd_Stop;
    mapCmd_[CmdStart]        = Cmd_Start;
    mapCmd_[CmdPreamp]       = Cmd_Preamp;
    mapCmd_[CmdDds]          = Cmd_Dds;
    mapCmd_[CmdIf]           = Cmd_If;
    mapCmd_[CmdTrx]          = Cmd_Trx;
    mapCmd_[CmdRxEnable]     = Cmd_RxEnable;
    mapCmd_[CmdTxEnable]     = Cmd_TxEnable;
    mapCmd_[CmdRxChannelEnable] = Cmd_RxChannelEnable;
    mapCmd_[CmdRitEnable]    = Cmd_RitEnable;
    mapCmd_[CmdRitOffset]    = Cmd_RitOffset;
    mapCmd_[CmdXitEnable]    = Cmd_XitEnable;
    mapCmd_[CmdXitOffset]    = Cmd_XitOffset;
    mapCmd_[CmdSplitEnable]  = Cmd_SplitEnable;
    mapCmd_[CmdIqSR]         = Cmd_IqSR;
    mapCmd_[CmdIqStart]      = Cmd_IqStart;
    mapCmd_[CmdIqStop]       = Cmd_IqStop;
    mapCmd_[CmdCWSpeed]      = Cmd_CWSpeed;
    mapCmd_[CmdCWDelay]      = Cmd_CWDelay;
    mapCmd_[CmdFilterBand]   = Cmd_FilterBand;
    mapCmd_[CmdVFO]          = Cmd_VFO;
    mapCmd_[CmdVersion]      = Cmd_Version;
    mapCmd_[CmdTune]         = Cmd_Tune;
    mapCmd_[CmdRxMute]       = Cmd_RxMute;
    mapCmd_[CmdSmeter]       = Cmd_Smeter;
    mapCmd_[CmdPower]        = Cmd_Power;
    mapCmd_[CmdSWR]          = Cmd_SWR;
    mapCmd_[CmdECoderRX]     = Cmd_ECoderRX;
    mapCmd_[CmdECoderVFO]    = Cmd_ECoderVFO;
    mapCmd_[CmdAudioSR]      = Cmd_AudioSR;
    mapCmd_[CmdAudioStart]   = Cmd_AudioStart;
    mapCmd_[CmdAudioStop]    = Cmd_AudioStop;
    mapCmd_[CmdAppFocus]     = Cmd_AppFocus;
    mapCmd_[CmdVolume]       = Cmd_Volume;
    mapCmd_[CmdSqlEnable]    = Cmd_SqlEnable;
    mapCmd_[CmdSqlLevel]     = Cmd_SqlLevel;
    mapCmd_[CmdDrive]        = Cmd_Drive;
    mapCmd_[CmdTuneDrive]    = Cmd_TuneDrive;
    mapCmd_[CmdMute]         = Cmd_Mute;
    mapCmd_[CmdRxSensorsEnable] = Cmd_RxSensorsEnable;
    mapCmd_[CmdTxSensorsEnable] = Cmd_TxSensorsEnable;
    mapCmd_[CmdRxSensors]    = Cmd_RxSensors;
    mapCmd_[CmdTxSensors]    = Cmd_TxSensors;
    mapCmd_[CmdAgcMode]      = Cmd_AgcMode;
    mapCmd_[CmdAgcGain]      = Cmd_AgcGain;
    mapCmd_[CmdLock]         = Cmd_Lock;
}

void TCITransceiver::onConnected()
{
    inConnected = true;
//    printf("%s(%0.1f) TCI connected\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset());
#if JTDX_DEBUG_TO_FILE
    FILE * pFile = fopen (debug_file_.c_str(),"a");
    fprintf (pFile,"%s(%0.1f) TCI connected\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset());
    fclose (pFile);
#endif
}

void TCITransceiver::onDisconnected()
{
    inConnected = false;
//    printf("%s(%0.1f) TCI disconnected\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset());
#if JTDX_DEBUG_TO_FILE
    FILE * pFile = fopen (debug_file_.c_str(),"a");
    fprintf (pFile,"%s(%0.1f) TCI disconnected\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset());
    fclose (pFile);
#endif
}


void TCITransceiver::onError(QAbstractSocket::SocketError err)
{
//qDebug() << "WebInThread::onError";
//    printf("%s(%0.1f) TCI error:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),err);
#if JTDX_DEBUG_TO_FILE
    FILE * pFile = fopen (debug_file_.c_str(),"a");
    fprintf (pFile,"%s(%0.1f) TCI error:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),err);
    fclose (pFile);
#endif
    error_ = tr ("TCI websocket error: %1").arg (errortable.at (err));
}

int TCITransceiver::do_start (JTDXDateTime * jtdxtime)
{
  if (tci_audio_) QThread::currentThread()->setPriority(QThread::HighPriority);
//  printf("do_start tci_Ready:%d\n",tci_Ready);
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf (pFile,"do_start tci_Ready:%d\n",tci_Ready);
  fclose (pFile);
#endif
  TRACE_CAT ("TCITransceiver", "starting");
  m_jtdxtime = jtdxtime;
  if (wrapped_) wrapped_->start (0,m_jtdxtime);
  url_.setUrl("ws://" + server_); //server_
  if (url_.host() == "") url_.setHost("localhost");
  if (url_.port() == -1) url_.setPort(40001);

  if (!commander_)
    {
      commander_ = new QWebSocket {}; // QObject takes ownership
//      printf ("commander created");
#if JTDX_DEBUG_TO_FILE
      FILE * pFile = fopen (debug_file_.c_str(),"a");
      fprintf (pFile,"commander created");
      fclose (pFile);
#endif
      connect(commander_,SIGNAL(connected()),this,SLOT(onConnected()));
      connect(commander_,SIGNAL(disconnected()),this,SLOT(onDisconnected()));
      connect(commander_,SIGNAL(binaryMessageReceived(QByteArray)),this,SLOT(onBinaryReceived(QByteArray)));
      connect(commander_,SIGNAL(textMessageReceived(QString)),this,SLOT(onMessageReceived(QString)));
      connect(commander_,SIGNAL(error(QAbstractSocket::SocketError)),this,SLOT(onError(QAbstractSocket::SocketError)));
    }
  if (!tci_loop1_) {
    tci_loop1_ = new QEventLoop  {this};
  }
  if (!tci_timer1_) {
    tci_timer1_ = new QTimer {this};
    tci_timer1_ -> setSingleShot(true);
    connect( tci_timer1_, &QTimer::timeout, tci_loop1_, &QEventLoop::quit);
    connect( this, &TCITransceiver::tci_done1, tci_loop1_, &QEventLoop::quit);
  }
  if (!tci_loop2_) {
    tci_loop2_ = new QEventLoop  {this};
  }
  if (!tci_timer2_) {
    tci_timer2_ = new QTimer {this};
    tci_timer2_ -> setSingleShot(true);
    connect( tci_timer2_, &QTimer::timeout, tci_loop2_, &QEventLoop::quit);
    connect( this, &TCITransceiver::tci_done2, tci_loop2_, &QEventLoop::quit);
  }
  if (!tci_loop3_) {
    tci_loop3_ = new QEventLoop  {this};
  }
  if (!tci_timer3_) {
    tci_timer3_ = new QTimer {this};
    tci_timer3_ -> setSingleShot(true);
    connect( tci_timer3_, &QTimer::timeout, tci_loop3_, &QEventLoop::quit);
    connect( this, &TCITransceiver::tci_done3, tci_loop3_, &QEventLoop::quit);
  }
  tx_fifo = 0; tx_top_ = true;
  tci_Ready = false;
  ESDR3 = false;
  HPSDR = false;
  band_change = false;
  trxA = 0;
  trxB = 0;
  busy_rx_frequency_ = false;
  busy_mode_ = false;
  busy_other_frequency_ = false;
  busy_split_ = false;
  busy_drive_ = false;
  busy_PTT_ = false;
  busy_rx2_ = false;
  rx2_ = false;
  requested_rx2_ = false;
  started_rx2_ = false;
  split_ = false;
  requested_split_ = false;
  started_split_ = false;
  PTT_ = false;
  requested_PTT_ = false;
  mode_ = "";
  requested_mode_ = "";
  started_mode_ = "";
  requested_rx_frequency_ = "";
  rx_frequency_ = "";
  requested_other_frequency_ = "";
  other_frequency_ = "";
  requested_drive_ = "";
  drive_ = "";
  level_ = -54;
  power_ = 0;
  swr_ = 0;
  m_bufferPos = 0;
  m_downSampleFactor =4;
  m_ns = 999;
  audio_ = false;
  requested_stream_audio_ = false;
  stream_audio_ = false;
  _power_ = false;
//  printf ("%s(%0.1f) TCI open %s rig_power:%d rig_power_off:%d tci_audio:%d do_snr:%d do_pwr:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),url_.toString().toStdString().c_str(),rig_power_,rig_power_off_,tci_audio_,do_snr_,do_pwr_);
#if JTDX_DEBUG_TO_FILE
  pFile = fopen (debug_file_.c_str(),"a");
  fprintf (pFile,"%s(%0.1f) TCI open %s rig_power:%d rig_power_off:%d tci_audio:%d do_snr:%d do_pwr:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),url_.toString().toStdString().c_str(),rig_power_,rig_power_off_,tci_audio_,do_snr_,do_pwr_);
  fclose (pFile);
#endif
  commander_->open (url_);
  mysleep1 (1500);
//  if (ESDR3) mysleep1 (500); else mysleep1 (100);
  mysleep1 (210);
  if (HPSDR) {
      busy_split_ = true;
      const QString cmd = CmdSplitEnable + SmDP + rx_ + SmTZ;
      sendTextMessage(cmd);
      mysleep2(500);
      busy_split_ = false;
  }
  if (error_.isEmpty()) {
    tci_Ready = true;
    if (!_power_) {
      if (rig_power_) {
        rig_power(true);
        mysleep1(1000);
        if(!_power_) throw error {tr ("TCI SDR could not be switched on")};
      } else {
        tci_Ready = false;
        throw error {tr ("TCI SDR is not switched on")};
      }
    }
    if (rx_ == "1" && !rx2_) {
      rx2_enable (true);
      if (!rx2_) {
        tci_Ready = false;
        throw error {tr ("TCI RX2 could not be enabled")};
      }
    }
    if (tci_audio_) {
        stream_audio (true);
        mysleep1(500);
        if (!stream_audio_) {
          tci_Ready = false;
          throw error {tr ("TCI Audio could not be switched on")};
        }
    }
    if (ESDR3) {
      const QString cmd = CmdRxSensorsEnable + SmDP + (do_snr_ ? "true" : "false") + SmCM + "500" +  SmTZ;
      sendTextMessage(cmd);
    } else if (do_snr_) {
      const QString cmd = CmdSmeter + SmDP + rx_ + SmCM + "0" +  SmTZ;
      sendTextMessage(cmd);
    }
    if (!requested_rx_frequency_.isEmpty()) do_frequency(string_to_frequency (requested_rx_frequency_),get_mode(true),false);
    if (!requested_other_frequency_.isEmpty()) do_tx_frequency(string_to_frequency (requested_other_frequency_),get_mode(true),false);
    else if (requested_split_ != split_) {/*printf("splt from start %d\n",requested_split_);*/ rig_split();} // split_ = requested_split_; mysleep2(100);}
    if (!requested_drive_.isEmpty() && requested_drive_ != drive_) {
        busy_drive_ = true;
        if (ESDR3) {
          const QString cmd = CmdDrive + SmDP + rx_ + SmCM + requested_drive_ + SmTZ;
          sendTextMessage(cmd);
        } else {
          const QString cmd = CmdDrive + SmDP + requested_drive_ + SmTZ;
          sendTextMessage(cmd);
        }
    }
    do_poll ();
    if (ESDR3) {
      const QString cmd = CmdTxSensorsEnable + SmDP + (do_pwr_ ? "true" : "false") + SmCM + "500" +  SmTZ;
      sendTextMessage(cmd);
    }
    if (stream_audio_) do_audio(true);

    TRACE_CAT ("TCITransceiver", "started");
//    printf("%s(%0.1f) TCI Transceiver started\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset());
#if JTDX_DEBUG_TO_FILE
    pFile = fopen (debug_file_.c_str(),"a");
    fprintf (pFile,"%s(%0.1f) TCI Transceiver started\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset());
    fclose (pFile);
#endif
    return 0;
  } else {
//    printf("%s(%0.1f) TCI Transceiver not started %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),error_.toStdString().c_str());
#if JTDX_DEBUG_TO_FILE
    pFile = fopen (debug_file_.c_str(),"a");
    fprintf (pFile,"%s(%0.1f) TCI Transceiver not started %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),error_.toStdString().c_str());
    fclose (pFile);
#endif
    if (error_.isEmpty())
      throw error {tr ("TCI could not be opened")};
    else
      throw error {error_};
  }
}

void TCITransceiver::do_stop ()
{
//  printf ("TCI close\n");
  if (!commander_) return;
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf (pFile,"TCI close\n");
#endif
  if (stream_audio_ && tci_Ready && inConnected && _power_) {
    stream_audio (false);
    mysleep1(500);
//    printf ("TCI audio closed\n");
#if JTDX_DEBUG_TO_FILE
    fprintf (pFile,"TCI audio closed\n");
#endif
  }
  if (tci_Ready && inConnected && _power_) {
    requested_other_frequency_ = "";
    if (started_split_ != split_) {requested_split_ = started_split_; rig_split();}
    if (started_mode_ != mode_) sendTextMessage(mode_to_command(started_mode_));
    if (!started_rx2_ && rx2_) {
      rx2_enable (false);
    }
  }
  if (_power_ && rig_power_off_ && tci_Ready && inConnected && _power_) {
    rig_power(false);
    mysleep1(500);
//    printf ("TCI power down\n");
#if JTDX_DEBUG_TO_FILE
    fprintf (pFile,"TCI power down\n");
#endif
  }
  tci_Ready = false;
  if (commander_)
    {
      if (inConnected) commander_->close(QWebSocketProtocol::CloseCodeNormal,"end");
      delete commander_, commander_ = nullptr;
//      printf ("commander ");
#if JTDX_DEBUG_TO_FILE
      fprintf (pFile,"commander ");
#endif
    }
  if (tci_timer1_)
    {
      if (tci_timer1_->isActive()) tci_timer1_->stop();
      delete tci_timer1_, tci_timer1_ = nullptr;
//      printf ("timer1 ");
#if JTDX_DEBUG_TO_FILE
      fprintf (pFile,"timer1 ");
#endif
    }
  if (tci_loop1_)
    {
      tci_loop1_->quit();
      delete tci_loop1_, tci_loop1_ = nullptr;
//      printf ("loop1 ");
#if JTDX_DEBUG_TO_FILE
      fprintf (pFile,"loop1 ");
#endif
    }
  if (tci_timer2_)
    {
      if (tci_timer2_->isActive()) tci_timer2_->stop();
      delete tci_timer2_, tci_timer2_ = nullptr;
//      printf ("timer2 ");
#if JTDX_DEBUG_TO_FILE
      fprintf (pFile,"timer2 ");
#endif
    }
  if (tci_loop2_)
    {
      tci_loop2_->quit();
      delete tci_loop2_, tci_loop2_ = nullptr;
//      printf ("loop2 ");
#if JTDX_DEBUG_TO_FILE
      fprintf (pFile,"loop2 ");
#endif
    }
  if (tci_timer3_)
    {
      if (tci_timer3_->isActive()) tci_timer3_->stop();
      delete tci_timer3_, tci_timer3_ = nullptr;
//      printf ("timer3 ");
#if JTDX_DEBUG_TO_FILE
      fprintf (pFile,"timer3 ");
#endif
    }
  if (tci_loop3_)
    {
      tci_loop3_->quit();
      delete tci_loop3_, tci_loop3_ = nullptr;
//      printf ("loop3 ");
#if JTDX_DEBUG_TO_FILE
      fprintf (pFile,"loop3 ");
#endif
    }

  if (wrapped_) wrapped_->stop ();
  TRACE_CAT ("TCITransceiver", "stopped");
//  printf ("deleted\nTCI closed\n");
#if JTDX_DEBUG_TO_FILE
  fprintf (pFile,"deleted\nTCI closed\n");
  fclose (pFile);
#endif
}

void TCITransceiver::onMessageReceived(const QString &str)
{
//qDebug() << "From WEB" << str;
    QStringList cmd_list = str.split(";", SkipEmptyParts);
#if JTDX_DEBUG_TO_FILE
    FILE * pFile;
#endif
    for (QString cmds : cmd_list){
        QStringList cmd = cmds.split(":", SkipEmptyParts);
        QStringList args = cmd.last().split(",", SkipEmptyParts);
        Tci_Cmd idCmd = mapCmd_[cmd.first()];
//        if (idCmd != Cmd_Power && idCmd != Cmd_SWR && idCmd != Cmd_Smeter && idCmd != Cmd_AppFocus && idCmd != Cmd_RxSensors && idCmd != Cmd_TxSensors) { printf ("%s(%0.1f) TCI message received:|%s| ",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),str.toStdString().c_str()); printf("idCmd : %d args : %s\n",idCmd,args.join("|").toStdString().c_str());}
#if JTDX_DEBUG_TO_FILE
        if (idCmd != Cmd_Power && idCmd != Cmd_SWR && idCmd != Cmd_Smeter && idCmd != Cmd_AppFocus && idCmd != Cmd_RxSensors && idCmd != Cmd_TxSensors) {
          pFile = fopen (debug_file_.c_str(),"a");
          fprintf (pFile,"%s(%0.1f) TCI message received:|%s| ",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),str.toStdString().c_str());
          fprintf (pFile,"idCmd : %d args : %s\n",idCmd,args.join("|").toStdString().c_str());
          fclose (pFile);
        }
#endif
//qDebug() << cmds << idCmd;
        if (idCmd <=0) continue;

        switch (idCmd) {
        case Cmd_Smeter:
          if(args.at(0)==rx_ && args.at(1) == "0") level_ = args.at(2).toInt() + 73;
          break;	
        case Cmd_RxSensors:
          if(args.at(0)==rx_) level_ = args.at(1).split(".")[0].toInt() + 73;
//            printf("Smeter=%d\n",level_);
          break;	
        case Cmd_TxSensors:
          if(args.at(0)==rx_) {
            power_ = 10 * args.at(3).split(".")[0].toInt() + args.at(3).split(".")[1].toInt();
            swr_ = 10 * args.at(4).split(".")[0].toInt() + args.at(4).split(".")[1].toInt();
//            printf("Power=%d SWR=%d\n",power_,swr_);
          }
          break;	
        case Cmd_SWR:
//          printf("%s(%0.1f) Cmd_SWR : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
          swr_ = 10 * args.at(0).split(".")[0].toInt() + args.at(0).split(".")[1].toInt();
          break;	
        case Cmd_Power:
          power_ = 10 * args.at(0).split(".")[0].toInt() + args.at(0).split(".")[1].toInt();
//          printf("%s(%0.1f) Cmd_Power : %s %d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str(),power_);
          break;	
        case Cmd_VFO:
//            printf("%s(%0.1f) Cmd_VFO : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
//            printf("band_change:%d busy_other_frequency_:%d timer1_remaining:%d timer2_remaining:%d",band_change,busy_other_frequency_,tci_timer1_->remainingTime(),tci_timer2_->remainingTime());
#if JTDX_DEBUG_TO_FILE
            pFile = fopen (debug_file_.c_str(),"a");
            fprintf (pFile,"%s(%0.1f) Cmd_VFO : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
            fprintf (pFile,"band_change:%d busy_other_frequency_:%d timer1_remaining:%d timer2_remaining:%d",band_change,busy_other_frequency_,tci_timer1_->remainingTime(),tci_timer2_->remainingTime());
#endif
            if(args.at(0)==rx_ && args.at(1) == "0") {
              if (args.at(2).left(1) != "-") rx_frequency_ = args.at(2);
              if (!tci_Ready && requested_rx_frequency_.isEmpty()) {requested_rx_frequency_ = rx_frequency_; }
              if (busy_rx_frequency_ && !band_change) {
//                printf (" cmdvfo0 done1");
#if JTDX_DEBUG_TO_FILE
                fprintf (pFile," cmdvfo0 done1");
#endif
                tci_done1();
              } else if (!tci_timer2_->isActive() && split_) {
//                printf (" cmdvfo0 timer2 start 210");
#if JTDX_DEBUG_TO_FILE
                fprintf (pFile," cmdvfo0 timer2 start 210");
#endif
                tci_timer2_->start(210);
              }
            }
            else if (args.at(0)==rx_ && args.at(1) == "1") {
              if (args.at(2).left(1) != "-") other_frequency_ = args.at(2);
//              if (!tci_Ready && requested_other_frequency_.isEmpty()) requested_other_frequency_ = other_frequency_;
              if (band_change && tci_timer1_->isActive()) {
//                printf (" cmdvfo1 done1");
#if JTDX_DEBUG_TO_FILE
                fprintf (pFile," cmdvfo1 done1");
#endif
                band_change = false;
                tci_timer2_->start(210);
                tci_done1();
              } else if (busy_other_frequency_) {
//                printf (" cmdvfo1 done2");
#if JTDX_DEBUG_TO_FILE
                fprintf (pFile," cmdvfo1 done2");
#endif
                tci_done2();
              } else if (tci_timer2_->isActive()) {
//                printf (" cmdvfo1 timer2 reset 210");
#if JTDX_DEBUG_TO_FILE
                fprintf (pFile," cmdvfo1 timer2 reset 210");
#endif
                tci_timer2_->start(210);
              } else if (other_frequency_ != requested_other_frequency_ && tci_Ready && split_ && !tci_timer2_->isActive()) {
//                printf (" cmdvfo1 timer2 start 210");
#if JTDX_DEBUG_TO_FILE
                fprintf (pFile," cmdvfo1 timer2 start 210");
#endif
                tci_timer2_->start(210);
              }
            }
//            printf("->%d\n",tci_timer2_->remainingTime());
#if JTDX_DEBUG_TO_FILE
            fprintf(pFile,"->%d\n",tci_timer2_->remainingTime());
            fclose (pFile);
#endif
            break;
        case Cmd_Mode:
//            printf("%s(%0.1f) Cmd_Mode : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
#if JTDX_DEBUG_TO_FILE
            pFile = fopen (debug_file_.c_str(),"a");
            fprintf (pFile,"%s(%0.1f) Cmd_Mode : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
            fclose (pFile);
#endif
            if(args.at(0)==rx_) {
              if (ESDR3 || HPSDR) {
                if (args.at(1) == "0" ) mode_ = args.at(2).toLower(); else mode_ = args.at(1).toLower();
              }  else mode_ = args.at(1);
              if (started_mode_.isEmpty()) started_mode_ = mode_;
              if (busy_mode_) tci_done1();
              else if (!requested_mode_.isEmpty() && requested_mode_ != mode_ && !band_change) {
                sendTextMessage(mode_to_command(requested_mode_));
              }
            }
            break;
        case Cmd_SplitEnable:
//            printf("%s(%0.1f) Cmd_SplitEnable : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
#if JTDX_DEBUG_TO_FILE
            pFile = fopen (debug_file_.c_str(),"a");
            fprintf (pFile,"%s(%0.1f) Cmd_SplitEnable : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
            fclose (pFile);
#endif
            if(args.at(0)==rx_) {
              if (args.at(1) == "false") split_ = false;
              else if (args.at(1) == "true") split_ = true;
              if (!tci_Ready) {started_split_ = split_;} 
              else if (busy_split_) tci_done2();
              else if (requested_split_ != split_ && !tci_timer2_->isActive()) {
                tci_timer2_->start(210);
                rig_split();
              }
            }
            break;
        case Cmd_Drive:
//            printf("%s(%0.1f) Cmd_Drive : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
#if JTDX_DEBUG_TO_FILE
            pFile = fopen (debug_file_.c_str(),"a");
            fprintf (pFile,"%s(%0.1f) Cmd_Drive : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
            fclose (pFile);
#endif
            if((!ESDR3 && !HPSDR) || args.at(0)==rx_) {
              if (ESDR3 || HPSDR) drive_ = args.at(1); else drive_ = args.at(0);
              if (requested_drive_.isEmpty()) requested_drive_ = drive_;
              busy_drive_ = false;
            }
            break;
        case Cmd_Trx:
//            printf("%s(%0.1f) Cmd_Trx : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
#if JTDX_DEBUG_TO_FILE
            pFile = fopen (debug_file_.c_str(),"a");
            fprintf (pFile,"%s(%0.1f) Cmd_Trx : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
            fclose (pFile);
#endif
            if(args.at(0)==rx_) {
              if (args.at(1) == "false") PTT_ = false;
              else if (args.at(1) == "true") PTT_ = true;
              if (tci_Ready && requested_PTT_ == PTT_) tci_done3();
              else if (tci_Ready && !PTT_) {
                requested_PTT_ = PTT_;
                update_PTT(PTT_);
                power_ = 0; if (do_pwr_) update_power (0);
                swr_ = 0; if (do_pwr_) update_swr (0);
              }  
            }
            break;
        case Cmd_AudioStart:
//          printf("%s(%0.1f) Cmd_AudioStart : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
#if JTDX_DEBUG_TO_FILE
          pFile = fopen (debug_file_.c_str(),"a");
          fprintf (pFile,"%s(%0.1f) Cmd_AudioStart : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
          fclose (pFile);
#endif
            if(args.at(0)==rx_) {
              stream_audio_ = true;
              if (tci_Ready) { //printf ("cmdaudiostart done1\n");
              tci_done1();}
            }
          break;	
        case Cmd_RxEnable:
//          printf("%s(%0.1f) Cmd_RxEnable : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
#if JTDX_DEBUG_TO_FILE
          pFile = fopen (debug_file_.c_str(),"a");
          fprintf (pFile,"%s(%0.1f) Cmd_RxEnable : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
          fclose (pFile);
#endif
            if(args.at(0)=="1") {
              if (args.at(1) == "false") rx2_ = false;
              else if (args.at(1) == "true") rx2_ = true;
              if(!tci_Ready) {requested_rx2_ = rx2_; started_rx2_ = rx2_;}
              else if (tci_Ready && busy_rx2_ && requested_rx2_ == rx2_) tci_done1();
            }
          break;	
        case Cmd_AudioStop:
//          printf("%s(%0.1f) CmdAudioStop : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
#if JTDX_DEBUG_TO_FILE
          pFile = fopen (debug_file_.c_str(),"a");
          fprintf (pFile,"%s(%0.1f) CmdAudioStop : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
          fclose (pFile);
#endif
            if(args.at(0)==rx_) {
              stream_audio_ = false;
              if (tci_Ready) { //printf ("cmdaudiostop done1\n");
              tci_done1();}
            }
          break;	
        case Cmd_Start:
//          printf("%s(%0.1f) CmdStart : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
#if JTDX_DEBUG_TO_FILE
          pFile = fopen (debug_file_.c_str(),"a");
          fprintf (pFile,"%s(%0.1f) CmdStart : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
          fclose (pFile);
#endif
          _power_ = true;
//          printf ("cmdstart done1\n");
          if (tci_Ready) tci_done1();
          break;	
        case Cmd_Stop:
//          printf("%s(%0.1f) CmdStop : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
#if JTDX_DEBUG_TO_FILE
          pFile = fopen (debug_file_.c_str(),"a");
          fprintf (pFile,"%s(%0.1f) CmdStop : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
          fclose (pFile);
#endif
          if (tci_Ready && PTT_) {
                PTT_ = false;
                requested_PTT_ = PTT_;
                update_PTT(PTT_);
                power_ = 0; if (do_pwr_) update_power (0);
                swr_ = 0; if (do_pwr_) update_swr (0);
                m_state = Idle;
                Q_EMIT tci_mod_active(m_state != Idle);
              }  
          _power_ = false;
          if (tci_timer1_->isActive()) { /*printf ("cmdstop done1\n");*/ tci_done1();}
          else {
            tci_Ready = false;
          }
          break;	
        case Cmd_Version:
//          printf("%s(%0.1f) CmdVersion : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
#if JTDX_DEBUG_TO_FILE
          pFile = fopen (debug_file_.c_str(),"a");
          fprintf (pFile,"%s(%0.1f) CmdVersion : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
          fclose (pFile);
#endif
          if(args.at(0)=="ExpertSDR3") ESDR3 = true;
          else if (args.at(0)=="Thetis") HPSDR = true;
          break;	
        case Cmd_Device:
//          printf("%s(%0.1f) CmdDevice : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
#if JTDX_DEBUG_TO_FILE
          pFile = fopen (debug_file_.c_str(),"a");
          fprintf (pFile,"%s(%0.1f) CmdDevice : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
          fclose (pFile);
#endif
          if((args.at(0)=="SunSDR2DX" || args.at(0)=="SunSDR2PRO") && !ESDR3) tx_top_ = false;
//          printf ("tx_top_:%d\n",tx_top_);
          break;	
        case Cmd_Ready:
//          printf("%s(%0.1f) CmdReady : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
#if JTDX_DEBUG_TO_FILE
          pFile = fopen (debug_file_.c_str(),"a");
          fprintf (pFile,"%s(%0.1f) CmdReady : %s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),args.join("|").toStdString().c_str());
          fclose (pFile);
#endif
          tci_done1();
          break;	
        
        default:
            break;
        }
    }

}

void TCITransceiver::sendTextMessage(const QString &message)
{
    if (inConnected) commander_->sendTextMessage(message);
}


void TCITransceiver::onBinaryReceived(const QByteArray &data)
{
/*    if (++cntIQ % 50 == 0){
        bIQ = !bIQ;
        nIqBytes+=data.size();
        printf("receiveIQ\n");
        emit receiveIQ(bIQ,nIqBytes);
        nIqBytes = 0;
    } else {
        nIqBytes+=data.size();
    } */
    Data_Stream *pStream = (Data_Stream*)(data.data());
    if (pStream->type != last_type) {
//        printf ("%s(%0.1f) binary received type=%d last_type=%d %d samplerate %d stream_size %d frame_size %d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),pStream->type,last_type,data.size(),pStream->sampleRate,pStream->length,data.size());
#if JTDX_DEBUG_TO_FILE
        FILE * pFile = fopen (debug_file_.c_str(),"a");
        fprintf (pFile,"%s(%0.1f) binary received type=%d last_type=%d %d samplerate %d stream_size %d frame_size %d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),pStream->type,last_type,data.size(),pStream->sampleRate,pStream->length,data.size());
        fclose (pFile);
#endif
        last_type = pStream->type;
#if JTDX_DEBUG_TO_FILE
        if (last_type == TxChrono) { // audio transmit starts
          struct {
            char ariff[4];         //ChunkID:    "RIFF"
            int nchunk;            //ChunkSize: 36+SubChunk2Size
            char awave[4];         //Format: "WAVE"
            char afmt[4];          //Subchunk1ID: "fmt "
            int lenfmt;            //Subchunk1Size: 16
            short int nfmt2;       //AudioFormat: 1
            short int nchan2;      //NumChannels: 1
            int nsamrate;          //SampleRate: 12000
            int nbytesec;          //ByteRate: SampleRate*NumChannels*BitsPerSample/8
            short int nbytesam2;   //BlockAlign: NumChannels*BitsPerSample/8
            short int nbitsam2;    //BitsPerSample: 16
            char adata[4];         //Subchunk2ID: "data"
            int ndata;             //Subchunk2Size: numSamples*NumChannels*BitsPerSample/8
          } hdr;
          int npts=static_cast<int>(m_period) * 48000;
          wavptr_ = fopen(wav_file_.c_str(),"wb");
          if (wavptr_  != NULL) {
            // Write a WAV header
            hdr.ariff[0]='R';
            hdr.ariff[1]='I';
            hdr.ariff[2]='F';
            hdr.ariff[3]='F';
            hdr.nchunk=36 + 2*npts;
            hdr.awave[0]='W';
            hdr.awave[1]='A';
            hdr.awave[2]='V';
            hdr.awave[3]='E';
            hdr.afmt[0]='f';
            hdr.afmt[1]='m';
            hdr.afmt[2]='t';
            hdr.afmt[3]=' ';
            hdr.lenfmt=16;
            hdr.nfmt2=1;
            hdr.nchan2=1;
            hdr.nsamrate=48000;
            hdr.nbytesec=2*48000;
            hdr.nbytesam2=2;
            hdr.nbitsam2=16;
            hdr.adata[0]='d';
            hdr.adata[1]='a';
            hdr.adata[2]='t';
            hdr.adata[3]='a';
            hdr.ndata=2*npts;
            fwrite(&hdr,sizeof(hdr),1,wavptr_);
          }

        }
        else if (last_type == RxAudioStream) { // audio switched back to resceive
          if (wavptr_ != NULL) {
            fclose(wavptr_);
            wavptr_ = nullptr;
          }
        }
#endif
    }
    if (pStream->type == Iq_Stream){
        bool tx = false;
        if (pStream->receiver == 0){
            tx = trxA == 0;
            trxA = 1;

        }
        if (pStream->receiver == 1) {
            tx = trxB == 0;
            trxB = 1;
        }
//        printf("sendIqData\n");
#if JTDX_DEBUG_TO_FILE
        FILE * pFile = fopen (debug_file_.c_str(),"a");
        fprintf (pFile,"sendIqData\n");
        fclose (pFile);
#endif
        emit sendIqData(pStream->receiver,pStream->length,pStream->data,tx);
qDebug() << "IQ" << data.size() << pStream->length;
    } else if (pStream->type == RxAudioStream && audio_  && pStream->receiver == rx_.toUInt()){
//        printf("%s(%0.1f) writeAudioData\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset());
        writeAudioData(pStream->data,pStream->length);
qDebug() << "Audio" << data.size() << pStream->length;
    } else if (pStream->type == TxChrono &&  pStream->receiver == rx_.toUInt()){
        mtx_.lock(); tx_fifo += 1; tx_fifo &= 7;
        int ssize = AudioHeaderSize+pStream->length*sizeof(float)*2;
//        printf("%s(%0.1f) TxChrono ",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset());
        quint16 tehtud;
        if (m_tx1[tx_fifo].size() != ssize) {m_tx1[tx_fifo].resize(ssize);/* m_tx1[tx_fifo].fill('\0x00');*/}
        Data_Stream * pOStream1 = (Data_Stream*)(m_tx1[tx_fifo].data());
        pOStream1->receiver = pStream->receiver;
        pOStream1->sampleRate = pStream->sampleRate; 
        pOStream1->format = pStream->format;
        pOStream1->codec = 0;
        pOStream1->crc = 0;
        pOStream1->length = pStream->length;
        pOStream1->type = TxAudioStream;
//        for (size_t i = 0; i < pStream->length; i++) pOStream1->data[i] = 0;
//        printf("%s(%0.1f) txAudioChrono %d %d %d",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),ssize,pStream->length,pStream->sampleRate);
        tehtud = readAudioData(pOStream1->data,pOStream1->length);
//        printf(" %s(%0.1f) tehtud%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),tehtud);
        if (tehtud && tehtud != pOStream1->length) {
          quint16 valmis = tehtud;
//          printf("%s(%0.1f) Audio build 1 mismatch requested %d done %d",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),pOStream1->length,tehtud);
#if JTDX_DEBUG_TO_FILE
          FILE * pFile = fopen (debug_file_.c_str(),"a");
          fprintf (pFile,"%s(%0.1f) Audio build 1 mismatch requested %d done %d",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),pOStream1->length,tehtud);
#endif
          tehtud = readAudioData(pOStream1->data + valmis,pOStream1->length - valmis);
//          printf("got %d\n",tehtud);
#if JTDX_DEBUG_TO_FILE
          fprintf (pFile,"got %d\n",tehtud);
          fclose (pFile);
#endif
//          if (tehtud && tehtud != pOStream1->length - valmis) {
//            valmis += tehtud;
//            for (size_t i = 0; i < pOStream1->length - valmis; i++) pOStream1->data[i+valmis] = 0;
//          }
//          else if (tehtud == 0)  for (size_t i = 0; i < pOStream1->length - valmis; i++) pOStream1->data[i+valmis] = 0;
//        }
//        else if (tehtud == 0) {
//          for (size_t i = 0; i < pStream->length; i++) pOStream1->data[i] = 0;
        }
#if JTDX_DEBUG_TO_FILE
        if (wavptr_ != NULL) {
          static constexpr float K = 0x7FFF;
          for (size_t i = 0; i < pOStream1->length; i+=2) {
            qint16 value = static_cast<int16_t>(K*pOStream1->data[i]);
            fwrite(&value,2,1,wavptr_);
          }
        }
#endif
        tx_fifo2 = tx_fifo; mtx_.unlock();
        if (!inConnected || commander_->sendBinaryMessage(m_tx1[tx_fifo2]) != m_tx1[tx_fifo2].size()) {
//          printf("%s(%0.1f) Sent 1 loaded failed\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset());
#if JTDX_DEBUG_TO_FILE
          FILE * pFile = fopen (debug_file_.c_str(),"a");
          fprintf (pFile,"%s(%0.1f) Sent 1 loaded failed\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset());
          fclose (pFile);
#endif
        }
    }// else printf ("Unused binary message received type=%d receiver=%d\n",pStream->type,pStream->receiver);
}

void TCITransceiver::txAudioData(quint32 len, float * data)
{
    QByteArray tx;
//    printf("%s(%0.1f) txAudioData %d %ld\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),len,(AudioHeaderSize+len*sizeof(float)*2));
    tx.resize(AudioHeaderSize+len*sizeof(float)*2);
//    printf("%s(%0.1f) txAudioData %d %d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),len,tx.size());
//    Data_Stream * pStream = reinterpret_cast<Data_Stream*>(tx.data());
    Data_Stream * pStream = (Data_Stream*)(tx.data());
    pStream->receiver = 0;
    pStream->sampleRate = audioSampleRate;
    pStream->format = 3;
    pStream->codec = 0;
    pStream->crc = 0;
    pStream->length = len;
    pStream->type = TxAudioStream;
    memcpy(pStream->data,data,len*sizeof(float)*2);
    commander_->sendBinaryMessage(tx);
}

quint32 TCITransceiver::writeAudioData (float * data, qint32 maxSize)
{
  static unsigned mstr0=999999;
  qint64 ms0 = m_jtdxtime->currentMSecsSinceEpoch2() % 86400000; //m_jtdxtime -> currentMSecsSinceEpoch2() % 86400000;
  unsigned mstr = ms0 % int(1000.0*m_period); // ms into the nominal Tx start time
  if(mstr < mstr0/2) {              //When mstr has wrapped around to 0, restart the buffer
    dec_data.params.kin = 0;
    m_bufferPos = 0;
//    printf("%s(%0.1f) reset buffer mstr:%d mstr0:%d maxSize:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),mstr,mstr0,maxSize);
#if JTDX_DEBUG_TO_FILE
    FILE * pFile = fopen (debug_file_.c_str(),"a");
    fprintf (pFile,"%s(%0.1f) reset buffer mstr:%d mstr0:%d maxSize:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),mstr,mstr0,maxSize);
    fclose (pFile);
#endif
  }
  mstr0=mstr;

  // no torn frames
  Q_ASSERT (!(maxSize % static_cast<qint32> (bytesPerFrame)));
  // these are in terms of input frames (not down sampled)
  size_t framesAcceptable ((sizeof (dec_data.d2) /
                            sizeof (dec_data.d2[0]) - dec_data.params.kin) * m_downSampleFactor);
  size_t framesAccepted (qMin (static_cast<size_t> (maxSize /
                                                    bytesPerFrame), framesAcceptable));

  if (framesAccepted < static_cast<size_t> (maxSize / bytesPerFrame)) {
    qDebug () << "dropped " << maxSize / bytesPerFrame - framesAccepted
                << " frames of data on the floor!"
                << dec_data.params.kin << mstr;
//    printf("%s(%0.1f) dropped %ld frames of data %d %d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),maxSize / bytesPerFrame - framesAccepted,dec_data.params.kin,mstr);
#if JTDX_DEBUG_TO_FILE
    FILE * pFile = fopen (debug_file_.c_str(),"a");
    fprintf (pFile,"%s(%0.1f) dropped %ld frames of data %d %d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),maxSize / bytesPerFrame - framesAccepted,dec_data.params.kin,mstr);
    fclose (pFile);
#endif
    }

    for (unsigned remaining = framesAccepted; remaining; ) {
      size_t numFramesProcessed (qMin (m_samplesPerFFT *
                                       m_downSampleFactor - m_bufferPos, remaining));

      if(m_downSampleFactor > 1) {
//  printf ("%s(%0.1f) writeAudioData maxs %d bytesPerFrame %ld Accepted %ld remaining %d Processed %ld Bufferpos %d kin %d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),
//  m_jtdxtime->GetOffset(),maxSize,bytesPerFrame,framesAccepted,remaining,numFramesProcessed,m_bufferPos,dec_data.params.kin);
        store (&data[(framesAccepted - remaining) * bytesPerFrame],
               numFramesProcessed, &m_buffer[m_bufferPos]);
        m_bufferPos += numFramesProcessed;

        if(m_bufferPos==m_samplesPerFFT*m_downSampleFactor) {
          qint32 framesToProcess (m_samplesPerFFT * m_downSampleFactor);
          qint32 framesAfterDownSample (m_samplesPerFFT);
          if(m_downSampleFactor > 1 && dec_data.params.kin>=0 &&
             dec_data.params.kin < (NTMAX*12000 - framesAfterDownSample)) {
            fil4_(&m_buffer[0], &framesToProcess, &dec_data.d2[dec_data.params.kin],
                  &framesAfterDownSample, &dec_data.dd2[dec_data.params.kin]);
            dec_data.params.kin += framesAfterDownSample;
          } else {
            // qDebug() << "framesToProcess     = " << framesToProcess;
            // qDebug() << "dec_data.params.kin = " << dec_data.params.kin;
            // qDebug() << "secondInPeriod      = " << secondInPeriod();
            // qDebug() << "framesAfterDownSample" << framesAfterDownSample;
          }
//    printf("%s(%0.1f) frameswritten %d downSampleFactor %d samplesPerFFT %d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),dec_data.params.kin,m_downSampleFactor,m_samplesPerFFT);
          Q_EMIT tciframeswritten (dec_data.params.kin);
//    printf("%s(%0.1f) frameswritten done\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset());
          m_bufferPos = 0;
        }

      } else {
//         printf ("%s(%0.1f) writeAudioData2 maxs %d bytesPerFrame %ld Accepted %ld remaining %d Processed %ld Bufferpos %d kin %d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),maxSize,bytesPerFrame,framesAccepted,remaining,numFramesProcessed,m_bufferPos,dec_data.params.kin);
         store (&data[(framesAccepted - remaining) * bytesPerFrame],
               numFramesProcessed, &dec_data.d2[dec_data.params.kin]);
        m_bufferPos += numFramesProcessed;
        dec_data.params.kin += numFramesProcessed;
        if (m_bufferPos == static_cast<unsigned> (m_samplesPerFFT)) {
//          printf("%s(%0.1f) frameswritten2 %d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),dec_data.params.kin);
          Q_EMIT tciframeswritten (dec_data.params.kin);
//          printf("%s(%0.1f) frameswritten2 done\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset());
          m_bufferPos = 0;
        }
      }
      remaining -= numFramesProcessed;
    }



  return maxSize;    // we drop any data past the end of the buffer on
  // the floor until the next period starts
}

  void TCITransceiver::rx2_enable (bool on)
{
//  printf ("%s(%0.1f) TCI rx2_enable:%d->%d busy:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),rx2_,on,busy_rx2_);
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf (pFile,"%s(%0.1f) TCI rx2_enable:%d->%d busy:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),rx2_,on,busy_rx2_);
  fclose (pFile);
#endif
  if (busy_rx2_) return;
  requested_rx2_ = on;
  busy_rx2_ = true;
  const QString cmd = CmdRxEnable + SmDP + "1" + SmCM + (requested_rx2_ ? "true" : "false") + SmTZ;
  sendTextMessage(cmd);
  mysleep1(1000);
  busy_rx2_ = false;
}

  void TCITransceiver::rig_split ()
{
//  printf ("%s(%0.1f) TCI rig_split:%d->%d busy:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),split_,requested_split_,busy_split_);
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf (pFile,"%s(%0.1f) TCI rig_split:%d->%d busy:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),split_,requested_split_,busy_split_);
  fclose (pFile);
#endif
  if (busy_split_) return;
  if (tci_timer2_->isActive()) mysleep2(0);
  busy_split_ = true;
  const QString cmd = CmdSplitEnable + SmDP + rx_ + SmCM + (requested_split_ ? "true" : "false") + SmTZ;
  sendTextMessage(cmd);
  mysleep2(500);
  busy_split_ = false;
  if (requested_split_ == split_) update_split (split_);
  else {
//    printf ("%s(%0.1f) TCI failed set split:%d->%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),split_,requested_split_);
#if JTDX_DEBUG_TO_FILE
    FILE * pFile = fopen (debug_file_.c_str(),"a");
    fprintf (pFile,"%s(%0.1f) TCI failed set split:%d->%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),split_,requested_split_);
    fclose (pFile);
#endif
    error_ = tr ("TCI failed set split");
  }
}

  void TCITransceiver::rig_power (bool on)
{
  TRACE_CAT ("TCITransceiver", on << state ());
//  printf ("%s(%0.1f) TCI rig_power:%d _power_:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),on,_power_);
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf (pFile,"%s(%0.1f) TCI rig_power:%d _power_:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),on,_power_);
  fclose (pFile);
#endif
  if (on != _power_) {
    if (on) {
      const QString cmd = CmdStart + SmTZ;
      sendTextMessage(cmd);
    } else {
      const QString cmd = CmdStop + SmTZ;
      sendTextMessage(cmd);
    }
  } 

}

  void TCITransceiver::stream_audio (bool on)
{
  TRACE_CAT ("TCITransceiver", on << state ());
//  printf ("%s(%0.1f) TCI stream_audio:%d stream_audio_:%d requested_stream_audio_:%d rx_=%s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),on,stream_audio_,requested_stream_audio_,rx_.toStdString().c_str());
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf (pFile,"%s(%0.1f) TCI stream_audio:%d stream_audio_:%d requested_stream_audio_:%d rx_=%s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),on,stream_audio_,requested_stream_audio_,rx_.toStdString().c_str());
  fclose (pFile);
#endif
  if (on != stream_audio_ && tci_Ready) {
    requested_stream_audio_ = on;
    if (on) {
      const QString cmd = CmdAudioStart + SmDP + rx_ + SmTZ;
      sendTextMessage(cmd);
    } else {
      const QString cmd = CmdAudioStop + SmDP + rx_ + SmTZ;
      sendTextMessage(cmd);
    }
  } 

}

  void TCITransceiver::do_audio (bool on)
{
  TRACE_CAT ("TCITransceiver", on << state ());
//  printf ("%s(%0.1f) TCI do_audio:%d audio_:%d state:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),on,audio_,state().audio());
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf (pFile,"%s(%0.1f) TCI do_audio:%d audio_:%d state:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),on,audio_,state().audio());
  fclose (pFile);
#endif
  if (on) {
    dec_data.params.kin = 0;
    m_bufferPos = 0;
  }
  audio_ = on;
}

  void TCITransceiver::do_period (double period)
{
  TRACE_CAT ("TCITransceiver", period << state ());
//  printf ("%s(%0.1f) TCI do_period:%0.1f m_period_:%0.1f state:%0.1f\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),period,m_period,state().period());
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf (pFile,"%s(%0.1f) TCI do_period:%0.1f m_period_:%0.1f state:%0.1f\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),period,m_period,state().period());
  fclose (pFile);
#endif
  m_period = period;
}

  void TCITransceiver::do_txvolume (qreal volume)
{
  TRACE_CAT ("TCITransceiver", volume << state ());
  QString drive = QString::number(round(100 - volume * 2.2222222));
//  printf ("%s(%0.1f) TCI tci_Ready:%d do_txvolume:%0.1f state:%0.1f drive:%s drive_:%s drive_busy:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),tci_Ready,volume,state().volume(),drive.toStdString().c_str(),drive_.toStdString().c_str(),busy_drive_);
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf (pFile,"%s(%0.1f) TCI tci_Ready:%d do_txvolume:%0.1f state:%0.1f drive:%s drive_:%s drive_busy:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),tci_Ready,volume,state().volume(),drive.toStdString().c_str(),drive_.toStdString().c_str(),busy_drive_);
  fclose (pFile);
#endif
  requested_drive_ = drive;
  if (busy_drive_ || !tci_Ready || drive_ == drive) {
  if (busy_drive_) busy_drive_ = false;
  } else {
    busy_drive_ = true;
    if (ESDR3) {
      const QString cmd = CmdDrive + SmDP + rx_ + SmCM + drive + SmTZ;
      sendTextMessage(cmd);
    } else {
      const QString cmd = CmdDrive + SmDP + drive + SmTZ;
      sendTextMessage(cmd);
    }
  }
}

  void TCITransceiver::do_blocksize (qint32 blocksize)
{
  TRACE_CAT ("TCITransceiver", blocksize << state ());
//  printf ("%s(%0.1f) TCI do_blocksize:%d m_samplesPerFFT:%d state:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),blocksize,m_samplesPerFFT,state().blocksize());
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf (pFile,"%s(%0.1f) TCI do_blocksize:%d m_samplesPerFFT:%d state:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),blocksize,m_samplesPerFFT,state().blocksize());
  fclose (pFile);
#endif
  m_samplesPerFFT = blocksize;
}

  void TCITransceiver::do_ptt (bool on)
{
  TRACE_CAT ("TCITransceiver", on << state ());
//  printf ("%s(%0.1f) TCI do_ptt:%d PTT_:%d requested_PTT_:%d busy_PTT_:%d state:%d use_for_ptt:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),on,PTT_,requested_PTT_,busy_PTT_,state().ptt(),use_for_ptt_);
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf (pFile,"%s(%0.1f) TCI do_ptt:%d PTT_:%d requested_PTT_:%d busy_PTT_:%d state:%d use_for_ptt:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),on,PTT_,requested_PTT_,busy_PTT_,state().ptt(),use_for_ptt_);
  fclose (pFile);
#endif
  if (use_for_ptt_)
    {
      if (on != PTT_) {
        if (!inConnected && !error_.isEmpty()) throw error {error_};
//        if (!inConnected){ PTT_ = on; update_PTT(on); return; }
        else if (busy_PTT_ || !tci_Ready || !_power_) return;
        else busy_PTT_ = true;
        requested_PTT_ = on;
        if (ESDR3) {
          const QString cmd = CmdTrx + SmDP + rx_ + SmCM + (on ? "true" : "false") + SmCM + "tci"+ SmTZ;
          sendTextMessage(cmd);
        } else {
          const QString cmd = CmdTrx + SmDP + rx_ + SmCM + (on ? "true" : "false") + SmTZ;
          sendTextMessage(cmd);
        }
        mysleep3(1000);
        busy_PTT_ = false;
        if (requested_PTT_ == PTT_) {
          update_PTT(PTT_);
          if (PTT_ && do_snr_) update_level (-54);
          else { power_ = 0; if (do_pwr_) update_power (0); swr_ = 0; if (do_pwr_) update_swr (0); }
        } else {
//          printf ("%s(%0.1f) TCI failed set ptt %d->%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),PTT_,requested_PTT_);
#if JTDX_DEBUG_TO_FILE
          FILE * pFile = fopen (debug_file_.c_str(),"a");
          fprintf (pFile,"%s(%0.1f) TCI failed set ptt %d->%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),PTT_,requested_PTT_);
          fclose (pFile);
#endif
          error_ = tr ("TCI failed to set ptt");
//          tci_Ready = false;
//          throw error {tr ("TCI failed to set ptt")};
        }
      } else update_PTT(on); 
    }
  else
    {
          TRACE_CAT ("TCITransceiver", "TCI should use PTT via CAT");
          throw error {tr ("TCI should use PTT via CAT")};
    }
}

void TCITransceiver::do_frequency (Frequency f, MODE m, bool no_ignore)
{
  TRACE_CAT ("TCITransceiver", f << state ());
  auto f_string = frequency_to_string (f);
//  printf ("%s(%0.1f) TCI do_frequency:%s current_frequency:%s mode:%s current_mode:%s no_ignore:%d busy:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),f_string.toStdString().c_str(),rx_frequency_.toStdString().c_str(),map_mode(m).toStdString().c_str(),requested_mode_.toStdString().c_str(),no_ignore,busy_rx_frequency_);
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf (pFile,"%s(%0.1f) TCI do_frequency:%s current_frequency:%s mode:%s current_mode:%s no_ignore:%d busy:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),f_string.toStdString().c_str(),rx_frequency_.toStdString().c_str(),map_mode(m).toStdString().c_str(),requested_mode_.toStdString().c_str(),no_ignore,busy_rx_frequency_);
  fclose (pFile);
#endif
  if  (tci_Ready && busy_rx_frequency_ && no_ignore) {
//    printf ("%s(%0.1f) TCI do_frequency critical no_ignore set vfo will be missed\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset());
#if JTDX_DEBUG_TO_FILE
    FILE * pFile = fopen (debug_file_.c_str(),"a");
    fprintf (pFile,"%s(%0.1f) TCI do_frequency critical no_ignore set vfo will be missed\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset());
    fclose (pFile);
#endif
  }
  if (busy_rx_frequency_) return;
  else {
    requested_rx_frequency_ = f_string;
    requested_mode_ = map_mode (m);
  }
  if (tci_Ready && _power_) {
    if (rx_frequency_ != requested_rx_frequency_) {
      busy_rx_frequency_ = true;
      band_change = abs(rx_frequency_.toInt()-requested_rx_frequency_.toInt()) > 1000000;
      const QString cmd = CmdVFO + SmDP + rx_ + SmCM + "0" + SmCM + requested_rx_frequency_ + SmTZ;
      sendTextMessage(cmd);
      mysleep1(2000);
//      if (band_change) mysleep1(500);
      if (requested_rx_frequency_ == rx_frequency_) update_rx_frequency (f);
      else {
//        printf ("%s(%0.1f) TCI failed set rxfreq:%s->%s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),rx_frequency_.toStdString().c_str(),requested_rx_frequency_.toStdString().c_str());
#if JTDX_DEBUG_TO_FILE
        FILE * pFile = fopen (debug_file_.c_str(),"a");
        fprintf (pFile,"%s(%0.1f) TCI failed set rxfreq:%s->%s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),rx_frequency_.toStdString().c_str(),requested_rx_frequency_.toStdString().c_str());
        fclose (pFile);
#endif
        error_ = tr ("TCI failed set rxfreq");
//        tci_Ready = false;
//        throw error {tr ("TCI failed set rxfreq")};
      }
      busy_rx_frequency_ = false;
    } else update_rx_frequency (string_to_frequency (rx_frequency_));
      
    if (!requested_mode_.isEmpty() && requested_mode_ != mode_ && !busy_mode_) {
      busy_mode_ = true;
      sendTextMessage(mode_to_command(requested_mode_));
      mysleep1(1000);
      if (requested_mode_.isEmpty() || requested_mode_ == mode_) update_mode (m);
      else {
//        printf ("%s(%0.1f) TCI failed set mode %s->%s",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),mode_.toStdString().c_str(),requested_mode_.toStdString().c_str());
#if JTDX_DEBUG_TO_FILE
        FILE * pFile = fopen (debug_file_.c_str(),"a");
        fprintf (pFile,"%s(%0.1f) TCI failed set mode %s->%s",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),mode_.toStdString().c_str(),requested_mode_.toStdString().c_str());
        fclose (pFile);
#endif
        error_ = tr ("TCI failed set mode");
//        tci_Ready = false;
//        throw error {tr ("TCI failed set mode")};
      }
      busy_mode_ = false;
    } 
//    if (band_change) {
//      tci_timer2_->start(210);
//      band_change = false;
//    }
  } else { 
    update_rx_frequency (f);
    update_mode (m);
  }
}

void TCITransceiver::do_tx_frequency (Frequency tx, MODE mode, bool no_ignore)
{
  TRACE_CAT ("TCITransceiver", tx << state ());
  auto f_string = frequency_to_string (tx);
//  printf ("%s(%0.1f) TCI do_tx_frequency:%s current_frequency:%s mode:%s no_ignore:%d busy:%d timer:%d remaining:%d band_change:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),f_string.toStdString().c_str(),other_frequency_.toStdString().c_str(),map_mode(mode).toStdString().c_str(),no_ignore,busy_other_frequency_,tci_timer2_->isActive(),tci_timer2_->remainingTime(),band_change);
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf (pFile,"%s(%0.1f) TCI do_tx_frequency:%s current_frequency:%s mode:%s no_ignore:%d busy:%d timer:%d remaining:%d band_change:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),f_string.toStdString().c_str(),other_frequency_.toStdString().c_str(),map_mode(mode).toStdString().c_str(),no_ignore,busy_other_frequency_,tci_timer2_->isActive(),tci_timer2_->remainingTime(),band_change);
  fclose (pFile);
#endif
  if  (tci_Ready && busy_other_frequency_ && no_ignore) {
//    printf ("%s(%0.1f) TCI do_txfrequency critical no_ignore set tx vfo will be missed\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset());
#if JTDX_DEBUG_TO_FILE
    FILE * pFile = fopen (debug_file_.c_str(),"a");
    fprintf (pFile,"%s(%0.1f) TCI do_txfrequency critical no_ignore set tx vfo will be missed\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset());
    fclose (pFile);
#endif
  }
  if (busy_other_frequency_) return;
  requested_other_frequency_ = f_string;
  requested_mode_ = map_mode (mode);
  if (tx)
    {
      requested_split_ = true;
      if (tci_Ready && _power_) {
        if (band_change && !tci_timer2_->isActive()) {if (!HPSDR) mysleep2(210); else mysleep2(2000);}
        else if (tci_timer2_->isActive()) mysleep2(0);
        if (requested_split_ != split_) rig_split();
        else update_split (split_);
        if (other_frequency_ != requested_other_frequency_) {
          busy_other_frequency_ = true;
//          printf ("%s(%0.1f) TCI VFO1 command sent\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset());
#if JTDX_DEBUG_TO_FILE
          FILE * pFile = fopen (debug_file_.c_str(),"a");
          fprintf (pFile,"%s(%0.1f) TCI VFO1 command sent\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset());
          fclose (pFile);
#endif
          const QString cmd = CmdVFO + SmDP + rx_ + SmCM + "1" + SmCM + requested_other_frequency_ + SmTZ;
          sendTextMessage(cmd);
          mysleep2(1000);
          if (requested_other_frequency_ == other_frequency_) update_other_frequency (tx);
          else {
//            printf ("%s(%0.1f) TCI failed set txfreq:%s->%s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),other_frequency_.toStdString().c_str(),requested_other_frequency_.toStdString().c_str());
#if JTDX_DEBUG_TO_FILE
            FILE * pFile = fopen (debug_file_.c_str(),"a");
            fprintf (pFile,"%s(%0.1f) TCI failed set txfreq:%s->%s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),other_frequency_.toStdString().c_str(),requested_other_frequency_.toStdString().c_str());
            fclose (pFile);
#endif
            error_ = tr ("TCI failed set txfreq");
//            tci_Ready = false;
//            throw error {tr ("TCI failed set txfreq")};
          }
          busy_other_frequency_ = false;
        } else update_other_frequency (string_to_frequency (other_frequency_));
          
      } else {
        update_split (requested_split_);
        update_other_frequency (tx);
      }
    }
  else {
    requested_split_ = false;
    requested_other_frequency_ = "";
    if (tci_Ready && _power_) {
      if (band_change) {mysleep2(2000); if (!HPSDR) mysleep2(200);}
      if (tci_timer2_->isActive()) mysleep2(0);
      if (requested_split_ != split_) rig_split();
      else update_split (split_);
      update_other_frequency (tx);
    } else {
      update_split (requested_split_);
      update_other_frequency (tx);
    }
  }
}

void TCITransceiver::do_mode (MODE m)
{
  TRACE_CAT ("TCITransceiver", m << state ());
  auto m_string = map_mode (m);
//  printf ("%s(%0.1f) TCI do_mode:%s->%s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),mode_.toStdString().c_str(),m_string.toStdString().c_str());
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf (pFile,"%s(%0.1f) TCI do_mode:%s->%s\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),mode_.toStdString().c_str(),m_string.toStdString().c_str());
  fclose (pFile);
#endif
  if (requested_mode_ != m_string) requested_mode_ = m_string;
  if (!requested_mode_.isEmpty() && mode_ != requested_mode_ && !busy_mode_) {
    busy_mode_ = true;
    sendTextMessage(mode_to_command(requested_mode_));
    mysleep1(1000);
    if (requested_mode_ == mode_) update_mode (m);
    else {
//      printf ("%s(%0.1f) TCI failed set mode %s->%s",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),mode_.toStdString().c_str(),requested_mode_.toStdString().c_str());
#if JTDX_DEBUG_TO_FILE
      FILE * pFile = fopen (debug_file_.c_str(),"a");
      fprintf (pFile,"%s(%0.1f) TCI failed set mode %s->%s",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),mode_.toStdString().c_str(),requested_mode_.toStdString().c_str());
      fclose (pFile);
#endif
      error_ = tr ("TCI failed set mode");
//      tci_Ready = false;
//      throw error {tr ("TCI failed set mode")};
    }
    busy_mode_ = false;
  }
}

void TCITransceiver::do_poll ()
{
  if (!inConnected) {
//    printf("%s(%0.1f) TCI do_poll |%s| split:%d ptt:%d rx_busy:%d tx_busy:%d level:%d power:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),error_.toStdString().c_str(),state ().split (),state (). ptt (),busy_rx_frequency_,busy_other_frequency_,level_,power_);
#if JTDX_DEBUG_TO_FILE
    FILE * pFile = fopen (debug_file_.c_str(),"a");
    fprintf (pFile,"%s(%0.1f) TCI do_poll |%s| split:%d ptt:%d rx_busy:%d tx_busy:%d level:%d power:%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),error_.toStdString().c_str(),state ().split (),state (). ptt (),busy_rx_frequency_,busy_other_frequency_,level_,power_);
    fclose (pFile);
#endif
  }
  if (/*!inConnected && */!error_.isEmpty()) {tci_Ready = false; throw error {error_};}
  else if (!tci_Ready) throw error {tr ("TCI could not be opened")};
  update_rx_frequency (string_to_frequency (rx_frequency_));
  update_split(split_);
  if (state ().split ()) {
    if (other_frequency_ == requested_other_frequency_) update_other_frequency (string_to_frequency (other_frequency_));
    else if (!busy_rx_frequency_) do_tx_frequency(string_to_frequency (requested_other_frequency_),get_mode(true),false);
  }
  update_mode (get_mode());
  if (do_pwr_ && PTT_) {update_power (power_ * 100); update_swr (swr_*10);}
  if (do_snr_ && !PTT_) {
      update_level (level_);
      if(!ESDR3) {
        const QString cmd = CmdSmeter + SmDP + rx_ + SmCM + "0" +  SmTZ;
        sendTextMessage(cmd);
      }
  }
}

auto TCITransceiver::get_mode (bool requested) -> MODE
{
  MODE m {UNK};
  if (requested) {
    if ("am" == requested_mode_)
      {
        m = AM;
      }
    else if ("cw" == requested_mode_)
      {
        m = CW;
      }
    else if ("wfm" == requested_mode_)
      {
        m = FM;
      }
    else if ("nfm" == requested_mode_)
      {
        m = DIG_FM;
      }
    else if ("lsb" == requested_mode_)
      {
        m = LSB;
      }
    else if ("usb" == requested_mode_)
      {
        m = USB;
      }
    else if ("digl" == requested_mode_)
      {
        m = DIG_L;
      }
    else if ("digu" == requested_mode_)
      {
        m = DIG_U;
      }
  } else {
    if ("am" == mode_)
      {
        m = AM;
      }
    else if ("cw" == mode_)
      {
        m = CW;
      }
    else if ("wfm" == mode_)
      {
        m = FM;
      }
    else if ("nfm" == mode_)
      {
        m = DIG_FM;
      }
    else if ("lsb" == mode_)
      {
        m = LSB;
      }
    else if ("usb" == mode_)
      {
        m = USB;
      }
    else if ("digl" == mode_)
      {
        m = DIG_L;
      }
    else if ("digu" == mode_)
      {
        m = DIG_U;
      }
  }
  return m;
}

QString TCITransceiver::mode_to_command (QString m_string) const
{
//    if (ESDR3) {
//      const QString cmd = CmdMode + SmDP + rx_ + SmCM + m_string.toUpper() + SmTZ;
//      return cmd;
//    } else {
      const QString cmd = CmdMode + SmDP + rx_ + SmCM + m_string + SmTZ;
      return cmd;
//    }
}

QString TCITransceiver::frequency_to_string (Frequency f) const
{
  // number is localized and in kHz, avoid floating point translation
  // errors by adding a small number (0.1Hz)
//  auto f_string = QString {"%L2"}.arg (f);
  auto f_string = QString {}.setNum(f);
//  f_string = f_string.simplified().remove(' ');
//  f_string = f_string.replace(",","");
//  f_string = f_string.replace(".","");
//  f_string = f_string.replace("'","");
//  f_string = f_string.replace("’","");
//  printf ("frequency_to_string3 |%s|\n",f_string.toStdString().c_str());
  return f_string;
  
}

auto TCITransceiver::string_to_frequency (QString s) const -> Frequency
{
  // temporary hack because Commander is returning invalid UTF-8 bytes
  s.replace (QChar {QChar::ReplacementCharacter}, locale_.groupSeparator ());

  bool ok;

  auto f = QLocale::c ().toDouble (s, &ok); // temporary fix

  if (!ok)
    {
      throw error {tr ("TCI sent an unrecognized frequency") + " |" + s + "|"};
    }
  return f;
}

void TCITransceiver::mysleep1 (int ms)
{
//  tci_timer1->setSingleShot(true);
//  printf("%s(%0.1f) TCI sleep1 start %d %d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),ms,tci_timer1_->isActive());
  if (ms) tci_timer1_->start(ms);
  tci_loop1_->exec();
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf (pFile,"%s(%0.1f) TCI sleep1 %d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),tci_timer1_->isActive());
  fclose (pFile);
#endif
//  printf("%s(%0.1f) TCI sleep1 end %d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),tci_timer1_->isActive());
  if (tci_timer1_->isActive() && tci_Ready) tci_timer1_->stop();
}
void TCITransceiver::mysleep2 (int ms)
{
//  tci_timer2->setSingleShot(true);
//  printf("%s(%0.1f) TCI sleep2 start %d %d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),ms,tci_timer2_->isActive());
  if (ms) tci_timer2_->start(ms);
  tci_loop2_->exec();
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf (pFile,"%s(%0.1f) TCI sleep2 %d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),tci_timer2_->isActive());
  fclose (pFile);
#endif
//  printf("%s(%0.1f) TCI sleep2 end %d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),tci_timer2_->isActive());
  if (tci_timer2_->isActive()) tci_timer2_->stop();
}
void TCITransceiver::mysleep3 (int ms)
{
//  tci_timer3->setSingleShot(true);
//  printf("%s(%0.1f) TCI sleep3 start %d %d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),ms,tci_timer3_->isActive());
  if (ms) tci_timer3_->start(ms);
  tci_loop3_->exec();
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf (pFile,"%s(%0.1f) TCI sleep3 %d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),tci_timer3_->isActive());
  fclose (pFile);
#endif
//  printf("%s(%0.1f) TCI sleep3 end %d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),tci_timer3_->isActive());
  if (tci_timer3_->isActive()) tci_timer3_->stop();
}
// Modulator part

void TCITransceiver::do_modulator_start (unsigned symbolsLength, double framesPerSymbol,
                       double frequency, double toneSpacing, bool synchronize, double dBSNR, double TRperiod)
{
//  QThread::currentThread()->setPriority(QThread::HighPriority);
//  Q_ASSERT (stream);

// Time according to this computer which becomes our base time
  qint64 ms0 = m_jtdxtime->currentMSecsSinceEpoch2() % 86400000;
//  qDebug() << "ModStart" << QDateTime::currentDateTimeUtc().toString("hh:mm:ss.sss");
  unsigned mstr = ms0 % int(1000.0*m_period); // ms into the nominal Tx start time
  if (m_state != Idle) {
//    stop ();
    throw error {tr ("TCI modulator not Idle")};
  }
  m_quickClose = false;
  m_symbolsLength = symbolsLength;
  m_isym0 = std::numeric_limits<unsigned>::max (); // big number
  m_frequency0 = 0.;
  m_phi = 0.;
  m_addNoise = dBSNR < 0.;
  m_nsps = framesPerSymbol;
  m_trfrequency = frequency;
  m_amp = std::numeric_limits<qint16>::max ();
  m_toneSpacing = toneSpacing;
  m_TRperiod=TRperiod;
  unsigned delay_ms=1000;
  if(m_nsps==1920) delay_ms=500;   //FT8
  else if(m_nsps==576) {
    delay_ms=500;   //FT4
  }
  // noise generator parameters
  if (m_addNoise) {
    m_snr = qPow (10.0, 0.05 * (dBSNR - 6.0));
    m_fac = 3000.0;
    if (m_snr > 1.0) m_fac = 3000.0 / m_snr;
  }

  // round up to an exact portion of a second that allows for startup delays
  //m_ic = (mstr / delay_ms) * audioSampleRate * delay_ms / 1000;
  auto mstr2 = mstr - delay_ms;
  if (mstr <= delay_ms) {
    m_ic = 0;
  } else {
    m_ic = mstr2 * (audioSampleRate / 1000);
  }
  m_silentFrames = 0;
  // calculate number of silent frames to send
  if (m_ic == 0 && synchronize && !m_tuning)	{
    m_silentFrames = audioSampleRate / (1000 / delay_ms) - (mstr * (audioSampleRate / 1000));
  }
  m_state = (synchronize && m_silentFrames) ?
                        Synchronizing : Active;
//  printf ("%s(%0.1f) TCI modulator startdelay_ms=%d ASR=%d mstr=%d mstr2=%d m_ic=%d s_Frames=%lld synchronize=%d m_tuning=%d State=%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),delay_ms,audioSampleRate,mstr,mstr2,m_ic,m_silentFrames,synchronize,m_tuning,m_state);
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf (pFile,"%s(%0.1f) TCI modulator startdelay_ms=%d ASR=%d mstr=%d mstr2=%d m_ic=%d s_Frames=%lld synchronize=%d m_tuning=%d State=%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),delay_ms,audioSampleRate,mstr,mstr2,m_ic,m_silentFrames,synchronize,m_tuning,m_state);
  fclose (pFile);
#endif
  Q_EMIT tci_mod_active(m_state != Idle);
}

void TCITransceiver::do_tune (bool newState)
{
//  printf("%s(%0.1f) TCI modulator tune %d ->%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),m_tuning,newState);
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf (pFile,"%s(%0.1f) TCI modulator tune %d ->%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),m_tuning,newState);
  fclose (pFile);
#endif
  m_tuning = newState;
  if (!m_tuning) do_modulator_stop (true);
}

void TCITransceiver::do_modulator_stop (bool quick)
{
  m_quickClose = quick;
  if(m_state != Idle) {
  m_state = Idle;
  Q_EMIT tci_mod_active(m_state != Idle);
  }
  tx_audio_ = false;
}

quint16 TCITransceiver::readAudioData (float * data, qint32 maxSize)
{
  double toneFrequency=1500.0;
  if(m_nsps==6) {
    toneFrequency=1000.0;
    m_trfrequency=1000.0;
    m_frequency0=1000.0;
  }
  if(maxSize==0) return 0;

  qint64 numFrames (maxSize/bytesPerFrame);
  float * samples (reinterpret_cast<float *> (data));
  float * end (samples + numFrames * bytesPerFrame);
//  printf("%s(%0.1f) readAudioData %f %f %lld %lld %d %p %p\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),m_nsps,m_TRperiod,numFrames,m_silentFrames,maxSize,samples,end);
  qint64 framesGenerated (0);

  switch (m_state)
    {
    case Synchronizing:
      {
        if (m_silentFrames)	{  // send silence up to first second
          framesGenerated = qMin (m_silentFrames, numFrames);
          for ( ; samples != end; samples = load (0, samples)) { // silence
          }
          m_silentFrames -= framesGenerated;
          return framesGenerated * bytesPerFrame;
        }
        m_state = Active;
        Q_EMIT tci_mod_active(m_state != Idle);
        m_cwLevel = false;
        m_ramp = 0;		// prepare for CW wave shaping
      }
      // fall through

    case Active:
      {
        unsigned int isym=0;
        qint16 sample=0;
        if(!m_tuning) isym=m_ic/(4.0*m_nsps);          // Actual fsample=48000
		bool slowCwId=((isym >= m_symbolsLength) && (icw[0] > 0));
        m_nspd=2560;                 // 22.5 WPM

        if(m_TRperiod > 16.0 && slowCwId) {     // Transmit CW ID?
          m_dphi = m_twoPi*m_trfrequency/audioSampleRate;
          unsigned ic0 = m_symbolsLength * 4 * m_nsps;
          unsigned j(0);

          while (samples != end) {
            j = (m_ic - ic0)/m_nspd + 1; // symbol of this sample
            bool level {bool (icw[j])};
            m_phi += m_dphi;
            if (m_phi > m_twoPi) m_phi -= m_twoPi;
            sample=0;
            float amp=32767.0;
            float x=0.0;
            if(m_ramp!=0) {
              x=qSin(float(m_phi));
              if(SOFT_KEYING) {
                amp=qAbs(qint32(m_ramp));
                if(amp>32767.0) amp=32767.0;
              }
              sample=round(amp*x);
            }
            if (int (j) <= icw[0] && j < NUM_CW_SYMBOLS) { // stopu condition
              samples = load (postProcessSample (sample), samples);
              ++framesGenerated;
              ++m_ic;
            } else {
              m_state = Idle;
//              printf("%s(%0.1f) TCI modulator Idle1 %lld frames generated\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),framesGenerated);
#if JTDX_DEBUG_TO_FILE
              FILE * pFile = fopen (debug_file_.c_str(),"a");
              fprintf (pFile,"%s(%0.1f) TCI modulator Idle1 %lld frames generated\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),framesGenerated);
              fclose (pFile);
#endif
              Q_EMIT tci_mod_active(m_state != Idle);
              return framesGenerated * bytesPerFrame;
            }

            // adjust ramp
            if ((m_ramp != 0 && m_ramp != std::numeric_limits<qint16>::min ()) || level != m_cwLevel) {
              // either ramp has terminated at max/min or direction has changed
              m_ramp += RAMP_INCREMENT; // ramp
            }
            m_cwLevel = level;
          }
          return framesGenerated * bytesPerFrame;
        } //End of code for CW ID

        double const baud (12000.0 / m_nsps);
        // fade out parameters (no fade out for tuning)
        unsigned int i0,i1;
        if(m_tuning) {
          i1 = i0 = 9999 * m_nsps;
        } else {
          i0=(m_symbolsLength - 0.017) * 4.0 * m_nsps;
          i1= m_symbolsLength * 4.0 * m_nsps;
        }

        sample=0;
        for (unsigned i = 0; i < numFrames && m_ic <= i1; ++i) {
//          printf("algus %d %lld %d %d",i,numFrames,m_ic,i1);
          if(m_TRperiod > 16.0 || m_tuning) {
            isym=0;
            if(!m_tuning) isym=m_ic / (4.0 * m_nsps);       //Actual fsample=48000
            if (isym != m_isym0 || m_trfrequency != m_frequency0) {
              if(itone[0]>=100) {
                m_toneFrequency0=itone[0];
              } else {
                if(m_toneSpacing==0.0) {
                  m_toneFrequency0=m_trfrequency + itone[isym]*baud;
                } else {
                  m_toneFrequency0=m_trfrequency + itone[isym]*m_toneSpacing;
                }
              }
//            qDebug() << "B" << m_ic << numFrames << isym << itone[isym] << toneFrequency0 << m_nsps;
              m_dphi = m_twoPi * m_toneFrequency0 / audioSampleRate;
              m_isym0 = isym;
              m_frequency0 = m_trfrequency;         //???
            }

            int j=m_ic/480;
            if(m_fSpread>0.0 and j!=m_j0) {
#if QT_VERSION >= QT_VERSION_CHECK(5, 15, 0)
            float x1=QRandomGenerator::global ()->generateDouble ();
            float x2=QRandomGenerator::global ()->generateDouble ();
#else
            float x1=(float)qrand()/RAND_MAX;
            float x2=(float)qrand()/RAND_MAX;
#endif
              toneFrequency = m_toneFrequency0 + 0.5*m_fSpread*(x1+x2-1.0);
              m_dphi = m_twoPi * toneFrequency / audioSampleRate;
              m_j0=j;
            }

            m_phi += m_dphi;
            if (m_phi > m_twoPi) m_phi -= m_twoPi;
            //ramp for first tone
            if (m_ic==0) m_amp = m_amp * 0.008144735;
            if (m_ic > 0 and  m_ic < 191) m_amp = m_amp / 0.975;
            //ramp for last tone
            if (m_ic > i0) m_amp = 0.99 * m_amp;
            if (m_ic > i1) m_amp = 0.0;
            sample=qRound(m_amp*qSin(m_phi));
          }
          //transmit from a precomputed FT8 wave[] array:
          if(!m_tuning and (m_toneSpacing < 0.0)) { m_amp=32767.0; sample=qRound(m_amp*foxcom_.wave[m_ic]); }
          samples = load (postProcessSample (sample), samples);
          ++framesGenerated; ++m_ic;
        }

//          printf("sample saved %lld %d\n",framesGenerated,m_ic);
        if (m_amp == 0.0) { // TODO G4WJS: compare double with zero might not be wise
          if (icw[0] == 0) {
            // no CW ID to send
            m_state = Idle;
//            printf("%s(%0.1f) TCI modulator Idle2 %lld frames generated\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),framesGenerated);
#if JTDX_DEBUG_TO_FILE
            FILE * pFile = fopen (debug_file_.c_str(),"a");
            fprintf (pFile,"%s(%0.1f) TCI modulator Idle2 %lld frames generated\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),framesGenerated);
            fclose (pFile);
#endif
            Q_EMIT tci_mod_active(m_state != Idle);
            return framesGenerated * bytesPerFrame;
          }
          m_phi = 0.0;
        }

        m_frequency0 = m_trfrequency;
        // done for this chunk - continue on next call
        if (samples != end && framesGenerated) {
//          printf("%s(%0.1f) TCI modulator Idle3 %lld frames generated\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),framesGenerated);
#if JTDX_DEBUG_TO_FILE
          FILE * pFile = fopen (debug_file_.c_str(),"a");
          fprintf (pFile,"%s(%0.1f) TCI modulator Idle3 %lld frames generated\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),framesGenerated);
          fclose (pFile);
#endif
        }
        while (samples != end) { // pad block with silence
          samples = load (0, samples);
          ++framesGenerated;
        }
        return framesGenerated * bytesPerFrame;
      }
      // fall through

    case Idle:
      while (samples != end) samples = load (0, samples);
    }

  Q_ASSERT (Idle == m_state);
  return 0;
}

qint16 TCITransceiver::postProcessSample (qint16 sample) const
{
  if (m_addNoise) {  // Test frame, we'll add noise
    qint32 s = m_fac * (gran () + sample * m_snr / 32768.0);
    if (s > std::numeric_limits<qint16>::max ()) {
      s = std::numeric_limits<qint16>::max ();
    }
    if (s < std::numeric_limits<qint16>::min ()) {
      s = std::numeric_limits<qint16>::min ();
    }
    sample = s;
  }
  return sample;
}
