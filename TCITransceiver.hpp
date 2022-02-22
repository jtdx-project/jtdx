#ifndef TCI_TRANSCEIVER_HPP__
#define TCI_TRANSCEIVER_HPP__

#include <memory>

#include "TransceiverFactory.hpp"
#include "PollingTransceiver.hpp"

#include <QtWebSockets/QWebSocket>
#include <QTimer>
#include <QEventLoop>
#include <mutex>

typedef float REAL;

class QWebSocket;
class QByteArray;
class QString;

//
// TCI Interface
//

typedef struct
{
    quint32 receiver;       //!< номер программного приёмника
    quint32 sampleRate;     //!< частота дискретизации
    quint32 format;         //!< формат поля данных (0 - int16, 1 - int24, 2 - int32, 3 - float32, 4 - float64)
    quint32 codec;          //!< алгоритм сжатия (не реализовано), всегда 0
    quint32 crc;            //!< контрольная сумма
    quint32 length;         //!< длина поля данных к-во float чисел
    quint32 type;           //!< тип потока данных
    quint32 reserv[9];      //!< зарезервировано
    float   data[8192];     //!< поле данных
}Data_Stream;

typedef enum
{
    Iq_Stream = 0,
    RxAudioStream,
    TxAudioStream,
    TxChrono,
}Stream_Type;

class TCITransceiver final
  : public PollingTransceiver
{
  Q_OBJECT;                     // for translation context

public:
  static void register_transceivers (TransceiverFactory::Transceivers *, unsigned id1, unsigned id2);

  // takes ownership of wrapped Transceiver
  explicit TCITransceiver (std::unique_ptr<TransceiverBase> wrapped, QString const& rignr,
                                           QString const& address, bool use_for_ptt,
                                           int poll_interval, QObject * parent = nullptr);

enum Tci_Cmd {
Cmd_Unknown,
Cmd_Device,
Cmd_ReceiveOnly,
Cmd_TrxCount,
Cmd_ChannelCount,
Cmd_VfoLimits,
Cmd_IfLimits,
Cmd_ModeList,
Cmd_Mode,
Cmd_Ready,
Cmd_Stop,
Cmd_Start,
Cmd_Preamp,
Cmd_Dds,
Cmd_If,
Cmd_Trx,
Cmd_RxEnable,
Cmd_TxEnable,
Cmd_RxChannelEnable,
Cmd_RitEnable,
Cmd_RitOffset,
Cmd_XitEnable,
Cmd_XitOffset,
Cmd_SplitEnable,
Cmd_IqSR,
Cmd_IqStart,
Cmd_IqStop,
Cmd_CWSpeed,
Cmd_CWDelay,
Cmd_Spot,
Cmd_SpotDelete,
Cmd_FilterBand,
Cmd_VFO,
Cmd_Version,
Cmd_Tune,
Cmd_RxMute,
Cmd_Smeter,
Cmd_Power,
Cmd_SWR,
Cmd_ECoderRX,
Cmd_ECoderVFO,
Cmd_AudioSR,
Cmd_AudioStart,
Cmd_AudioStop,
Cmd_AppFocus,
Cmd_Volume,
Cmd_SqlEnable,
Cmd_SqlLevel,
Cmd_Drive,
Cmd_TuneDrive,
Cmd_Mute,
Cmd_RxSensorsEnable,
Cmd_TxSensorsEnable,
Cmd_RxSensors,
Cmd_TxSensors,
Cmd_AgcMode,
Cmd_AgcGain,
Cmd_Lock
};
Q_ENUM (Tci_Cmd);

public slots:
//  void close();
  void sendTextMessage(const QString &message);
//  void trxChanged(quint32 trx, bool state);
  void txAudioData(quint32 len, float * data);
//  void setAudioSampleRate(const quint32 &sr);

private slots:

  void onBinaryReceived(const QByteArray &data);
  void onMessageReceived(const QString &str);
  void onError(QAbstractSocket::SocketError err);
  void onConnected();
  void onDisconnected();

signals:


  void sendIqData(int, quint32, float*, bool);
  void sendAudioData(int, quint32, float*);
  void receiveIQ(bool,int);
  void txAudioChrono(quint32);
  void tci_done1();
  void tci_done2();
  void tci_done3();
  
protected:
  int do_start (JTDXDateTime*) override;
  void do_stop () override;
  void do_frequency (Frequency, MODE, bool no_ignore) override;
  void do_tx_frequency (Frequency, MODE, bool no_ignore) override;
  void do_mode (MODE) override;
  void do_ptt (bool on) override;

  void do_poll () override;
//for Audio
  void do_audio (bool on) override; 
  void do_tune (bool on) override;
  void do_period(double period) override;
  void do_blocksize(qint32 blocksize) override; 
  void do_spread(double spread) override {m_fSpread=spread;}
  void do_nsym(int nsym) override {m_symbolsLength=nsym;}
  void do_trfrequency(double newfrequency) override {m_trfrequency = newfrequency;}
  void do_txvolume (qreal volume) override; 
//bounch
  void do_modulator_start(unsigned symbolsLength, double framesPerSymbol, double frequency,
                     double toneSpacing, bool synchronize = true, double dBSNR = 99., double TRperiod=60.0) override;
  void do_modulator_stop(bool quick = false) override;
  
  void rx2_enable (bool on);
  void rig_split ();
  void rig_power (bool on);
  void stream_audio (bool on);
  void store (float * source, size_t numFrames, qint16 * dest)
  {
    static constexpr float K = 0x7FFF;
    for (size_t i {0}; i < numFrames; ++i) {
       dest[i] = static_cast<int16_t>(K*source[i*2]);
    }

  }

  float * load (qint16 sample, float * dest)
  
  {
    static constexpr float K1 = 0.999/0x7FFF;
    static constexpr float K2 = 0.499/0x7FFF;
    float value;
    if (tx_top_)  value = K1*static_cast<float>(sample);
    else  value = K2*static_cast<float>(sample);
    *dest++ = value;
    *dest++ = value;
      
    return dest;
  }
  enum ModulatorState {Synchronizing, Active, Idle};

private:
  MODE get_mode (bool requested = false);
  QString frequency_to_string (Frequency) const;
  Frequency string_to_frequency (QString) const;
  void mysleep1 (int ms = 1);
  void mysleep2 (int ms = 1);
  void mysleep3 (int ms = 1);
  QString mode_to_command (QString) const;
  std::unique_ptr<TransceiverBase> wrapped_; // may be null
  QString rx_;
  QString server_;
  bool use_for_ptt_;
  QStringList errortable;
  QString error_;
  bool do_snr_;
  bool do_pwr_;
  bool rig_power_;
  bool rig_power_off_;
  bool tci_audio_;
  bool _power_;
  QWebSocket * commander_;
  QLocale locale_;
  QTimer * tci_timer1_;
  QEventLoop * tci_loop1_;
  QTimer * tci_timer2_;
  QEventLoop * tci_loop2_;
  QTimer * tci_timer3_;
  QEventLoop * tci_loop3_;
  int nIqBytes;
  bool inConnected;
  bool tci_Ready;
  bool ESDR3;
  bool HPSDR;
  bool tx_top_;
  bool band_change;
  QUrl url_;
  quint32 audioSampleRate;
  FILE * wavptr_;
  QByteArray t_iqData;
  int trxA;
  int trxB;
  int cntIQ;
  bool bIQ;

// CAT internal variables
  QString requested_mode_;
  QString mode_;
  QString started_mode_;
  QString requested_rx_frequency_;
  QString rx_frequency_;
  QString requested_other_frequency_;
  QString other_frequency_;
  QString requested_drive_;
  QString drive_;
  
  bool requested_stream_audio_;
  bool stream_audio_;
  bool audio_;
  bool tx_audio_;
  bool requested_PTT_;
  bool PTT_;
  bool requested_split_;
  bool split_;
  bool started_split_;
  bool requested_rx2_;
  bool rx2_;
  bool started_rx2_;
  unsigned int power_;
  unsigned int swr_;
  int level_;
  bool busy_rx_frequency_;
  bool busy_mode_;  
  bool busy_other_frequency_;
  bool busy_split_;
  bool busy_drive_;
  bool busy_PTT_; 
  bool busy_rx2_;  
  QHash<QString,Tci_Cmd> mapCmd_;
  JTDXDateTime * m_jtdxtime;
  // from Detector
    void clear ();                // discard buffer contents

  double m_period = 15.0;
  unsigned m_downSampleFactor;
  qint32 m_samplesPerFFT;       // after any down sampling
  qint32 m_ns;
  static size_t const max_buffer_size {7 * 512};
  QScopedArrayPointer<short> m_buffer; // de-interleaved sample buffer
  // big enough for all the
  // samples for one increment of
  // data (a signals worth) at
  // the input sample rate
  unsigned m_bufferPos;
  quint32 writeAudioData (float * data, qint32 maxSize);
  static size_t const bytesPerFrame = 2;
  // from Modulator
  quint16 readAudioData (float * data, qint32 maxSize);
  qint16 postProcessSample (qint16 sample) const;
  bool m_quickClose = false;

  unsigned m_symbolsLength;

  static double constexpr m_twoPi = 2.0 * 3.141592653589793238462;
  unsigned m_nspd = 2048 + 512; // CW ID WPM factor = 22.5 WPM

  double m_phi;
  double m_dphi;
  double m_amp;
  double m_nsps;
  double  m_trfrequency;
  double m_frequency0;
  double m_snr;
  double m_fac;
  double m_toneSpacing;
  double m_fSpread;
  double m_TRperiod;

  qint64 m_silentFrames;
  qint16 m_ramp;
  ModulatorState m_state;

  bool m_tuning;
  bool m_addNoise;
  bool m_bFastMode;

  bool m_cwLevel;
  unsigned m_ic;
  unsigned m_isym0;
  int m_j0;
  double m_toneFrequency0;
  
  QByteArray m_tx1[8];
  int tx_fifo, tx_fifo2;
  quint32 last_type;  
  std::string debug_file_;
  std::string wav_file_;
  std::mutex mtx_;
};

#endif
