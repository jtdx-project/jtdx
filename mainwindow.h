
// -*- Mode: C++ -*-
#ifndef MAINWINDOW_H
#define MAINWINDOW_H
#ifdef QT5
#include <QtWidgets>
#else
#include <QtGui>
#endif
#include <QTranslator>
#include <QThread>
#include <QTimer>
#include <QList>
#include <QStringList>
#include <QAudioDeviceInfo>
#include <QScopedPointer>
#include <QDir>
#include <QProgressDialog>
#include <QAbstractSocket>
#include <QHostAddress>
#include <QPointer>
#include <QSet>
#include <QFuture>
#include <QFutureWatcher>
#include <QFileSystemWatcher>
#include <QClipboard>

#include "AudioDevice.hpp"
#include "commons.h"
#include "Radio.hpp"
#include "Modes.hpp"
#include "Configuration.hpp"
#include "WSPRBandHopping.hpp"
#include "Transceiver.hpp"
#include "DisplayManual.hpp"
#include "psk_reporter.h"
#include "logbook/logbook.h"
#include "decodedtext.h"
#include "JTDXMessageBox.hpp"
#include "qsohistory.h"
#include "JTDXDateTime.h"

#define NUM_JT65_SYMBOLS 126               //63 data + 63 sync
#define NUM_JT9_SYMBOLS 85                 //69 data + 16 sync
#define NUM_T10_SYMBOLS 85                 //69 data + 16 sync
#define NUM_WSPR_SYMBOLS 162               //(50+31)*2, embedded sync
#define NUM_FT8_SYMBOLS 79
#define NUM_FT4_SYMBOLS 105

#define NUM_CW_SYMBOLS 250
#define TX_SAMPLE_RATE 48000

extern int volatile itone[NUM_WSPR_SYMBOLS];   //Audio tones for all Tx symbols
extern int volatile icw[NUM_CW_SYMBOLS];	    //Dits for CW ID

//--------------------------------------------------------------- MainWindow
namespace Ui {
  class MainWindow;
}

class QSettings;
class QNetworkAccessManager;
class QLineEdit;
class QFont;
class QHostInfo;
class WideGraph;
class LogQSO;
class Transceiver;
class MessageClient;
class QTime;
class WSPRBandHopping;
class HelpTextWindow;
class EQSL;
class WSPRNet;
class SoundOutput;
class Modulator;
class SoundInput;
class Detector;
class SampleDownloader;
class DecodedText;
class MainWindow : public QMainWindow
{
  Q_OBJECT;

public:
  using Frequency = Radio::Frequency;
  using FrequencyDelta = Radio::FrequencyDelta;
  using Mode = Modes::Mode;
  qint32  m_exitCode;

  // Multiple instances: call MainWindow() with *thekey
  explicit MainWindow(bool multiple, QSettings *, QSharedMemory *shdmem,
                      unsigned downSampleFactor, QNetworkAccessManager * network_manager,
                      QWidget *parent = 0);
  ~MainWindow();

public slots:
  void showSoundInError(const QString& errorMsg);
  void showSoundOutError(const QString& errorMsg);
  void showStatusMessage(const QString& statusMsg);
  void dataSink(qint64 frames);
  void diskDat();
  void freezeDecode(int n);
  void guiUpdate();
  void doubleClickOnCall(bool alt, bool ctrl);
  void doubleClickOnCall2(bool alt, bool ctrl);
  void readFromStdout();
  void process_Auto();
  void p1ReadFromStdout();
  void setXIT(int n, Frequency base = 0u);
  void setFreq4(int rxFreq, int txFreq);
  void setRxFreq4(int rxFreq);
  void filter_on();
  void toggle_filter();
  void escapeHalt();

protected:
  virtual void keyPressEvent( QKeyEvent *e );
  void  closeEvent(QCloseEvent*);
  void childEvent(QChildEvent *) override;
  virtual bool eventFilter(QObject *object, QEvent *event);
  virtual void resizeEvent(QResizeEvent *event);
  virtual void mousePressEvent(QMouseEvent *event);

private slots:
  void on_tx1_editingFinished();
  void on_tx2_editingFinished();
  void on_tx3_editingFinished();
  void on_tx4_editingFinished();
  void on_tx5_currentTextChanged (QString const&);
  void on_tx5_currentIndexChanged(int index);
  void on_tx6_editingFinished();
  void on_wantedCall_textChanged(const QString &arg1);
  void on_wantedCountry_textChanged(const QString &arg1);
  void on_wantedPrefix_textChanged(const QString &arg1);
  void on_wantedGrid_textChanged(const QString &arg1);
  void on_directionLineEdit_textChanged(const QString &arg1);
  void on_direction1LineEdit_textChanged(const QString &arg1);
  void on_spotLineEdit_textChanged(const QString &text);
  void on_propLineEdit_textChanged(const QString &text);
  void on_actionSettings_triggered();
  void on_monitorButton_clicked (bool);
  void on_swlButton_clicked (bool);
  void on_filterButton_clicked (bool);
  void on_AGCcButton_clicked (bool);
  void on_actionAbout_triggered();
  void on_enableTxButton_clicked (bool);
  void on_stopTxButton_clicked();
  void on_stopButton_clicked();
  void on_AnsB4Button_clicked (bool);
  void on_singleQSOButton_clicked (bool);
  void on_bypassButton_clicked (bool);
  void on_pbSpotDXCall_clicked ();  
  void on_actionJTDX_Web_Site_triggered();
  void on_actionJTDX_Forum_triggered();
//  void on_actionLocal_User_Guide_triggered();
  void on_actionWide_Waterfall_triggered();
  void on_actionOpen_triggered();
  void on_actionOpen_next_in_directory_triggered();
  void on_actionDecode_remaining_files_in_directory_triggered();
  void on_actionDelete_all_wav_files_in_SaveDir_triggered();
  void on_actionOpen_log_directory_triggered ();
  void on_actionNone_triggered();
  void on_actionSave_all_triggered();
  void on_actionEnglish_triggered();
  void on_actionEstonian_triggered();
  void on_actionRussian_triggered();
  void on_actionPolish_triggered();
  void on_actionPortuguese_triggered();
  void on_actionPortuguese_BR_triggered();
  void on_actionCatalan_triggered();
  void on_actionCroatian_triggered();
  void on_actionDanish_triggered();
  void on_actionDutch_triggered();
  void on_actionHungarian_triggered();
  void on_actionSpanish_triggered();
  void on_actionSwedish_triggered();
  void on_actionFrench_triggered();
  void on_actionItalian_triggered();
  void on_actionLatvian_triggered();
  void on_actionChinese_simplified_triggered();
  void on_actionChinese_traditional_triggered();
  void on_actionJapanese_triggered();
  void on_actionCallNone_toggled(bool checked);
  void on_actionCallFirst_toggled(bool checked);
  void on_actionCallMid_toggled(bool checked);
  void on_actionCallEnd_toggled(bool checked);
  void on_actionCallPriorityAndSearchCQ_toggled(bool checked);
  void on_actionMaxDistance_toggled(bool checked);
  void on_actionAnswerWorkedB4_toggled(bool checked);
  void on_actionCallWorkedB4_toggled(bool checked);
  void on_actionCallHigherNewCall_toggled(bool checked);
  void on_actionSingleShot_toggled(bool checked);
  void on_actionAutoFilter_toggled(bool checked);
  void on_actionEnable_hound_mode_toggled(bool checked);
  void on_actionUse_TX_frequency_jumps_toggled(bool checked);
  void on_actionMTAuto_triggered();
  void on_actionMT1_triggered();
  void on_actionMT2_triggered();
  void on_actionMT3_triggered();
  void on_actionMT4_triggered();
  void on_actionMT5_triggered();
  void on_actionMT6_triggered();
  void on_actionMT7_triggered();
  void on_actionMT8_triggered();
  void on_actionMT9_triggered();
  void on_actionMT10_triggered();
  void on_actionMT11_triggered();
  void on_actionMT12_triggered();
  void on_actionAcceptUDPCQ_triggered();
  void on_actionAcceptUDPCQ73_triggered();
  void on_actionAcceptUDPAny_triggered();
  void on_actionDisableTx73_toggled(bool checked);
  void on_actionShow_tooltips_main_window_toggled(bool checked);
  void on_actionColor_Tx_message_buttons_toggled(bool checked);
  void on_actionCallsign_to_clipboard_toggled(bool checked);
  void on_actionCrossband_160m_JA_toggled(bool checked);
  void on_actionCrossband_160m_HL_toggled(bool checked);
  void on_actionShow_messages_decoded_from_harmonics_toggled(bool checked);
  void on_actionMyCallRXFwindow_toggled(bool checked);
  void on_actionWantedCallRXFwindow_toggled(bool checked);
  void on_actionFT8SensMin_toggled(bool checked);
  void on_actionlowFT8thresholds_toggled(bool checked);
  void on_actionFT8subpass_toggled(bool checked);
  void on_actionFT8LateStart_toggled(bool checked);
  void on_actionFT8WidebandDXCallSearch_toggled(bool checked);
  void on_actionBypass_text_filters_on_RX_frequency_toggled(bool checked);
  void on_actionBypass_all_text_filters_toggled(bool checked);
  void on_actionEnable_main_window_popup_toggled(bool checked);
  void on_actionAutoErase_toggled(bool checked);
  void on_actionEraseWindowsAtBandChange_toggled(bool checked);
  void on_actionReport_message_priority_toggled(bool checked);
  void on_actionKeyboard_shortcuts_triggered();
  void on_actionSpecial_mouse_commands_triggered();
  void on_actionCopyright_Notice_triggered();
  void on_DecodeButton_clicked (bool);
  void decode();
  void decodeBusy(bool b);
  void on_EraseButton_clicked();
  void on_ClearDxButton_clicked();
  void on_txb1_clicked();
  void on_TxMinuteButton_clicked(bool checked);
  void on_rrrCheckBox_stateChanged(int arg1);
  void on_rrr1CheckBox_stateChanged(int arg1);
  void set_ntx(int n);
  void on_txb2_clicked();
  void on_txb3_clicked();
  void on_txb4_clicked();
  void on_txb5_clicked();
  void on_txb6_clicked();
  void on_lookupButton_clicked();
  void on_addButton_clicked();
  void on_dxCallEntry_textChanged(const QString &arg1);
  void on_dxGridEntry_textChanged(const QString &arg1);
  void on_genStdMsgsPushButton_clicked();
  void on_logQSOButton_clicked();
  void on_actionJT9_triggered();
  void on_actionT10_triggered();
  void on_actionFT4_triggered();
  void on_actionFT8_triggered();
  void on_actionJT65_triggered();
  void on_actionJT9_JT65_triggered();
  void on_TxFreqSpinBox_valueChanged(int arg1);
  void on_actionSave_decoded_triggered();
  void on_actionQuickDecode_triggered();
  void on_actionMediumDecode_triggered();
  void on_actionDeepestDecode_triggered();
  void on_actionDecFT8cycles1_triggered();
  void on_actionDecFT8cycles2_triggered();
  void on_actionDecFT8cycles3_triggered();
  void on_actionDecFT8SWLcycles1_triggered();
  void on_actionDecFT8SWLcycles2_triggered();
  void on_actionDecFT8SWLcycles3_triggered();
  void on_actionRXfLow_triggered();
  void on_actionRXfMedium_triggered();
  void on_actionRXfHigh_triggered();
  void on_actionFT4fast_triggered();
  void on_actionFT4medium_triggered();
  void on_actionFT4deep_triggered();
  void on_actionSwitch_Filter_OFF_at_sending_73_triggered(bool checked);
  void on_actionSwitch_Filter_OFF_at_getting_73_triggered(bool checked);
  void bumpFqso(int n);
  void on_actionErase_ALL_TXT_triggered();
  void on_actionErase_wsjtx_log_adi_triggered();
  void on_actionOpen_wsjtx_log_adi_triggered();
  void startTx2();
  void stopTx();
  void stopTx2();
  void on_pbCallCQ_clicked();
  void on_pbAnswerCaller_clicked();
  void on_pbSendRRR_clicked();
  void on_pbAnswerCQ_clicked();
  void on_pbSendReport_clicked();
  void on_pbSend73_clicked();
  void on_rbGenMsg_clicked(bool checked);
  void on_rbFreeText_clicked(bool checked);
  void on_freeTextMsg_currentTextChanged (QString const&);
  void on_freeTextMsg_currentIndexChanged(int index);
  void on_rptSpinBox_valueChanged(int n);
  void killFile();
  void set_language(QString const& lang);
  void on_tuneButton_clicked (bool);
  void on_pbR2T_clicked();
  void on_pbT2R_clicked();
  void acceptQSO2(QDateTime const&, QString const& call, QString const& grid
                  , Frequency dial_freq, QString const& mode
                  , QString const& rpt_sent, QString const& rpt_received
                  , QString const& tx_power, QString const& comments
                  , QString const& name, QDateTime const&, QString const& eqslcomments
                  , QByteArray const& myadif2);
  void on_bandComboBox_currentIndexChanged (int index);
  void on_bandComboBox_activated (int index);
  void on_readFreq_clicked();
  void on_pbTxMode_clicked();
  void on_RxFreqSpinBox_valueChanged(int n);
  void on_candListSpinBox_valueChanged(int n);
  void on_pbTxLock_clicked(bool);
  void on_skipTx1_clicked(bool checked);
  void on_skipGrid_clicked(bool checked);
  void on_outAttenuation_valueChanged (int);
  void rigOpen ();
  void handle_transceiver_update (Transceiver::TransceiverState const&);
  void handle_transceiver_failure (QString const& reason);
  void on_actionShort_list_of_add_on_prefixes_and_suffixes_triggered();
  void band_changed (Frequency);
  void monitor (bool);
  void stop_tuning ();
  void stopTuneATU();
  void enableTx_mode(bool);
  void enableTxButton_off();
  void on_hintButton_clicked(bool);
  void on_HoundButton_clicked(bool);
  void on_AutoTxButton_clicked(bool);
  void on_AutoSeqButton_clicked(bool);
  void networkError (QString const&);
  void statusUpdate () const;
  void add_child_to_event_filter (QObject *);
  void remove_child_from_event_filter (QObject *);
  void txwatchdog (bool triggered);
  void update_watchdog_label ();
  void on_cbMenus_toggled(bool b);
  void on_cbShowWanted_toggled(bool b);
  void on_cbShowSpot_toggled(bool b);
  void dynamicButtonsInit();
  void on_actionWSPR_2_triggered();
  void on_TxPowerComboBox_currentIndexChanged(const QString &arg1);
  void on_sbTxPercent_valueChanged(int n);
  void on_cbUploadWSPR_Spots_toggled(bool b);
  void WSPR_config(bool b);
  void uploadSpots();
  void TxAgain();
  void RxQSY();
  void uploadResponse(QString response);
  void on_WSPRfreqSpinBox_valueChanged(int n);
  void on_pbTxNext_clicked(bool b);
  void set_scheduler(QString const& band,bool mixed);
  void haltTx(QString reason);
  void haltTxTuneTimer();
  void logChanged();
  bool stdCall(QString const& w);
  void ScrollBarPosition(int n);

private:
  Q_SIGNAL void initializeAudioOutputStream (QAudioDeviceInfo,
      unsigned channels, unsigned msBuffered) const;
  Q_SIGNAL void stopAudioOutputStream () const;
  Q_SIGNAL void startAudioInputStream (QAudioDeviceInfo const&,
      int framesPerBuffer, AudioDevice * sink,
      unsigned downSampleFactor, AudioDevice::Channel) const;
  Q_SIGNAL void suspendAudioInputStream () const;
  Q_SIGNAL void resumeAudioInputStream () const;
  Q_SIGNAL void startDetector (AudioDevice::Channel) const;
  Q_SIGNAL void FFTSize (unsigned) const;
  Q_SIGNAL void detectorClose () const;
  Q_SIGNAL void finished () const;
  Q_SIGNAL void transmitFrequency (double) const;
  Q_SIGNAL void endTransmitMessage (bool quick = false) const;
  Q_SIGNAL void tune (bool = true) const;
  Q_SIGNAL void sendMessage (unsigned symbolsLength, double framesPerSymbol,
      double frequency, double toneSpacing,
      SoundOutput *, AudioDevice::Channel = AudioDevice::Mono,
      bool synchronize = true, double dBSNR = 99., int TRperiod=60) const;
  Q_SIGNAL void outAttenuationChanged (qreal) const;
  Q_SIGNAL void toggleShorthand () const;

private:
  void hideMenus (bool b);

  JTDXDateTime * m_jtdxtime;
  QDir m_dataDir;
  bool m_valid;
  QString m_revision;
  bool m_multiple;
  QSettings * m_settings;

  QScopedPointer<Ui::MainWindow> ui;

//  bool m_olek;
//  bool m_olek2;
//  QTranslator m_translator_from_resources;
//  QTranslator m_translator_from_files;

  // other windows
  Configuration m_config;
  WSPRBandHopping m_WSPR_band_hopping;
  bool m_WSPR_tx_next;
  JTDXMessageBox m_rigErrorMessageBox;
  QScopedPointer<SampleDownloader> m_sampleDownloader;

  QScopedPointer<WideGraph> m_wideGraph;
  QScopedPointer<LogQSO> m_logDlg;
  QScopedPointer<HelpTextWindow> m_shortcuts;
  QScopedPointer<HelpTextWindow> m_prefixes;
  QScopedPointer<HelpTextWindow> m_mouseCmnds;

  Transceiver::TransceiverState m_rigState;
  Frequency  m_lastDialFreq;
  QString m_lastBand;
  Frequency  m_callingFrequency;
  Frequency  m_dialFreqRxWSPR;  // best guess at WSPR QRG

  Detector * m_detector;
  unsigned m_FFTSize;
  SoundInput * m_soundInput;
  Modulator * m_modulator;
  SoundOutput * m_soundOutput;
  QThread m_audioThread;
  QClipboard *clipboard = QGuiApplication::clipboard();

  double  m_TRperiod;

  qint64  m_msErase;
  qint64  m_secBandChanged;
  qint64  m_secTxStopped;
  qint64  m_msDecStarted;
  Frequency m_freqNominal;
  Frequency m_freqTxNominal;
  quint64  m_lastDisplayFreq;
  quint64  m_mslastTX;
  quint64  m_mslastMon;
//  quint64  m_msDecoderStarted;

  qint32  m_waterfallAvg;
  qint32  m_ntx;
  qint32  m_addtx;
  qint32  m_nlasttx;
  qint32  m_lapmyc;
  qint32  m_delay;
  qint32  m_timeout;
  qint32  m_XIT;
  qint32  m_ndepth;
  qint32  m_ncandthin;
  qint32  m_nFT8Cycles;
  qint32  m_nFT8SWLCycles;
  qint32  m_nFT8RXfSens;
  qint32  m_nFT4depth;
  qint32  m_sec0;
  qint32  m_RxLog;
  qint32  m_nutc0;
  qint32  m_ntr;
  qint32  m_tx;
  qint32  m_hsym;
  qint32  m_nsps;
  qint32  m_hsymStop;
  qint32  m_ncw;
  qint32  m_secID;
  qint32  m_dBm;
  qint32  m_pctx;
  qint32  m_nseq;
  qint32  m_nWSPRdecodes;
  qint32  m_used_freq;
  qint32  m_nguardfreq;
  qint32  m_idleMinutes;
  qint32  m_oldTx5Index;
  qint32  m_oldFreeMsgIndex;
  qint32  m_ft8threads;
  qint32  m_acceptUDP;
  qint32  m_lastCallingFreq;
  qint32  m_saveWav;
  qint32  m_callMode;
  qint32  m_ft8Sensitivity;
  qint32  m_position;
  qint32  m_nsecBandChanged;
  qint32  m_nDecodes;
  qint32  m_ncand;
    
  bool    m_btxok;		//True if OK to transmit
  bool    m_diskData;
  bool    m_loopall;
  bool    m_decoderBusy;
  bool    m_txFirst;
  bool    m_txGenerated;
  bool    m_rrr;
  bool    m_enableTx;
  bool    m_restart;
  bool    m_startAnother;
  bool    m_showHarmonics;
  bool	  m_showMyCallMsgRxWindow;
  bool    m_showWantedCallRxWindow;
  bool    m_FT8LateStart;
  bool    m_bypassRxfFilters;
  bool    m_bypassAllFilters;
  bool    m_windowPopup;
  bool    m_autoErase;
  bool    m_autoEraseBC;
  bool    m_rprtPriority;
  bool    m_call3Modified;
  bool    m_dataAvailable;
  bool    m_killAll;
  bool    m_bDecoded;
  bool    m_noSuffix;
  bool    m_blankLine;
  bool    m_notified;
  bool    m_start;
  bool    m_start2;
  bool    m_decodedText2;
  bool    m_freeText;
  bool    m_sentFirst73;
  bool    m_reply_me;
  bool	  m_reply_other;
  bool	  m_reply_CQ73;
  qint32  m_counter;
  qint32  m_currentMessageType;
  QString m_currentMessage;
  QString m_curMsgTx;
  qint32  m_lastMessageType;
  QString m_lastMessageSent;
  bool    m_lockTxFreq;
  bool    m_skipTx1;
  bool    m_swl;
  bool    m_filter;
  bool    m_agcc;
  bool    m_hint;
  bool    m_disable_TX_on_73;
  bool    m_showTooltips;
  bool    m_autoTx;
  bool    m_autoseq;
  bool    m_wasAutoSeq;
  bool    m_Tx5setAutoSeqOff;
  bool    m_FTsetAutoSeqOff;
  bool    m_uploadSpots;
  bool    m_uploading;
  bool    m_txNext;
  bool    m_grid6;
  bool    m_tuneup;
  bool    m_bTxTime;
  bool    m_rxDone;
  bool    m_bSimplex; // not using split even if it is available
  bool	  m_logqso73;
  bool	  m_processAuto_done;
  bool    m_haltTrans;
  bool	  m_crossbandOptionEnabled;
  bool	  m_crossbandHLOptionEnabled;
  QString m_repliedCQ;
  QString m_dxbcallTxHalted;
  QString m_currentQSOcallsign;
  bool m_callPrioCQ;
  bool m_callFirst73;
  bool m_maxDistance;
  bool m_answerWorkedB4;
  bool m_callWorkedB4;
  bool m_callHigherNewCall;
  bool m_singleshot;
  bool m_autofilter;
  bool m_houndMode;
  bool m_commonFT8b;
  bool m_houndTXfreqJumps;
  bool m_spotDXsummit;
  qint32 m_FilterState;
  bool m_manualDecode;
  bool m_haltTxWritten;
  bool m_strictdirCQ;
  bool m_colorTxMsgButtons;
  bool m_txbColorSet;
  bool m_bMyCallStd;
  bool m_bHisCallStd;
  bool m_callNotif;
  bool m_gridNotif;
  bool m_qsoLogged;
  bool m_logInitNeeded;
  bool m_wantedchkd;
  bool m_menus;
  bool m_wasSkipTx1;
  bool m_modeChanged;
  bool m_FT8WideDxCallSearch;
  bool m_multInst;
  bool m_myCallCompound;
  bool m_hisCallCompound;
  bool m_callToClipboard;
  bool m_rigOk;
  bool m_bandChanged;
  bool m_useDarkStyle;
  bool m_lostaudio;
  bool m_lasthint;
  bool m_monitoroff;
  bool m_savedRRR;
  QString m_lang;
  QString m_lastloggedcall;
  QString m_cqdir;
  QString m_lastMode;
  QString m_callsign;
  QString m_grid;
  QString m_name;
  QString m_timeFrom;
  QString m_m_prefix;
  QString m_m_continent;
  QString m_spotText;
  QDateTime m_lastloggedtime;
  enum QSOProgress
    {
      CALLING,
      REPLYING,
      REPORT,
      ROGER_REPORT,
      ROGERS,
      SIGNOFF
    };
  QSOProgress m_QSOProgress;
  QSOProgress m_transmittedQSOProgress;
  char    m_msg[100][80];

  // labels in status bar
  QLabel * tx_status_label;
  QLabel * mode_label;
  QLabel * last_tx_label;
  QLabel * txwatchdog_label;
  QProgressBar* progressBar;
  QLabel * date_label;
  QLabel * lastlogged_label;
  QLabel * qso_count_label;

  JTDXMessageBox msgBox0;

  QFuture<void> m_wav_future;
  QFutureWatcher<void> m_wav_future_watcher;
  QFutureWatcher<QString> m_saveWAVWatcher;

  QFileSystemWatcher *fsWatcher;

  QProcess proc_jtdxjt9;
  QProcess p1;
  QProcess p3;

  WSPRNet *wsprNet;
  EQSL *Eqsl;

  QTimer m_guiTimer;
  QTimer ptt1Timer;                 //StartTx delay
  QTimer ptt0Timer;                 //StopTx delay
  QTimer logQSOTimer;
  QTimer killFileTimer;
  QTimer tuneButtonTimer;
  QTimer cqButtonTimer;
  QTimer enableTxButtonTimer;
  QTimer tx73ButtonTimer;
  QTimer logClearDXTimer;
  QTimer dxbcallTxHaltedClearTimer;
  QTimer uploadTimer;
  QTimer tuneATU_Timer;
  QTimer TxAgainTimer;
  QTimer StopTuneTimer;
  QTimer minuteTimer;
  QTimer RxQSYTimer;

  QString m_path;
  QString m_baseCall;
  QString m_hisCall;
  QString m_hisGrid;
  QString m_wantedCall;
  QString m_wantedCountry;
  QString m_wantedPrefix;
  QString m_wantedGrid;
  QStringList m_wantedCallList;
  QStringList m_wantedCountryList;
  QStringList m_wantedPrefixList;
  QStringList m_wantedGridList;
  QString m_appDir;
  QString m_palette;
//  QString m_dateTime;
  QString m_mode;
  QString m_oldmode;
  QString m_modeTx;
  QString m_fnameWE; // save path without extension
  QString m_rpt;
  QString m_rptSent;
  QString m_rptRcvd;
  QString m_cmnd;
  QString m_msgSent0;
  QString m_fileToKill;
  QString m_fileToSave;
  QString m_calls;

  QSet<QString> m_pfx;
  QSet<QString> m_sfx;

  QDateTime m_dateTimeQSOOn;
  QsoHistory::Status m_status;

  QSharedMemory *mem_jtdxjt9;
  LogBook m_logBook;
  QsoHistory m_qsoHistory;
  QsoHistory m_qsoHistory2;
  QString m_QSOText {""};
  unsigned m_msAudioOutputBuffered;
  unsigned m_framesAudioInputBuffered;
  unsigned m_downSampleFactor;
  QThread::Priority m_audioThreadPriority;
  bool m_bandEdited;
  bool m_splitMode;
  bool m_monitoring;
  bool m_tx_when_ready;
  bool m_transmitting;
  bool m_tune;
  bool m_txwatchdog;
  bool m_block_pwr_tooltip;
  bool m_PwrBandSetOK;
  bool m_okToPost;
  Frequency m_lastMonitoredFrequency;
  double m_toneSpacing;
  qint32 m_geometry_restored;
  qint32 m_firstDecode;
  QProgressDialog m_optimizingProgress;
  QTimer m_heartbeat;
  MessageClient * m_messageClient;
  PSK_Reporter *psk_Reporter;
  DisplayManual m_manual;
  QHash<QString, QVariant> m_pwrBandTxMemory; // Remembers power level by band
  QHash<QString, QVariant> m_pwrBandTuneMemory; // Remembers power level by band for tuning
  QByteArray m_geometry;

  //---------------------------------------------------- private functions
  void readSettings();
  void setDecodedTextFont (QFont const&);
  void setStopHSym();
  void setClockStyle(bool reset);
  void setAutoSeqButtonStyle(bool checked);
  void setMinButton();
  void autoStopTx(QString reason);
  void writeHaltTxEvent(QString reason);
  void writeSettings();
  void createStatusBar();
  void msgBox(QString t);
  void genCQMsg();
  void genStdMsgs(QString rpt);
  void clearDX (QString reason);
  void clearDXfields (QString reason);
  void logClearDX ();
  void countQSOs ();
  void autoFilter (bool);
  void enableTab1TXRB(bool);
  void dxbcallTxHaltedClear ();
  void lookup();
  void ba2msg(QByteArray ba, char* message);
  void msgtype(QString t, QLineEdit* tx);
  void stub();
  void statusChanged();
  void styleChanged();
  bool gridOK(QString g);
  bool gridRR73(QString g);
  bool reportRCVD(QStringList msg);
  bool rReportRCVD(QStringList msg);
  bool shortList(QString callsign);
  bool isAutoSeq73(QString const& text);
  void enableHoundAccess(bool b);
  void setHoundAppearance(bool hound);
  void setLastLogdLabel();
  void writeToALLTXT(QString const& text);
  void setTxMsgBtnColor();
  void resetTxMsgBtnColor();
  void transmit (double snr = 99.);
  void rigFailure (QString const& reason, QString const& detail);
  void pskSetLocal ();
  void displayDialFrequency ();
  void transmitDisplay (bool);
  void processMessage(QString const& messages, qint32 position, bool alt, bool ctrl);
  void replyToUDP (QTime, qint32 snr, float delta_time, quint32 delta_frequency, QString const& mode, QString const& message_text, bool low_confidence, quint8 modifiers);
  void replayDecodes ();
  void postDecode (bool is_new, QString const& message);
  void postWSPRDecode (bool is_new, QStringList message_parts);
  void enable_DXCC_entity ();
  void switch_mode (Mode);
  void commonActions();
  void WSPR_scheduling ();
  void setRig ();
  void WSPR_history(Frequency dialFreq, int ndecodes);
  QString WSPR_hhmm(int n);
  QString save_wave_file (QString const& name
                          , short const * data
                          , int seconds
                          , QString const& my_callsign
                          , QString const& my_grid
                          , QString const& mode
                          , Frequency frequency
                          , QString const& his_call
                          , QString const& his_grid
                          , JTDXDateTime * jtdxtime) const;
  void read_wav_file (QString const& fname);
  bool subProcessFailed (QProcess *, int exit_code, QProcess::ExitStatus);
  void subProcessError (QProcess *, QProcess::ProcessError);
  void on_the_minute ();
};

extern int killbyname(const char* progName);
extern void getDev(int* numDevices,char hostAPI_DeviceName[][50],
                   int minChan[], int maxChan[],
                   int minSpeed[], int maxSpeed[]);
extern int next_tx_state(int pctx);

#endif // MAINWINDOW_H
