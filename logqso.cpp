#include "logqso.h"

#include <QTcpSocket>
#include <QString>
#include <QSettings>
#include <QStandardPaths>
#include <QDir>
#include <QDebug>

#include "JTDXMessageBox.hpp"

#include "logbook/adif.h"
#include "Configuration.hpp"
#include "Bands.hpp"

#include "ui_logqso.h"
#include "moc_logqso.cpp"

LogQSO::LogQSO(QSettings * settings, Configuration const * config, JTDXDateTime * jtdxtime, QWidget *parent)
  : QDialog(parent)
  , ui(new Ui::LogQSO)
  , m_settings (settings)
  , m_config {config}
  , m_jtdxtime {jtdxtime}
{
  ui->setupUi(this);
  ui->buttonBox->button(QDialogButtonBox::Ok)->setText(tr("&OK"));
  ui->buttonBox->button(QDialogButtonBox::Cancel)->setText(tr("&Cancel"));
//  setWindowTitle("JTDX " + versnumber.simplified () + " - Log QSO");
  setWindowTitle(QCoreApplication::applicationName () + " v" + QCoreApplication::applicationVersion () + " - Log QSO");

  loadSettings ();
}

LogQSO::~LogQSO ()
{
}

void LogQSO::loadSettings ()
{
  m_settings->beginGroup ("LogQSO");
  restoreGeometry (m_settings->value ("geometry", saveGeometry ()).toByteArray ());
  ui->cbTxPower->setChecked (m_settings->value ("SaveTxPower", false).toBool ());
  ui->cbComments->setChecked (m_settings->value ("SaveComments", false).toBool ());
  ui->cbEqslComments->setChecked (m_settings->value ("SaveEQSLComments", false).toBool ());
  m_txPower = m_settings->value ("TxPower", "").toString ();
  m_comments = m_settings->value ("LogComments", "").toString();
  m_eqslcomments = m_settings->value ("LogEQSLComments", "").toString();
  m_settings->endGroup ();
}

void LogQSO::storeSettings () const
{
  m_settings->beginGroup ("LogQSO");
  m_settings->setValue ("geometry", saveGeometry ());
  m_settings->setValue ("SaveTxPower", ui->cbTxPower->isChecked ());
  m_settings->setValue ("SaveComments", ui->cbComments->isChecked ());
  m_settings->setValue ("SaveEQSLComments", ui->cbEqslComments->isChecked ());
  m_settings->setValue ("TxPower", m_txPower);
  m_settings->setValue ("LogComments", m_comments);
  m_settings->setValue ("LogEQSLComments", m_eqslcomments);
  m_settings->endGroup ();
}

void LogQSO::initLogQSO(QString const& hisCall, QString const& hisGrid, QString mode,
                        QString const& rptSent, QString const& rptRcvd, QString const& distance,
                        QString const& name, QDateTime const& dateTimeOn, QDateTime const& dateTimeOff,
                        Radio::Frequency dialFreq, bool autologging)
{
  m_send_to_eqsl=m_config->send_to_eqsl();
  ui->call->setText(hisCall);
  ui->grid->setText(hisGrid);
  ui->name->setText(name);
  ui->txPower->setText("");
  ui->comments->setText("");
  ui->eqslcomments->setText("");
  ui->lab11->setVisible(m_send_to_eqsl);
  ui->eqslcomments->setVisible(m_send_to_eqsl);
  ui->cbEqslComments->setVisible(m_send_to_eqsl);
  if (ui->cbTxPower->isChecked ()) ui->txPower->setText(m_txPower);
  if (ui->cbComments->isChecked ()) ui->comments->setText(m_comments);
  if (ui->cbEqslComments->isChecked ()) ui->eqslcomments->setText(m_eqslcomments);
  QString t="";
  if(m_config->report_in_comments()) {
    t=mode;
    if(rptSent!="") t+="  Sent: " + rptSent;
    if(rptRcvd!="") t+="  Rcvd: " + rptRcvd;
    ui->comments->setText(t);
  }
  if(m_config->distance_in_comments()) {
    if(t.isEmpty()) t="Distance: " + distance;
    else t+="  Distance: " + distance;
    ui->comments->setText(t);
  }
  if(m_config->log_as_RTTY() and mode.left(3)=="JT9") mode="RTTY";
  ui->mode->setText(mode);
  ui->sent->setText(rptSent);
  ui->rcvd->setText(rptRcvd);
  ui->start_date_time->setDateTime (dateTimeOn);
  ui->end_date_time->setDateTime (dateTimeOff);
  m_dialFreq=dialFreq;
  m_myCall=m_config->my_callsign();
  m_myGrid=m_config->my_grid();
  m_tcp_server_name=m_config->tcp_server_name();
  m_tcp_server_port=m_config->tcp_server_port();
  m_enable_tcp_connection=m_config->enable_tcp_connection();
  m_debug=m_config->write_decoded_debug();
  ui->band->setText(m_config->bands ()->find (dialFreq));

  if(!autologging) {
	 show ();
  }
  else {
	 accept();
  }
}

void LogQSO::accept()
{
  QString hisCall,hisGrid,mode,rptSent,rptRcvd,time,band;
  QString comments,eqslcomments,name;
  hisCall=ui->call->text();
  hisGrid=ui->grid->text();
  mode=ui->mode->text();
  rptSent=ui->sent->text();
  rptRcvd=ui->rcvd->text();
  m_dateTimeOn = ui->start_date_time->dateTime ();
  m_dateTimeOff = ui->end_date_time->dateTime ();
  band=ui->band->text();
  name=ui->name->text();
  m_txPower=ui->txPower->text();
  comments=ui->comments->text();
  m_comments=comments;
  eqslcomments=ui->eqslcomments->text();
  m_eqslcomments=eqslcomments;
  QString strDialFreq(QString::number(m_dialFreq / 1.e6,'f',6));

  //Log this QSO to ADIF file "wsjtx_log.adi"
  QString filename = "wsjtx_log.adi";  // TODO allow user to set
  ADIF adifile;
  auto adifilePath = QDir {QStandardPaths::writableLocation (QStandardPaths::DataLocation)}.absoluteFilePath ("wsjtx_log.adi");
  adifile.init(adifilePath);
  if (!adifile.addQSOToFile(hisCall,hisGrid,mode,rptSent,rptRcvd,m_dateTimeOn,m_dateTimeOff,band,comments,name,strDialFreq,m_myCall,m_myGrid,m_txPower,m_send_to_eqsl))
  {
      JTDXMessageBox::information_message(0,"","Cannot open file \"" + adifilePath + "\".");
   }

//Log this QSO to file "wsjtx.log"
  static QFile f {QDir {QStandardPaths::writableLocation (QStandardPaths::DataLocation)}.absoluteFilePath ("wsjtx.log")};
  if(!f.open(QIODevice::Text | QIODevice::Append)) {
    JTDXMessageBox::information_message(0,"","Cannot open file \"" + f.fileName () + "\" for append:" + f.errorString ());
  } else {
    QString logEntry=m_dateTimeOn.date().toString("yyyy-MM-dd,") +
      m_dateTimeOn.time().toString("hh:mm:ss,") + 
      m_dateTimeOff.date().toString("yyyy-MM-dd,") +
      m_dateTimeOff.time().toString("hh:mm:ss,") + hisCall + "," +
      hisGrid + "," + strDialFreq + "," + mode +
      "," + rptSent + "," + rptRcvd + "," + m_txPower +
      "," + comments + "," + name;
    QTextStream out(&f);
    out << logEntry <<
#if QT_VERSION < QT_VERSION_CHECK(5, 15, 0)
                 endl;
#else
                 Qt::endl;
#endif

    f.close();
  }

//Clean up and finish logging
    QString myadif;
    QByteArray myadif2;
    myadif="<BAND:" + QString::number(band.length()) + ">" + band;
    myadif+=" <STATION_CALLSIGN:" + QString::number(m_myCall.length()) + ">" + m_myCall;
    if (m_myGrid.length() > 7) {
      myadif+=" <MY_GRIDSQUARE:8>" + m_myGrid.left(8);
    }
    else if (m_myGrid.length() > 3) {
      myadif+=" <MY_GRIDSQUARE:" + QString::number(m_myGrid.length()) + ">" + m_myGrid;
    }
    myadif+=" <CALL:" + QString::number(hisCall.length()) + ">" + hisCall;
    myadif+=" <FREQ:" + QString::number(strDialFreq.length()) + ">" + strDialFreq;
    if (mode == "FT4") myadif+=" <MODE:4>MFSK <SUBMODE:"  + QString::number(mode.length()) + ">" + mode;
    else myadif+=" <MODE:"  + QString::number(mode.length()) + ">" + mode;
    myadif+=" <QSO_DATE:8>" + m_dateTimeOn.date().toString("yyyyMMdd");
    myadif+=" <TIME_ON:6>" + m_dateTimeOn.time().toString("hhmmss");
    myadif+=" <QSO_DATE_OFF:8>" + m_dateTimeOff.date().toString("yyyyMMdd");
    myadif+=" <TIME_OFF:6>" + m_dateTimeOff.time().toString("hhmmss");
    myadif+=" <RST_SENT:" + QString::number(rptSent.length()) + ">" + rptSent;
    myadif+=" <RST_RCVD:" + QString::number(rptRcvd.length()) + ">" + rptRcvd;
    if (m_txPower.length() >0) {
       myadif+=" <TX_PWR:" + QString::number(m_txPower.length()) + ">" + m_txPower;
    }
    if (hisGrid.length() > 3) {
      myadif+=" <GRIDSQUARE:" + QString::number(hisGrid.length()) + ">" + hisGrid;
    }
    if (name.length() > 0) {
      myadif+=" <NAME:" + QString::number(name.length()) + ">" + name;
    }
    if (comments.length() > 0) {
      myadif+=" <COMMENT:" + QString::number(comments.length()) + ">" + comments;
    }
    if (m_send_to_eqsl) {
      myadif+=" <EQSL_QSL_SENT:1>Y <EQSL_QSLSDATE:8>" + m_jtdxtime->currentDateTimeUtc2().toString("yyyyMMdd");
    }
    myadif+=" <EOR> ";
    myadif2 = myadif.trimmed().toUtf8();

  Q_EMIT acceptQSO (m_dateTimeOff, hisCall, hisGrid, m_dialFreq, mode, rptSent, rptRcvd, m_txPower, comments, name, m_dateTimeOn, eqslcomments, myadif2);
  
  myadif="<command:3>Log <parameters:" + QString::number(myadif.length()) + "> " + myadif;
  myadif2 = myadif.toUtf8();
  if (m_enable_tcp_connection){
    QTcpSocket socket;
    static QFile f2 {QDir {QStandardPaths::writableLocation (QStandardPaths::DataLocation)}.absoluteFilePath ("tcptrace.txt")};
    bool fopen = false;
    if(m_debug) { 
      if(f2.open(QIODevice::WriteOnly | QIODevice::Text | QIODevice::Append)) fopen = true;
      else {
      JTDXMessageBox::warning_message (0, "", " File Open Error "
                                  , tr ("Cannot open \"%1\" for append: %2")
                                  .arg (f2.fileName ()).arg (f2.errorString ()));
      }
    }
    if(fopen)  { QTextStream out2(&f2); out2 << m_jtdxtime->currentDateTimeUtc2().toString("yyyyMMdd_hhmmss.zzz") << "(" << m_jtdxtime->GetOffset() << ")" << " Connecting to " << m_tcp_server_name << ":" << m_tcp_server_port <<
#if QT_VERSION < QT_VERSION_CHECK(5, 15, 0)
                 endl;
#else
                 Qt::endl;
#endif
 }
    socket.connectToHost(m_tcp_server_name, m_tcp_server_port);
    if (socket.waitForConnected(1000)) {
      socket.write(myadif2);
      if(fopen) { QTextStream out2(&f2); out2 << m_jtdxtime->currentDateTimeUtc2().toString("yyyyMMdd_hhmmss.zzz") << "(" << m_jtdxtime->GetOffset() << ")" << " Host connected, sent message: " << myadif2 <<
#if QT_VERSION < QT_VERSION_CHECK(5, 15, 0)
                 endl;
#else
                 Qt::endl;
#endif
 }
      if (socket.waitForReadyRead(1000)){
        myadif2 = socket.readAll();
        if(fopen) { QTextStream out2(&f2); out2 << m_jtdxtime->currentDateTimeUtc2().toString("yyyyMMdd_hhmmss.zzz") << "(" << m_jtdxtime->GetOffset() << ")" << " Received response from host: " << myadif2 <<
#if QT_VERSION < QT_VERSION_CHECK(5, 15, 0)
                 endl;
#else
                 Qt::endl;
#endif
 }
        if (myadif2.left(3) == "NAK") {
          JTDXMessageBox::critical_message(0, "",myadif2 + " QSO data rejected by external software");
        }
      } else {
      if(fopen) { QTextStream out2(&f2); out2 << m_jtdxtime->currentDateTimeUtc2().toString("yyyyMMdd_hhmmss.zzz") << "(" << m_jtdxtime->GetOffset() << ")" << " Getting response from host is timed out" <<
#if QT_VERSION < QT_VERSION_CHECK(5, 15, 0)
                 endl;
#else
                 Qt::endl;
#endif
 }
      }  
      socket.close();
    } else {
      if(fopen) { QTextStream out2(&f2); out2 << m_jtdxtime->currentDateTimeUtc2().toString("yyyyMMdd_hhmmss.zzz") << "(" << m_jtdxtime->GetOffset() << ")" << " Host connection timed out" <<
#if QT_VERSION < QT_VERSION_CHECK(5, 15, 0)
                 endl;
#else
                 Qt::endl;
#endif
 }
      JTDXMessageBox::critical_message(0, "", "TCP QSO data transfer: " + socket.errorString());
    }
    if(fopen) f2.close();
  }
  QDialog::accept();
}

// closeEvent is only called from the system menu close widget for a
// modeless dialog so we use the hideEvent override to store the
// window settings
void LogQSO::hideEvent (QHideEvent * e)
{
  storeSettings ();
  QDialog::hideEvent (e);
}
