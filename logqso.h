// -*- Mode: C++ -*-

#ifndef LogQSO_H
#define LogQSO_H

#ifdef QT5
#include <QtWidgets>
#else
#include <QtGui>
#endif

#include <QScopedPointer>

#include "Radio.hpp"
#include "JTDXDateTime.h"

namespace Ui {
  class LogQSO;
}

class QSettings;
class Configuration;

class LogQSO : public QDialog
{
  Q_OBJECT;

public:
  explicit LogQSO(QSettings *, Configuration const *, JTDXDateTime * jtdxtime, QWidget *parent = 0);
  ~LogQSO();
  void initLogQSO(QString const& hisCall, QString const& hisGrid, QString mode,
                  QString const& rptSent, QString const& rptRcvd, QString const& distance,
                  QString const& name, QDateTime const& dateTimeOn,
                  QDateTime const& dateTimeOff, Radio::Frequency dialFreq, bool autologging);

public slots:
  void accept();

signals:
  void acceptQSO (QDateTime const& QSO_date_off, QString const& call, QString const& grid
                  , Radio::Frequency dial_freq, QString const& mode
                  , QString const& rpt_sent, QString const& rpt_received
                  , QString const& tx_power, QString const& comments
                  , QString const& name, QDateTime const& QSO_date_on
                  , QString const& eqslcomments, QByteArray const& myadif2);

protected:
  void hideEvent (QHideEvent *);

private:
  void loadSettings ();
  void storeSettings () const;

  QScopedPointer<Ui::LogQSO> ui;
  QSettings * m_settings;
  Configuration const * m_config;
  QString m_txPower;
  QString m_comments;
  QString m_eqslcomments;
  Radio::Frequency m_dialFreq;
  QString m_myCall;
  QString m_myGrid;
  QDateTime m_dateTimeOn;
  QDateTime m_dateTimeOff;
  bool m_send_to_eqsl;
  QString m_eqsl_username;
  QString m_eqsl_passwd;
  QString m_eqsl_nickname;
  QString m_tcp_server_name;
  uint m_tcp_server_port;
  uint m_eqsltimer;
  bool m_enable_tcp_connection;
  bool m_debug;
  JTDXDateTime * m_jtdxtime;
};

#endif // LogQSO_H
