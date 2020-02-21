// This source code file was last time modified by Arvo ES1JA on 20181229

#ifndef EQSL_H
#define EQSL_H

#include <QObject>
#include <QString>
//#include <QList>
//#include <QHash>
//#include <QQueue>
#include "Radio.hpp"


class QNetworkAccessManager;
class QTimer;
class QNetworkReply;

class EQSL : public QObject
{
  Q_OBJECT;

public:
  explicit EQSL(QNetworkAccessManager *, QObject *parent = nullptr);
    void upload(QString const& eqsl_username, QString const& eqsl_passwd, QString const& eqsl_nickname
                  , QString const& call, QString const& mode
                  , QDateTime const& QSO_date_on
                  , QString const& rpt_sent, QString const& band
                  , QString const& eqslcomments);

//signals:
//    void uploadStatus(QString);

public slots:
    void networkReply(QNetworkReply *);
    void work();

private:
    QNetworkAccessManager *networkManager;
    QString myadif;
    QTimer *uploadTimer;
    QNetworkReply *reply;
    bool m_in_progress;
};

#endif // EQSL_H
