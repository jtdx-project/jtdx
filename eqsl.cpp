// This source code file was last time modified by Arvo ES1JA on 20181229

#include "wsprnet.h"

#include <cmath>

#include <QTimer>
#include <QFile>
#include <QNetworkAccessManager>
#include <QNetworkRequest>
#include <QNetworkReply>
#include <QUrl>
#include <QUrlQuery>
#include <QDebug>

#include "moc_eqsl.cpp"

namespace
{
  char const * const EqslUrl = "http://www.eqsl.cc/qslcard/importADIF.cfm";
  // char const * const wsprNetUrl = "http://127.0.0.1/post?";
};

EQSL::EQSL(QNetworkAccessManager * manager, QObject *parent)
  : QObject{parent}
  , networkManager {manager}
  , uploadTimer {new QTimer {this}}
  , m_in_progress {false}
{
  uploadTimer->setSingleShot(true);
  connect(networkManager, SIGNAL(finished(QNetworkReply*)), this, SLOT(networkReply(QNetworkReply*)));
  connect( uploadTimer, SIGNAL(timeout()), this, SLOT(work()));
}

void EQSL::upload(QString const& eqsl_username, QString const& eqsl_passwd, QString const& eqsl_nickname
                  , QString const& call, QString const& mode
                  , QDateTime const& QSO_date_on
                  , QString const& rpt_sent, QString const& band
                  , QString const& eqslcomments)
{

    if (m_in_progress) {
        reply->abort();
        m_in_progress = false;
    }
    myadif="<ADIF_VER:5>2.1.9";
    myadif+="<EQSL_USER:" + QString::number(eqsl_username.length()) + ">" + eqsl_username;
    myadif+="<EQSL_PSWD:" + QString::number(eqsl_passwd.length()) + ">" + eqsl_passwd;
    myadif+="<PROGRAMID:4>JTDX<EOH><APP_EQSL_QTH_NICKNAME:" + QString::number(eqsl_nickname.length()) + ">" + eqsl_nickname;
    myadif+="<CALL:" + QString::number(call.length()) + ">" + call;
    myadif+="<MODE:"  + QString::number(mode.length()) + ">" + mode;
    myadif+="<QSO_DATE:8>" + QSO_date_on.date().toString("yyyyMMdd");
    myadif+="<TIME_ON:4>" + QSO_date_on.time().toString("hhmm");
    myadif+="<RST_SENT:" + QString::number(rpt_sent.length()) + ">" + rpt_sent;
    myadif+="<BAND:" + QString::number(band.length()) + ">" + band;
    if(eqslcomments!="") myadif+="<QSLMSG:" + QString::number(eqslcomments.length()) + ">" + eqslcomments;
//    myadif+="<QSLMSG:19>TNX For QSO TU 73!.";
    myadif+="<EOR>";

    uploadTimer->start(1);
}

void EQSL::networkReply(QNetworkReply *reply)
{
    if (QNetworkReply::NoError != reply->error ()) {
      
      printf ("eqsl upload error:%d\n",reply->error ());
    }
    else {
      QString serverResponse = reply->readAll();
      
//      printf("eqsl upload result:%s\n",serverResponse.toStdString().c_str());
    }


    // delete request object instance on return to the event loop otherwise it is leaked
    reply->deleteLater ();
    m_in_progress = false;
}

void EQSL::work()
{
#if QT_VERSION < QT_VERSION_CHECK (5, 15, 0)
      if (QNetworkAccessManager::Accessible != networkManager->networkAccessible ()) {
        // try and recover network access for QNAM
        networkManager->setNetworkAccessible (QNetworkAccessManager::Accessible);
      }
#endif
    m_in_progress = true;
    QUrl url(EqslUrl);
    QUrlQuery query;
    query.addQueryItem("ADIFdata", myadif);
    url.setQuery(query.query());
    m_in_progress = true;
//    printf ("eqsl upload request:%s\n",myadif.toStdString().c_str());
    QNetworkRequest request(url);
    reply = networkManager->get(request);
//    printf ("sent\n");
}

