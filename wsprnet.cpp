// Interface to WSPRnet website
//
// by Edson Pereira - PY2SDR

#include "wsprnet.h"

#include <cmath>

#include <QTimer>
#include <QFile>
#include <QNetworkAccessManager>
#include <QNetworkRequest>
#include <QNetworkReply>
#include <QUrl>
#include <QDebug>

#include "moc_wsprnet.cpp"

namespace
{
  char const * const wsprNetUrl = "http://wsprnet.org/post?";
  // char const * const wsprNetUrl = "http://127.0.0.1/post?";
};

WSPRNet::WSPRNet(QNetworkAccessManager * manager, QObject *parent)
  : QObject{parent}
  , networkManager {manager}
  , uploadTimer {new QTimer {this}}
  , m_urlQueueSize {0}
{
  connect(networkManager, SIGNAL(finished(QNetworkReply*)), this, SLOT(networkReply(QNetworkReply*)));
  connect( uploadTimer, SIGNAL(timeout()), this, SLOT(work()));
}

void WSPRNet::upload(QString const& call, QString const& grid, QString const& rfreq, QString const& tfreq,
                     QString const& mode, QString const& tpct, QString const& dbm, QString const& version,
                     QString const& fileName)
{
    m_call = call;
    m_grid = grid.left(6);
    m_rfreq = rfreq;
    m_tfreq = tfreq;
    m_mode = mode;
    m_tpct = tpct;
    m_dbm = dbm;
    m_vers = version;
    m_file = fileName;

    // Open the wsprd.out file
    QFile wsprdOutFile(fileName);
    if (!wsprdOutFile.open(QIODevice::ReadOnly | QIODevice::Text) ||
            wsprdOutFile.size() == 0) {
        urlQueue.enqueue( wsprNetUrl + urlEncodeNoSpot());
        m_uploadType = 1;
        uploadTimer->start(200);
        return;
    }

    // Read the contents
    while (!wsprdOutFile.atEnd()) {
      QHash<QString,QString> query;
      if ( decodeLine(wsprdOutFile.readLine(), query) ) {
        // Prevent reporting data ouside of the current frequency band
        float f = fabs(m_rfreq.toFloat() - query["tqrg"].toFloat());
        if (f < 0.0002) {
          urlQueue.enqueue( wsprNetUrl + urlEncodeSpot(query));
          m_uploadType = 2;
        }
      }
    }
    m_urlQueueSize = urlQueue.size();
    uploadTimer->start(200);
}

void WSPRNet::networkReply(QNetworkReply *reply)
{
  // check if request was ours
  if (m_outstandingRequests.removeOne (reply)) {
    if (QNetworkReply::NoError != reply->error ()) {
      Q_EMIT uploadStatus (QString {"Error: %1"}.arg (reply->error ()));
      // not clearing queue or halting queuing as it may be a transient
      // one off request error
    }
    else {
      QString serverResponse = reply->readAll();
      if( m_uploadType == 2) {
        if (!serverResponse.contains(QRegExp("spot\\(s\\) added"))) {
          emit uploadStatus(QString {"Upload Failed: %1"}.arg (serverResponse));
          urlQueue.clear();
          uploadTimer->stop();
        }
      }

      if (urlQueue.isEmpty()) {
        emit uploadStatus("done");
        QFile::remove(m_file);
        uploadTimer->stop();
      }
    }

    qDebug () << QString {"WSPRnet.org %1 outstanding requests"}.arg (m_outstandingRequests.size ());

    // delete request object instance on return to the event loop otherwise it is leaked
    reply->deleteLater ();
  }
}

bool WSPRNet::decodeLine(QString const& line, QHash<QString,QString> &query)
{
    // 130223 2256 7    -21 -0.3  14.097090  DU1MGA PK04 37          0    40    0
    // Date   Time Sync dBm  DT   Freq       Msg
    // 1      2    3     4   5     6         -------7------          8     9    10
    QRegExp rx("^(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+([+-]?\\d+)\\s+([+-]?\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(.*)\\s+([+-]?\\d+)\\s+([+-]?\\d+)\\s+([+-]?\\d+)");
    if (rx.indexIn(line) != -1) {
        int msgType = 0;
        QString msg = rx.cap(7);
        msg.remove(QRegExp("\\s+$"));
        msg.remove(QRegExp("^\\s+"));
        QString call, grid, dbm;
        QRegExp msgRx;

        // Check for Message Type 1
        msgRx.setPattern("^([A-Z0-9]{3,6})\\s+([A-Z]{2}\\d{2})\\s+(\\d+)");
        if (msgRx.indexIn(msg) != -1) {
            msgType = 1;
            call = msgRx.cap(1);
            grid = msgRx.cap(2);
            dbm = msgRx.cap(3);
        }

        // Check for Message Type 2
        msgRx.setPattern("^([A-Z0-9/]+)\\s+(\\d+)");
        if (msgRx.indexIn(msg) != -1) {
            msgType = 2;
            call = msgRx.cap(1);
            grid = "";
            dbm = msgRx.cap(2);
        }

        // Check for Message Type 3
        msgRx.setPattern("^<([A-Z0-9/]+)>\\s+([A-Z]{2}\\d{2}[A-Z]{2})\\s+(\\d+)");
        if (msgRx.indexIn(msg) != -1) {
            msgType = 3;
            call = msgRx.cap(1);
            grid = msgRx.cap(2);
            dbm = msgRx.cap(3);
        }

        // Unknown message format
        if (!msgType) {
            return false;
        }

        query["function"] = "wspr";
        query["date"] = rx.cap(1);
        query["time"] = rx.cap(2);
        query["sig"] = rx.cap(4);
        query["dt"] = rx.cap(5);
        query["drift"] = rx.cap(8);
        query["tqrg"] = rx.cap(6);
        query["tcall"] = call;
        query["tgrid"] = grid;
        query["dbm"] = dbm;
    } else {
        return false;
    }
    return true;
}

QString WSPRNet::urlEncodeNoSpot()
{
    QString queryString;
    queryString += "function=wsprstat&";
    queryString += "rcall=" + m_call + "&";
    queryString += "rgrid=" + m_grid + "&";
    queryString += "rqrg=" + m_rfreq + "&";
    queryString += "tpct=" + m_tpct + "&";
    queryString += "tqrg=" + m_tfreq + "&";
    queryString += "dbm=" + m_dbm + "&";
    queryString += "version=" +  m_vers;
    if(m_mode=="WSPR-2") queryString += "&mode=2";
    if(m_mode=="WSPR-15") queryString += "&mode=15";
    return queryString;;
}

QString WSPRNet::urlEncodeSpot(QHash<QString,QString> const& query)
{
    QString queryString;
    queryString += "function=" + query["function"] + "&";
    queryString += "rcall=" + m_call + "&";
    queryString += "rgrid=" + m_grid + "&";
    queryString += "rqrg=" + m_rfreq + "&";
    queryString += "date=" + query["date"] + "&";
    queryString += "time=" + query["time"] + "&";
    queryString += "sig=" + query["sig"] + "&";
    queryString += "dt=" + query["dt"] + "&";
    queryString += "drift=" + query["drift"] + "&";
    queryString += "tqrg=" + query["tqrg"] + "&";
    queryString += "tcall=" + query["tcall"] + "&";
    queryString += "tgrid=" + query["tgrid"] + "&";
    queryString += "dbm=" + query["dbm"] + "&";
    queryString += "version=" + m_vers;
    if(m_mode=="WSPR-2") queryString += "&mode=2";
    if(m_mode=="WSPR-15") queryString += "&mode=15";
    return queryString;
}

void WSPRNet::work()
{
  if (!urlQueue.isEmpty()) {
#if QT_VERSION < QT_VERSION_CHECK (5, 15, 0)
      if (QNetworkAccessManager::Accessible != networkManager->networkAccessible ()) {
        // try and recover network access for QNAM
        networkManager->setNetworkAccessible (QNetworkAccessManager::Accessible);
      }
#endif
    QUrl url(urlQueue.dequeue());
    QNetworkRequest request(url);
    m_outstandingRequests << networkManager->get(request);
    emit uploadStatus(QString {"Uploading Spot %1/%2"}.arg (m_urlQueueSize - urlQueue.size()).arg (m_urlQueueSize));
  } else {
    uploadTimer->stop();
  }
}

void WSPRNet::abortOutstandingRequests () {
  urlQueue.clear ();
  for (auto& request : m_outstandingRequests) {
    request->abort ();
  }
  m_urlQueueSize = 0;
}
