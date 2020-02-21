#ifndef WSPRNET_H
#define WSPRNET_H

#include <QObject>
#include <QString>
#include <QList>
#include <QHash>
#include <QQueue>

class QNetworkAccessManager;
class QTimer;
class QNetworkReply;

class WSPRNet : public QObject
{
  Q_OBJECT;

public:
  explicit WSPRNet(QNetworkAccessManager *, QObject *parent = nullptr);
    void upload(QString const& call, QString const& grid, QString const& rfreq, QString const& tfreq,
                QString const& mode, QString const& tpct, QString const& dbm, QString const& version,
                QString const& fileName);
    static bool decodeLine(QString const& line, QHash<QString,QString> &query);

signals:
    void uploadStatus(QString);

public slots:
    void networkReply(QNetworkReply *);
    void work();
    void abortOutstandingRequests ();

private:
    QNetworkAccessManager *networkManager;
    QList<QNetworkReply *> m_outstandingRequests;
    QString m_call, m_grid, m_rfreq, m_tfreq, m_mode, m_tpct, m_dbm, m_vers, m_file;
    QQueue<QString> urlQueue;
    QTimer *uploadTimer;
    int m_urlQueueSize;
    int m_uploadType;

    QString urlEncodeNoSpot();
    QString urlEncodeSpot(QHash<QString,QString> const& spot);
};

#endif // WSPRNET_H
