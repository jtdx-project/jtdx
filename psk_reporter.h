// -*- Mode: C++ -*-
#ifndef PSK_REPORTER_H
#define PSK_REPORTER_H

#include <QObject>
#include <QString>
#include <QHostAddress>
#include <QQueue>
#include <QHash>

class MessageClient;
class QTimer;
class QHostInfo;

class PSK_Reporter : public QObject
{
    Q_OBJECT
public:
  explicit PSK_Reporter(MessageClient *, QObject *parent = nullptr);
    void setLocalStation(QString call, QString grid, QString antenna, QString programInfo);
    void addRemoteStation(QString call, QString grid, QString freq, QString mode, QString snr, QString time);
    
signals:
    
public slots:
    void sendReport();

private slots:
    void dnsLookupResult(QHostInfo info);

private:
    QString m_header_h;
    QString m_rxInfoDescriptor_h;
    QString m_txInfoDescriptor_h;
    QString m_randomId_h;
    QString m_linkId_h;

    QString m_rxCall;
    QString m_rxGrid;
    QString m_rxAnt;
    QString m_progId;

    QHostAddress m_pskReporterAddress;

    QQueue< QHash<QString,QString> > m_spotQueue;

    MessageClient * m_messageClient;

    QTimer *reportTimer;

    int m_sequenceNumber;
};

#endif // PSK_REPORTER_H
