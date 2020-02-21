// KISS Interface for posting spots to PSK Reporter web site
// Implemented by Edson Pereira PY2SDR
//
// Reports will be sent in batch mode every 5 minutes.

#include "psk_reporter.h"

#include <QHostInfo>
#include <QTimer>

#include "MessageClient.hpp"

#include "moc_psk_reporter.cpp"

namespace
{
  int constexpr MAX_PAYLOAD_LENGTH {1400};
}

PSK_Reporter::PSK_Reporter(MessageClient * message_client, QObject *parent) :
    QObject {parent},
    m_messageClient {message_client},
    reportTimer {new QTimer {this}},
    m_sequenceNumber {0}
{
    m_header_h = "000Allllttttttttssssssssiiiiiiii";

    // We use 50E2 and 50E3 for link Id
    m_rxInfoDescriptor_h = "0003002C50E200040000"
                           "8002FFFF0000768F"     // 2. Rx Call
                           "8004FFFF0000768F"     // 4. Rx Grid
                           "8008FFFF0000768F"     // 8. Rx Soft
                           "8009FFFF0000768F"     // 9. Rx Antenna
                           "0000";

    m_txInfoDescriptor_h = "0002003C50E30007"
                           "8001FFFF0000768F" // 1. Tx Call
                           "800500040000768F" // 5. Tx Freq
                           "800600010000768F" // 6. Tx snr
                           "800AFFFF0000768F" // 10. Tx Mode
                           "8003FFFF0000768F" // 3. Tx Grid
                           "800B00010000768F" // 11. Tx info src
                           "00960004";        // Report time


    m_randomId_h = QString("%1").arg(qrand(),8,16,QChar('0'));

    QHostInfo::lookupHost("report.pskreporter.info", this, SLOT(dnsLookupResult(QHostInfo)));

    connect(reportTimer, SIGNAL(timeout()), this, SLOT(sendReport()));
    reportTimer->start(5*60*1000); // 5 minutes;
}

void PSK_Reporter::setLocalStation(QString call, QString gridSquare, QString antenna, QString programInfo)
{
  m_rxCall = call;
  m_rxGrid = gridSquare;
  m_rxAnt = antenna;
  m_progId = programInfo;
}

void PSK_Reporter::addRemoteStation(QString call, QString grid, QString freq, QString mode, QString snr, QString time )
{
    QHash<QString,QString> spot;
    spot["call"] = call;
    spot["grid"] = grid;
    spot["snr"] = snr;
    spot["freq"] = freq;
    spot["mode"] = mode;
    spot["time"] = time;
    m_spotQueue.enqueue(spot);
}

void PSK_Reporter::sendReport()
{
  while (!m_spotQueue.isEmpty()) {
    QString report_h;

    // Header
    QString header_h = m_header_h;
    header_h.replace("tttttttt", QString("%1").arg(QDateTime::currentDateTime().toTime_t(),8,16,QChar('0')));
    header_h.replace("ssssssss", QString("%1").arg(++m_sequenceNumber,8,16,QChar('0')));
    header_h.replace("iiiiiiii", m_randomId_h);

    // Receiver information
    QString rxInfoData_h = "50E2llll";
    rxInfoData_h += QString("%1").arg(m_rxCall.length(),2,16,QChar('0')) + m_rxCall.toUtf8().toHex();
    rxInfoData_h += QString("%1").arg(m_rxGrid.length(),2,16,QChar('0')) + m_rxGrid.toUtf8().toHex();
    rxInfoData_h += QString("%1").arg(m_progId.length(),2,16,QChar('0')) + m_progId.toUtf8().toHex();
    rxInfoData_h += QString("%1").arg(m_rxAnt.length(),2,16,QChar('0')) + m_rxAnt.toUtf8().toHex();
    rxInfoData_h += "0000";
    rxInfoData_h.replace("50E2llll", "50E2" + QString("%1").arg(rxInfoData_h.length()/2,4,16,QChar('0')));

    // Sender information
    QString txInfoData_h = "50E3llll";
    while (!m_spotQueue.isEmpty()
           && (header_h.size () + m_rxInfoDescriptor_h.size () + m_txInfoDescriptor_h.size () + rxInfoData_h.size () + txInfoData_h.size ()) / 2 < MAX_PAYLOAD_LENGTH) {
      QHash<QString,QString> spot = m_spotQueue.dequeue();
      txInfoData_h += QString("%1").arg(spot["call"].length(),2,16,QChar('0')) + spot["call"].toUtf8().toHex();
      txInfoData_h += QString("%1").arg(spot["freq"].toLongLong(),8,16,QChar('0'));
      txInfoData_h += QString("%1").arg(spot["snr"].toInt(),8,16,QChar('0')).right(2);
      txInfoData_h += QString("%1").arg(spot["mode"].length(),2,16,QChar('0')) + spot["mode"].toUtf8().toHex();
      txInfoData_h += QString("%1").arg(spot["grid"].length(),2,16,QChar('0')) + spot["grid"].toUtf8().toHex();
      txInfoData_h += QString("%1").arg(1,2,16,QChar('0')); // REPORTER_SOURCE_AUTOMATIC
      txInfoData_h += QString("%1").arg(spot["time"].toInt(),8,16,QChar('0'));
    }
    txInfoData_h += "0000";
    txInfoData_h.replace("50E3llll", "50E3" + QString("%1").arg(txInfoData_h.length()/2,4,16,QChar('0')));
    report_h = header_h + m_rxInfoDescriptor_h + m_txInfoDescriptor_h + rxInfoData_h + txInfoData_h;
    //qDebug() << "Sending Report TX: ";

    report_h.replace("000Allll", "000A" + QString("%1").arg(report_h.length()/2,4,16,QChar('0')));
    QByteArray report = QByteArray::fromHex(report_h.toUtf8());

    // Send data to PSK Reporter site
    if (!m_pskReporterAddress.isNull()) {
      m_messageClient->send_raw_datagram (report, m_pskReporterAddress, 4739);
    }
  }
}

void PSK_Reporter::dnsLookupResult(QHostInfo info)
{
    if (!info.addresses().isEmpty()) {
        m_pskReporterAddress = info.addresses().at(0);
        //        qDebug() << "PSK Reporter IP: " << m_pskReporterAddress;

        // deal with miss-configured settings that attempt to set a
        // Pskreporter Internet address for the WSJT-X UDP protocol
        // server address
        m_messageClient->add_blocked_destination (m_pskReporterAddress);
    }
}
