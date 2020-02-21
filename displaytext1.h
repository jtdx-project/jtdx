// -*- Mode: C++ -*-
#ifndef DISPLAYTEXT_H
#define DISPLAYTEXT_H

#include <QTextEdit>
#include "logbook/logbook.h"
#include "decodedtext.h"


class DisplayText : public QTextEdit
{
    Q_OBJECT
public:
    explicit DisplayText(QWidget *parent = 0);

    void setContentFont (QFont const&);
    void insertLineSpacer(QString const&);
    void displayDecodedText(DecodedText decodedText, QString myCall, bool displayCountryName,
                            LogBook logBook, QColor color_CQ, QColor color_MyCall,
                            QColor color_DXCC, QColor color_NewCall);
    void displayTransmittedText(QString text, QString modeTx, qint32 txFreq,
                                QColor color_TxMsg, bool bFastMode);
    void displayQSY(QString text);

signals:
    void selectCallsign(bool shift, bool ctrl);

public slots:
  void appendText(QString const& text, QString const& bg = "white");

protected:
    void mouseDoubleClickEvent(QMouseEvent *e);

private:
    void _appendDXCCWorkedB4(/*mod*/DecodedText& t1, QString &bg, LogBook logBook,
                 QColor color_CQ, QColor color_DXCC, QColor color_NewCall);

  QTextCharFormat m_charFormat;
};

#endif // DISPLAYTEXT_H
