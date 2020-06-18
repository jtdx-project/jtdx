
// -*- Mode: C++ -*-
#ifndef DISPLAYTEXT_H
#define DISPLAYTEXT_H

#include <QTextEdit>
#include "logbook/logbook.h"
#include "decodedtext.h"
#include "qsohistory.h"
#include "Radio.hpp"

class Configuration;

class DisplayText : public QTextEdit
{
    Q_OBJECT;
public:
    explicit DisplayText(QWidget *parent = 0);
    void setConfiguration(Configuration const *);
    void setContentFont (QFont const&);
    void insertLineSpacer(QString const&);
    int displayDecodedText(DecodedText* decodedText, QString myCall, QString hisCall, QString hisGrid,
                           bool once_notified, LogBook logBook, QsoHistory& qsoHistory,
                           QsoHistory& qsoHistory2, double dialFreq = 0, const QString app_mode = "",
                           bool bypassRxfFilters = false, bool bypassAllFilters = false, int rx_frq = 0,
                           QStringList wantedCallList = QStringList(), QStringList wantedPrefixList = QStringList(), QStringList wantedGridList = QStringList(),
                           QStringList wantedCountryList = QStringList(), bool windowPopup = false, QWidget* window = NULL);
    void displayTransmittedText(QString text, QString myCall, QString hisCall, QString skip_tx1, QString modeTx, qint32 txFreq,
                                QColor color_TxMsg, QsoHistory& qsoHistory);
    void displayQSY(QString text);

signals:
    void selectCallsign(bool alt, bool ctrl);

public slots:
  void appendText(QString const& text, QString const& bg = "white", QString const& color = "black", int std_type = 0, QString const& servis = " ", QString const& cntry = " ", bool forceBold = false, bool strikethrough = false, bool underline = false, bool DXped = false, bool overwrite = false);

protected:
    void mouseDoubleClickEvent(QMouseEvent *e);

private:

    bool bold_;
    bool wastx_;
    bool useDarkStyle_;
    QsoHistory::Status mystatus_ = QsoHistory::NONE;
    unsigned max_r_time = 0;
    QTextCharFormat m_charFormat;
    Configuration const * m_config;
    unsigned last_tx = 0;
    QString mygrid_ = "";
    QString myhisCall_ = "";
};

#endif // DISPLAYTEXT_H
