// -*- Mode: C++ -*-
/*
 * Class to handle the formatted string as returned from the fortran decoder
 *
 * VK3ACF August 2013
 */


#ifndef DECODEDTEXT_H
#define DECODEDTEXT_H

#include <QObject>
#include <QString>
#include <QRegularExpression>
#include <QHash>


/*
012345678901234567890123456789012345678901234
^    ^    ^   ^    ^ ^                     ^
2343 -11  0.8 1259 # YV6BFE F6GUU R-08
2343 -19  0.3  718 # VE6WQ SQ2NIJ -14
2343  -7  0.3  815 # KK4DSD W7VP -16
2343 -13  0.1 3627 @ CT1FBK IK5YZT R+02
1021 -20  0.3 1598 # CQ PU2NRT GG66        *
1051 -18  0.3 1355 # UA3DJY LU1DA -23      *
1306 -26  0.7 1747 # CQ HS6OKJ OK25        *
1619 -29  2.5  702 # CQ ZS6BK KG43         *   real string_
"
0605  Tx      1259 # CQ VK3ACF QF22
*/

class DecodedText : public QObject
{
Q_OBJECT;
public:
    explicit DecodedText (QString const& message, QObject *parent = nullptr);

    QString string();
    QString message() { return message_; };
    int indexOf(QString s) { return string_.indexOf(s); };
    int indexOf(QString s, int i) { return string_.indexOf(s,i); };
    QString mid(int f, int t) { return string_.mid(f,t); };
    QString left(int i) { return string_.left(i); };

    void clear() { string_.clear(); };

    QString CQersCall(QString& grid,QString& tyyp);

    bool isJT65();
    bool isJT9();
    bool isTX();
    bool isHint();
    bool isStandardMessage () const {return is_standard_;}

    bool isWrong();
    bool isNonStd1();
    bool isNonStd2();
    bool isEnd();
    bool isFin();
    bool isDebug();
    bool isDXped();
    int frequencyOffset();  // hertz offset from the tuned dial or rx frequency, aka audio frequency
    int snr();
    float dt();

    // find and extract any report. Returns true if this is a standard message
  bool report(QString const& myBaseCall, QString const& dxBaseCall, /*mod*/QString& report,QString& type);

    // get the first text word, usually the call
    QString call();

    // get the second word, most likely the de call and the third word, most likely grid
    void deCallAndGrid(/*out*/QString& call, QString& grid);

    unsigned timeInSeconds() const;

    // returns a string of the SNR field with a leading + or - followed by two digits
    QString report();

private:
     enum Columns { column_time    = 0,
                    column_snr     = 5,
                    column_dt      = 9,
                    column_freq    = 14,
                    column_mode    = 19,
                    column_qsoText = 21};
    QString string_;
    QRegularExpression _cqLongerRe = QRegularExpression(" CQ ([A-Z]{2,2}|[0-9]{3,3}) ");
    QRegularExpression _gridRe = QRegularExpression("^(?![Rr]{2}73)[A-Ra-r]{2,2}[0-9]{2,2}$");
    QRegularExpression _repRe = QRegularExpression("[<>]");
//    QRegularExpression _callRe = QRegularExpression("(([A-Z]{1,2})|([A-Z][0-9]))[0-9][A-Z]{1,3}");
//    QRegularExpression _callRe = QRegularExpression("([BFGIKMNRTW]|[A-Z0-9]{2})[0-9][A-Z0-9]{0,3}[A-Z]");
    QRegularExpression _callRe = QRegularExpression("[2-9]{0,1}[A-Z]{1,2}[0-9]{1,4}[A-Z]{0,6}");
    int padding_;
    QString message_;
    bool is_standard_;
    QHash<QString, QString> debug_translation_;
};



#endif // DECODEDTEXT_H
