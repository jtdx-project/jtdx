// This source code file was last time modified by Arvo ES1JA on November 17th, 2019
// All changes are shown in the patch file coming together with the full JTDX source code.

#include "adif.h"
#include "../Radio.hpp"
#include <QFile>
#include <QTextStream>
//#include <QDateTime>
#include <QDebug>

/*
<CALL:4>W1XT<BAND:3>20m<FREQ:6>14.076<GRIDSQUARE:4>DM33<MODE:4>JT65<RST_RCVD:3>-21<RST_SENT:3>-14<QSO_DATE:8>20110422<TIME_ON:4>0417<TIME_OFF:4>0424<TX_PWR:1>4<COMMENT:34>1st JT65A QSO.   Him: mag loop 20W<STATION_CALLSIGN:6>VK3ACF<MY_GRIDSQUARE:6>qf22lb<eor>
<CALL:6>IK1SOW<BAND:3>20m<FREQ:6>14.076<GRIDSQUARE:4>JN35<MODE:4>JT65<RST_RCVD:3>-19<RST_SENT:3>-11<QSO_DATE:8>20110422<TIME_ON:4>0525<TIME_OFF:4>0533<TX_PWR:1>3<STATION_CALLSIGN:6>VK3ACF<MY_GRIDSQUARE:6>qf22lb<eor>
<CALL:6:S>W4ABC> ...
*/

void ADIF::init(QString filename)
{
    _filename = filename;
    _data.clear();
    _countries.init("","");
}

void ADIF::init(QString filename, CountryDat *countries)
{
    _filename = filename;
    _data.clear();
    _countries = *countries;
}

QString ADIF::_extractField(const QString line, const QString fieldName)
{
    int fieldNameIndex = line.indexOf('<' + fieldName  ,0,Qt::CaseInsensitive);
    if (fieldNameIndex >=0)
    {
        int closingBracketIndex = line.indexOf('>',fieldNameIndex);
        int fieldLengthIndex = line.indexOf(':',fieldNameIndex);  // find the size delimiter
        int dataTypeIndex = -1;
        if (fieldLengthIndex >= 0)
        {
          dataTypeIndex = line.indexOf(':',fieldLengthIndex+1);  // check for a second : indicating there is a data type
          if (dataTypeIndex > closingBracketIndex)
            dataTypeIndex = -1; // second : was found but it was beyond the closing >
        }

        if ((closingBracketIndex > fieldNameIndex) && (fieldLengthIndex > fieldNameIndex) && (fieldLengthIndex< closingBracketIndex))
        {
            int fieldLengthCharCount = closingBracketIndex - fieldLengthIndex -1;
            if (dataTypeIndex >= 0)
              fieldLengthCharCount -= 2; // data type indicator is always a colon followed by a single character
            QString fieldLengthString = line.mid(fieldLengthIndex+1,fieldLengthCharCount);
            int fieldLength = fieldLengthString.toInt();
            if (fieldLength > 0)
            {
              QString field = line.mid(closingBracketIndex+1,fieldLength);
              return field;
            }
       }
    }
    return "";
}



void ADIF::load(const QString mycall,const QString mygrid,const QString mydate)
{
    _data.clear();
    _cqzWorked.clear();
    _ituzWorked.clear();
    _countriesWorked.clear();
    _gridsWorked.clear();
    _pxsWorked.clear();
    _counts.clear();
    
    QFile inputFile(_filename);
    if (inputFile.open(QIODevice::ReadOnly))
    {
        QTextStream in(&inputFile);
        while ( !in.atEnd() )
        {
            QString record = in.readLine();
			while (record.indexOf("<EOR>", 0, Qt::CaseInsensitive) == -1 && !in.atEnd()) {
				record.append(in.readLine());
			}				
            QString mytime = _extractField(record,"QSO_DATE:")+_extractField(record,"TIME_ON:");
            while (mytime.length() < 14) mytime += "0";
            if ((mycall.isEmpty () || _extractField(record,"STATION_CALLSIGN:") == mycall) && 
                (mygrid.isEmpty () || mygrid.left(4) == _extractField(record,"MY_GRIDSQUARE:").left(4)) && 
                (mydate.isEmpty () || mytime.toLongLong() >= mydate.toLongLong())) {
                QSO q;
                q.call = _extractField(record,"CALL:");
                q.band = _extractField(record,"BAND:");
                q.mode = _extractField(record,"MODE:");
                if (q.mode == "MFSK") q.mode = _extractField(record,"SUBMODE:");
                if (q.mode.left(3) == "JT9") q.mode = "JT9";
                else if (q.mode.left(4) == "JT65") q.mode = "JT65";
                q.date = _extractField(record,"QSO_DATE:");
                q.gridsquare = _extractField(record,"GRIDSQUARE:");
                q.name = _extractField(record,"NAME:");
                if (!q.call.isEmpty ()) {
                    _data.insert(q.call,q);
                    _pxsWorked.insert(Radio::striped_prefix(Radio::effective_prefix(q.call)), q);
                    _counts.insert(q.mode,_counts.value(q.mode,0)+1);
                    QString country = _countries.find(Radio::effective_prefix(q.call));
                    if (!country.isEmpty ()) { //  country was found
                        auto items = country.split(',');
                        _countriesWorked.insert(items[0]+','+items[1]+','+items[2], q);
                        _cqzWorked.insert(items[3],q);
                        _ituzWorked.insert(items[4],q);
                    }
                    if (q.gridsquare.length() > 3) { // grid exists
                        _gridsWorked.insert(q.gridsquare.left(4).toUpper(),q);
                    }
                }
            }
        }
        inputFile.close();
    }
}


void ADIF::add(const QString call, const QString band, const QString mode, const QString date, const QString gridsquare, const QString name)
{
    QSO q;
    q.call = call;
    q.band = band;
    q.mode = mode;
    q.date = date;
    q.gridsquare = gridsquare;
    q.name = name;
    _data.insert(q.call,q);
    _pxsWorked.insert(Radio::striped_prefix(Radio::effective_prefix(q.call)), q);
    _counts.insert(q.mode,_counts.value(q.mode,0)+1);
    QString country = _countries.find(Radio::effective_prefix(q.call));
    if (!country.isEmpty ()) {
        auto items = country.split(',');
        _countriesWorked.insert(items[0]+','+items[1]+','+items[2], q);
        _cqzWorked.insert(items[3],q);
        _ituzWorked.insert(items[4],q);
    }
    if (q.gridsquare.length() > 3) {
        _gridsWorked.insert(q.gridsquare.left(4).toUpper(),q);
    }        
    //qDebug() << "Added as worked:" << call << band << mode << date;
}

// return true if in the log same band and mode
bool ADIF::match(const QString call, const QString band, const QString mode)
{
    
    QList<QSO> qsos = _data.values(call);
    if (qsos.size()>0)
    {
        QSO q;
        foreach(q,qsos)
        {
            if (     (band.compare(q.band,Qt::CaseInsensitive) == 0)
                  || (band.isEmpty ())
                  || (q.band.isEmpty ()))
            {
                if (       (mode.compare(q.mode,Qt::CaseInsensitive)==0)
                        || (mode.isEmpty ())
                        || (q.mode.isEmpty ())) {
                    return true;
                }
            }
        }
    }
    return false;
}    

// return true if in the log same band and mode
bool ADIF::matchPx(const QString call, const QString band, const QString mode)
{
    
    QList<QSO> qsos = _pxsWorked.values(Radio::striped_prefix(Radio::effective_prefix(call)));
    if (qsos.size()>0)
    {
        QSO q;
        foreach(q,qsos)
        {
            if (     (band.compare(q.band,Qt::CaseInsensitive) == 0)
                  || (band.isEmpty ())
                  || (q.band.isEmpty ()))
            {
                if (       (mode.compare(q.mode,Qt::CaseInsensitive)==0)
                        || (mode.isEmpty ())
                        || (q.mode.isEmpty ())) {
//                    printf("Match Prefix %d %s|%s|%s -> %s|%s|%s\n",qsos.size(),Radio::striped_prefix(Radio::effective_prefix(call)).toStdString().c_str(),band.toStdString().c_str(),mode.toStdString().c_str(),
//                        q.call.toStdString().c_str(),q.band.toStdString().c_str(),q.mode.toStdString().c_str());
                    return true;
                }
            }
        }
    }
//    printf("Match Prefix %d %s|%s|%s->\n",qsos.size(),Radio::striped_prefix(Radio::effective_prefix(call)).toStdString().c_str(),band.toStdString().c_str(),mode.toStdString().c_str());
    return false;
}    

// return true if in the log same band and mode
bool ADIF::getData(const QString call, QString &gridsquare, QString &name)
{
    
    bool setgrid = call.endsWith("/MM") || call.endsWith("/M") || call.endsWith("/AM") || call.endsWith("/P") || call.endsWith("/A");
    QList<QSO> qsos = _data.values(call);
    if (qsos.size()>0)
    {
        QSO q;
        foreach(q,qsos)
        {
            if (     (gridsquare.isEmpty() && !q.gridsquare.isEmpty())
                  || (gridsquare.left(4) == q.gridsquare.left(4) && q.gridsquare.length() > gridsquare.length()))
            {
                if (!setgrid ) gridsquare = q.gridsquare;
                name = q.name;
            }
            if (     (name.isEmpty() && !q.name.isEmpty())
                  || (name.compare(q.name,Qt::CaseInsensitive) == 0))
            {
                name = q.name;
            }
            
        }
        return true;
    }
    return false;
}    

// return true if in the log same band and mode
bool ADIF::matchCqz(const QString Cqz, const QString band, const QString mode)
{
    QList<QSO> qsos = _cqzWorked.values(Cqz);
    if (qsos.size()>0)
    {
        QSO q;
        foreach(q,qsos) {
            if (     (band.compare(q.band,Qt::CaseInsensitive) == 0)
                  || (band.isEmpty ())
                  || (q.band.isEmpty ()))
            {
                if (       (mode.compare(q.mode,Qt::CaseInsensitive)==0)
                        || (mode.isEmpty ())
                        || (q.mode.isEmpty ())) {
//                    printf("Match Cqz %d %s|%s|%s -> %s|%s|%s\n",qsos.size(),Cqz.toStdString().c_str(),band.toStdString().c_str(),mode.toStdString().c_str(),
//                        q.call.toStdString().c_str(),q.band.toStdString().c_str(),q.mode.toStdString().c_str());
                    return true;
                }
            }
        }
    }
//    printf("Match Cqz %d %s|%s|%s ->\n",qsos.size(),Cqz.toStdString().c_str(),band.toStdString().c_str(),mode.toStdString().c_str());
    return false;
}    

// return true if in the log same band and mode
bool ADIF::matchItuz(const QString Ituz, const QString band, const QString mode)
{
    QList<QSO> qsos = _ituzWorked.values(Ituz);
    if (qsos.size()>0)
    {
        QSO q;
        foreach(q,qsos) {
            if (     (band.compare(q.band,Qt::CaseInsensitive) == 0)
                  || (band.isEmpty ())
                  || (q.band.isEmpty ()))
            {
                if (       (mode.compare(q.mode,Qt::CaseInsensitive)==0)
                        || (mode.isEmpty ())
                        || (q.mode.isEmpty ())) {
//                    printf("Match Ituz %d %s|%s|%s -> %s|%s|%s\n",qsos.size(),Ituz.toStdString().c_str(),band.toStdString().c_str(),mode.toStdString().c_str(),
//                        q.call.toStdString().c_str(),q.band.toStdString().c_str(),q.mode.toStdString().c_str());
                    return true;
                }
            }
        }
    }
//    printf("Match Ituz %d %s|%s|%s ->\n",qsos.size(),Ituz.toStdString().c_str(),band.toStdString().c_str(),mode.toStdString().c_str());
    return false;
}    

// return true if in the log same band and mode
bool ADIF::matchCountry(const QString countryName, const QString band, const QString mode)
{
    QList<QSO> qsos = _countriesWorked.values(countryName);
    if (qsos.size()>0)
    {
        QSO q;
        foreach(q,qsos) {
            if (     (band.compare(q.band,Qt::CaseInsensitive) == 0)
                  || (band.isEmpty ())
                  || (q.band.isEmpty ()))
            {
                if (       (mode.compare(q.mode,Qt::CaseInsensitive)==0)
                        || (mode.isEmpty ())
                        || (q.mode.isEmpty ())) {
                    return true;
                }
            }
        }
    }
    return false;
}    

// return true if in the log same band and mode
bool ADIF::matchGrid(const QString gridsquare, const QString band, const QString mode)
{
    QList<QSO> qsos = _gridsWorked.values(gridsquare);
    if (qsos.size()>0)
    {
        QSO q;
        foreach(q,qsos) {
            if (     (band.compare(q.band,Qt::CaseInsensitive) == 0)
                  || (band.isEmpty ())
                  || (q.band.isEmpty ()))
            {
                if (       (mode.compare(q.mode,Qt::CaseInsensitive)==0)
                        || (mode.isEmpty ())
                        || (q.mode.isEmpty ())) {
                    return true;
                }
            }
        }
    }
    return false;
}    

QList<QString> ADIF::getCallList()
{
    QList<QString> p;
    QMultiHash<QString,QSO>::const_iterator i = _data.constBegin();
     while (i != _data.constEnd())
     {
         p << i.key();
         ++i;
     }
    return p;
}   
    
int ADIF::getCount(const QString mode)
{
    if (mode.isEmpty ()){
        return _data.size();
    } else {
        return _counts.value(mode,0);
    }
}   
    

// open ADIF file and append the QSO details. Return true on success
bool ADIF::addQSOToFile(const QString hisCall, const QString hisGrid, const QString mode, const QString rptSent, const QString rptRcvd, QDateTime const& dateTimeOn, QDateTime const& dateTimeOff, const QString band,
                        const QString comments, const QString name, const QString strDialFreq, const QString m_myCall, const QString m_myGrid, const QString m_txPower,const bool send_to_eqsl)
{
    QFile f2(_filename);
    if (!f2.open(QIODevice::Text | QIODevice::Append))
        return false;
    else
    {
        QTextStream out(&f2);
        if (f2.size()==0)
            out << "WSJT-X ADIF Export<eoh>" <<
#if QT_VERSION < QT_VERSION_CHECK(5, 15, 0)
                 endl;
#else
                 Qt::endl;
#endif
  // new file

        QString t;
        t="<call:" + QString::number(hisCall.length()) + ">" + hisCall;
        t+=" <gridsquare:" + QString::number(hisGrid.length()) + ">" + hisGrid;
        if (mode == "FT4") t+=" <mode:4>MFSK <submode:" + QString::number(mode.length()) + ">" + mode;
        else t+=" <mode:" + QString::number(mode.length()) + ">" + mode;
        t+=" <rst_sent:" + QString::number(rptSent.length()) + ">" + rptSent;
        t+=" <rst_rcvd:" + QString::number(rptRcvd.length()) + ">" + rptRcvd;
        t+=" <qso_date:8>" + dateTimeOn.date ().toString ("yyyyMMdd");
        t+=" <time_on:6>" + dateTimeOn.time ().toString ("hhmmss");
        t+=" <qso_date_off:8>" + dateTimeOff.date ().toString ("yyyyMMdd");
        t+=" <time_off:6>" + dateTimeOff.time ().toString ("hhmmss");
        t+=" <band:" + QString::number(band.length()) + ">" + band;
        t+=" <freq:" + QString::number(strDialFreq.length()) + ">" + strDialFreq;
        t+=" <station_callsign:" + QString::number(m_myCall.length()) + ">" +
                m_myCall;
        t+=" <my_gridsquare:" + QString::number(m_myGrid.length()) + ">" +
                m_myGrid;
        if(!m_txPower.isEmpty ()) t+= " <tx_pwr:" + QString::number(m_txPower.length()) +
                ">" + m_txPower;
        if(!comments.isEmpty ()) t+=" <comment:" + QString::number(comments.length()) +
                ">" + comments;
        if(!name.isEmpty ()) t+=" <name:" + QString::number(name.length()) +
                ">" + name;
        if (send_to_eqsl) t+=" <eqsl_qsl_sent:1>Y";
        t+=" <eor>";
        out << t <<
#if QT_VERSION < QT_VERSION_CHECK(5, 15, 0)
                 endl;
#else
                 Qt::endl;
#endif

        f2.close();
    }
    return true;
}

QString ADIF::bandFromFrequency(double dialFreq)
{
    QString band="";
    if(dialFreq>0.135 and dialFreq<0.139) band="2200m";
    else if(dialFreq>0.45 and dialFreq<0.55) band="630m";
    else if(dialFreq>1.8 and dialFreq<2.0) band="160m";
    else if(dialFreq>3.5 and dialFreq<4.0) band="80m";
    else if(dialFreq>5.1 and dialFreq<5.45) band="60m";
    else if(dialFreq>7.0 and dialFreq<7.3) band="40m";
    else if(dialFreq>10.0 and dialFreq<10.15) band="30m";
    else if(dialFreq>14.0 and dialFreq<14.35) band="20m";
    else if(dialFreq>18.068 and dialFreq<18.168) band="17m";
    else if(dialFreq>21.0 and dialFreq<21.45) band="15m";
    else if(dialFreq>24.890 and dialFreq<24.990) band="12m";
    else if(dialFreq>28.0 and dialFreq<29.7) band="10m";
    else if(dialFreq>50.0 and dialFreq<54.0) band="6m";
    else if(dialFreq>70.0 and dialFreq<71.0) band="4m";
    else if(dialFreq>144.0 and dialFreq<148.0) band="2m";
    else if(dialFreq>222.0 and dialFreq<225.0) band="1.25m";
    else if(dialFreq>420.0 and dialFreq<450.0) band="70cm";
    else if(dialFreq>902.0 and dialFreq<928.0) band="33cm";
    else if(dialFreq>1240.0 and dialFreq<1300.0) band="23cm";
    else if(dialFreq>2300.0 and dialFreq<2450.0) band="13cm";
    else if(dialFreq>3300.0 and dialFreq<3500.0) band="9cm";
    else if(dialFreq>5650.0 and dialFreq<5925.0) band="6cm";
    else if(dialFreq>10000.0 and dialFreq<10500.0) band="3cm";
    else if(dialFreq>24000.0 and dialFreq<24250.0) band="1.25cm";
    else if(dialFreq>47000.0 and dialFreq<47200.0) band="6mm";
    else if(dialFreq>75500.0 and dialFreq<81000.0) band="4mm";
    return band;
}
