//
// JTDXDateTime to manipulate system clock time
//
//Created by Arvo, ES1JA 2020

#include "JTDXDateTime.h"

JTDXDateTime::JTDXDateTime() : QDateTime {}
{}

JTDXDateTime::JTDXDateTime(QDateTime &&other) : QDateTime {other}
{}

JTDXDateTime::JTDXDateTime(const QDateTime &other) : QDateTime {other}
{}

JTDXDateTime::JTDXDateTime(const QDate &date, const QTime &time, const QTimeZone &timeZone) : QDateTime {date,time,timeZone}
{}

JTDXDateTime::JTDXDateTime(const QDate &date, const QTime &time, Qt::TimeSpec spec, int offsetSeconds) : QDateTime {date,time,spec,offsetSeconds}
{}

JTDXDateTime::JTDXDateTime(const QDate &date, const QTime &time, Qt::TimeSpec spec) : QDateTime {date,time,spec}
{}

qint64 JTDXDateTime::currentMSecsSinceEpoch2()
  {
    return offset_ + QDateTime::currentMSecsSinceEpoch();
  }

QDateTime JTDXDateTime::currentDateTimeUtc2()
  {
    return QDateTime::currentDateTimeUtc().addMSecs(offset_); //result;
  }

QDateTime JTDXDateTime::currentDateTime2()
  {
    return QDateTime::currentDateTime().addMSecs(offset_); //result;
  }
