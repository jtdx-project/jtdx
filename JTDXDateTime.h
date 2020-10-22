#ifndef JTDXDATETIME_H__
#define JTDXDATETIME_H__

#include <QDateTime>

//
// JTDXDateTime to manipulate system clock time
//
//Created by Arvo, ES1JA 2020

class JTDXDateTime : public QDateTime
{
 public:
  explicit JTDXDateTime ();
  explicit JTDXDateTime (QDateTime &&other);
  explicit JTDXDateTime(const QDateTime &other);
  explicit JTDXDateTime(const QDate &date, const QTime &time, const QTimeZone &timeZone);
  explicit JTDXDateTime(const QDate &date, const QTime &time, Qt::TimeSpec spec, int offsetSeconds);
  explicit JTDXDateTime(const QDate &date, const QTime &time, Qt::TimeSpec spec = Qt::LocalTime);

  QDateTime currentDateTimeUtc2();
  QDateTime currentDateTime2();
  qint64 currentMSecsSinceEpoch2();
  void SetOffset (float offset) { foffset_ = offset; offset_ = foffset_ * 1000;}
  float GetOffset () { return foffset_; }
 private:
  float foffset_ =0;
  qint64 offset_ = 0;

  using QDateTime::currentDateTimeUtc;
  using QDateTime::currentDateTime;
  using QDateTime::currentMSecsSinceEpoch;
};

#endif
