#include "decodedtext.h"
#include <QStringList>
#include <QDebug>
#include "moc_decodedtext.cpp"
#include "qt_helpers.hpp"
//#include <QRegularExpression>

extern "C" {  bool stdmsg_(char const * msg, fortran_charlen_t); }

namespace
{
  QRegularExpression words_re {R"(^(?:(?<word1>(?:CQ|DE|QRZ)(?:\s?DX|\s(?:[A-Z]{2}|\d{3}))|...|[A-Z0-9/]+)\s)(?:(?<word2>[A-Z0-9/]+)(?:\s(?<word3>[-+A-Z0-9]+)(?:\s(?<word4>(?:OOO|(?!RR73)[A-R]{2}[0-9]{2})))?)?)?)"};
}

DecodedText::DecodedText (QString const& the_string, QObject *parent)
  : QObject {parent}
  ,string_ {the_string.left (the_string.indexOf (QChar::Nbsp))} // discard appended info
//  : string_ {"185415   4  0.2  950 ~  CQ 5B/SQ9UM"}
  , padding_ {string_.indexOf (" ") > 4 ? 2 : 0} // allow for
                                                    // seconds
  , message_ {string_.mid (column_qsoText + padding_).trimmed ()}
  , is_standard_ {false}
{
  if (!message_.isEmpty ())
    {
      debug_translation_.clear();
      debug_translation_.insert("partial loss of data",tr("partial loss of data"));
      debug_translation_.insert("ALLCALL7.TXT is too short or broken?",tr("ALLCALL7.TXT is too short or broken?"));
      debug_translation_.insert("nQSOProgress",tr("nQSOProgress"));
      debug_translation_.insert("input signal low rms",tr("input signal low rms"));
      debug_translation_.insert("audio gap detected",tr("audio gap detected"));
      debug_translation_.insert("nfqso is out of bandwidth",tr("nfqso is out of bandwidth"));
            
      message_ = message_.left (24).remove (QRegularExpression {"[<>]"});
      int i1 = message_.indexOf ('\r');
      if (i1 > 0)
        {
          message_ = message_.left (i1 - 1);
        }
      if (message_.contains (QRegularExpression {"^(CQ|QRZ)\\s"}))
        {
          // TODO this magic position 16 is guaranteed to be after the
          // last space in a decoded CQ or QRZ message but before any
          // appended DXCC entity name or worked before information
          auto eom_pos = message_.indexOf (' ', 16);
          // we always want at least the characters to position 16
          if (eom_pos < 16) eom_pos = message_.size () - 1;
          // remove DXCC entity and worked B4 status. TODO need a better way to do this
          message_ = message_.left (eom_pos + 1);
        }
      // stdmsg is a fortran routine that packs the text, unpacks it
      // and compares the result
        auto message_c_string = message_.toLocal8Bit ();
        message_c_string += QByteArray {37 - message_c_string.size (), ' '};
        is_standard_ = stdmsg_ (message_c_string.constData (),37);
    }
};


QString DecodedText::string()
{
  if (isDebug()) {
    return string_.left(1 + column_snr + padding_) + debug_translation_.value(string_.mid(1 + column_snr + padding_,46 - column_snr).trimmed(),string_.mid(1 + column_snr + padding_,46 -column_snr).trimmed());
  } 
  else
    return string_; 
}

QString DecodedText::CQersCall(QString& grid,QString& tyyp)
{
//  QRegularExpression callsign_re {R"(^(CQ|DE|QRZ)(\s?DX|\s([A-Z]{2}|\d{3}))?\s(?<callsign>(([2-9]{0,1}[A-Z]{1,2}[0-9]{0,1}[/]{1}){0,1})[2-9]{0,1}[A-Z]{1,2}[0-9]{1,4}[A-Z]{1,6}([A-Z0-9/]{1,4}){0,1})(?:\s(?<grid>[A-R]{2}[0-9]{2})?)?)"};
  QRegularExpression callsign_re {R"(^(CQ|DE|QRZ)(\s?(?<tyyp>([A-Z]{1,2}|\d{3}))?)?\s(?<callsign>(([2-9]{0,1}[A-Z]{1,2}[0-9]{0,2}[/]{1}){0,1})[2-9]{0,1}[A-Z]{1,2}[0-9]{1,4}[A-Z]{0,6}([A-Z0-9/]{1,7}){0,1})(\s?(?<grid>[A-R]{2}[0-9]{2})?)?)"};
  auto const& match = callsign_re.match (message_);
  grid = match.captured ("grid");
  tyyp = match.captured ("tyyp");
  if (tyyp == "CQ") tyyp = "";
  else if (tyyp == "908") tyyp = "JA";
  return match.captured ("callsign");
}


bool DecodedText::isHint()
{
  	return string_.mid(47 + padding_,1) == "*" || string_.mid(47 + padding_,1) == "°" || string_.mid(47 + padding_,1) == "^" ;
}

bool DecodedText::isWrong()
{
	return string_.mid(47 + padding_,1) == "?";
}

bool DecodedText::isNonStd1()
{
	return string_.mid(47 + padding_,1) == ",";
}

bool DecodedText::isNonStd2()
{
	return string_.mid(47 + padding_,1) == ".";
}

bool DecodedText::isDebug()
{
	return string_.mid(47 + padding_,1) == "d";
}

bool DecodedText::isDXped()
{
	return string_.mid(47 + padding_,1) == "1";
}

bool DecodedText::isEnd()
{
	return message_.indexOf(" RRR") >= 0 || message_.indexOf(" RR73") >= 0 || message_.indexOf(" 73") >= 0;
}

bool DecodedText::isFin()
{
	return message_.indexOf(" RR73") >= 0 || message_.indexOf(" 73") >= 0;
}

bool DecodedText::isJT65()
{
    return string_.indexOf("#") == column_mode + padding_;
}

bool DecodedText::isJT9()
{
    return string_.indexOf("@") == column_mode + padding_;
}

bool DecodedText::isTX()
{
    int i = string_.indexOf("Tx");
    return (i >= 0 && i < 15); // TODO guessing those numbers. Does Tx ever move?
}

int DecodedText::frequencyOffset()
{
    return string_.mid(column_freq + padding_,4).toInt();
}

int DecodedText::snr()
{
  int i1=string_.indexOf(" ")+1;
  return string_.mid(i1,3).toInt();
}

float DecodedText::dt()
{
  return string_.mid(column_dt + padding_,5).toFloat();
}

/*
2343 -11  0.8 1259 # YV6BFE F6GUU R-08
2343 -19  0.3  718 # VE6WQ SQ2NIJ -14
2343  -7  0.3  815 # KK4DSD W7VP -16
2343 -13  0.1 3627 @ CT1FBK IK5YZT R+02

0605  Tx      1259 # CQ VK3ACF QF22
*/

// find and extract any report. Returns true if this is a standard message
bool DecodedText::report(QString const& myBaseCall, QString const& dxBaseCall, /*mod*/QString& report, QString& type)
{
    bool ret = is_standard_;
    QStringList w=message_.split(" ",SkipEmptyParts);
    if(w.size ()
       && (w[0] == myBaseCall
             || w[0] == "DE"
             || w[0].endsWith ("/" + myBaseCall)
             || w[0].startsWith (myBaseCall + "/"))
             && (dxBaseCall.isEmpty () || (w.size () > 1 
                 && (w[1] == dxBaseCall
                     || w[1].endsWith ("/" + dxBaseCall)
                     || w[1].startsWith (dxBaseCall + "/")))))
    {
        QString tt="";
        if(w.size() > 2) {
          tt=w[2];
          ret = true;
        }
        bool ok;
        auto i1=tt.toInt(&ok);
        if (ok and i1>=-50 and i1<50)
        {
            report = tt;
            type = "";
        }
        else
        {
            if (tt.left(1)=="R")
            {
                i1=tt.mid(1).toInt(&ok);
                if(ok and i1>=-50 and i1<50)
                {
                    report = tt.mid(1);
                    type = tt.left(1);
                }
            }
        }
    }
    return ret;
}

// get the first text word, usually the call
QString DecodedText::call()
{
  auto call = words_re.match (message_).captured ("word1");
  if (!_callRe.match(call).hasMatch() || call.contains("TU73") > 0 || call.contains("73GL") > 0) {
    call = "";
  }
  return call;
/*  auto call = string_;
  call = call.replace (_cqLongerRe, " CQ_\\1 ").mid (column_qsoText + padding_);
  int i = call.indexOf(" ");
  if (_callRe.match(call.left(i)).hasMatch() && string_.mid (0, i).contains("TU73") == 0 && string_.mid (0, i).contains("73GL") == 0) {
      return call.left(i);
  } else {
      return call.left(0);
  }*/
}

// get the second word, most likely the de call and the third word, most likely grid
void DecodedText::deCallAndGrid(/*out*/QString& call, QString& grid)
{
  auto const& match = words_re.match (message_);
  call = match.captured ("word2");
  grid = match.captured ("word3");
  if ("R" == grid) grid = match.captured ("word4");
  if(match.captured("word1")=="CQ" and call.length()>=3 and call.length()<=4
     and !call.contains(QRegExp("[0-9]"))) {
    //Second word has length 3 or 4 and contains no digits
    call = match.captured ("word3");
    grid = match.captured ("word4");
  }
  if (!_callRe.match(call).hasMatch() || call.contains("TU73") > 0 || call.contains("73GL") > 0) {
    call = "";
  }
  if (!_gridRe.match(grid).hasMatch()) {
    grid = "";
  }
/*  auto msg = string_;
  msg = msg.replace (_cqLongerRe, " CQ_\\1 ").mid (column_qsoText + padding_);
  msg = msg.trimmed();
  int i1 = msg.indexOf(" ");
  call = msg.mid(i1+1);
  int i2 = call.indexOf(" ");
  if (_gridRe.match(call.mid(i2+1,4)).hasMatch() && call.mid(i2+1,4) != "RR73") {
    grid = call.mid(i2+1,4);
  }
  call = call.left(i2).replace(">","");
  if (!_callRe.match(call).hasMatch() ||  call.contains("TU73") > 0 || call.contains("73GL") > 0) {
      call = "";
  }*/
}

unsigned DecodedText::timeInSeconds() const
 {
   return 3600 * string_.mid (column_time, 2).toUInt ()
     + 60 * string_.mid (column_time + 2, 2).toUInt()
     + (padding_ ? string_.mid (column_time + 2 + padding_, 2).toUInt () : 0U);
 }

/*
2343 -11  0.8 1259 # YV6BFE F6GUU R-08
2343 -19  0.3  718 # VE6WQ SQ2NIJ -14
2343  -7  0.3  815 # KK4DSD W7VP -16
2343 -13  0.1 3627 @ CT1FBK IK5YZT R+02

0605  Tx      1259 # CQ VK3ACF QF22
*/

QString DecodedText::report()  // returns a string of the SNR field with a leading + or - followed by two digits
{
    int sr = snr();
    if (sr<-50)
        sr = -50;
    else
        if (sr > 49)
            sr = 49;

    QString rpt;
    rpt = QString::asprintf("%d",qAbs(sr));
    if (sr > 9)
        rpt = "+" + rpt;
    else
        if (sr >= 0)
            rpt = "+0" + rpt;
        else
            if (sr >= -9)
                rpt = "-0" + rpt;
            else
                rpt = "-" + rpt;
    return rpt;
}
