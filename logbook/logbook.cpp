// This source code file was last time modified by Arvo ES1JA on January 5th, 2019
// All changes are shown in the patch file coming together with the full JTDX source code.

#include "logbook.h"
#include <QDebug>
#include <QFontMetrics>
#include <QStandardPaths>
#include <QDir>
#include <iostream>
#include "../Radio.hpp"

namespace
{
  auto logFileName = "wsjtx_log.adi";
  auto countryFileName = "cty.dat";
  auto lotwFileName = "lotw-user-activity.csv";
}

void LogBook::init(const QString mycall,const QString mygrid,const QString mydate)
{
  QDir dataPath {QStandardPaths::writableLocation (QStandardPaths::DataLocation)};
  QString countryDataFilename,lotwDataFilename;
  if (dataPath.exists (countryFileName))
    {
      // User override
      countryDataFilename = dataPath.absoluteFilePath (countryFileName);
    }
  else
    {
      countryDataFilename = QString {":/"} + countryFileName;
    }
  if (dataPath.exists (lotwFileName))
    {
      // User override
      lotwDataFilename = dataPath.absoluteFilePath (lotwFileName);
    }
  else
    {
      lotwDataFilename = QString {":/"} + lotwFileName;
    }

  _countries.init(countryDataFilename,lotwDataFilename);
  _countries.load();

  _log.init(dataPath.absoluteFilePath (logFileName), &_countries);
  _log.load(mycall,mygrid,mydate);

  /*
    int QSOcount = _log.getCount();
    int count = _worked.getWorkedCount();
    qDebug() << QSOcount << "QSOs and" << count << "countries worked in file" << logFilename;
  */

  //    QString call = "ok1ct";
  //    QString countryName;
  //    bool callWorkedBefore,countryWorkedBefore;
  //    match(/*in*/call, /*out*/ countryName,callWorkedBefore,countryWorkedBefore);
  //    qDebug() << countryName;

}

void LogBook::matchDXCC(/*in*/const QString call,
                    /*out*/ QString &countryName,
                    bool &WorkedBefore,
                    bool &WorkedBeforeBandMode,
                    /*in*/ double dialFreq,
                    const QString mode)
{
    if (!call.isEmpty ()) {
        QString band = ADIF::bandFromFrequency(dialFreq / 1.e6);
        if (countryName.isEmpty ()) {
            countryName = _countries.find(Radio::effective_prefix(call));
        }
        if (!countryName.isEmpty () && countryName.left(5) != "  ,?,") { //  is there, do checks
            WorkedBefore = _log.matchCountry(countryName, "", "");
            if (!WorkedBefore) {
                WorkedBeforeBandMode = false;
            } else if (!band.isEmpty () || !mode.isEmpty ()) {
                WorkedBeforeBandMode = _log.matchCountry(countryName, band, mode);   
            }
        } else {
            WorkedBefore = true;
            WorkedBeforeBandMode = true;
        }
        
        // qDebug() << "Logbook:" << call << ":" << countryName << "Cty B4:" << countryWorkedBefore << "call B4:" << callWorkedBefore << "Freq B4:" << dialFreq << "Band B4:" << band << "Mode B4:" << mode;
    }
}

void LogBook::matchGrid(/*in*/const QString gridsquare,
                    /*out*/ bool &WorkedBefore,
                    bool &WorkedBeforeBandMode,
                    /*in*/ double dialFreq,
                    const QString mode)
{
    if (!gridsquare.isEmpty ()) {
        QString band = ADIF::bandFromFrequency(dialFreq / 1.e6);
        WorkedBefore = _log.matchGrid(gridsquare.left(4), "", "");
        if (!WorkedBefore) {
            WorkedBeforeBandMode = false;
        } else if (!band.isEmpty () || !mode.isEmpty ()) {
            WorkedBeforeBandMode = _log.matchGrid(gridsquare.left(4), band, mode);
        }
        
        // qDebug() << "Logbook:" << call << ":" << countryName << "Cty B4:" << countryWorkedBefore << "call B4:" << callWorkedBefore << "Freq B4:" << dialFreq << "Band B4:" << band << "Mode B4:" << mode;
    }
}

void LogBook::matchPX(/*in*/const QString call,
                    /*out*/ QString &countryName,
                    bool &WorkedBefore,
                    bool &WorkedBeforeBandMode,
                    /*in*/ double dialFreq,
                    const QString mode)
{
    if (!call.isEmpty ()) {
        QString band = ADIF::bandFromFrequency(dialFreq / 1.e6);
//        printf("logbook matchPX %s|%s|%s|%s\n",call.toStdString().c_str(),countryName.toStdString().c_str(),band.toStdString().c_str(),mode.toStdString().c_str());
        WorkedBefore = _log.matchPx(call, "", "");
        if (!WorkedBefore) {
            WorkedBeforeBandMode = false;
        } else if (!band.isEmpty () || !mode.isEmpty ()) {
            WorkedBeforeBandMode = _log.matchPx(call, band, mode);
        }
        if (countryName.isEmpty ()) {
            countryName = _countries.find(Radio::effective_prefix(call));
//            if (countryName.isEmpty ()) {
//              countryName = "  ,'?,where?"; //error: prefix not found
//            }
        }
        
//        printf("logbook resultPX %s|%s|%s -> %s %s %s\n",call.toStdString().c_str(),band.toStdString().c_str(),mode.toStdString().c_str(),
//        (WorkedBefore) ? "true" : "false",(WorkedBeforeBandMode) ? "true" : "false",countryName.toStdString().c_str());
        // qDebug() << "Logbook:" << call << ":" << countryName << "Cty B4:" << countryWorkedBefore << "call B4:" << callWorkedBefore << "Freq B4:" << dialFreq << "Band B4:" << band << "Mode B4:" << mode;
    }
}

void LogBook::matchCall(/*in*/const QString call,
                    /*out*/ QString &countryName,
                    bool &WorkedBefore,
                    bool &WorkedBeforeBandMode,
                    /*in*/ double dialFreq,
                    const QString mode)
{
    if (!call.isEmpty ()) {
        QString band = ADIF::bandFromFrequency(dialFreq / 1.e6);
        WorkedBefore = _log.match(call, "", "");
        if (!WorkedBefore) {
            WorkedBeforeBandMode = false;
        } else if (!band.isEmpty () || !mode.isEmpty ()) {
            WorkedBeforeBandMode = _log.match(call, band, mode);
        }
        if (countryName.isEmpty ()) {
            countryName = _countries.find(Radio::effective_prefix(call));
//            if (countryName.isEmpty ()) {
//              countryName = "  ,'?,where?"; //error: prefix not found
//            }
        }
        
        // qDebug() << "Logbook:" << call << ":" << countryName << "Cty B4:" << countryWorkedBefore << "call B4:" << callWorkedBefore << "Freq B4:" << dialFreq << "Band B4:" << band << "Mode B4:" << mode;
    }
}

void LogBook::getDXCC(/*in*/const QString call,
                    /*out*/ QString &countryName)
{
    if (!call.isEmpty ()) {
//        printf("logbook getDXCC %s|%s\n",call.toStdString().c_str(),countryName.toStdString().c_str());
        if (countryName.isEmpty ()) {
            countryName = _countries.find(Radio::effective_prefix(call));
//            if (countryName.isEmpty ()) {
//              countryName = "  ,?,where?"; //error: prefix not found
//            }
        }
//        printf("logbook resultgetDXCC %s -> %s\n",call.toStdString().c_str(),countryName.toStdString().c_str());
        // qDebug() << "Logbook:" << call << ":" << countryName << "Cty B4:" << countryWorkedBefore << "call B4:" << callWorkedBefore << "Freq B4:" << dialFreq << "Band B4:" << band << "Mode B4:" << mode;
    }  else countryName = "  ,?,where?";
}

void LogBook::getLOTW(/*in*/const QString call,
                    /*out*/ QString &lotw)
{
    if (!call.isEmpty ()) {
        lotw = _countries.find2(call);
//        printf("logbook resultgetLOTW %s -> %s\n",call.toStdString().c_str(),lotw.toStdString().c_str());
    }
}

void LogBook::addAsWorked(const QString call, const QString band, const QString mode, const QString date, const QString gridsquare, const QString name)
{
    //qDebug() << "adding " << call << " as worked";
    _log.add(call,band,mode,date,gridsquare,name);
}

bool LogBook::getData(const QString call, QString &gridsquare, QString &name)
{
    return _log.getData(call,gridsquare,name);
}

int LogBook::get_qso_count(const QString mod)
{
    return _log.getCount(mod);
}
