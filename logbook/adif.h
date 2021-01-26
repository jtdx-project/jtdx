// This source code file was last time modified by Arvo ES1JA on January 29th, 2017
// All changes are shown in the patch file coming together with the full JTDX source code.

/*
 * Reads an ADIF log file into memory
 * Searches log for call, band and mode
 * VK3ACF July 2013
 */


#ifndef __ADIF_H
#define __ADIF_H

#if defined (QT5)
#include <QList>
#include <QString>
#include <QMultiHash>
#else
#include <QtGui>
#endif
#include "countrydat.h"

class QDateTime;

class ADIF
{
    public:
        void init(QString filename);
        void init(QString filename, CountryDat* countries);
        void load(const QString mycall,const QString mygrid,const QString mydate);
        void add(const QString call, const QString band, const QString mode, const QString date, const QString gridsquare, const QString name);
        bool match(const QString call, const QString band="", const QString mode="");
        bool matchPx(const QString call, const QString band="", const QString mode="");
        bool getData(const QString call, QString &gridsquare, QString &name);
        bool matchCqz(const QString Cqz, const QString band="", const QString mode="");
        bool matchItuz(const QString Ituz, const QString band="", const QString mode="");
        bool matchCountry(const QString countryName, const QString band="", const QString mode="");
        bool matchGrid(const QString gridsquare, const QString band="", const QString mode="");
        QList<QString> getCallList();
        int getCount(const QString mode="");

        // open ADIF file and append the QSO details. Return true on success
        bool addQSOToFile(const QString hisCall, const QString hisGrid, const QString mode, const QString rptSent, const QString rptRcvd, QDateTime const& dateTimeOn, QDateTime const& dateTimeOff, const QString band,
                                const QString comments, const QString name, const QString strDialFreq, const QString m_myCall, const QString m_myGrid, const QString m_txPower,const bool send_to_eqsl);

        static QString bandFromFrequency(double dialFreq);

    private:
        struct QSO
        {
            QString call,band,mode,date,gridsquare,name;
        };

        QHash<QString, int> _counts;
        QMultiHash<QString, QSO> _data;
        QHash<QString, int> _cqzWorked;
        QHash<QString, int> _ituzWorked;
        QHash<QString, int> _countriesWorked;
        QHash<QString, int> _gridsWorked;
        QHash<QString, int> _pxsWorked;
        QHash<QString, int> _callsWorked;
        QHash<QString, int> _cqzbandWorked;
        QHash<QString, int> _ituzbandWorked;
        QHash<QString, int> _countriesbandWorked;
        QHash<QString, int> _gridsbandWorked;
        QHash<QString, int> _pxsbandWorked;
        QHash<QString, int> _callsbandWorked;
        QHash<QString, int> _cqzmodeWorked;
        QHash<QString, int> _ituzmodeWorked;
        QHash<QString, int> _countriesmodeWorked;
        QHash<QString, int> _gridsmodeWorked;
        QHash<QString, int> _pxsmodeWorked;
        QHash<QString, int> _callsmodeWorked;
        QHash<QString, int> _cqzbandmodeWorked;
        QHash<QString, int> _ituzbandmodeWorked;
        QHash<QString, int> _countriesbandmodeWorked;
        QHash<QString, int> _gridsbandmodeWorked;
        QHash<QString, int> _pxsbandmodeWorked;
        QHash<QString, int> _callsbandmodeWorked;
        QString _filename;
        CountryDat _countries;

        QString _extractField(const QString line, const QString fieldName);
};


#endif

