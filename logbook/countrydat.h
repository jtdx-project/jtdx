// This source code file was last time modified by Arvo ES1JA on January 5th, 2019
// All changes are shown in the patch file coming together with the full JTDX source code.

/*
 * Reads cty.dat file
 * Establishes a map between prefixes and their country names
 * VK3ACF July 2013
 */


#ifndef __COUNTRYDAT_H
#define __COUNTRYDAT_H

#include <QCoreApplication>
#include <QString>
#include <QStringList>
#include <QHash>
#include <QDate>

class CountryDat
{
public:
  static inline QString tr(const char *sourceText, const char *disambiguation = Q_NULLPTR, int n = -1) \
        { return QCoreApplication::translate("CountryDat", sourceText, disambiguation, n); }
  void init(const QString filename,const QString filename2);
  void load();
  QString find(const QString prefix); // return country name or ""
  QString find2(const QString call); // return lotw date or ""
   
private:
  QString _extractName(const QString line);
  QString _extractMasterPrefix(const QString line);
  QString _extractContinent(const QString line);
  QString _extractCQZ(const QString line);
  QString _extractITUZ(const QString line);
  QString _removeBrackets(QString &line, const QString a, const QString b);
  QStringList _extractPrefix(QString &line, bool &more);

  QString _filename,_filename2;
  QHash<QString, QString> _data;
  QHash<QString, QString> _data2;
  QHash<QString, QString> _name;
};

#endif
