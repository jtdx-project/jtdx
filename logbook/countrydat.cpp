// This source code file was last time modified by Arvo ES1JA on 20190224
// All changes are shown in the patch file coming together with the full JTDX source code.

/*
#Sov Mil Order of Malta:   15:  28:  EU:   41.90:   -12.43:    -1.0:  1A:
    #1A;
#Spratly Islands:          26:  50:  AS:    9.88:  -114.23:    -8.0:  1S:
    #1S,9M0,BV9S;
#Monaco:                   14:  27:  EU:   43.73:    -7.40:    -1.0:  3A:
    #3A;
#Heard Island:             39:  68:  AF:  -53.08:   -73.50:    -5.0:  VK0H:
    #=VK0IR;
#Macquarie Island:         30:  60:  OC:  -54.60:  -158.88:   -10.0:  VK0M:
    #=VK0KEV;
#Cocos-Keeling:            29:  54:  OC:  -12.15:   -96.82:    -6.5:  VK9C:
    #AX9C,AX9Y,VH9C,VH9Y,VI9C,VI9Y,VJ9C,VJ9Y,VK9C,VK9Y,VL9C,VL9Y,VM9C,VM9Y,
    #VN9C,VN9Y,VZ9C,VZ9Y,=VK9AA;
*/


#include "countrydat.h"
#include <QFile>
#include <QTextStream>


void CountryDat::init(const QString filename,const QString filename2)
{
    _filename = filename;
    _data.clear();
    _filename2 = filename2;
    _data2.clear();
}

QString CountryDat::_extractName(const QString line)
{
    int s1 = line.indexOf(':');
    if (s1>=0)
    {
        QString name = line.left(s1);
        return name;
    }
    return "";
}

QString CountryDat::_extractMasterPrefix(const QString line)
{
    int s1 = line.lastIndexOf(' ');
    int s2 = line.lastIndexOf(':');
    if (s1 >= 0 && s1 < s2)
    {
        QString pfx = line.mid(s1, s2-s1);
        return pfx.toUpper();
    }
    return "";
}

QString CountryDat::_extractContinent(const QString line)
{
    int s1;
    s1 = line.indexOf(':');
    if (s1>=0){
        s1 = line.indexOf(':',s1+1);
        if (s1>=0){
            s1 = line.indexOf(':',s1+1);
            if (s1>=0){
                s1 = line.indexOf(':',s1+1);
                if (s1>=0){
                     QString cont = line.mid(s1-2, 2);
                     return cont;
                }
            }
        }
    }
    return "";
}

void CountryDat::_removeBrackets(QString &line, const QString a, const QString b)
{
    int s1 = line.indexOf(a);
    while (s1 >= 0)
    {
      int s2 = line.indexOf(b);
      line = line.left(s1) + line.mid(s2+1,-1);
      s1 = line.indexOf(a);
    }
}    

QStringList CountryDat::_extractPrefix(QString &line, bool &more)
{
    line = line.remove(" \n");
    line = line.replace(" ","");

    _removeBrackets(line,"(",")");
    _removeBrackets(line,"[","]");
    _removeBrackets(line,"<",">");
    _removeBrackets(line,"~","~");

    int s1 = line.indexOf(';');
    more = true;
    if (s1 >= 0)
    {
      line = line.left(s1);
      more = false;
    }

    QStringList r = line.split(',');

    return r;
}


void CountryDat::load()
{
    _data.clear();
  
    QFile inputFile(_filename);
    if (inputFile.open(QIODevice::ReadOnly))
    {
       QTextStream in(&inputFile);
       while ( !in.atEnd() )
       {
          QString line1 = in.readLine();
          if ( !in.atEnd() )
          {
            QString line2 = in.readLine();
              
            QString name;
            name = _extractContinent(line1)+','+_extractMasterPrefix(line1).trimmed()+','+_extractName(line1).trimmed();
            if (!name.isEmpty ())
            {
                bool more = true;
                QStringList prefixs;
                while (more)
                {
                    QStringList p = _extractPrefix(line2,more);
                    prefixs += p;
                    if (more)
                        line2 = in.readLine();
                }

                QString p;
                foreach(p,prefixs)
                {
                    if (!p.isEmpty ())
                        _data.insert(p,name);
                }
            }
          }
       }
    inputFile.close();
    }
    _data2.clear();
  
    QFile inputFile2(_filename2);
    if (inputFile2.open(QIODevice::ReadOnly))
    {
       QDate first = QDate::currentDate().addDays(-365);
       QDate last;
       QTextStream in(&inputFile2);
       while ( !in.atEnd() )
       {
          QString line1 = in.readLine();
          if ( !in.atEnd() )
          {
            auto items = line1.split(',');
            if (items.size() > 1)
            {
                last = QDate::fromString(items[1],"yyyy-MM-dd");
                if (last > first) _data2.insert(items[0],items[1]);
            }
          }
       }
//    printf("%d active lotw users.\n",_data2.size());
    inputFile2.close();
    }
}

// return country name else ""
QString CountryDat::find(const QString prefix)
{
    QString pf = prefix.toUpper();
    while(!pf.isEmpty ())
  	{
        QString country = "";
        if (pf.length() == prefix.length()) country = _data.value("="+pf,country);
        if (!country.isEmpty ())
            return country;
        if (pf == "KG4" && prefix.length() != 5) pf = "AA";
        country = _data.value(pf,country);
        if (!country.isEmpty ())
	    return country;
	
       pf = pf.left(pf.length()-1);
	 }
	 return "";
}	   
// return last lotw date else ""
QString CountryDat::find2(const QString call)
{
     return _data2.value(call,"");
}	   

      

