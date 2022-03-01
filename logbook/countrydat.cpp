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
#include "../Radio.hpp"
#include <QFile>
#include <QTextStream>


void CountryDat::init(const QString filename,const QString filename2)
{
    _filename = filename;
    _data.clear();
    _filename2 = filename2;
    _data2.clear();
    _name.clear();
    _name.insert("where?",tr("where?"));
    _name.insert("Sov Mil Order of Malta",tr("Sov Mil Order of Malta"));
    _name.insert("Spratly Islands",tr("Spratly Is."));
    _name.insert("Monaco",tr("Monaco"));
    _name.insert("Agalega & St. Brandon",tr("Agalega & St. Brandon"));
    _name.insert("Mauritius",tr("Mauritius"));
    _name.insert("Rodriguez Island",tr("Rodriguez Is."));
    _name.insert("Equatorial Guinea",tr("Equatorial Guinea"));
    _name.insert("Annobon Island",tr("Annobon Is."));
    _name.insert("Fiji",tr("Fiji"));
    _name.insert("Conway Reef",tr("Conway Reef"));
    _name.insert("Rotuma Island",tr("Rotuma Is."));
    _name.insert("Kingdom of Eswatini",tr("Kingdom of Eswatini"));
    _name.insert("Tunisia",tr("Tunisia"));
    _name.insert("Vietnam",tr("Vietnam"));
    _name.insert("Guinea",tr("Guinea"));
    _name.insert("Bouvet",tr("Bouvet"));
    _name.insert("Peter 1 Island",tr("Peter 1 Is."));
    _name.insert("Azerbaijan",tr("Azerbaijan"));
    _name.insert("Georgia",tr("Georgia"));
    _name.insert("Montenegro",tr("Montenegro"));
    _name.insert("Sri Lanka",tr("Sri Lanka"));
    _name.insert("ITU HQ",tr("ITU HQ"));
    _name.insert("United Nations HQ",tr("United Nations HQ"));
    _name.insert("Vienna Intl Ctr",tr("Vienna Intl Ctr"));
    _name.insert("Timor - Leste",tr("Timor - Leste"));
    _name.insert("Israel",tr("Israel"));
    _name.insert("Libya",tr("Libya"));
    _name.insert("Cyprus",tr("Cyprus"));
    _name.insert("Tanzania",tr("Tanzania"));
    _name.insert("Nigeria",tr("Nigeria"));
    _name.insert("Madagascar",tr("Madagascar"));
    _name.insert("Mauritania",tr("Mauritania"));
    _name.insert("Niger",tr("Niger"));
    _name.insert("Togo",tr("Togo"));
    _name.insert("Samoa",tr("Samoa"));
    _name.insert("Uganda",tr("Uganda"));
    _name.insert("Kenya",tr("Kenya"));
    _name.insert("Senegal",tr("Senegal"));
    _name.insert("Jamaica",tr("Jamaica"));
    _name.insert("Yemen",tr("Yemen"));
    _name.insert("Lesotho",tr("Lesotho"));
    _name.insert("Malawi",tr("Malawi"));
    _name.insert("Algeria",tr("Algeria"));
    _name.insert("Barbados",tr("Barbados"));
    _name.insert("Maldives",tr("Maldives"));
    _name.insert("Guyana",tr("Guyana"));
    _name.insert("Croatia",tr("Croatia"));
    _name.insert("Ghana",tr("Ghana"));
    _name.insert("Malta",tr("Malta"));
    _name.insert("Zambia",tr("Zambia"));
    _name.insert("Kuwait",tr("Kuwait"));
    _name.insert("Sierra Leone",tr("Sierra Leone"));
    _name.insert("West Malaysia",tr("W. Malaysia"));
    _name.insert("East Malaysia",tr("E. Malaysia"));
    _name.insert("Nepal",tr("Nepal"));
    _name.insert("Dem. Rep. of the Congo",tr("Dem. Rep. of the Congo"));
    _name.insert("Burundi",tr("Burundi"));
    _name.insert("Singapore",tr("Singapore"));
    _name.insert("Rwanda",tr("Rwanda"));
    _name.insert("Trinidad & Tobago",tr("Trinidad & Tobago"));
    _name.insert("Botswana",tr("Botswana"));
    _name.insert("Tonga",tr("Tonga"));
    _name.insert("Oman",tr("Oman"));
    _name.insert("Bhutan",tr("Bhutan"));
    _name.insert("United Arab Emirates",tr("United Arab Emirates"));
    _name.insert("Qatar",tr("Qatar"));
    _name.insert("Bahrain",tr("Bahrain"));
    _name.insert("Pakistan",tr("Pakistan"));
    _name.insert("Scarborough Reef",tr("Scarborough Reef"));
    _name.insert("Taiwan",tr("Taiwan"));
    _name.insert("Pratas Island",tr("Pratas Is."));
    _name.insert("China",tr("China"));
    _name.insert("Nauru",tr("Nauru"));
    _name.insert("Andorra",tr("Andorra"));
    _name.insert("The Gambia",tr("The Gambia"));
    _name.insert("Bahamas",tr("Bahamas"));
    _name.insert("Mozambique",tr("Mozambique"));
    _name.insert("Chile",tr("Chile"));
    _name.insert("San Felix & San Ambrosio",tr("San Felix & San Ambrosio"));
    _name.insert("Easter Island",tr("Easter Is."));
    _name.insert("Juan Fernandez Islands",tr("Juan Fernandez Is."));
    _name.insert("Antarctica",tr("Antarctica"));
    _name.insert("Cuba",tr("Cuba"));
    _name.insert("Morocco",tr("Morocco"));
    _name.insert("Bolivia",tr("Bolivia"));
    _name.insert("Portugal",tr("Portugal"));
    _name.insert("Madeira Islands",tr("Madeira Is."));
    _name.insert("Azores",tr("Azores"));
    _name.insert("Uruguay",tr("Uruguay"));
    _name.insert("Sable Island",tr("Sable Is."));
    _name.insert("St. Paul Island",tr("St. Paul Is."));
    _name.insert("Angola",tr("Angola"));
    _name.insert("Cape Verde",tr("Cape Verde"));
    _name.insert("Comoros",tr("Comoros"));
    _name.insert("Fed. Rep. of Germany",tr("Germany"));
    _name.insert("Philippines",tr("Philippines"));
    _name.insert("Eritrea",tr("Eritrea"));
    _name.insert("Palestine",tr("Palestine"));
    _name.insert("North Cook Islands",tr("N. Cook Is."));
    _name.insert("South Cook Islands",tr("S. Cook Is."));
    _name.insert("Niue",tr("Niue"));
    _name.insert("Bosnia-Herzegovina",tr("Bosnia-Herzegovina"));
    _name.insert("Spain",tr("Spain"));
    _name.insert("Balearic Islands",tr("Balearic Is."));
    _name.insert("Canary Islands",tr("Canary Is."));
    _name.insert("Ceuta & Melilla",tr("Ceuta & Melilla"));
    _name.insert("Ireland",tr("Ireland"));
    _name.insert("Armenia",tr("Armenia"));
    _name.insert("Liberia",tr("Liberia"));
    _name.insert("Iran",tr("Iran"));
    _name.insert("Moldova",tr("Moldova"));
    _name.insert("Estonia",tr("Estonia"));
    _name.insert("Ethiopia",tr("Ethiopia"));
    _name.insert("Belarus",tr("Belarus"));
    _name.insert("Kyrgyzstan",tr("Kyrgyzstan"));
    _name.insert("Tajikistan",tr("Tajikistan"));
    _name.insert("Turkmenistan",tr("Turkmenistan"));
    _name.insert("France",tr("France"));
    _name.insert("Guadeloupe",tr("Guadeloupe"));
    _name.insert("Mayotte",tr("Mayotte"));
    _name.insert("St. Barthelemy",tr("St. Barthelemy"));
    _name.insert("New Caledonia",tr("New Caledonia"));
    _name.insert("Chesterfield Islands",tr("Chesterfield Is."));
    _name.insert("Martinique",tr("Martinique"));
    _name.insert("French Polynesia",tr("Fr. Polynesia"));
    _name.insert("Austral Islands",tr("Austral Is."));
    _name.insert("Clipperton Island",tr("Clipperton Is."));
    _name.insert("Marquesas Islands",tr("Marquesas Is."));
    _name.insert("St. Pierre & Miquelo",tr("St. Pierre & Miquelo"));
    _name.insert("Reunion Island",tr("Reunion Is."));
    _name.insert("St. Martin",tr("St. Martin"));
    _name.insert("Glorioso Islands",tr("Glorioso Is."));
    _name.insert("Juan de Nova, Europa",tr("Juan de Nova, Europa"));
    _name.insert("Tromelin Island",tr("Tromelin Is."));
    _name.insert("Crozet Island",tr("Crozet Is."));
    _name.insert("Kerguelen Islands",tr("Kerguelen Is."));
    _name.insert("Amsterdam & St. Paul Is.",tr("Amsterdam & St. Paul Is."));
    _name.insert("Wallis & Futuna Islands",tr("Wallis & Futuna Is."));
    _name.insert("French Guiana",tr("Fr. Guiana"));
    _name.insert("England",tr("England"));
    _name.insert("Isle of Man",tr("Isle of Man"));
    _name.insert("Northern Ireland",tr("N. Ireland"));
    _name.insert("Jersey",tr("Jersey"));
    _name.insert("Shetland Islands",tr("Shetland Is."));
    _name.insert("Scotland",tr("Scotland"));
    _name.insert("Guernsey",tr("Guernsey"));
    _name.insert("Wales",tr("Wales"));
    _name.insert("Solomon Islands",tr("Solomon Is."));
    _name.insert("Temotu Province",tr("Temotu Province"));
    _name.insert("Hungary",tr("Hungary"));
    _name.insert("Switzerland",tr("Switzerland"));
    _name.insert("Liechtenstein",tr("Liechtenstein"));
    _name.insert("Ecuador",tr("Ecuador"));
    _name.insert("Galapagos Islands",tr("Galapagos Is."));
    _name.insert("Haiti",tr("Haiti"));
    _name.insert("Dominican Republic",tr("Dominican Rep."));
    _name.insert("Colombia",tr("Colombia"));
    _name.insert("San Andres & Providencia",tr("San Andres & Providencia"));
    _name.insert("Malpelo Island",tr("Malpelo Is."));
    _name.insert("Republic of Korea",tr("Rep. of Korea"));
    _name.insert("Panama",tr("Panama"));
    _name.insert("Honduras",tr("Honduras"));
    _name.insert("Thailand",tr("Thailand"));
    _name.insert("Vatican City",tr("Vatican City"));
    _name.insert("Saudi Arabia",tr("Saudi Arabia"));
    _name.insert("Italy",tr("Italy"));
    _name.insert("African Italy",tr("AF Italy"));
    _name.insert("Sardinia",tr("Sardinia"));
    _name.insert("Sicily",tr("Sicily"));
    _name.insert("Djibouti",tr("Djibouti"));
    _name.insert("Grenada",tr("Grenada"));
    _name.insert("Guinea-Bissau",tr("Guinea-Bissau"));
    _name.insert("St. Lucia",tr("St. Lucia"));
    _name.insert("Dominica",tr("Dominica"));
    _name.insert("St. Vincent",tr("St. Vincent"));
    _name.insert("Japan",tr("Japan"));
    _name.insert("Minami Torishima",tr("Minami Torishima"));
    _name.insert("Ogasawara",tr("Ogasawara"));
    _name.insert("Mongolia",tr("Mongolia"));
    _name.insert("Svalbard",tr("Svalbard"));
    _name.insert("Bear Island",tr("Bear Is."));
    _name.insert("Jan Mayen",tr("Jan Mayen"));
    _name.insert("Jordan",tr("Jordan"));
    _name.insert("United States",tr("U.S.A."));
    _name.insert("Guantanamo Bay",tr("Guantanamo Bay"));
    _name.insert("Mariana Islands",tr("Mariana Is."));
    _name.insert("Baker & Howland Islands",tr("Baker & Howland Is."));
    _name.insert("Guam",tr("Guam"));
    _name.insert("Johnston Island",tr("Johnston Is."));
    _name.insert("Midway Island",tr("Midway Is."));
    _name.insert("Palmyra & Jarvis Islands",tr("Palmyra & Jarvis Is."));
    _name.insert("Hawaii",tr("Hawaii"));
    _name.insert("Kure Island",tr("Kure Is."));
    _name.insert("American Samoa",tr("American Samoa"));
    _name.insert("Swains Island",tr("Swains Is."));
    _name.insert("Wake Island",tr("Wake Is."));
    _name.insert("Alaska",tr("Alaska"));
    _name.insert("Navassa Island",tr("Navassa Is."));
    _name.insert("US Virgin Islands",tr("US Virgin Is."));
    _name.insert("Puerto Rico",tr("Puerto Rico"));
    _name.insert("Desecheo Island",tr("Desecheo Is."));
    _name.insert("Norway",tr("Norway"));
    _name.insert("Argentina",tr("Argentina"));
    _name.insert("Luxembourg",tr("Luxembourg"));
    _name.insert("Lithuania",tr("Lithuania"));
    _name.insert("Bulgaria",tr("Bulgaria"));
    _name.insert("Peru",tr("Peru"));
    _name.insert("Lebanon",tr("Lebanon"));
    _name.insert("Austria",tr("Austria"));
    _name.insert("Finland",tr("Finland"));
    _name.insert("Aland Islands",tr("Aland Is."));
    _name.insert("Market Reef",tr("Market Reef"));
    _name.insert("Czech Republic",tr("Czech Rep."));
    _name.insert("Slovak Republic",tr("Slovak Rep."));
    _name.insert("Belgium",tr("Belgium"));
    _name.insert("Greenland",tr("Greenland"));
    _name.insert("Faroe Islands",tr("Faroe Is."));
    _name.insert("Denmark",tr("Denmark"));
    _name.insert("Papua New Guinea",tr("Papua New Guinea"));
    _name.insert("Aruba",tr("Aruba"));
    _name.insert("DPR of Korea",tr("DPR of Korea"));
    _name.insert("Netherlands",tr("Netherlands"));
    _name.insert("Curacao",tr("Curacao"));
    _name.insert("Bonaire",tr("Bonaire"));
    _name.insert("Saba & St. Eustatius",tr("Saba & St. Eustatius"));
    _name.insert("Sint Maarten",tr("Sint Maarten"));
    _name.insert("Brazil",tr("Brazil"));
    _name.insert("Fernando de Noronha",tr("Fernando de Noronha"));
    _name.insert("St. Peter & St. Paul",tr("St. Peter & St. Paul"));
    _name.insert("Trindade & Martim Vaz",tr("Trindade & Martim Vaz"));
    _name.insert("Suriname",tr("Suriname"));
    _name.insert("Franz Josef Land",tr("Franz Josef Land"));
    _name.insert("Western Sahara",tr("Western Sahara"));
    _name.insert("Bangladesh",tr("Bangladesh"));
    _name.insert("Slovenia",tr("Slovenia"));
    _name.insert("Seychelles",tr("Seychelles"));
    _name.insert("Sao Tome & Principe",tr("Sao Tome & Principe"));
    _name.insert("Sweden",tr("Sweden"));
    _name.insert("Poland",tr("Poland"));
    _name.insert("Sudan",tr("Sudan"));
    _name.insert("Egypt",tr("Egypt"));
    _name.insert("Greece",tr("Greece"));
    _name.insert("Mount Athos",tr("Mount Athos"));
    _name.insert("Dodecanese",tr("Dodecanese"));
    _name.insert("Crete",tr("Crete"));
    _name.insert("Tuvalu",tr("Tuvalu"));
    _name.insert("Western Kiribati",tr("W. Kiribati"));
    _name.insert("Central Kiribati",tr("C. Kiribati"));
    _name.insert("Eastern Kiribati",tr("E. Kiribati"));
    _name.insert("Banaba Island",tr("Banaba Is."));
    _name.insert("Somalia",tr("Somalia"));
    _name.insert("San Marino",tr("San Marino"));
    _name.insert("Palau",tr("Palau"));
    _name.insert("Asiatic Turkey",tr("AS Turkey"));
    _name.insert("European Turkey",tr("EU Turkey"));
    _name.insert("Iceland",tr("Iceland"));
    _name.insert("Guatemala",tr("Guatemala"));
    _name.insert("Costa Rica",tr("Costa Rica"));
    _name.insert("Cocos Island",tr("Cocos Is."));
    _name.insert("Cameroon",tr("Cameroon"));
    _name.insert("Corsica",tr("Corsica"));
    _name.insert("Central African Republic",tr("C. African Rep."));
    _name.insert("Republic of the Congo",tr("Rep. of the Congo"));
    _name.insert("Gabon",tr("Gabon"));
    _name.insert("Chad",tr("Chad"));
    _name.insert("Cote d'Ivoire",tr("Cote d'Ivoire"));
    _name.insert("Benin",tr("Benin"));
    _name.insert("Mali",tr("Mali"));
    _name.insert("European Russia",tr("EU Russia"));
    _name.insert("Kaliningrad",tr("Kaliningrad"));
    _name.insert("Asiatic Russia",tr("AS Russia"));
    _name.insert("Uzbekistan",tr("Uzbekistan"));
    _name.insert("Kazakhstan",tr("Kazakhstan"));
    _name.insert("Ukraine",tr("Ukraine"));
    _name.insert("Antigua & Barbuda",tr("Antigua & Barbuda"));
    _name.insert("Belize",tr("Belize"));
    _name.insert("St. Kitts & Nevis",tr("St. Kitts & Nevis"));
    _name.insert("Namibia",tr("Namibia"));
    _name.insert("Micronesia",tr("Micronesia"));
    _name.insert("Marshall Islands",tr("Marshall Is."));
    _name.insert("Brunei Darussalam",tr("Brunei Darussalam"));
    _name.insert("Canada",tr("Canada"));
    _name.insert("Australia",tr("Australia"));
    _name.insert("Heard Island",tr("Heard Is."));
    _name.insert("Macquarie Island",tr("Macquarie Is."));
    _name.insert("Cocos (Keeling) Islands",tr("Cocos (Keeling) Is."));
    _name.insert("Lord Howe Island",tr("Lord Howe Is."));
    _name.insert("Mellish Reef",tr("Mellish Reef"));
    _name.insert("Norfolk Island",tr("Norfolk Is."));
    _name.insert("Willis Island",tr("Willis Is."));
    _name.insert("Christmas Island",tr("Christmas Is."));
    _name.insert("Anguilla",tr("Anguilla"));
    _name.insert("Montserrat",tr("Montserrat"));
    _name.insert("British Virgin Islands",tr("British Virgin Is."));
    _name.insert("Turks & Caicos Islands",tr("Turks & Caicos Is."));
    _name.insert("Pitcairn Island",tr("Pitcairn Is."));
    _name.insert("Ducie Island",tr("Ducie Is."));
    _name.insert("Falkland Islands",tr("Falkland Is."));
    _name.insert("South Georgia Island",tr("S. Georgia Is."));
    _name.insert("South Shetland Islands",tr("S. Shetland Is."));
    _name.insert("South Orkney Islands",tr("S. Orkney Is."));
    _name.insert("South Sandwich Islands",tr("S. Sandwich Is."));
    _name.insert("Bermuda",tr("Bermuda"));
    _name.insert("Chagos Islands",tr("Chagos Is."));
    _name.insert("Hong Kong",tr("Hong Kong"));
    _name.insert("India",tr("India"));
    _name.insert("Andaman & Nicobar Is.",tr("Andaman & Nicobar Is."));
    _name.insert("Lakshadweep Islands",tr("Lakshadweep Is."));
    _name.insert("Mexico",tr("Mexico"));
    _name.insert("Revillagigedo",tr("Revillagigedo"));
    _name.insert("Burkina Faso",tr("Burkina Faso"));
    _name.insert("Cambodia",tr("Cambodia"));
    _name.insert("Laos",tr("Laos"));
    _name.insert("Macao",tr("Macao"));
    _name.insert("Myanmar",tr("Myanmar"));
    _name.insert("Afghanistan",tr("Afghanistan"));
    _name.insert("Indonesia",tr("Indonesia"));
    _name.insert("Iraq",tr("Iraq"));
    _name.insert("Vanuatu",tr("Vanuatu"));
    _name.insert("Syria",tr("Syria"));
    _name.insert("Latvia",tr("Latvia"));
    _name.insert("Nicaragua",tr("Nicaragua"));
    _name.insert("Romania",tr("Romania"));
    _name.insert("El Salvador",tr("El Salvador"));
    _name.insert("Serbia",tr("Serbia"));
    _name.insert("Venezuela",tr("Venezuela"));
    _name.insert("Aves Island",tr("Aves Is."));
    _name.insert("Zimbabwe",tr("Zimbabwe"));
    _name.insert("North Macedonia",tr("N. Macedonia"));
    _name.insert("Republic of Kosovo",tr("Rep. of Kosovo"));
    _name.insert("Republic of South Sudan",tr("Rep. of S. Sudan"));
    _name.insert("Albania",tr("Albania"));
    _name.insert("Gibraltar",tr("Gibraltar"));
    _name.insert("UK Base Areas on Cyprus",tr("UK Base Areas on Cyprus"));
    _name.insert("St. Helena",tr("St. Helena"));
    _name.insert("Ascension Island",tr("Ascension Is."));
    _name.insert("Tristan da Cunha & Gough",tr("Tristan da Cunha & Gough"));
    _name.insert("Cayman Islands",tr("Cayman Is."));
    _name.insert("Tokelau Islands",tr("Tokelau Is."));
    _name.insert("New Zealand",tr("New Zealand"));
    _name.insert("Chatham Islands",tr("Chatham Is."));
    _name.insert("Kermadec Islands",tr("Kermadec Is."));
    _name.insert("N.Z. Subantarctic Is.",tr("N.Z. Subantarctic Is."));
    _name.insert("Paraguay",tr("Paraguay"));
    _name.insert("South Africa",tr("S. Africa"));
    _name.insert("Pr. Edward & Marion Is.",tr("Pr. Edward & Marion Is."));
}

QString CountryDat::_extractName(const QString line)
{
    int s1 = line.indexOf(':');
    if (s1>=0)
    {
        QString name = line.left(s1);
        return _name.value(name,name);
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

QString CountryDat::_extractITUZ(const QString line)
{
    int s1;
    s1 = line.indexOf(':');
    if (s1>=0){
        s1 = line.indexOf(':',s1+1);
        if (s1>=0){
            s1 = line.indexOf(':',s1+1);
            if (s1>=0){
                 QString cont = line.mid(s1-2, 2);
                 if (cont.size() == 1) cont = " " + cont;
                 return cont;
            }
        }
    }
    return "";
}

QString CountryDat::_extractCQZ(const QString line)
{
    int s1;
    s1 = line.indexOf(':');
    if (s1>=0){
        s1 = line.indexOf(':',s1+1);
        if (s1>=0){
             QString cont = line.mid(s1-2, 2);
             if (cont.size() == 1) cont = " " + cont;
             return cont;
        }
    }
    return "";
}

QString CountryDat::_removeBrackets(QString &line, const QString a, const QString b)
{
    QString res = "";
    int s1 = line.indexOf(a);
    while (s1 >= 0)
    {
      int s2 = line.indexOf(b);
      res += line.mid(s1+1,s2-s1-1);
      line = line.left(s1) + line.mid(s2+1,-1);
      s1 = line.indexOf(a);
    }
    return res;
}    

QStringList CountryDat::_extractPrefix(QString &line, bool &more)
{
    QString a;
    line = line.remove(" \n");
    line = line.replace(" ","");

    a = _removeBrackets(line,"<",">");
    a = _removeBrackets(line,"~","~");

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
              
            QString name,cqz,ituz,continent;
            cqz = _extractCQZ(line1);
            ituz = _extractITUZ(line1);
            continent = _extractContinent(line1);
            name = _extractMasterPrefix(line1).trimmed()+','+_extractName(line1).trimmed();
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

                QString p,_cqz,_ituz,_continent;
                foreach(p,prefixs)
                {
                    if (!p.isEmpty ()) {
                        _cqz = _removeBrackets(p,"(",")");
                        if (_cqz.isEmpty()) _cqz = cqz;
                        if (_cqz.size() == 1) _cqz = "0" + _cqz;
                        _ituz = _removeBrackets(p,"[","]");
                        if (_ituz.isEmpty()) _ituz = ituz;
                        if (_ituz.size() == 1) _ituz = "0" + _ituz;
                        _continent = _removeBrackets(p,"{","}");
                        if (_continent.isEmpty()) _continent = continent;                       
                        _data.insert(p,_continent+','+name+','+_cqz+','+_ituz);
                    }
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
            QStringList items = line1.split(',');
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
        if (pf.length() == prefix.length()) {
             country = _data.value("="+pf,country);
            if (!country.isEmpty ())
                return country;
            else pf = Radio::effective_prefix(prefix);
        }
        if (pf == "KG4" && prefix.length() != 5) pf = "AA";
        country = _data.value(pf,country);
        if (!country.isEmpty ())
	    return country;
	
       pf = pf.left(pf.length()-1);
	 }
	 return "  ,?,"+_name.value("where?","where?")+",  ,  ";
}	   
// return last lotw date else ""
QString CountryDat::find2(const QString call)
{
     return _data2.value(call,"");
}	   

      

