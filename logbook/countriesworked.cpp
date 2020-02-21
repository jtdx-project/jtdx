#include "countriesworked.h"

void CountriesWorked::init(const QStringList countryNames)
{
    _data.clear();
    foreach(QString name,countryNames)
      _data.insert(name,false);
}

void CountriesWorked::setAsWorked(const QString countryName)
{
    if (_data.contains(countryName))
      _data.insert(countryName,true);
}     
      
bool CountriesWorked::getHasWorked(const QString countryName)
{  
    if (_data.contains(countryName))
      return _data.value(countryName);  

    return false;
}
    
int CountriesWorked::getWorkedCount()
{
    int count = 0;
	foreach (bool value,_data)
		if (value)
			count += 1;
    return count;
}
    
int CountriesWorked::getSize()
{
    return _data.count();
}
    
    
        
    
    
      
