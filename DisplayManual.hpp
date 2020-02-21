#ifndef DISPLAY_MANUAL_HPP__
#define DISPLAY_MANUAL_HPP__

#include <QObject>

#include "pimpl_h.hpp"

class QNetworkAccessManager;
class QDir;
class QUrl;
class QString;

class DisplayManual
  : public QObject
{
public:
  DisplayManual (QNetworkAccessManager *, QObject * = nullptr);
  ~DisplayManual ();
  void display_html_url (QUrl const& url, QString const& name_we);
  void display_html_file (QDir const& dir, QString const& name_we);

private:
  class impl;
  pimpl<impl> m_;
};

#endif
