#ifndef SAMPLE_DOWNLOADER_DIRECTORY_HPP__
#define SAMPLE_DOWNLOADER_DIRECTORY_HPP__

#include <QObject>
#include <QString>
#include <QTreeWidget>
#include <QIcon>
#include <QSize>
#include <QDir>
#include <QUrl>

#include "DirectoryDelegate.hpp"
#include "RemoteFile.hpp"

class Configuration;
class QNetworkAccessManager;
class QTreeWidgetItem;
class QNetworkReply;
class QAuthenticator;
class QJsonArray;

class Directory final
  : public QTreeWidget
  , protected RemoteFile::ListenerInterface
{
  Q_OBJECT

public:
  explicit Directory (Configuration const * configuration
                      , QNetworkAccessManager * network_manager
                      , QWidget * parent = nullptr);

  QSize sizeHint () const override {return {400, 500};}

  bool url_root (QUrl);
  bool refresh ();
  void abort ();
  void update (QTreeWidgetItem * item);

protected:
  void error (QString const& title, QString const& message) override;
  bool redirect_request (QUrl const&) override {return true;} // allow
  void download_finished (bool success) override;

private:
  Q_SLOT void authentication (QNetworkReply *, QAuthenticator *);
  void parse_entries (QJsonArray const& entries, QDir const& dir, QTreeWidgetItem * parent);

  Configuration const * configuration_;
  QNetworkAccessManager * network_manager_;
  QDir root_dir_;
  QUrl url_root_;
  RemoteFile contents_;
  DirectoryDelegate item_delegate_;
  QIcon dir_icon_;
  QIcon file_icon_;
};

#endif
