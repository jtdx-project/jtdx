#ifndef FILE_NODE_HPP__
#define FILE_NODE_HPP__

#include <QTreeWidgetItem>

#include "RemoteFile.hpp"

class QNetworkAccessManager;
class QString;
class QUrl;

//
// A holder for a RemoteFile object linked to a QTreeWidget row.
//
// It  renders  the file  name  in  first  column and  holds  download
// progress data in  the second column. The progress  information is a
// 64 bit integer number of bytes in the DisplayRole and a total bytes
// expected in the UserRole. The first column also renders a check box
// that downloads  the file  when checked  and removes  the downloaded
// file  when unchecked.  The URL  and  local absolute  file path  are
// stored in the UserData and UserData+1 roles of the first column.
//
class FileNode final
  : public QTreeWidgetItem
  , protected RemoteFile::ListenerInterface
{
public:
  explicit FileNode (QTreeWidgetItem * parent
                     , QNetworkAccessManager * network_manager
                     , QString const& local_path
                     , QUrl const& url);

  bool local () const {return remote_file_.local ();}
  bool sync (bool local);
  void abort ();

  static int const Type {UserType + 1};

  //
  // Clients may  use this RAII  class to  block nested calls  to sync
  // which may be troublesome, e.g. when UI updates cause recursion.
  //
  struct sync_blocker
  {
    sync_blocker (FileNode * node) : node_ {node} {node_->block_sync_ = true;}
    sync_blocker (sync_blocker const&) = delete;
    sync_blocker& operator = (sync_blocker const&) = delete;
    ~sync_blocker () {node_->block_sync_ = false;}
  private:
    FileNode * node_;
  };

protected:
  void error (QString const& title, QString const& message) override;
  bool redirect_request (QUrl const&) override {return true;} // allow
  void download_progress (qint64 bytes_received, qint64 total_bytes) override;
  void download_finished (bool success) override;

private:
  RemoteFile remote_file_;      // active download
  bool block_sync_;

  friend struct sync_blocker;
};

#endif
