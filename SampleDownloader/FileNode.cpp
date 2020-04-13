#include "FileNode.hpp"

#include <QVariant>
#include <QUrl>
#include <QDir>
#include <QFileInfo>

#include "JTDXMessageBox.hpp"
#include "Directory.hpp"

FileNode::FileNode (QTreeWidgetItem * parent
                    , QNetworkAccessManager * network_manager
                    , QString const& local_file_path
                    , QUrl const& url)
  : QTreeWidgetItem {parent, Type}
  , remote_file_ {this, network_manager, local_file_path}
  , block_sync_ {false}
{
  sync_blocker b {this};
  setFlags (flags () | Qt::ItemIsUserCheckable);
  setText (0, QFileInfo {local_file_path}.fileName ()); // display
  setData (0, Qt::UserRole, url);
  setData (0, Qt::UserRole + 1, local_file_path); // local absolute path
  setCheckState (0, Qt::Unchecked);
}

void FileNode::error (QString const& title, QString const& message)
{
  JTDXMessageBox::warning_message (treeWidget (), "", title, message);
}

bool FileNode::sync (bool local)
{
  if (block_sync_)
    {
      return true;
    }
  return remote_file_.sync (data (0, Qt::UserRole).toUrl (), local);
}

void FileNode::download_progress (qint64 bytes_received, qint64 total_bytes)
{
  sync_blocker b {this};
  setData (1, Qt::UserRole, total_bytes);
  if (bytes_received < 0)
    {
      setData (1, Qt::DisplayRole, 0ll);
      setCheckState (0, Qt::Unchecked);
    }
  else
    {
      setData (1, Qt::DisplayRole, bytes_received);
    }
  static_cast<Directory *> (treeWidget ())->update (parent ());
}

void FileNode::download_finished (bool success)
{
  sync_blocker b {this};
  if (!success)
    {
      setData (1, Qt::UserRole, 0ll);
      setData (1, Qt::DisplayRole, 0ll);
    }
  setCheckState (0, success ? Qt::Checked : Qt::Unchecked);
  static_cast<Directory *> (treeWidget ())->update (parent ());
}

void FileNode::abort ()
{
  sync_blocker b {this};
  remote_file_.abort ();
}
