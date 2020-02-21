#include "RemoteFile.hpp"

#include <utility>

#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QDir>
#include <QByteArray>

#include "moc_RemoteFile.cpp"

RemoteFile::RemoteFile (ListenerInterface * listener, QNetworkAccessManager * network_manager
                        , QString const& local_file_path, QObject * parent)
  : QObject {parent}
  , listener_ {listener}
  , network_manager_ {network_manager}
  , local_file_ {local_file_path}
  , reply_ {nullptr}
  , is_valid_ {false}
  , redirect_count_ {0}
  , file_ {local_file_path}
{
  local_file_.setCaching (false);
}

void RemoteFile::local_file_path (QString const& name)
{
  QFileInfo new_file {name};
  new_file.setCaching (false);
  if (new_file != local_file_)
    {
      if (local_file_.exists ())
        {
          QFile file {local_file_.absoluteFilePath ()};
          if (!file.rename (new_file.absoluteFilePath ()))
            {
              listener_->error (tr ("File System Error")
                                , tr ("Cannot rename file:\n\"%1\"\nto: \"%2\"\nError(%3): %4")
                                .arg (file.fileName ())
                                .arg (new_file.absoluteFilePath ())
                                .arg (file.error ())
                                .arg (file.errorString ()));
            }
        }
      std::swap (local_file_, new_file);
    }
}

bool RemoteFile::local () const
{
  auto is_local = (reply_ && !reply_->isFinished ()) || local_file_.exists ();
  if (is_local)
    {
      auto size = local_file_.size ();
      listener_->download_progress (size, size);
      listener_->download_finished (true);
    }
  else
    {
      listener_->download_progress (-1, 0);
    }
  return is_local;
}

bool RemoteFile::sync (QUrl const& url, bool local, bool force)
{
  if (local)
    {
      if (!reply_ || reply_->isFinished ()) // not active download
        {
          if (force || !local_file_.exists () || url != url_)
            {
              url_ = url;
              redirect_count_ = 0;
              Q_ASSERT (!is_valid_);
              download (url_);
            }
        }
      else
        {
          return false;
        }
    }
  else
    {
      if (reply_ && reply_->isRunning ())
        {
          reply_->abort ();
        }
      if (local_file_.exists ())
        {
          auto path = local_file_.absoluteDir ();
          if (path.remove (local_file_.fileName ()))
            {
              listener_->download_progress (-1, 0);
            }
          else
            {
              listener_->error (tr ("File System Error")
                                , tr ("Cannot delete file:\n\"%1\"")
                                .arg (local_file_.absoluteFilePath ()));
              return false;
            }
          path.rmpath (".");
        }
    }
  return true;
}

void RemoteFile::download (QUrl const& url)
{
  QNetworkRequest request {url};
  request.setRawHeader ("User-Agent", "WSJT Sample Downloader");
  request.setOriginatingObject (this);

  // this blocks for a second or two the first time it is used on
  // Windows - annoying
  if (!is_valid_)
    {
      reply_ = network_manager_->head (request);
    }
  else
    {
      reply_ = network_manager_->get (request);
    }

  connect (reply_, &QNetworkReply::finished, this, &RemoteFile::reply_finished);
  connect (reply_, &QNetworkReply::readyRead, this, &RemoteFile::store);
  connect (reply_, &QNetworkReply::downloadProgress
           , [this] (qint64 bytes_received, qint64 total_bytes) {
             // report progress of wanted file
             if (is_valid_)
               {
                 listener_->download_progress (bytes_received, total_bytes);
               }
           });
  connect (reply_, &QNetworkReply::sslErrors, [this] (QList<QSslError> const& errors) {
      QString message;
      for (auto const& error: errors)
        {
          message += '\n' + reply_->request ().url ().toDisplayString () + ": "
            + error.errorString ();
        }
      listener_->error ("Network SSL Errors", message);
    });
}

void RemoteFile::abort ()
{
  if (reply_ && reply_->isRunning ())
    {
      reply_->abort ();
    }
}

void RemoteFile::reply_finished ()
{
  auto saved_reply = reply_;
  auto redirect_url = reply_->attribute (QNetworkRequest::RedirectionTargetAttribute).toUrl ();
  if (!redirect_url.isEmpty ())
    {
      if (listener_->redirect_request (redirect_url))
        {
          if (++redirect_count_ < 10) // maintain sanity
            {
              // follow redirect
              download (reply_->url ().resolved (redirect_url));
            }
          else
            {
              listener_->download_finished (false);
              listener_->error (tr ("Network Error")
                                , tr ("Too many redirects: %1")
                                .arg (redirect_url.toDisplayString ()));
              is_valid_ = false; // reset
            }
        }
      else
        {
          listener_->download_finished (false);
          listener_->error (tr ("Network Error")
                            , tr ("Redirect not followed: %1")
                            .arg (redirect_url.toDisplayString ()));
          is_valid_ = false;    // reset
        }
    }
  else if (reply_->error () != QNetworkReply::NoError)
    {
      file_.cancelWriting ();
      file_.commit ();
      listener_->download_finished (false);
      is_valid_ = false;        // reset
      // report errors that are not due to abort
      if (QNetworkReply::OperationCanceledError != reply_->error ())
        {
          listener_->error (tr ("Network Error"), reply_->errorString ());
        }
    }
  else
    {
      auto path = QFileInfo {file_.fileName ()}.absoluteDir ();
      if (is_valid_ && !file_.commit ())
        {
          listener_->error (tr ("File System Error")
                            , tr ("Cannot commit changes to:\n\"%1\"")
                            .arg (file_.fileName ()));
          path.rmpath (".");    // tidy empty directories
          listener_->download_finished (false);
          is_valid_ = false;    // reset
        }
      else
        {
          if (!is_valid_)
            {
              // now get the body content
              is_valid_ = true;
              download (reply_->url () .resolved (redirect_url));
            }
          else
            {
              listener_->download_finished (true);
              is_valid_ = false; // reset
            }
        }
    }
  if (reply_->isFinished ()) reply_ = nullptr;
  disconnect (saved_reply);
  saved_reply->deleteLater ();  // finished with QNetworkReply
}

void RemoteFile::store ()
{
  if (is_valid_)
    {
      if (!file_.isOpen ())
        {
          // create temporary file in the final location
          auto path = QFileInfo {file_.fileName ()}.absoluteDir ();
          if (path.mkpath ("."))
            {
              if (!file_.open (QSaveFile::WriteOnly))
                {
                  abort ();
                  listener_->error (tr ("File System Error")
                                    , tr ("Cannot open file:\n\"%1\"\nError(%2): %3")
                                    .arg (path.path ())
                                    .arg (file_.error ())
                                    .arg (file_.errorString ()));
                }
            }
          else
            {
              abort ();
              listener_->error (tr ("File System Error")
                                , tr ("Cannot make path:\n\"%1\"")
                                .arg (path.path ()));
            }
        }
      if (file_.write (reply_->read (reply_->bytesAvailable ())) < 0)
        {
          abort ();
          listener_->error (tr ("File System Error")
                            , tr ("Cannot write to file:\n\"%1\"\nError(%2): %3")
                            .arg (file_.fileName ())
                            .arg (file_.error ())
                            .arg (file_.errorString ()));
        }
    }
}
