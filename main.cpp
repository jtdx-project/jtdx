// last time modified by Arvo ES1JA on 20200127

#include <iostream>
#include <exception>
#include <stdexcept>
#include <string>

#include <locale.h>

#include <QApplication>
#include <QTranslator>
#include <QNetworkAccessManager>
#include <QRegularExpression>
#include <QObject>
#include <QSettings>
#include <QLibraryInfo>
#include <QSysInfo>
#include <QDir>
#include <QStandardPaths>
#include <QStringList>
#include <QLockFile>

#if QT_VERSION >= 0x050200
#include <QCommandLineParser>
#include <QCommandLineOption>
#endif
#if QT_VERSION >= QT_VERSION_CHECK(5, 15, 0)
#include <QRandomGenerator>
#endif

#include "JTDXMessageBox.hpp"
#include "revision_utils.hpp"
#include "MetaDataRegistry.hpp"
#include "SettingsGroup.hpp"
#include "TraceFile.hpp"
#include "mainwindow.h"
#include "commons.h"
#include "lib/init_random_seed.h"

namespace
{
  struct RNGSetup
  {
    RNGSetup ()
    {
      // one time seed of pseudo RNGs from current time
#if QT_VERSION >= QT_VERSION_CHECK(5, 15, 0)
      QRandomGenerator();
#else
      auto seed = QDateTime::currentMSecsSinceEpoch ();
      qsrand (seed);            // this is good for rand() as well
#endif
    }
  } seeding;

  // We  can't use  the GUI  after QApplication::exit()  is called  so
  // uncaught exceptions can  get lost on Windows  systems where there
  // is    no    console    terminal,     so    here    we    override
  // QApplication::notify() and  wrap the base  class call with  a try
  // block to catch and display exceptions in a message box.
  class ExceptionCatchingApplication final
    : public QApplication
  {
  public:
    explicit ExceptionCatchingApplication (int& argc, char * * argv)
      : QApplication {argc, argv}
    {
    }
    bool notify (QObject * receiver, QEvent * e) override
    {
      try
        {
          return QApplication::notify (receiver, e);
        }
      catch (std::exception const& e)
        {
          JTDXMessageBox::critical_message (nullptr, "", translate ("main", "Fatal error"), e.what ());
          throw;
        }
      catch (...)
        {
          JTDXMessageBox::critical_message (nullptr, "", translate ("main", "Unexpected fatal error"));
          throw;
        }
    }
  };
}

int main(int argc, char *argv[])
{
#if defined(Q_OS_WIN)
  if (AttachConsole(ATTACH_PARENT_PROCESS)) {
    freopen("CONOUT$", "w", stdout);
    freopen("CONOUT$", "w", stderr);
  }
#endif

  bool has_style = true;
  int result = 0;
  for (auto i = 0; i < argc; i++) {if (std::string(argv[i]).find("-style") != std::string::npos && std::string(argv[i]).find("-stylesheet") == std::string::npos) has_style = false;}

  init_random_seed ();

  register_types ();            // make the Qt magic happen

  // Multiple instances:
  QSharedMemory mem_jtdxjt9;

  ExceptionCatchingApplication a(argc, argv);
  if (has_style) a.setStyle("Fusion");
  try
    {

      setlocale (LC_NUMERIC, "C"); // ensure number forms are in
                                   // consistent format, do this after
                                   // instantiating QApplication so
                                   // that GUI has correct l18n

      // Override programs executable basename as application name.
      a.setApplicationName ("JTDX");
      a.setApplicationVersion (version ());
  if (version().replace("_32A","").indexOf("_") > 1) {
    #include <QDate>
    auto expire_date = QLocale(QLocale::English).toDate(QString(__DATE__).replace("  "," "),"MMM d yyyy").addMonths(3);
    if (QDate().currentDate() > expire_date) {
      JTDXMessageBox::critical_message (nullptr, a.applicationName(), "Release candidate expired on " + expire_date.toString("dd.MM.yyyy"));
          return -1;
    } else if (QDate().currentDate().addDays(5) > expire_date) {
      JTDXMessageBox::information_message (nullptr, a.applicationName(), "Release candidate will expire on " + expire_date.toString("dd.MM.yyyy"));
    }
  }
      bool multiple {false};

#if QT_VERSION >= 0x050200
      QCommandLineParser parser;
      parser.setApplicationDescription ("\nFT8,JT65A,JT9 & T10 Weak Signal Communications Program.");
      auto help_option = parser.addHelpOption ();
      auto version_option = parser.addVersionOption ();

      // support for multiple instances running from a single installation
      QCommandLineOption style_option (QString {"style"}
                                     , a.translate ("main", "<style> can be Fusion (default) or Windows")
                                     , a.translate ("main" , "style"));
      parser.addOption (style_option);

      QCommandLineOption rig_option (QStringList {} << "r" << "rig-name"
                                     , a.translate ("main", "Where <rig-name> is for multi-instance support.")
                                     , a.translate ("main", "rig-name"));
      parser.addOption (rig_option);

      QCommandLineOption test_option (QStringList {} << "test-mode"
                                      , a.translate ("main", "Writable files in test location.  Use with caution, for testing only."));
      parser.addOption (test_option);

      if (!parser.parse (a.arguments ()))
        {
          JTDXMessageBox::critical_message (nullptr, a.applicationName (), parser.errorText ());
          return -1;
        }
      else
        {
          if (parser.isSet (help_option))
            {
              JTDXMessageBox::information_message (nullptr, a.applicationName (), parser.helpText ());
              return 0;
            }
          else if (parser.isSet (version_option))
            {
              JTDXMessageBox::information_message (nullptr, a.applicationName (), a.applicationVersion ());
              return 0;
            }
        }

      QStandardPaths::setTestModeEnabled (parser.isSet (test_option));

      // support for multiple instances running from a single installation
      if (parser.isSet (rig_option) || parser.isSet (test_option))
        {
          auto temp_name = parser.value (rig_option);
          if (!temp_name.isEmpty ())
            {
              if (temp_name.contains (QRegularExpression {R"([\\/,])"}))
                {
                  std::cerr << QObject::tr ("Invalid rig name - \\ & / not allowed").toLocal8Bit ().data () << std::endl;
                  parser.showHelp (-1);
                }
                
              a.setApplicationName (a.applicationName () + " - " + temp_name);
            }

          if (parser.isSet (test_option))
            {
              a.setApplicationName (a.applicationName () + " - test");
            }

          multiple = true;
        }

      // disallow multiple instances with same instance key
      QLockFile instance_lock {QDir {QStandardPaths::writableLocation (QStandardPaths::TempLocation)}.absoluteFilePath (a.applicationName () + ".lock")};
      instance_lock.setStaleLockTime (0);
      auto ok = false;
      while (!(ok = instance_lock.tryLock ()))
        {
          if (QLockFile::LockFailedError == instance_lock.error ())
            {
              auto button = JTDXMessageBox::query_message (nullptr
                                                   , QApplication::applicationName ()
                                                   , QObject::tr ("Another instance may be running, try to remove stale lock file?")
                                                   , "" ,""
                                                   , JTDXMessageBox::Yes | JTDXMessageBox::Retry | JTDXMessageBox::No
                                                   , JTDXMessageBox::Yes);
              switch (button)
                {
                case JTDXMessageBox::Yes:
                  instance_lock.removeStaleLockFile ();
                  break;

                case JTDXMessageBox::Retry:
                  break;

                default:
                  throw std::runtime_error {"Multiple instances must have unique rig names"};
                }
            }
        }
#endif

      auto config_directory = QStandardPaths::writableLocation (QStandardPaths::ConfigLocation);
      QDir config_path {config_directory}; // will be "." if config_directory is empty
      if (!config_path.mkpath ("."))
        {
          throw std::runtime_error {"Cannot find a usable configuration path \"" + config_path.path ().toStdString () + '"'};
        }

      auto settings_file = config_path.absoluteFilePath (a.applicationName () + ".ini");
      QSettings settings(settings_file, QSettings::IniFormat);
      if (!settings.isWritable ())
        {
          throw std::runtime_error {QString {"Cannot access \"%1\" for writing"}.arg (settings_file).toStdString ()};
        }

#if WSJT_QDEBUG_TO_FILE
      // // open a trace file
      TraceFile trace_file {QDir {QStandardPaths::writableLocation (QStandardPaths::TempLocation)}.absoluteFilePath (a.applicationName () + "_trace.log")};

      // announce to trace file
      qDebug () << program_title (revision ()) + " - Program startup";
#endif
      QString lang;
      QLocale localeUsedToDeterminateTranslators;
      QTranslator translator_from_qt;
      QTranslator translator_from_resources;
      QTranslator translator_from_files;
      bool qt_OK = false;
      bool resources_OK = false;
      bool files_OK = false;
      do {
        settings.beginGroup("Common");
        lang = settings.value ("Language","en_US").toString();
        settings.endGroup();
        if (files_OK) has_style = a.removeTranslator (&translator_from_files);
        if (resources_OK) has_style = a.removeTranslator (&translator_from_resources);
        if (qt_OK) has_style = a.removeTranslator (&translator_from_qt);
        if (lang != "en_US") {
          //
          // Enable i18n
          //
          localeUsedToDeterminateTranslators = QLocale (lang);
          
          /* load the system translations provided by Qt Currently None useable*/
          has_style = translator_from_qt.load("qt_" + localeUsedToDeterminateTranslators.name(),QLibraryInfo::location(QLibraryInfo::TranslationsPath));
          if (has_style) {
              qt_OK = a.installTranslator(&translator_from_qt);
          }

          // Default translations for releases  use translations stored in
          // the   resources   file    system   under   the   Translations
          // directory. These are built by the CMake build system from .ts
          // files in the translations source directory. New languages are
          // added by  enabling the  UPDATE_TRANSLATIONS CMake  option and
          // building with the  new language added to  the LANGUAGES CMake
          // list  variable.  UPDATE_TRANSLATIONS  will preserve  existing
          // translations  but   should  only  be  set   when  adding  new
          // languages.  The  resulting .ts  files should be  checked info
          // source control for translators to access and update.
          has_style = translator_from_resources.load (localeUsedToDeterminateTranslators, "jtdx", "_", ":/Translations");
          if (has_style) {
              resources_OK = a.installTranslator (&translator_from_resources);
          } 

          // Load  any matching  translation  from  the current  directory
          // using the locale name. This allows translators to easily test
          // their translations  by releasing  (lrelease) a .qm  file into
          // the    current    directory     with    a    suitable    name
          // (e.g.  jtdx_et_EE.qm),  then  running   wsjtx  to  view  the
          // results. Either the system  locale setting or the environment
          // variable LANG can be used to select the target language.
          has_style = translator_from_files.load (QString {"jtdx_"} + localeUsedToDeterminateTranslators.name ());
          if (has_style) {
              files_OK = a.installTranslator (&translator_from_files);
          }
        }
        // Create and initialize shared memory segment
        // Multiple instances: use rig_name as shared memory key

        mem_jtdxjt9.setKey(a.applicationName ());

        if(!mem_jtdxjt9.attach()) {
          if (!mem_jtdxjt9.create(sizeof(struct dec_data))) {
            JTDXMessageBox::critical_message (nullptr, "Error", "Unable to create shared memory segment.");
            exit(1);
          }
        }
        memset(mem_jtdxjt9.data(),0,sizeof(struct dec_data)); //Zero all decoding params in shared memory

        unsigned downSampleFactor;
        {
          SettingsGroup {&settings, "Tune"};

          // deal with Windows Vista and earlier input audio rate
          // converter problems
          downSampleFactor = settings.value ("Audio/DisableInputResampling",
  #if defined (Q_OS_WIN)
                                             // default to true for
                                             // Windows Vista and older
                                             QSysInfo::WV_VISTA >= QSysInfo::WindowsVersion ? true : false
  #else
                                             false
  #endif
                                             ).toBool () ? 1u : 4u;
        }

        MainWindow w(multiple, &settings, &mem_jtdxjt9, downSampleFactor, new QNetworkAccessManager {&a});
        w.show();

        QObject::connect (&a, SIGNAL (lastWindowClosed()), &a, SLOT (quit()));
        result = a.exec();
        if (w.m_exitCode == 1337) result = 1337;
      }
      while(result==1337);
      return result;
    }
  catch (std::exception const& e)
    {
      JTDXMessageBox::critical_message (nullptr, a.applicationName (), e.what ());
      std::cerr << "Error: " << e.what () << '\n';
    }
  catch (...)
    {
      JTDXMessageBox::critical_message (nullptr, a.applicationName (), QObject::tr ("Unexpected error"));
      std::cerr << "Unexpected error\n";
      throw;			// hoping the runtime might tell us more about the exception
    }
  return -1;
}
