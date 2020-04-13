#include "HelpTextWindow.hpp"

#include <QApplication>
#include <QString>
#include <QPalette>
#include <QFile>
#include <QTextStream>

#include "JTDXMessageBox.hpp"
#include "qt_helpers.hpp"

HelpTextWindow::HelpTextWindow (QString const& title, QString const& file_name, QFont const& font, QWidget * parent)
  : QLabel {parent, Qt::WindowCloseButtonHint | Qt::WindowMinimizeButtonHint}
{
  QFile source {file_name};
  if (!source.open (QIODevice::ReadOnly | QIODevice::Text))
    {
      JTDXMessageBox::warning_message (this, QApplication::applicationName ()
                            , "Cannot open \"" + source.fileName ()
                            + "\" for reading:" + source.errorString ());
      return;
    }
  setText (QTextStream {&source}.readAll ());
  setWindowTitle(QApplication::applicationName () + " - " + title);
  setMargin (10);
  setBackgroundRole (QPalette::Base);
  setAutoFillBackground (true);
  setStyleSheet (font_as_stylesheet (font));
}
