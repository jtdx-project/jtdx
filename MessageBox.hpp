#ifndef MESSAGE_BOX_HPP__
#define MESSAGE_BOX_HPP__

#include <QObject>
#include <QMessageBox>

// get rid of the nasty MS define
#ifdef MessageBox
#undef MessageBox
#endif

//
// MessageBox - wrap the Qt QMessageBox class to give a more platform
// 							neutral and functional interface
//
class MessageBox : public QMessageBox
{
Q_OBJECT;
 public:
  explicit MessageBox (QWidget * parent = nullptr);
  explicit MessageBox (Icon,  QString const& text, StandardButtons = NoButton
                       , QWidget * parent = nullptr
                       , Qt::WindowFlags = Qt::Dialog | Qt::MSWindowsFixedSizeDialogHint);
         void translate_buttons();     
  static void about_message (QWidget * parent, QString const& text);
  static void about_Qt_message (QWidget * parent);
  static StandardButton information_message (QWidget * parent, QString const& title
                                             , QString const& text
                                             , QString const& informative = QString {}
                                             , QString const& detail = QString {}
                                             , StandardButtons buttons = Ok
                                             , StandardButton default_button = NoButton
                                             , QString const& lang = QString {});
  static StandardButton query_message (QWidget * parent, QString const& title
                                       , QString const& text
                                       , QString const& informative = QString {}
                                       , QString const& detail = QString {}
                                       , StandardButtons buttons = Yes | No
                                       , StandardButton default_button = NoButton
                                       , QString const& lang = QString {});
  static StandardButton warning_message (QWidget * parent, QString const& title
                                         , QString const& text
                                         , QString const& informative = QString {}
                                         , QString const& detail = QString {}
                                         , StandardButtons buttons = Ok
                                         , StandardButton default_button = NoButton
                                         , QString const& lang = QString {});
  static StandardButton critical_message (QWidget * parent, QString const& title
                                          , QString const& text
                                          , QString const& informative = QString {}
                                          , QString const& detail = QString {}
                                          , StandardButtons buttons = Ok
                                          , StandardButton default_button = NoButton
                                          , QString const& lang = QString {});
private:

  static StandardButton show_it (QWidget * parent, MessageBox::Icon icon
                                       , QString const& title
                                       , QString const& text
                                       , QString const& informative
                                       , QString const& detail
                                       , MessageBox::StandardButtons buttons
                                       , MessageBox::StandardButton default_button
                                       , QString const& lang);
 
  // hide the parent static functions so that users use our versions
  // above that are correctly branded and have better platform
  // independence
  using QMessageBox::about;
  using QMessageBox::aboutQt;
  using QMessageBox::information;
  using QMessageBox::question;
  using QMessageBox::warning;
  using QMessageBox::critical;
};

#endif
