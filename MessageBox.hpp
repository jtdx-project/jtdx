#ifndef MESSAGE_BOX_HPP__
#define MESSAGE_BOX_HPP__

#include <QMessageBox>

// get rid of the nasty MS define
#ifdef MessageBox
#undef MessageBox
#endif

//
// MessageBox - wrap the Qt QMessageBox class to give a more platform
// 							neutral and functional interface
//
class MessageBox
  : public QMessageBox
{
public:
  explicit MessageBox (QWidget * parent = nullptr);
  explicit MessageBox (Icon, QString const& text, StandardButtons = NoButton
                       , QWidget * parent = nullptr
                       , Qt::WindowFlags = Qt::Dialog | Qt::MSWindowsFixedSizeDialogHint);

  static void about_message (QWidget * parent, QString const& text);
  static void about_Qt_message (QWidget * parent);
  static StandardButton information_message (QWidget * parent, QString const& text
                                             , QString const& informative = QString {}
                                             , QString const& detail = QString {}
                                             , StandardButtons buttons = Ok
                                             , StandardButton default_button = NoButton);
  static StandardButton query_message (QWidget * parent, QString const& text
                                       , QString const& informative = QString {}
                                       , QString const& detail = QString {}
                                       , StandardButtons buttons = Yes | No
                                       , StandardButton default_button = NoButton);
  static StandardButton warning_message (QWidget * parent, QString const& text
                                         , QString const& informative = QString {}
                                         , QString const& detail = QString {}
                                         , StandardButtons buttons = Ok
                                         , StandardButton default_button = NoButton);
  static StandardButton critical_message (QWidget * parent, QString const& text
                                          , QString const& informative = QString {}
                                          , QString const& detail = QString {}
                                          , StandardButtons buttons = Ok
                                          , StandardButton default_button = NoButton);
private:
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
