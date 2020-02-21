#include "MessageBox.hpp"

#include <QDialogButtonBox>
#include <QPushButton>

#include "revision_utils.hpp"

MessageBox::MessageBox (QWidget * parent)
  : QMessageBox {parent}
{
  setWindowTitle ("JTDX");
}

MessageBox::MessageBox (Icon icon, QString const& text, StandardButtons buttons
                        , QWidget * parent, Qt::WindowFlags flags)
  : QMessageBox {icon, "JTDX", text, buttons, parent, flags}
{
}

void MessageBox::about_message (QWidget * parent, QString const& text)
{
  QMessageBox::about (parent, "JTDX", text);
}

void MessageBox::about_Qt_message (QWidget * parent)
{
  QMessageBox::aboutQt (parent, "JTDX");
}

namespace
{
  QMessageBox::StandardButton show_it (QWidget * parent, MessageBox::Icon icon
                                       , QString const& text
                                       , QString const& informative
                                       , QString const& detail
                                       , MessageBox::StandardButtons buttons
                                       , MessageBox::StandardButton default_button)
  {
    MessageBox mb {icon, text, MessageBox::NoButton, parent};
    QDialogButtonBox * button_box = mb.findChild<QDialogButtonBox *> ();
    Q_ASSERT (button_box);

    uint mask = MessageBox::FirstButton;
    while (mask <= MessageBox::LastButton) {
      uint sb = buttons & mask;
      mask <<= 1;
      if (!sb)
        continue;
      QPushButton * button = mb.addButton (static_cast<MessageBox::StandardButton> (sb));
      // Choose the first accept role as the default
      if (mb.defaultButton ())
        continue;
      if ((default_button == MessageBox::NoButton
           && button_box->buttonRole (button) == QDialogButtonBox::AcceptRole)
          || (default_button != MessageBox::NoButton
              && sb == static_cast<uint> (default_button)))
        mb.setDefaultButton (button);
    }
    mb.setInformativeText (informative);
    mb.setDetailedText (detail);
    if (mb.exec() == -1)
      return MessageBox::Cancel;
    return mb.standardButton (mb.clickedButton ());
  }
}

auto MessageBox::information_message (QWidget * parent, QString const& text
                                      , QString const& informative
                                      , QString const& detail
                                      , StandardButtons buttons
                                      , StandardButton default_button) -> StandardButton
{
  return show_it (parent, Information, text, informative, detail, buttons, default_button);
}

auto MessageBox::query_message (QWidget * parent, QString const& text
                                , QString const& informative
                                , QString const& detail
                                , StandardButtons buttons
                                , StandardButton default_button) -> StandardButton
{
  return show_it (parent, Question, text, informative, detail, buttons, default_button);
}

auto MessageBox::warning_message (QWidget * parent, QString const& text
                                  , QString const& informative
                                  , QString const& detail
                                  , StandardButtons buttons
                                  , StandardButton default_button) -> StandardButton
{
  return show_it (parent, Warning, text, informative, detail, buttons, default_button);
}

auto MessageBox::critical_message (QWidget * parent, QString const& text
                                   , QString const& informative
                                   , QString const& detail
                                   , StandardButtons buttons
                                   , StandardButton default_button) -> StandardButton
{
  return show_it (parent, Critical, text, informative, detail, buttons, default_button);
}
