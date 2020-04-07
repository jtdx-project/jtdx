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

void MessageBox::translate_buttons()
{
    StandardButtons buttons = standardButtons();
    if (buttons & MessageBox::Ok) button(MessageBox::Ok)->setText(tr("&OK"));
    if (buttons & MessageBox::Save) button(MessageBox::Save)->setText(tr("Save"));
    if (buttons & MessageBox::SaveAll) button(MessageBox::SaveAll)->setText(tr("Save All"));
    if (buttons & MessageBox::Open) button(MessageBox::Open)->setText(tr("Open"));
    if (buttons & MessageBox::Yes) button(MessageBox::Yes)->setText(tr("&Yes"));
    if (buttons & MessageBox::YesToAll) button(MessageBox::YesToAll)->setText(tr("Yes to &All"));
    if (buttons & MessageBox::No) button(MessageBox::No)->setText(tr("&No"));
    if (buttons & MessageBox::NoToAll) button(MessageBox::NoToAll)->setText(tr("N&o to All"));
    if (buttons & MessageBox::Abort) button(MessageBox::Abort)->setText(tr("Abort"));
    if (buttons & MessageBox::Retry) button(MessageBox::Retry)->setText(tr("&Retry"));
    if (buttons & MessageBox::Ignore) button(MessageBox::Ignore)->setText(tr("Ignore"));
    if (buttons & MessageBox::Close) button(MessageBox::Close)->setText(tr("Close"));
    if (buttons & MessageBox::Cancel) button(MessageBox::Cancel)->setText(tr("&Cancel"));
    if (buttons & MessageBox::Discard) button(MessageBox::Discard)->setText(tr("Discard"));
    if (buttons & MessageBox::Help) button(MessageBox::Help)->setText(tr("Help"));
    if (buttons & MessageBox::Apply) button(MessageBox::Apply)->setText(tr("Apply"));
    if (buttons & MessageBox::Reset) button(MessageBox::Reset)->setText(tr("Reset"));
    if (buttons & MessageBox::RestoreDefaults) button(MessageBox::RestoreDefaults)->setText(tr("Restore Defaults"));
    if (buttons & MessageBox::NoButton) button(MessageBox::NoButton)->setText("");
}  

MessageBox::StandardButton MessageBox::show_it (QWidget * parent, MessageBox::Icon icon
                                       , QString const& title
                                       , QString const& text
                                       , QString const& informative
                                       , QString const& detail
                                       , MessageBox::StandardButtons buttons
                                       , MessageBox::StandardButton default_button)
  {
    MessageBox mb {icon, text, MessageBox::NoButton, parent};
    if (!title.isEmpty()) mb.setWindowTitle(title);
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
    QMessageBox::tr("Show Details...");
    QMessageBox::tr("Hide Details...");
    mb.translate_buttons();
    if (mb.exec() == -1)
      return MessageBox::Cancel;
    return mb.standardButton (mb.clickedButton ());
  }

auto MessageBox::information_message (QWidget * parent, QString const& title
                                      , QString const& text
                                      , QString const& informative
                                      , QString const& detail
                                      , StandardButtons buttons
                                      , StandardButton default_button) -> StandardButton
{
  return show_it (parent, Information, title, text, informative, detail, buttons, default_button);
}

auto MessageBox::query_message (QWidget * parent, QString const& title
                                , QString const& text
                                , QString const& informative
                                , QString const& detail
                                , StandardButtons buttons
                                , StandardButton default_button) -> StandardButton
{
  return show_it (parent, Question, title, text, informative, detail, buttons, default_button);
}

auto MessageBox::warning_message (QWidget * parent, QString const& title
                                  , QString const& text
                                  , QString const& informative
                                  , QString const& detail
                                  , StandardButtons buttons
                                  , StandardButton default_button) -> StandardButton
{
  return show_it (parent, Warning, title, text, informative, detail, buttons, default_button);
}

auto MessageBox::critical_message (QWidget * parent, QString const& title
                                   , QString const& text
                                   , QString const& informative
                                   , QString const& detail
                                   , StandardButtons buttons
                                   , StandardButton default_button) -> StandardButton
{
  return show_it (parent, Critical, title, text, informative, detail, buttons, default_button);
}
