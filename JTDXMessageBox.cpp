//
// JTDXMessageBox - wrap the Qt QMessageBox class to give a more platform
// neutral and functional interface and translation of buttons
//
//Created by Arvo, ES1JA 2020

#include "JTDXMessageBox.hpp"

#include <QDialogButtonBox>
#include <QPushButton>

#include "revision_utils.hpp"

JTDXMessageBox::JTDXMessageBox (QWidget * parent)
  : QMessageBox {parent}
{
  setWindowTitle ("JTDX");
}

JTDXMessageBox::JTDXMessageBox (Icon icon, QString const& text, StandardButtons buttons
                        , QWidget * parent, Qt::WindowFlags flags)
  : QMessageBox {icon, "JTDX", text, buttons, parent, flags}
{
}

void JTDXMessageBox::about_message (QWidget * parent, QString const& text)
{
  QMessageBox::about (parent, "JTDX", text);
}

void JTDXMessageBox::about_Qt_message (QWidget * parent)
{
  QMessageBox::aboutQt (parent, "JTDX");
}

void JTDXMessageBox::translate_buttons()
{
    StandardButtons buttons = standardButtons();
    if (buttons & JTDXMessageBox::Ok) button(JTDXMessageBox::Ok)->setText(tr("&OK"));
    if (buttons & JTDXMessageBox::Save) button(JTDXMessageBox::Save)->setText(tr("Save"));
    if (buttons & JTDXMessageBox::SaveAll) button(JTDXMessageBox::SaveAll)->setText(tr("Save All"));
    if (buttons & JTDXMessageBox::Open) button(JTDXMessageBox::Open)->setText(tr("Open"));
    if (buttons & JTDXMessageBox::Yes) button(JTDXMessageBox::Yes)->setText(tr("&Yes"));
    if (buttons & JTDXMessageBox::YesToAll) button(JTDXMessageBox::YesToAll)->setText(tr("Yes to &All"));
    if (buttons & JTDXMessageBox::No) button(JTDXMessageBox::No)->setText(tr("&No"));
    if (buttons & JTDXMessageBox::NoToAll) button(JTDXMessageBox::NoToAll)->setText(tr("N&o to All"));
    if (buttons & JTDXMessageBox::Abort) button(JTDXMessageBox::Abort)->setText(tr("Abort"));
    if (buttons & JTDXMessageBox::Retry) button(JTDXMessageBox::Retry)->setText(tr("&Retry"));
    if (buttons & JTDXMessageBox::Ignore) button(JTDXMessageBox::Ignore)->setText(tr("Ignore"));
    if (buttons & JTDXMessageBox::Close) button(JTDXMessageBox::Close)->setText(tr("Close"));
    if (buttons & JTDXMessageBox::Cancel) button(JTDXMessageBox::Cancel)->setText(tr("&Cancel"));
    if (buttons & JTDXMessageBox::Discard) button(JTDXMessageBox::Discard)->setText(tr("Discard"));
    if (buttons & JTDXMessageBox::Help) button(JTDXMessageBox::Help)->setText(tr("Help"));
    if (buttons & JTDXMessageBox::Apply) button(JTDXMessageBox::Apply)->setText(tr("Apply"));
    if (buttons & JTDXMessageBox::Reset) button(JTDXMessageBox::Reset)->setText(tr("Reset"));
    if (buttons & JTDXMessageBox::RestoreDefaults) button(JTDXMessageBox::RestoreDefaults)->setText(tr("Restore Defaults"));
    if (buttons & JTDXMessageBox::NoButton) button(JTDXMessageBox::NoButton)->setText("");
}  

JTDXMessageBox::StandardButton JTDXMessageBox::show_it (QWidget * parent, JTDXMessageBox::Icon icon
                                       , QString const& title
                                       , QString const& text
                                       , QString const& informative
                                       , QString const& detail
                                       , JTDXMessageBox::StandardButtons buttons
                                       , JTDXMessageBox::StandardButton default_button)
  {
    JTDXMessageBox mb {icon, text, JTDXMessageBox::NoButton, parent};
    if (!title.isEmpty()) mb.setWindowTitle(title);
    QDialogButtonBox * button_box = mb.findChild<QDialogButtonBox *> ();
    Q_ASSERT (button_box);

    uint mask = JTDXMessageBox::FirstButton;
    while (mask <= JTDXMessageBox::LastButton) {
      uint sb = buttons & mask;
      mask <<= 1;
      if (!sb)
        continue;
      QPushButton * button = mb.addButton (static_cast<JTDXMessageBox::StandardButton> (sb));
      // Choose the first accept role as the default
      if (mb.defaultButton ())
        continue;
      if ((default_button == JTDXMessageBox::NoButton
           && button_box->buttonRole (button) == QDialogButtonBox::AcceptRole)
          || (default_button != JTDXMessageBox::NoButton
              && sb == static_cast<uint> (default_button)))
        mb.setDefaultButton (button);
    }
    mb.setInformativeText (informative);
    mb.setDetailedText (detail);
    QMessageBox::tr("Show Details...");
    QMessageBox::tr("Hide Details...");
    mb.translate_buttons();
    if (mb.exec() == -1)
      return JTDXMessageBox::Cancel;
    return mb.standardButton (mb.clickedButton ());
  }

auto JTDXMessageBox::information_message (QWidget * parent, QString const& title
                                      , QString const& text
                                      , QString const& informative
                                      , QString const& detail
                                      , StandardButtons buttons
                                      , StandardButton default_button) -> StandardButton
{
  return show_it (parent, Information, title, text, informative, detail, buttons, default_button);
}

auto JTDXMessageBox::query_message (QWidget * parent, QString const& title
                                , QString const& text
                                , QString const& informative
                                , QString const& detail
                                , StandardButtons buttons
                                , StandardButton default_button) -> StandardButton
{
  return show_it (parent, Question, title, text, informative, detail, buttons, default_button);
}

auto JTDXMessageBox::warning_message (QWidget * parent, QString const& title
                                  , QString const& text
                                  , QString const& informative
                                  , QString const& detail
                                  , StandardButtons buttons
                                  , StandardButton default_button) -> StandardButton
{
  return show_it (parent, Warning, title, text, informative, detail, buttons, default_button);
}

auto JTDXMessageBox::critical_message (QWidget * parent, QString const& title
                                   , QString const& text
                                   , QString const& informative
                                   , QString const& detail
                                   , StandardButtons buttons
                                   , StandardButton default_button) -> StandardButton
{
  return show_it (parent, Critical, title, text, informative, detail, buttons, default_button);
}
