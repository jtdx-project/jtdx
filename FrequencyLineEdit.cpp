#include "FrequencyLineEdit.hpp"

#include <QRegExpValidator>
#include <QRegExp>
#include <QString>
#include <QLocale>

#include "moc_FrequencyLineEdit.cpp"

FrequencyLineEdit::FrequencyLineEdit (QWidget * parent)
  : QLineEdit (parent)
{
  setValidator (new QRegExpValidator {QRegExp {QString {R"(\d{0,6}(\)"} + QLocale {}.decimalPoint () + R"(\d{0,6})?)"}, this});
}

auto FrequencyLineEdit::frequency () const -> Frequency
{
  return Radio::frequency (text (), 6);
}

void FrequencyLineEdit::frequency (Frequency f)
{
  setText (Radio::frequency_MHz_string (f));
}


FrequencyDeltaLineEdit::FrequencyDeltaLineEdit (QWidget * parent)
  : QLineEdit (parent)
{
  setValidator (new QRegExpValidator {QRegExp {QString {R"(-?\d{0,6}(\)"} + QLocale {}.decimalPoint () + R"(\d{0,6})?)"}, this});
}

auto FrequencyDeltaLineEdit::frequency_delta () const -> FrequencyDelta
{
  return Radio::frequency_delta (text (), 6);
}

void FrequencyDeltaLineEdit::frequency_delta (FrequencyDelta d)
{
  setText (Radio::frequency_MHz_string (d));
}
