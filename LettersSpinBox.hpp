#ifndef LETTERS_SPIN_BOX_HPP_
#define LETTERS_SPIN_BOX_HPP_

#include <QSpinBox>

class QString;

//
// LettersSpinBox - select from consecutive letters
//
class LettersSpinBox final
  : public QSpinBox
{
  Q_OBJECT;
  Q_PROPERTY (bool lowercase MEMBER lowercase_)

public:
  LettersSpinBox (QWidget * parent = nullptr)
    : QSpinBox {parent}
    , lowercase_ {false}
  {
  }

  QString textFromValue (int) const override;
//  int valueFromText (QString const&) const override;

private:
  bool lowercase_;
};

#endif
