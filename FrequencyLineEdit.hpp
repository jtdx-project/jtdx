#ifndef FREQUENCY_LINE_EDIT_HPP_
#define FREQUENCY_LINE_EDIT_HPP_

#include <QLineEdit>

#include "Radio.hpp"

class QWidget;

//
// MHz frequency line edit with validation
//
class FrequencyLineEdit final
  : public QLineEdit
{
  Q_OBJECT;
  Q_PROPERTY (Frequency frequency READ frequency WRITE frequency USER true);

public:
  using Frequency = Radio::Frequency;

  explicit FrequencyLineEdit (QWidget * parent = nullptr);

  // Property frequency implementation
  Frequency frequency () const;
  void frequency (Frequency);
};

#endif
