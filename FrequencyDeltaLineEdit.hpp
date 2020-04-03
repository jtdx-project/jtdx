#ifndef FREQUENCY_DELTA_LINE_EDIT_HPP_
#define FREQUENCY_DELTA_LINE_EDIT_HPP_

#include <QLineEdit>

#include "Radio.hpp"

class QWidget;

//
// MHz frequency delta line edit with validation
//
class FrequencyDeltaLineEdit final
  : public QLineEdit
{
  Q_OBJECT;
  Q_PROPERTY (FrequencyDelta frequency_delta READ frequency_delta WRITE frequency_delta USER true);

public:
  using FrequencyDelta = Radio::FrequencyDelta;

  explicit FrequencyDeltaLineEdit (QWidget * parent = nullptr);

  // Property frequency_delta implementation
  FrequencyDelta frequency_delta () const;
  void frequency_delta (FrequencyDelta);
};

#endif
