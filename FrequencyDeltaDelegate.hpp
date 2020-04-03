#ifndef FREQUENCY_DELTA_DELEGATE_HPP_
#define FREQUENCY_DELTA_DELEGATE_HPP_

#include <QStyledItemDelegate>

//
// Class FrequencyDeltaDelegate
//
//	Item delegate for editing a frequency delta in Hertz but displayed in MHz
//
class FrequencyDeltaDelegate final
  : public QStyledItemDelegate
{
public:
  explicit FrequencyDeltaDelegate (QObject * parent = nullptr);
  QWidget * createEditor (QWidget * parent, QStyleOptionViewItem const&, QModelIndex const&) const override;
  void setEditorData (QWidget * editor, QModelIndex const&) const override;
  void setModelData (QWidget * editor, QAbstractItemModel *, QModelIndex const&) const override;
};

#endif
