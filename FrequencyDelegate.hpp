#ifndef FREQUENCY_DELEGATE_HPP_
#define FREQUENCY_DELEGATE_HPP_

#include <QStyledItemDelegate>

//
// Class FrequencyDelegate
//
//	Item delegate for editing a frequency in Hertz but displayed in MHz
//
class FrequencyDelegate final
  : public QStyledItemDelegate
{
public:
  explicit FrequencyDelegate (QObject * parent = nullptr);
  QWidget * createEditor (QWidget * parent, QStyleOptionViewItem const&, QModelIndex const&) const override;
  void setEditorData (QWidget * editor, QModelIndex const&) const override;
  void setModelData (QWidget * editor, QAbstractItemModel *, QModelIndex const&) const override;
};

#endif
