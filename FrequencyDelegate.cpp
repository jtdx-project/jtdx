#include "FrequencyDelegate.hpp"

#include "FrequencyLineEdit.hpp"

FrequencyDelegate::FrequencyDelegate (QObject * parent)
  : QStyledItemDelegate {parent}
{
}

QWidget * FrequencyDelegate::createEditor (QWidget * parent, QStyleOptionViewItem const&
                                          , QModelIndex const&) const
{
  auto * editor = new FrequencyLineEdit {parent};
  editor->setFrame (false);
  return editor;
}

void FrequencyDelegate::setEditorData (QWidget * editor, QModelIndex const& index) const
{
  static_cast<FrequencyLineEdit *> (editor)->frequency (index.model ()->data (index, Qt::EditRole).value<Radio::Frequency> ());
}

void FrequencyDelegate::setModelData (QWidget * editor, QAbstractItemModel * model, QModelIndex const& index) const
{
  model->setData (index, static_cast<FrequencyLineEdit *> (editor)->frequency (), Qt::EditRole);
}

