#ifndef DIRECTORY_DELEGATE_HPP__
#define DIRECTORY_DELEGATE_HPP__

#include <QStyledItemDelegate>

class QObject;
class QStyleOptionVoew;
class QModelIndex;
class QPainter;

//
// Styled item delegate that renders a progress bar in column #1
//
// model column #1 DisplayRole is the progress in bytes
// model column #1 UserRole is the expected number of bytes
//
class DirectoryDelegate final
  : public QStyledItemDelegate
{
public:
  explicit DirectoryDelegate (QObject * parent = nullptr)
    : QStyledItemDelegate {parent}
  {
  }

  void paint (QPainter * painter, QStyleOptionViewItem const& option
              , QModelIndex const& index) const override;
};

#endif
