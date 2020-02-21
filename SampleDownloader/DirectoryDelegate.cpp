#include "DirectoryDelegate.hpp"

#include <QApplication>
#include <QVariant>
#include <QString>
#include <QStyle>
#include <QModelIndex>
#include <QPainter>
#include <QStyleOptionViewItem>
#include <QStyleOptionProgressBar>

void DirectoryDelegate::paint (QPainter * painter, QStyleOptionViewItem const& option
                               , QModelIndex const& index) const
{
  if (1 == index.column ())
    {
      QStyleOptionProgressBar progress_bar_option;
      progress_bar_option.rect = option.rect;
      progress_bar_option.state = QStyle::State_Enabled;
      progress_bar_option.direction = QApplication::layoutDirection ();
      progress_bar_option.fontMetrics = QApplication::fontMetrics ();
      progress_bar_option.minimum = 0;
      progress_bar_option.maximum = 100;
      auto progress = index.data ().toLongLong ();
      if (progress > 0)
        {
          auto percent = int (progress * 100 / index.data (Qt::UserRole).toLongLong ());
          progress_bar_option.progress = percent;
          progress_bar_option.text = QString::number (percent) + '%';
          progress_bar_option.textVisible = true;
          progress_bar_option.textAlignment = Qt::AlignCenter;
        }
      else
        {
          // not started
          progress_bar_option.progress = -1;
        }
      QApplication::style ()->drawControl (QStyle::CE_ProgressBar, &progress_bar_option, painter);
    }
  else
    {
      QStyledItemDelegate::paint (painter, option, index);
    }
}
