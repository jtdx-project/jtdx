#ifndef FOREIGN_KEY_DELEGATE_HPP_
#define FOREIGN_KEY_DELEGATE_HPP_

#include <QStyledItemDelegate>
#include <QScopedPointer>

class CandidateKeyFilter;

//
// Class ForeignKeyDelegate
//
//	Item delegate for editing a foreign key item in a one or many
//	to one relationship. A QComboBox is used as an item delegate
//	for the edit role.
//
class ForeignKeyDelegate final
  : public QStyledItemDelegate
{
public:
  // many to many relationship
  explicit ForeignKeyDelegate (QAbstractItemModel * referenced_model
                               , int referenced_key_column
                               , QObject * parent = nullptr
                               , int referenced_key_role = Qt::EditRole);

  // one to many (referenced to referencing) relationship
  explicit ForeignKeyDelegate (QAbstractItemModel * referenced_model
                               , QAbstractItemModel const * referencing_model
                               , int referenced_key_column
                               , int referencing_key_column
                               , QObject * parent = nullptr
                               , int referenced_key_role = Qt::EditRole
                               , int referencing_key_role = Qt::EditRole);
  ~ForeignKeyDelegate ();

  QWidget * createEditor (QWidget * parent, QStyleOptionViewItem const&, QModelIndex const&) const override;

private:
  QScopedPointer<CandidateKeyFilter> candidate_key_filter_;
};

#endif
