#include "ForeignKeyDelegate.hpp"

#include <QComboBox>

#include "CandidateKeyFilter.hpp"

ForeignKeyDelegate::ForeignKeyDelegate (QAbstractItemModel * referenced_model
                                        , int referenced_key_column
                                        , QObject * parent
                                        , int referenced_key_role)
  : QStyledItemDelegate {parent}
  , candidate_key_filter_ {new CandidateKeyFilter {referenced_model, referenced_key_column, nullptr, referenced_key_role}}
{
}

ForeignKeyDelegate::ForeignKeyDelegate (QAbstractItemModel * referenced_model
                                        , QAbstractItemModel const * referencing_model
                                        , int referenced_key_column
                                        , int referencing_key_column
                                        , QObject * parent
                                        , int referenced_key_role
                                        , int referencing_key_role)
  : QStyledItemDelegate {parent}
  , candidate_key_filter_ {new CandidateKeyFilter {referenced_model, referencing_model, referenced_key_column, referencing_key_column, nullptr, referenced_key_role, referencing_key_role}}
{
}

ForeignKeyDelegate::~ForeignKeyDelegate ()
{
}

QWidget * ForeignKeyDelegate::createEditor (QWidget * parent
                                            , QStyleOptionViewItem const& /* option */
                                            , QModelIndex const& index) const
{
  auto editor = new QComboBox {parent};
  editor->setFrame (false);
  candidate_key_filter_->set_active_key (index);
  editor->setModel (candidate_key_filter_.data ());
  return editor;
}
