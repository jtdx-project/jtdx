#include "CandidateKeyFilter.hpp"

#include <QModelIndex>
#include <QAbstractItemModel>

#include "pimpl_impl.hpp"

class CandidateKeyFilter::impl final
{
public:
  explicit impl (int referenced_key_column
                 , int referenced_key_role
                 , QAbstractItemModel const * referencing_model
                 , int referencing_key_column
                 , int referencing_key_role)
    : referencing_ {referencing_model}
    , referencing_key_column_ {referencing_key_column}
    , referencing_key_role_ {referencing_key_role}
    , referenced_key_column_ {referenced_key_column}
    , referenced_key_role_ {referenced_key_role}
  {
  }

  QAbstractItemModel const * referencing_;
  int referencing_key_column_;
  int referencing_key_role_;
  int referenced_key_column_;
  int referenced_key_role_;
  QModelIndex active_key_;
};

CandidateKeyFilter::CandidateKeyFilter (QAbstractItemModel * referenced_model
                                        , int referenced_key_column
                                        , QObject * parent
                                        , int referenced_key_role)
  : QSortFilterProxyModel {parent}
  , m_ {referenced_key_column, referenced_key_role, nullptr, 0, Qt::EditRole}
{
  setSourceModel (referenced_model);
}

CandidateKeyFilter::CandidateKeyFilter (QAbstractItemModel * referenced_model
                                        , QAbstractItemModel const * referencing_model
                                        , int referenced_key_column
                                        , int referencing_key_column
                                        , QObject * parent
                                        , int referenced_key_role
                                        , int referencing_key_role)
  : QSortFilterProxyModel {parent}
  , m_ {referenced_key_column, referenced_key_role, referencing_model, referencing_key_column, referencing_key_role}
{
  setSourceModel (referenced_model);
}

CandidateKeyFilter::~CandidateKeyFilter ()
{
}

void CandidateKeyFilter::set_active_key (QModelIndex const& index)
{
  if (m_->referencing_)
    {
      if (index.isValid () )
        {
          Q_ASSERT (index.column () == m_->referencing_key_column_);
          m_->active_key_ = index;
        }
      invalidateFilter ();
    }
}

bool CandidateKeyFilter::filterAcceptsRow (int candidate_row, QModelIndex const& candidate_parent) const
{
  if (!m_->referencing_)            // many to many passes all
    {
      return true;
    }

  auto candidate_key = sourceModel ()->index (candidate_row, m_->referenced_key_column_, candidate_parent).data (m_->referenced_key_role_);

  // Include the current key.
  if (m_->active_key_.isValid () && candidate_key == m_->active_key_.data (m_->referencing_key_role_))
    {
      return true;
    }

  // Filter out any candidates already in the referencing key rows.
  return m_->referencing_->match (m_->referencing_->index (0, m_->referencing_key_column_), m_->referencing_key_role_, candidate_key, 1, Qt::MatchExactly).isEmpty ();
}
