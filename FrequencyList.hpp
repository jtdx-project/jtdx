#ifndef FREQUENCY_LIST_HPP__
#define FREQUENCY_LIST_HPP__

#include "pimpl_h.hpp"

#include <QList>
#include <QSortFilterProxyModel>

#include "Radio.hpp"
#include "Modes.hpp"

class Bands;

//
// Class FrequencyList
//
//  Encapsulates a  collection of  frequencies with  associated modes.
//  The implementation is a table containing the list of Frequency and
//  mode tuples which  are editable. A third column is  modeled in the
//  model  which   is  an  immutable  double   representation  of  the
//  corresponding Frequency item scaled to mega-Hertz.
//
//  The list is ordered.  A filter on mode is available  and is set by
//  the filter(Mode)  method. The  Mode value  Modes::NULL_MODE passes
//  all rows in the filter.
//
// Responsibilities
//
//  Stores  internally  a  list   of  unique  frequency  mode  tuples.
//  Provides methods to add and delete list elements. Provides range
//  iterators for a filtered view of the underlying table.
//
// Collaborations
//
//  Implements the QSortFilterProxyModel interface  for a list of spot
//  frequencies.
//
class FrequencyList final
  : public QSortFilterProxyModel
{
  Q_OBJECT

public:
  using Frequency = Radio::Frequency;
  using Mode = Modes::Mode;

  struct Item
  {
    Frequency frequency_;
    Mode mode_;
    bool default_;
  };
  using FrequencyItems = QList<Item>;
  using BandSet = QSet<QString>;

  enum Column {mode_column, frequency_column, frequency_mhz_column, mode_frequency_mhz_column};

  // an iterator that meets the requirements of the C++ for range statement
  class const_iterator
  {
  public:
    const_iterator (FrequencyList const * parent, int row)
      : parent_ {parent}
      , row_ {row}
    {
    }

    Item const& operator * ();
    bool operator != (const_iterator const&) const;
    const_iterator& operator ++ ();

  private:
    FrequencyList const * parent_;
    int row_;
  };

  explicit FrequencyList (Bands const *, QObject * parent = nullptr);
  ~FrequencyList ();

  // Load and store underlying items
  FrequencyItems frequency_list (FrequencyItems);
  FrequencyItems const& frequency_list () const;

  // Iterators for the sorted and filtered items
  //
  // Note that these iterators are on the final sorted and filtered
  // rows, if you need to access the underlying unfiltered and
  // unsorted frequencies then use the frequency_list() member to
  // access the underlying list of rows.
  const_iterator begin () const;
  const_iterator end () const;

  // Bands of the frequencies
  BandSet all_bands (Mode = Modes::NULL_MODE) const;
  BandSet filtered_bands () const;

  // Find the row of the nearest best working frequency given a
  // frequency. Returns -1 if no suitable working frequency is found
  // in the list.
  int best_working_frequency (Frequency) const;

  // Find the row  of the nearest best working frequency  given a band
  // name. Returns -1 if no suitable working frequency is found in the
  // list.
  int best_working_frequency (QString const& band) const;

  // Set filter
  Q_SLOT void filter (Mode);

  // Reset
  Q_SLOT void reset_to_defaults ();

  // Model API
  QModelIndex add (Item);
  bool remove (Item);
  bool removeDisjointRows (QModelIndexList);

  // Proxy API
  bool filterAcceptsRow (int source_row, QModelIndex const& parent) const override;

  // Custom roles.
  static int constexpr SortRole = Qt::UserRole;

private:
  class impl;
  pimpl<impl> m_;
};

inline
bool operator == (FrequencyList::Item const& lhs, FrequencyList::Item const& rhs)
{
  return
    lhs.frequency_ == rhs.frequency_
    && lhs.mode_ == rhs.mode_;
}

QDataStream& operator << (QDataStream&, FrequencyList::Item const&);
QDataStream& operator >> (QDataStream&, FrequencyList::Item&);

#if !defined (QT_NO_DEBUG_STREAM)
QDebug operator << (QDebug, FrequencyList::Item const&);
#endif

Q_DECLARE_METATYPE (FrequencyList::Item);
Q_DECLARE_METATYPE (FrequencyList::FrequencyItems);

#endif
