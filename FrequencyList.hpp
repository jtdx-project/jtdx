#ifndef FREQUENCY_LIST_HPP__
#define FREQUENCY_LIST_HPP__

#include "pimpl_h.hpp"

#include <QList>
#include <QSortFilterProxyModel>

#include "Radio.hpp"
#include "IARURegions.hpp"
#include "Modes.hpp"

class Bands;

//
// Class FrequencyList_v2
//
//  Encapsulates a  collection of  frequencies with  associated modes.
//  The implementation is a table  containing the list of IARU region,
//  Frequency and  mode tuples which  are editable. A third  column is
//  modeled in the  model which is an  immutable double representation
//  of the corresponding Frequency item scaled to mega-Hertz.
//
//  The  list  is ordered.   A  filter  on  IARU  region and  mode  is
//  available  and is  set by  the filter(Region,  Mode) method.   The
//  Region value IARURegions::ALL and the Mode value Modes::ALL may be
//  optionally given which passes all rows in the filtered column.
//
// Responsibilities
//
//  Stores internally a list of  unique region, frequency mode tuples.
//  Provides methods to  add and delete list  elements. Provides range
//  iterators for a filtered view of the underlying table.
//
// Collaborations
//
//  Implements the QSortFilterProxyModel interface  for a list of spot
//  frequencies.
//
class FrequencyList_v2 final
  : public QSortFilterProxyModel
{
  Q_OBJECT;

public:
  using Region = IARURegions::Region;
  using Frequency = Radio::Frequency;
  using Mode = Modes::Mode;

  struct Item
  {
    Frequency frequency_;
    Mode mode_;
    Region region_;
    bool default_;
  };
  using FrequencyItems = QList<Item>;
  using BandSet = QSet<QString>;

  enum Column {region_column, mode_column, frequency_column, frequency_mhz_column, mode_frequency_mhz_column, SENTINAL};

  // an iterator that meets the requirements of the C++ for range statement
  class const_iterator
  {
  public:
    const_iterator (FrequencyList_v2 const * parent, int row)
      : parent_ {parent}
      , row_ {row}
    {
    }

    Item const& operator * () const;
    Item const * operator -> () const;
    bool operator != (const_iterator const&) const;
    bool operator == (const_iterator const&) const;
    const_iterator& operator ++ ();

  private:
    FrequencyList_v2 const * parent_;
    int row_;
  };

  explicit FrequencyList_v2 (Bands const *, QObject * parent = nullptr);
  ~FrequencyList_v2 ();

  // Load and store underlying items
  FrequencyItems frequency_list (FrequencyItems);
  FrequencyItems const& frequency_list () const;
  FrequencyItems frequency_list (QModelIndexList const&) const;
  void frequency_list_merge (FrequencyItems const&);

  // Iterators for the sorted and filtered items
  //
  // Note that these iterators are on the final sorted and filtered
  // rows, if you need to access the underlying unfiltered and
  // unsorted frequencies then use the frequency_list() member to
  // access the underlying list of rows.
  const_iterator begin () const;
  const_iterator end () const;

  // Find a row with a given frequency
  const_iterator find (Frequency) const;

  // Bands of the frequencies
  BandSet all_bands (Region = IARURegions::ALL, Mode = Modes::ALL) const;
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
  Q_SLOT void filter (Region, Mode);

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
bool operator == (FrequencyList_v2::Item const& lhs, FrequencyList_v2::Item const& rhs)
{
  return
    lhs.frequency_ == rhs.frequency_
    && lhs.region_ == rhs.region_
    && lhs.mode_ == rhs.mode_;
}

QDataStream& operator << (QDataStream&, FrequencyList_v2::Item const&);
QDataStream& operator >> (QDataStream&, FrequencyList_v2::Item&);

#if !defined (QT_NO_DEBUG_STREAM)
QDebug operator << (QDebug, FrequencyList_v2::Item const&);
#endif

Q_DECLARE_METATYPE (FrequencyList_v2::Item);
Q_DECLARE_METATYPE (FrequencyList_v2::FrequencyItems);


//
// Obsolete version of FrequencyList no longer used but needed to
// allow loading and saving of old settings contents without damage
//
class FrequencyList final
{
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

private:
  FrequencyItems frequency_list_;
};

QDataStream& operator << (QDataStream&, FrequencyList::Item const&);
QDataStream& operator >> (QDataStream&, FrequencyList::Item&);

Q_DECLARE_METATYPE (FrequencyList::Item);
Q_DECLARE_METATYPE (FrequencyList::FrequencyItems);

#endif
