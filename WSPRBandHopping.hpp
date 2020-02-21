#ifndef WSPR_BAND_HOPPING_HPP__
#define WSPR_BAND_HOPPING_HPP__

#include <QObject>

#include "pimpl_h.hpp"

class QSettings;
class Configuration;
class QWidget;

//
// WSPR Band Hopping Control
//
//  WSPR specifies  a globally  coordinated band hopping  schedule and
//  this class implements that.
//
// Responsibilities
//
//  Provides a  maintenance dialog allowing  the user to  define which
//  bands are allowed from the band hopping schedule as defined here:
//
//    http://physics.princeton.edu/pulsar/K1JT/doc/wspr/wspr-main.html
//
//  Along with selecting bands a flag  indicating that a short tune up
//  signal is  required for specified  bands before they are  used for
//  receive.
//
//  Provides a Qt property that holds  the Tx percentage which is used
//  to generate a semi-randomized schedule of period to transmit. This
//  schedule is random but adjusted to limit the number of consecutive
//  transmission periods, it also adjusts  the schedule to ensure that
//  the overall number of transmission periods in any two hour hopping
//  schedule reflects the percentage provided.
//
// Collaborations
//
//  Settings including  the selected bands  with periods, the  tune up
//  flags  and the  gray line  duration are  maintained in  persistent
//  storage using the provided QSettings object instance.
//
//  A passed  in Configuration  object instance is  used to  query the
//  FrequencyList  model to  determine  working  frequencies for  each
//  band. The  row index  of this  model is  returned by  this classes
//  hopping scheduling method so it may be conveniently used to select
//  a new working frequency by a client.
//
class WSPRBandHopping
  : public QObject
{
  Q_OBJECT;
  Q_PROPERTY (int tx_percent READ tx_percent WRITE set_tx_percent);

public:
  WSPRBandHopping (QSettings *, Configuration const *, QWidget * parent = nullptr);
  ~WSPRBandHopping ();

  // display the band hopping maintenance dialog
  Q_SLOT void show_dialog (bool);

  // Property tx_percent implementation
  int tx_percent () const;
  Q_SLOT void set_tx_percent (int);

  // structure that defines the results of the next_hop() method
  struct Hop
  {
    QString period_name_;
    int frequencies_index_;     // may be -1 indicating no change
    bool tune_required_;
    bool tx_next_;
  };
  // return the next band parameters
  Hop next_hop (bool tx_enabled);
  // determine if the next period should be a transmit period
  bool next_is_tx ();

private:
  // implementation hidden from public interface
  class impl;
  pimpl<impl> m_;
};

#endif
