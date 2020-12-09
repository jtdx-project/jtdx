#include "Configuration.hpp"

//
// Read me!
//
// This file defines a configuration dialog with the user. The general
// strategy is to expose agreed  configuration parameters via a custom
// interface (See  Configuration.hpp). The state exposed  through this
// public   interface  reflects   stored  or   derived  data   in  the
// Configuration::impl object.   The Configuration::impl  structure is
// an implementation of the PIMPL (a.k.a.  Cheshire Cat or compilation
// firewall) implementation hiding idiom that allows internal state to
// be completely removed from the public interface.
//
// There  is a  secondary level  of parameter  storage which  reflects
// current settings UI  state, these parameters are not  copied to the
// state   store  that   the  public   interface  exposes   until  the
// Configuration:impl::accept() operation is  successful. The accept()
// operation is  tied to the settings  OK button. The normal  and most
// convenient place to store this intermediate settings UI state is in
// the data models of the UI  controls, if that is not convenient then
// separate member variables  must be used to store that  state. It is
// important for the user experience that no publicly visible settings
// are changed  while the  settings UI are  changed i.e.  all settings
// changes   must    be   deferred   until   the    "OK"   button   is
// clicked. Conversely, all changes must  be discarded if the settings
// UI "Cancel" button is clicked.
//
// There is  a complication related  to the radio interface  since the
// this module offers  the facility to test the  radio interface. This
// test means  that the  public visibility to  the radio  being tested
// must be  changed.  To  maintain the  illusion of  deferring changes
// until they  are accepted, the  original radio related  settings are
// stored upon showing  the UI and restored if the  UI is dismissed by
// canceling.
//
// It  should be  noted that  the  settings UI  lives as  long as  the
// application client that uses it does. It is simply shown and hidden
// as it  is needed rather than  creating it on demand.  This strategy
// saves a  lot of  expensive UI  drawing at the  expense of  a little
// storage and  gives a  convenient place  to deliver  settings values
// from.
//
// Here is an overview of the high level flow of this module:
//
// 1)  On  construction the  initial  environment  is initialized  and
// initial   values  for   settings  are   read  from   the  QSettings
// database. At  this point  default values for  any new  settings are
// established by  providing a  default value  to the  QSettings value
// queries. This should be the only place where a hard coded value for
// a   settings  item   is   defined.   Any   remaining  one-time   UI
// initialization is also done. At the end of the constructor a method
// initialize_models()  is called  to  load the  UI  with the  current
// settings values.
//
// 2) When the settings UI is displayed by a client calling the exec()
// operation, only temporary state need be stored as the UI state will
// already mirror the publicly visible settings state.
//
// 3) As  the user makes  changes to  the settings UI  only validation
// need be  carried out since the  UI control data models  are used as
// the temporary store of unconfirmed settings.  As some settings will
// depend  on each  other a  validate() operation  is available,  this
// operation implements a check of any complex multi-field values.
//
// 4) If the  user discards the settings changes by  dismissing the UI
// with the  "Cancel" button;  the reject()  operation is  called. The
// reject() operation calls initialize_models()  which will revert all
// the  UI visible  state  to  the values  as  at  the initial  exec()
// operation.  No   changes  are  moved   into  the  data   fields  in
// Configuration::impl that  reflect the  settings state  published by
// the public interface (Configuration.hpp).
//
// 5) If  the user accepts the  settings changes by dismissing  the UI
// with the "OK" button; the  accept() operation is called which calls
// the validate() operation  again and, if it passes,  the fields that
// are used  to deliver  the settings  state are  updated from  the UI
// control models  or other temporary  state variables. At the  end of
// the accept()  operation, just  before hiding  the UI  and returning
// control to the caller; the new  settings values are stored into the
// settings database by a call to the write_settings() operation, thus
// ensuring that  settings changes are  saved even if  the application
// crashes or is subsequently killed.
//
// 6)  On  destruction,  which   only  happens  when  the  application
// terminates,  the settings  are saved  to the  settings database  by
// calling the  write_settings() operation. This is  largely redundant
// but is still done to save the default values of any new settings on
// an initial run.
//
// To add a new setting:
//
// 1) Update the UI with the new widget to view and change the value.
//
// 2)  Add  a member  to  Configuration::impl  to store  the  accepted
// setting state. If the setting state is dynamic; add a new signal to
// broadcast the setting value.
//
// 3) Add a  query method to the  public interface (Configuration.hpp)
// to access the  new setting value. If the settings  is dynamic; this
// step  is optional  since  value  changes will  be  broadcast via  a
// signal.
//
// 4) Add a forwarding operation to implement the new query (3) above.
//
// 5)  Add a  settings read  call to  read_settings() with  a sensible
// default value. If  the setting value is dynamic, add  a signal emit
// call to broadcast the setting value change.
//
// 6) Add  code to  initialize_models() to  load the  widget control's
// data model with the current value.
//
// 7) If there is no convenient data model field, add a data member to
// store the proposed new value. Ensure  this member has a valid value
// on exit from read_settings().
//
// 8)  Add  any  required  inter-field validation  to  the  validate()
// operation.
//
// 9) Add code to the accept()  operation to extract the setting value
// from  the  widget   control  data  model  and  load   it  into  the
// Configuration::impl  member  that  reflects  the  publicly  visible
// setting state. If  the setting value is dynamic; add  a signal emit
// call to broadcast any changed state of the setting.
//
// 10) Add  a settings  write call  to save the  setting value  to the
// settings database.
//

#include <stdexcept>
#include <iterator>
#include <algorithm>
#include <functional>
#include <limits>
#include <cmath>

#include <QApplication>
#include <QMetaType>
#include <QList>
#include <QSettings>
#include <QAudioDeviceInfo>
#include <QAudioInput>
#include <QDialog>
#include <QAction>
#include <QFileDialog>
#include <QFileSystemModel>
#include <QDir>
#include <QTemporaryFile>
#include <QFormLayout>
#include <QString>
#include <QStringList>
#include <QStringListModel>
#include <QLineEdit>
#include <QRegExpValidator>
#include <QIntValidator>
#include <QThread>
#include <QTimer>
#include <QStandardPaths>
#include <QFont>
#include <QFontDialog>
#include <QColorDialog>
#include <QSerialPortInfo>
#include <QScopedPointer>
#include <QDebug>
#include <QtGui>
#include "qt_helpers.hpp"
#include "MetaDataRegistry.hpp"
#include "SettingsGroup.hpp"
#include "FrequencyLineEdit.hpp"
#include "FrequencyDeltaLineEdit.hpp"
#include "CandidateKeyFilter.hpp"
#include "ForeignKeyDelegate.hpp"
#include "FrequencyDelegate.hpp"
#include "FrequencyDeltaDelegate.hpp"
#include "TransceiverFactory.hpp"
#include "Transceiver.hpp"
#include "Bands.hpp"
#include "IARURegions.hpp"
#include "Modes.hpp"
#include "FrequencyList.hpp"
#include "StationList.hpp"
#include "NetworkServerLookup.hpp"
#include "JTDXMessageBox.hpp"

#include "pimpl_impl.hpp"

#include "ui_Configuration.h"
#include "moc_Configuration.cpp"

namespace
{
  // these undocumented flag values when stored in (Qt::UserRole - 1)
  // of a ComboBox item model index allow the item to be enabled or
  // disabled
  int const combo_box_item_enabled (32 | 1);
  int const combo_box_item_disabled (0);

  QRegExp message_alphabet {"[- @A-Za-z0-9+./?#<>&^]*"};

  // Magic numbers for file validation
  constexpr quint32 qrg_magic {0xadbccbdb};
  constexpr quint32 qrg_version {101}; // M.mm

}


//
// Dialog to get a new Frequency item
//
class FrequencyDialog final
  : public QDialog
{
  Q_OBJECT;
public:
  using Item = FrequencyList_v2::Item;

  explicit FrequencyDialog (IARURegions * regions_model, Modes * modes_model, QWidget * parent = nullptr)
    : QDialog {parent}
  {
    setWindowTitle (QApplication::applicationName () + " - " +
                    tr ("Add Frequency"));
    region_combo_box_.setModel (regions_model);
    mode_combo_box_.setModel (modes_model);

    auto form_layout = new QFormLayout ();
    form_layout->addRow (tr ("IARU &Region:"), &region_combo_box_);
    form_layout->addRow (tr ("&Mode:"), &mode_combo_box_);
    form_layout->addRow (tr ("&Frequency (MHz):"), &frequency_line_edit_);

    auto main_layout = new QVBoxLayout (this);
    main_layout->addLayout (form_layout);

    auto button_box = new QDialogButtonBox {QDialogButtonBox::Ok | QDialogButtonBox::Cancel};
    button_box->button(QDialogButtonBox::Ok)->setText(tr("&OK"));
    button_box->button(QDialogButtonBox::Cancel)->setText(tr("&Cancel"));
    main_layout->addWidget (button_box);

    connect (button_box, &QDialogButtonBox::accepted, this, &FrequencyDialog::accept);
    connect (button_box, &QDialogButtonBox::rejected, this, &FrequencyDialog::reject);
  }

  Item item () const
  {
    return {frequency_line_edit_.frequency (), Modes::value (mode_combo_box_.currentText ()), IARURegions::value (region_combo_box_.currentText ()),false};
  }

private:
  QComboBox region_combo_box_;
  QComboBox mode_combo_box_;
  FrequencyLineEdit frequency_line_edit_;
};


//
// Dialog to get a new Station item
//
class StationDialog final
  : public QDialog
{
  Q_OBJECT;
public:
  explicit StationDialog (StationList const * stations, Bands * bands, QWidget * parent = nullptr)
    : QDialog {parent}
    , filtered_bands_ {new CandidateKeyFilter {bands, stations, 0, 0}}
  {
    setWindowTitle (QApplication::applicationName () + " - " + tr ("Add Station"));

    band_.setModel (filtered_bands_.data ());
      
    auto form_layout = new QFormLayout ();
    form_layout->addRow (tr ("&Band:"), &band_);
    form_layout->addRow (tr ("&Offset (MHz):"), &delta_);
    form_layout->addRow (tr ("&Antenna:"), &description_);

    auto main_layout = new QVBoxLayout (this);
    main_layout->addLayout (form_layout);

    auto button_box = new QDialogButtonBox {QDialogButtonBox::Ok | QDialogButtonBox::Cancel};
    button_box->button(QDialogButtonBox::Ok)->setText(tr("&OK"));
    button_box->button(QDialogButtonBox::Cancel)->setText(tr("&Cancel"));
    main_layout->addWidget (button_box);

    connect (button_box, &QDialogButtonBox::accepted, this, &StationDialog::accept);
    connect (button_box, &QDialogButtonBox::rejected, this, &StationDialog::reject);

    if (delta_.text ().isEmpty ())
      {
        delta_.setText ("0");
      }
  }

  StationList::Station station () const
  {
    return {band_.currentText (), delta_.frequency_delta (), description_.text ()};
  }

  int exec () override
  {
    filtered_bands_->set_active_key ();
    return QDialog::exec ();
  }

private:
  QScopedPointer<CandidateKeyFilter> filtered_bands_;

  QComboBox band_;
  FrequencyDeltaLineEdit delta_;
  QLineEdit description_;
};

class RearrangableMacrosModel
  : public QStringListModel
{
public:
  Qt::ItemFlags flags (QModelIndex const& index) const override
  {
    auto flags = QStringListModel::flags (index);
    if (index.isValid ())
      {
        // disallow drop onto existing items
        flags &= ~Qt::ItemIsDropEnabled;
      }
    return flags;
  }
};


//
// Class MessageItemDelegate
//
//	Item delegate for message entry such as free text message macros.
//
class MessageItemDelegate final
  : public QStyledItemDelegate
{
public:
  explicit MessageItemDelegate (QObject * parent = nullptr)
    : QStyledItemDelegate {parent}
  {
  }

  QWidget * createEditor (QWidget * parent
                          , QStyleOptionViewItem const& /* option*/
                          , QModelIndex const& /* index */
                          ) const override
  {
    auto editor = new QLineEdit {parent};
    editor->setFrame (false);
    editor->setValidator (new QRegExpValidator {message_alphabet, editor});
    return editor;
  }
};

// Internal implementation of the Configuration class.
class Configuration::impl final
  : public QDialog
{
  Q_OBJECT;

public:
  using FrequencyDelta = Radio::FrequencyDelta;
  using port_type = Configuration::port_type;

  explicit impl (Configuration * self, QSettings * settings, QWidget * parent);
  ~impl ();

  bool have_rig ();

  void transceiver_frequency (Frequency);
  void transceiver_tx_frequency (Frequency);
  void transceiver_mode (MODE);
  void transceiver_ptt (bool);
  void transceiver_ft4_mode (bool);
  void sync_transceiver (bool force_signal);
 
  Q_SLOT int exec () override;
  Q_SLOT void accept () override;
  Q_SLOT void reject () override;
  Q_SLOT void done (int) override;

private:
  typedef QList<QAudioDeviceInfo> AudioDevices;

  void read_settings ();
  void write_settings ();

  bool load_audio_devices (QAudio::Mode, QComboBox *, QAudioDeviceInfo *);
  void update_audio_channels (QComboBox const *, int, QComboBox *, bool);

  void set_application_font (QFont const&);

  void initialize_models ();
  bool split_mode () const
  {
    return
      (WSJT_RIG_NONE_CAN_SPLIT || !rig_is_dummy_) &&
      (rig_params_.split_mode != TransceiverFactory::split_mode_none);
  }
  void set_cached_mode ();
  bool open_rig (bool force = false);
  //bool set_mode ();
  void close_rig ();
  TransceiverFactory::ParameterPack gather_rig_data ();
  void enumerate_rigs ();
  void set_rig_invariants ();
  bool validate ();
  void message_box_critical (QString const& reason, QString const& detail = QString ());
  void fill_port_combo_box (QComboBox *);
  Frequency apply_calibration (Frequency) const;
  Frequency remove_calibration (Frequency) const;

  void delete_frequencies ();
  void load_frequencies ();
  void merge_frequencies ();
  void save_frequencies ();
  void reset_frequencies ();
  void insert_frequency ();
  FrequencyList_v2::FrequencyItems read_frequencies_file (QString const&);

  void delete_stations ();
  void insert_station ();

  Q_SLOT void on_font_push_button_clicked ();
  Q_SLOT void on_decoded_text_font_push_button_clicked ();
  Q_SLOT void on_PTT_port_combo_box_activated (int);
  Q_SLOT void on_CAT_port_combo_box_activated (int);
  Q_SLOT void on_CAT_serial_baud_combo_box_currentIndexChanged (int);
  Q_SLOT void on_CAT_data_bits_button_group_buttonClicked (int);
  Q_SLOT void on_CAT_stop_bits_button_group_buttonClicked (int);
  Q_SLOT void on_CAT_handshake_button_group_buttonClicked (int);
  Q_SLOT void on_CAT_poll_interval_spin_box_valueChanged (int);
  Q_SLOT void on_split_mode_button_group_buttonClicked (int);
  Q_SLOT void on_test_CAT_push_button_clicked ();
  Q_SLOT void on_test_PTT_push_button_clicked (bool checked);
  Q_SLOT void on_force_DTR_combo_box_currentIndexChanged (int);
  Q_SLOT void on_force_RTS_combo_box_currentIndexChanged (int);
  Q_SLOT void on_rig_combo_box_currentIndexChanged (int);
  Q_SLOT void on_sound_input_combo_box_currentTextChanged (QString const&);
  Q_SLOT void on_sound_output_combo_box_currentTextChanged (QString const&);
  Q_SLOT void on_add_macro_push_button_clicked (bool = false);
  Q_SLOT void on_delete_macro_push_button_clicked (bool = false);
  Q_SLOT void on_PTT_method_button_group_buttonClicked (int);
  Q_SLOT void on_callsign_line_edit_textChanged ();
  Q_SLOT void on_grid_line_edit_textChanged ();
  Q_SLOT void on_content_line_edit_textChanged(QString const&);
  Q_SLOT void on_countries_line_edit_textChanged(QString const&);
  Q_SLOT void on_callsigns_line_edit_textChanged(QString const&);
  Q_SLOT void on_add_macro_line_edit_editingFinished ();
  Q_SLOT void delete_macro ();
  void delete_selected_macros (QModelIndexList);
  Q_SLOT void on_save_path_select_push_button_clicked (bool);
  Q_SLOT void on_content_reset_push_button_clicked ();
  Q_SLOT void on_countries_clear_push_button_clicked ();
  Q_SLOT void on_callsigns_clear_push_button_clicked ();
  Q_SLOT void handle_transceiver_update (TransceiverState const&, unsigned sequence_number);
  Q_SLOT void handle_transceiver_failure (QString const& reason);
  Q_SLOT void on_countryName_check_box_clicked(bool checked);
  Q_SLOT void on_callNotif_check_box_clicked(bool checked);
  Q_SLOT void on_otherMessagesMarker_check_box_clicked(bool checked);
  Q_SLOT void on_RR73_marker_check_box_clicked(bool checked);
  Q_SLOT void on_redMarker_check_box_clicked(bool checked);
  Q_SLOT void on_ShowCQ_check_box_clicked(bool checked);
  Q_SLOT void on_ShowCQRRR73_check_box_clicked(bool checked);
  Q_SLOT void on_ShowCQ73_check_box_clicked(bool checked);
  Q_SLOT void on_prompt_to_log_check_box_clicked(bool checked);
  Q_SLOT void on_autolog_check_box_clicked(bool checked);
  Q_SLOT void on_write_decoded_check_box_clicked(bool checked);
  Q_SLOT void on_write_decoded_debug_check_box_clicked(bool checked);
  Q_SLOT void on_txtColor_check_box_clicked(bool checked);
  Q_SLOT void on_workedStriked_check_box_clicked(bool checked);
  Q_SLOT void on_workedUnderlined_check_box_clicked(bool checked);
  Q_SLOT void on_workedColor_check_box_clicked(bool checked);
  Q_SLOT void on_workedDontShow_check_box_clicked(bool checked);
  Q_SLOT void on_newCQZ_check_box_clicked(bool checked);
  Q_SLOT void on_newITUZ_check_box_clicked(bool checked);
  Q_SLOT void on_newDXCC_check_box_clicked(bool checked);
  Q_SLOT void on_newCall_check_box_clicked(bool checked);
  Q_SLOT void on_newPx_check_box_clicked(bool checked);
  Q_SLOT void on_newGrid_check_box_clicked(bool checked);
  Q_SLOT void on_newCQZBand_check_box_clicked(bool checked);
  Q_SLOT void on_newITUZBand_check_box_clicked(bool checked);
  Q_SLOT void on_newDXCCBand_check_box_clicked(bool checked);
  Q_SLOT void on_newCallBand_check_box_clicked(bool checked);
  Q_SLOT void on_newPxBand_check_box_clicked(bool checked);
  Q_SLOT void on_newGridBand_check_box_clicked(bool checked);
  Q_SLOT void on_newCQZBandMode_check_box_clicked(bool checked);
  Q_SLOT void on_newITUZBandMode_check_box_clicked(bool checked);
  Q_SLOT void on_newDXCCBandMode_check_box_clicked(bool checked);
  Q_SLOT void on_newCallBandMode_check_box_clicked(bool checked);
  Q_SLOT void on_newPxBandMode_check_box_clicked(bool checked);
  Q_SLOT void on_newGridBandMode_check_box_clicked(bool checked);
  Q_SLOT void on_newPotential_check_box_clicked(bool checked);

  Q_SLOT void on_eqsluser_edit_textEdited(const QString &arg1);
  Q_SLOT void on_eqslpasswd_edit_textEdited(const QString &arg1);
  Q_SLOT void on_eqslnick_edit_textEdited(const QString &arg1);

  Q_SLOT void on_pbCQmsg_clicked();
  Q_SLOT void on_pbMyCall_clicked();
  Q_SLOT void on_pbTxMsg_clicked();
  Q_SLOT void on_pbNewCQZ_clicked();
  Q_SLOT void on_pbNewCQZBand_clicked();
  Q_SLOT void on_pbNewITUZ_clicked();
  Q_SLOT void on_pbNewITUZBand_clicked();
  Q_SLOT void on_pbNewDXCC_clicked();
  Q_SLOT void on_pbNewDXCCBand_clicked();
  Q_SLOT void on_pbNewCall_clicked();
  Q_SLOT void on_pbNewCallBand_clicked();
  Q_SLOT void on_pbNewPx_clicked();
  Q_SLOT void on_pbNewPxBand_clicked();
  Q_SLOT void on_pbNewGrid_clicked();
  Q_SLOT void on_pbNewGridBand_clicked();
  Q_SLOT void on_pbStandardCall_clicked();
  Q_SLOT void on_pbWorkedCall_clicked();

  Q_SLOT void on_hhComboBox_1_currentIndexChanged (int);
  Q_SLOT void on_mmComboBox_1_currentIndexChanged (int);
  Q_SLOT void on_hhComboBox_2_currentIndexChanged (int);
  Q_SLOT void on_mmComboBox_2_currentIndexChanged (int);
  Q_SLOT void on_hhComboBox_3_currentIndexChanged (int);
  Q_SLOT void on_mmComboBox_3_currentIndexChanged (int);
  Q_SLOT void on_hhComboBox_4_currentIndexChanged (int);
  Q_SLOT void on_mmComboBox_4_currentIndexChanged (int);
  Q_SLOT void on_hhComboBox_5_currentIndexChanged (int);
  Q_SLOT void on_mmComboBox_5_currentIndexChanged (int);
  
  Q_SLOT void on_bandComboBox_1_currentIndexChanged (QString const&);
  Q_SLOT void on_bandComboBox_2_currentIndexChanged (QString const&);
  Q_SLOT void on_bandComboBox_3_currentIndexChanged (QString const&);
  Q_SLOT void on_bandComboBox_4_currentIndexChanged (QString const&);
  Q_SLOT void on_bandComboBox_5_currentIndexChanged (QString const&);

  // typenames used as arguments must match registered type names :(
  Q_SIGNAL void start_transceiver (unsigned seqeunce_number) const;
  Q_SIGNAL void set_transceiver (Transceiver::TransceiverState const&,
                                 unsigned sequence_number) const;
  Q_SIGNAL void stop_transceiver () const;

  Configuration * const self_;	// back pointer to public interface

  QThread * transceiver_thread_;
  TransceiverFactory transceiver_factory_;
  QList<QMetaObject::Connection> rig_connections_;

  QScopedPointer<Ui::configuration_dialog> ui_;

  QSettings * settings_;

  JTDXDateTime * jtdxtime_;
  
  QDir doc_dir_;
  QDir data_dir_;
  QDir temp_dir_;
  QDir default_save_directory_;
  QDir save_directory_;

  QFont font_ = QGuiApplication::font ();
  QFont next_font_;

  QFont decoded_text_font_;
  QFont next_decoded_text_font_;

  bool restart_sound_input_device_;
  bool restart_sound_output_device_;

  Type2MsgGen type_2_msg_gen_;

  QStringListModel macros_;
  RearrangableMacrosModel next_macros_;
  QAction * macro_delete_action_;

  Bands bands_;
  IARURegions regions_;
  IARURegions::Region region_;
  Modes modes_;
  FrequencyList_v2 frequencies_;
  FrequencyList_v2 next_frequencies_;
  StationList stations_;
  StationList next_stations_;
  FrequencyDelta current_offset_;
  FrequencyDelta current_tx_offset_;

  QAction * frequency_delete_action_;
  QAction * frequency_insert_action_;
  QAction * load_frequencies_action_;
  QAction * save_frequencies_action_;
  QAction * merge_frequencies_action_;
  QAction * reset_frequencies_action_;
  FrequencyDialog * frequency_dialog_;

  QAction * station_delete_action_;
  QAction * station_insert_action_;
  StationDialog * station_dialog_;

  TransceiverFactory::ParameterPack rig_params_;
  TransceiverFactory::ParameterPack saved_rig_params_;
  TransceiverFactory::Capabilities::PortType last_port_type_;
  bool rig_is_dummy_;
  bool rig_active_;
  bool have_rig_;
  bool rig_changed_;
  TransceiverState cached_rig_state_;
  int rig_resolution_;          // see Transceiver::resolution signal
  double frequency_calibration_intercept_;
  double frequency_calibration_slope_ppm_;
  unsigned transceiver_command_number_;

  // configuration fields that we publish
  QString my_callsign_;
  QString my_grid_;
  QString timeFrom_;
  QString content_;
  QString countries_;
  QString callsigns_;
  QColor color_CQ_;
  QColor next_color_CQ_;
  QColor color_MyCall_;
  QColor next_color_MyCall_;
  QColor color_TxMsg_;
  QColor next_color_TxMsg_;
  QColor color_NewCQZ_;
  QColor next_color_NewCQZ_;
  QColor color_NewCQZBand_;
  QColor next_color_NewCQZBand_;
  QColor color_NewITUZ_;
  QColor next_color_NewITUZ_;
  QColor color_NewITUZBand_;
  QColor next_color_NewITUZBand_;
  QColor color_NewDXCC_;
  QColor next_color_NewDXCC_;
  QColor color_NewDXCCBand_;
  QColor next_color_NewDXCCBand_;
  QColor color_NewGrid_;
  QColor next_color_NewGrid_;
  QColor color_NewGridBand_;
  QColor next_color_NewGridBand_;
  QColor color_NewPx_;
  QColor next_color_NewPx_;
  QColor color_NewPxBand_;
  QColor next_color_NewPxBand_;
  QColor color_NewCall_;
  QColor next_color_NewCall_;
  QColor color_NewCallBand_;
  QColor next_color_NewCallBand_;
  QColor color_StandardCall_;
  QColor next_color_StandardCall_;
  QColor color_WorkedCall_;
  QColor next_color_WorkedCall_;
  qint32 id_interval_;
  qint32 ntrials_;
  qint32 ntrials10_;
  qint32 ntrialsrxf10_;
  qint32 npreampass_;
  qint32 aggressive_;
  qint32 harmonicsdepth_;
  qint32 nsingdecatt_;
  qint32 ntopfreq65_;
  qint32 nAnswerCQCounter_;
  qint32 nAnswerInCallCounter_;
  qint32 nSentRReportCounter_;
  qint32 nSentRR7373Counter_;
  double txDelay_;
  bool fmaskact_;
  bool answerCQCount_;
  bool answerInCallCount_;
  bool sentRReportCount_;
  bool sentRR7373Count_;
  bool strictdirCQ_;
  bool halttxreplyother_;
  bool hidefree_;
  bool hide2ndHash_;
  bool showcq_;
  bool showcqrrr73_;
  bool showcq73_;
  bool enableContent_;
  bool enableCountryFilter_;
  bool enableCallsignFilter_;
  bool do_snr_;
  bool do_pwr_;
  bool rig_power_;
  bool id_after_73_;
  bool tx_QSY_allowed_;
  bool spot_to_psk_reporter_;
  bool spot_to_dxsummit_;
  bool prevent_spotting_false_;
  bool filterUDP_;
  bool send_to_eqsl_;
  QString eqsl_username_;
  QString eqsl_passwd_;  
  QString eqsl_nickname_;
  bool usesched_;
  QString sched_hh_1_;
  QString sched_mm_1_;
  QString sched_band_1_;
  bool sched_mix_1_;
  QString sched_hh_2_;
  QString sched_mm_2_;
  QString sched_band_2_;
  bool sched_mix_2_;
  QString sched_hh_3_;
  QString sched_mm_3_;
  QString sched_band_3_;
  bool sched_mix_3_;
  QString sched_hh_4_;
  QString sched_mm_4_;
  QString sched_band_4_;
  bool sched_mix_4_;
  QString sched_hh_5_;
  QString sched_mm_5_;
  QString sched_band_5_;
  bool sched_mix_5_;
  bool monitor_off_at_startup_;
  bool monitor_last_used_;
  bool log_as_RTTY_;
  bool report_in_comments_;
  bool prompt_to_log_;
  bool autolog_;
  bool insert_blank_;
  bool useDarkStyle_;
  bool countryName_;
  bool countryPrefix_;
  bool callNotif_;
  bool gridNotif_;
  bool otherMessagesMarker_;
  bool RR73Marker_;
  bool redMarker_;
  bool blueMarker_;
  bool txtColor_;
  bool workedColor_;
  bool workedStriked_;
  bool workedUnderlined_;
  bool workedDontShow_;
  bool newCQZ_;
  bool newCQZBand_;
  bool newCQZBandMode_;
  bool newITUZ_;
  bool newITUZBand_;
  bool newITUZBandMode_;
  bool newDXCC_;
  bool newDXCCBand_;
  bool newDXCCBandMode_;
  bool newCall_;
  bool newCallBand_;
  bool newCallBandMode_;
  bool newPx_;
  bool newPxBand_;
  bool newPxBandMode_;
  bool newGrid_;
  bool newGridBand_;
  bool newGridBandMode_;
  bool newPotential_;
  bool hideAfrica_;
  bool hideAntarctica_;
  bool hideAsia_;
  bool hideEurope_;
  bool hideOceania_;
  bool hideNAmerica_;
  bool hideSAmerica_;
  bool next_txtColor_;
  bool next_workedColor_;
  bool next_workedStriked_;
  bool next_workedUnderlined_;
  bool next_workedDontShow_;
  bool next_newCQZ_;
  bool next_newCQZBand_;
  bool next_newCQZBandMode_;
  bool next_newITUZ_;
  bool next_newITUZBand_;
  bool next_newITUZBandMode_;
  bool next_newDXCC_;
  bool next_newDXCCBand_;
  bool next_newDXCCBandMode_;
  bool next_newCall_;
  bool next_newCallBand_;
  bool next_newCallBandMode_;
  bool next_newPx_;
  bool next_newPxBand_;
  bool next_newPxBandMode_;
  bool next_newGrid_;
  bool next_newGridBand_;
  bool next_newGridBandMode_;
  bool next_newPotential_;
  bool clear_DX_;
  bool clear_DX_exit_;
  bool miles_;
  int watchdog_;
  int tunetimer_;
  bool TX_messages_;
  bool hide_TX_messages_;
  bool decode_at_52s_;
  bool beepOnMyCall_;
  bool beepOnNewCQZ_;
  bool beepOnNewITUZ_;
  bool beepOnNewDXCC_;
  bool beepOnNewGrid_;
  bool beepOnNewCall_;
  bool beepOnNewPx_;
  bool beepOnFirstMsg_;
  QString udp_server_name_;
  port_type udp_server_port_;
  QString tcp_server_name_;
  port_type tcp_server_port_;
  QString udp2_server_name_;
  port_type udp2_server_port_;
  bool accept_udp_requests_;
  bool enable_udp1_adif_sending_;
  bool enable_tcp_connection_;
  bool enable_udp2_broadcast_;
  bool write_decoded_;
  bool write_decoded_debug_;
  bool udpWindowToFront_;
  bool udpWindowRestore_;
  DataMode data_mode_;
  bool pwrBandTxMemory_;
  bool pwrBandTuneMemory_;

  QAudioDeviceInfo audio_input_device_;
  bool default_audio_input_device_selected_;
  AudioDevice::Channel audio_input_channel_;
  QAudioDeviceInfo audio_output_device_;
  bool default_audio_output_device_selected_;
  AudioDevice::Channel audio_output_channel_;
  
  

  friend class Configuration;
};

#include "Configuration.moc"


// delegate to implementation class
Configuration::Configuration (QSettings * settings, QWidget * parent)
  : m_ {this, settings, parent}
{
}

Configuration::~Configuration ()
{
}

QDir Configuration::doc_dir () const {return m_->doc_dir_;}
QDir Configuration::data_dir () const {return m_->data_dir_;}
QDir Configuration::temp_dir () const {return m_->temp_dir_;}

int Configuration::exec () {return m_->exec ();}
bool Configuration::is_active () const {return m_->isVisible ();}

QAudioDeviceInfo const& Configuration::audio_input_device () const {return m_->audio_input_device_;}
AudioDevice::Channel Configuration::audio_input_channel () const {return m_->audio_input_channel_;}
QAudioDeviceInfo const& Configuration::audio_output_device () const {return m_->audio_output_device_;}
AudioDevice::Channel Configuration::audio_output_channel () const {return m_->audio_output_channel_;}
bool Configuration::restart_audio_input () const {return m_->restart_sound_input_device_;}
bool Configuration::restart_audio_output () const {return m_->restart_sound_output_device_;}
auto Configuration::type_2_msg_gen () const -> Type2MsgGen {return m_->type_2_msg_gen_;}
QString Configuration::my_callsign () const {return m_->my_callsign_;}
QString Configuration::my_grid () const {return m_->my_grid_;}
QString Configuration::timeFrom () const {return m_->timeFrom_;}
QString Configuration::content () const {return m_->content_;}
QString Configuration::countries () const {return m_->countries_;}
QString Configuration::callsigns () const {return m_->callsigns_;}
QString Configuration::hideContinents () const
{
  QString result;
  if (m_->hideAfrica_) result.append("AF");
  if (m_->hideAntarctica_) result.append("AN");
  if (m_->hideEurope_) result.append("EU");
  if (m_->hideAsia_) result.append("AS");
  if (m_->hideSAmerica_) result.append("SA");
  if (m_->hideOceania_) result.append("OC");
  if (m_->hideNAmerica_) result.append("NA");
  return result;
}
QColor Configuration::color_CQ () const {return m_->color_CQ_;}
QColor Configuration::color_MyCall () const {return m_->color_MyCall_;}
QColor Configuration::color_TxMsg () const {return m_->color_TxMsg_;}
QColor Configuration::color_NewCQZ () const {return m_->color_NewCQZ_;}
QColor Configuration::color_NewCQZBand () const {return m_->color_NewCQZBand_;}
QColor Configuration::color_NewITUZ () const {return m_->color_NewITUZ_;}
QColor Configuration::color_NewITUZBand () const {return m_->color_NewITUZBand_;}
QColor Configuration::color_NewDXCC () const {return m_->color_NewDXCC_;}
QColor Configuration::color_NewDXCCBand () const {return m_->color_NewDXCCBand_;}
QColor Configuration::color_NewGrid () const {return m_->color_NewGrid_;}
QColor Configuration::color_NewGridBand () const {return m_->color_NewGridBand_;}
QColor Configuration::color_NewCall () const {return m_->color_NewCall_;}
QColor Configuration::color_NewCallBand () const {return m_->color_NewCallBand_;}
QColor Configuration::color_NewPx () const {return m_->color_NewPx_;}
QColor Configuration::color_NewPxBand () const {return m_->color_NewPxBand_;}
QColor Configuration::color_StandardCall () const {return m_->color_StandardCall_;}
QColor Configuration::color_WorkedCall () const {return m_->color_WorkedCall_;}
QFont Configuration::decoded_text_font () const {return m_->decoded_text_font_;}
qint32 Configuration::id_interval () const {return m_->id_interval_;}
qint32 Configuration::ntrials() const {return m_->ntrials_;}
qint32 Configuration::ntrials10() const {return m_->ntrials10_;}
qint32 Configuration::ntrialsrxf10() const {return m_->ntrialsrxf10_;}
qint32 Configuration::npreampass() const {return m_->npreampass_;}
qint32 Configuration::aggressive() const {return m_->aggressive_;}
qint32 Configuration::harmonicsdepth() const {return m_->harmonicsdepth_;}
qint32 Configuration::nsingdecatt() const {return m_->nsingdecatt_;}
qint32 Configuration::ntopfreq65() const {return m_->ntopfreq65_;}
qint32 Configuration::nAnswerCQCounter() const {return m_->nAnswerCQCounter_;}
qint32 Configuration::nAnswerInCallCounter() const {return m_->nAnswerInCallCounter_;}
qint32 Configuration::nSentRReportCounter() const {return m_->nSentRReportCounter_;}
qint32 Configuration::nSentRR7373Counter() const {return m_->nSentRR7373Counter_;}
double Configuration::txDelay() const {return m_->txDelay_;}
bool Configuration::fmaskact () const {return m_->fmaskact_;}
bool Configuration::answerCQCount () const {return m_->answerCQCount_;}
bool Configuration::answerInCallCount () const {return m_->answerInCallCount_;}
bool Configuration::sentRReportCount () const {return m_->sentRReportCount_;}
bool Configuration::sentRR7373Count () const {return m_->sentRR7373Count_;}
bool Configuration::strictdirCQ () const {return m_->strictdirCQ_;}
bool Configuration::halttxreplyother () const {return m_->halttxreplyother_;}
bool Configuration::hidefree () const {return m_->hidefree_;}
bool Configuration::hide2ndHash () const {return m_->hide2ndHash_;}
bool Configuration::showcq () const {return m_->showcq_;}
bool Configuration::showcqrrr73 () const {return m_->showcqrrr73_;}
bool Configuration::showcq73 () const {return m_->showcq73_;}
bool Configuration::enableContent () const {return m_->enableContent_;}
bool Configuration::enableCountryFilter () const {return m_->enableCountryFilter_;}
bool Configuration::enableCallsignFilter () const {return m_->enableCallsignFilter_;}
bool Configuration::do_snr () const {return m_->do_snr_;}
bool Configuration::do_pwr () const {return m_->do_pwr_;}
bool Configuration::rig_power () const {return m_->rig_power_;}
bool Configuration::id_after_73 () const {return m_->id_after_73_;}
bool Configuration::tx_QSY_allowed () const {return m_->tx_QSY_allowed_;}
bool Configuration::spot_to_psk_reporter () const
{
  // rig must be open and working to spot externally
  return is_transceiver_online () && m_->spot_to_psk_reporter_;
}
bool Configuration::spot_to_dxsummit () const {return m_->spot_to_dxsummit_;}
bool Configuration::prevent_spotting_false () const {return m_->prevent_spotting_false_;}
bool Configuration::filterUDP () const {return m_->filterUDP_;}
bool Configuration::monitor_off_at_startup () const {return m_->monitor_off_at_startup_;}
bool Configuration::monitor_last_used () const {return m_->rig_is_dummy_ || m_->monitor_last_used_;}
bool Configuration::log_as_RTTY () const {return m_->log_as_RTTY_;}
bool Configuration::send_to_eqsl () const {return m_->send_to_eqsl_;}
QString Configuration::eqsl_username () const {return m_->eqsl_username_;}
QString Configuration::eqsl_passwd () const {return m_->eqsl_passwd_;}
QString Configuration::eqsl_nickname () const {return m_->eqsl_nickname_;}
bool Configuration::usesched () const {return m_->usesched_;}
QString Configuration::sched_hh_1 () const {return m_->sched_hh_1_;}
QString Configuration::sched_mm_1 () const {return m_->sched_mm_1_;}
QString Configuration::sched_band_1 () const {return m_->sched_band_1_;}
bool Configuration::sched_mix_1 () const {return m_->sched_mix_1_;}
QString Configuration::sched_hh_2 () const {return m_->sched_hh_2_;}
QString Configuration::sched_mm_2 () const {return m_->sched_mm_2_;}
QString Configuration::sched_band_2 () const {return m_->sched_band_2_;}
bool Configuration::sched_mix_2 () const {return m_->sched_mix_2_;}
QString Configuration::sched_hh_3 () const {return m_->sched_hh_3_;}
QString Configuration::sched_mm_3 () const {return m_->sched_mm_3_;}
QString Configuration::sched_band_3 () const {return m_->sched_band_3_;}
bool Configuration::sched_mix_3 () const {return m_->sched_mix_3_;}
QString Configuration::sched_hh_4 () const {return m_->sched_hh_4_;}
QString Configuration::sched_mm_4 () const {return m_->sched_mm_4_;}
QString Configuration::sched_band_4 () const {return m_->sched_band_4_;}
bool Configuration::sched_mix_4 () const {return m_->sched_mix_4_;}
QString Configuration::sched_hh_5 () const {return m_->sched_hh_5_;}
QString Configuration::sched_mm_5 () const {return m_->sched_mm_5_;}
QString Configuration::sched_band_5 () const {return m_->sched_band_5_;}
bool Configuration::sched_mix_5 () const {return m_->sched_mix_5_;}
bool Configuration::report_in_comments () const {return m_->report_in_comments_;}
bool Configuration::prompt_to_log () const {return m_->prompt_to_log_;}
bool Configuration::autolog () const {return m_->autolog_;}
bool Configuration::insert_blank () const {return m_->insert_blank_;}
bool Configuration::useDarkStyle () const {return m_->useDarkStyle_;}
bool Configuration::countryName () const {return m_->countryName_;}
bool Configuration::countryPrefix () const {return m_->countryPrefix_;}
bool Configuration::callNotif () const {return m_->callNotif_;}
bool Configuration::gridNotif () const {return m_->gridNotif_;}
bool Configuration::otherMessagesMarker () const {return m_->otherMessagesMarker_;}
bool Configuration::RR73Marker () const {return m_->RR73Marker_;}
bool Configuration::redMarker () const {return m_->redMarker_;}
bool Configuration::blueMarker () const {return m_->blueMarker_;}
bool Configuration::txtColor () const {return m_->txtColor_;}
bool Configuration::workedColor () const {return m_->workedColor_;}
bool Configuration::workedStriked () const {return m_->workedStriked_;}
bool Configuration::workedUnderlined () const {return m_->workedUnderlined_;}
bool Configuration::workedDontShow () const {return m_->workedDontShow_;}
bool Configuration::newCQZ () const {return m_->newCQZ_;}
bool Configuration::newCQZBand () const {return m_->newCQZBand_;}
bool Configuration::newCQZBandMode () const {return m_->newCQZBandMode_;}
bool Configuration::newITUZ () const {return m_->newITUZ_;}
bool Configuration::newITUZBand () const {return m_->newITUZBand_;}
bool Configuration::newITUZBandMode () const {return m_->newITUZBandMode_;}
bool Configuration::newDXCC () const {return m_->newDXCC_;}
bool Configuration::newDXCCBand () const {return m_->newDXCCBand_;}
bool Configuration::newDXCCBandMode () const {return m_->newDXCCBandMode_;}
bool Configuration::newCall () const {return m_->newCall_;}
bool Configuration::newCallBand () const {return m_->newCallBand_;}
bool Configuration::newCallBandMode () const {return m_->newCallBandMode_;}
bool Configuration::newPx () const {return m_->newPx_;}
bool Configuration::newPxBand () const {return m_->newPxBand_;}
bool Configuration::newPxBandMode () const {return m_->newPxBandMode_;}
bool Configuration::newGrid () const {return m_->newGrid_;}
bool Configuration::newGridBand () const {return m_->newGridBand_;}
bool Configuration::newGridBandMode () const {return m_->newGridBandMode_;}
bool Configuration::newPotential () const {return m_->newPotential_;}
bool Configuration::clear_DX () const {return m_->clear_DX_;}
bool Configuration::clear_DX_exit () const {return m_->clear_DX_exit_;}
bool Configuration::miles () const {return m_->miles_;}
int Configuration::watchdog () const {return m_->watchdog_;}
int Configuration::tunetimer () const {return m_->tunetimer_;}
bool Configuration::TX_messages () const {return m_->TX_messages_;}
bool Configuration::hide_TX_messages () const {return m_->hide_TX_messages_;}
bool Configuration::decode_at_52s () const {return m_->decode_at_52s_;}
bool Configuration::beepOnMyCall () const {return m_->beepOnMyCall_;}
bool Configuration::beepOnNewCQZ () const {return m_->beepOnNewCQZ_;}
bool Configuration::beepOnNewITUZ () const {return m_->beepOnNewITUZ_;}
bool Configuration::beepOnNewDXCC () const {return m_->beepOnNewDXCC_;}
bool Configuration::beepOnNewGrid () const {return m_->beepOnNewGrid_;}
bool Configuration::beepOnNewCall () const {return m_->beepOnNewCall_;}
bool Configuration::beepOnNewPx () const {return m_->beepOnNewPx_;}
bool Configuration::beepOnFirstMsg () const {return m_->beepOnFirstMsg_;}
bool Configuration::split_mode () const {return m_->split_mode ();}
QString Configuration::udp_server_name () const {return m_->udp_server_name_;}
auto Configuration::udp_server_port () const -> port_type {return m_->udp_server_port_;}
QString Configuration::udp2_server_name () const {return m_->udp2_server_name_;}
auto Configuration::udp2_server_port () const -> port_type {return m_->udp2_server_port_;}
QString Configuration::tcp_server_name () const {return m_->tcp_server_name_;}
auto Configuration::tcp_server_port () const -> port_type {return m_->tcp_server_port_;}
bool Configuration::accept_udp_requests () const {return m_->accept_udp_requests_;}
bool Configuration::enable_udp1_adif_sending () const {return m_->enable_udp1_adif_sending_;}
bool Configuration::enable_udp2_broadcast () const {return m_->enable_udp2_broadcast_;}
bool Configuration::enable_tcp_connection () const {return m_->enable_tcp_connection_;}
bool Configuration::write_decoded () const {return m_->write_decoded_;}
bool Configuration::write_decoded_debug () const {return m_->write_decoded_debug_;}
bool Configuration::udpWindowToFront () const {return m_->udpWindowToFront_;}
bool Configuration::udpWindowRestore () const {return m_->udpWindowRestore_;}
Bands * Configuration::bands () {return &m_->bands_;}
Bands const * Configuration::bands () const {return &m_->bands_;}
StationList * Configuration::stations () {return &m_->stations_;}
StationList const * Configuration::stations () const {return &m_->stations_;}
IARURegions::Region Configuration::region () const {return m_->region_;}
FrequencyList_v2 * Configuration::frequencies () {return &m_->frequencies_;}
FrequencyList_v2 const * Configuration::frequencies () const {return &m_->frequencies_;}
QStringListModel * Configuration::macros () {return &m_->macros_;}
QStringListModel const * Configuration::macros () const {return &m_->macros_;}
QDir Configuration::save_directory () const {return m_->save_directory_;}
QString Configuration::rig_name () const {return m_->rig_params_.rig_name;}
bool Configuration::pwrBandTxMemory () const {return m_->pwrBandTxMemory_;}
bool Configuration::pwrBandTuneMemory () const {return m_->pwrBandTuneMemory_;}

bool Configuration::is_transceiver_online () const
{
  return m_->rig_active_;
}

bool Configuration::is_dummy_rig () const
{
  return m_->rig_is_dummy_;
}

bool Configuration::transceiver_online ()
{
#if WSJT_TRACE_CAT
  qDebug () << "Configuration::transceiver_online: " << m_->cached_rig_state_;
#endif

  return m_->have_rig ();
}

int Configuration::transceiver_resolution () const
{
  return m_->rig_resolution_;
}

void Configuration::transceiver_offline ()
{
#if WSJT_TRACE_CAT
  qDebug () << "Configuration::transceiver_offline:" << m_->cached_rig_state_;
#endif

  m_->close_rig ();
}

void Configuration::transceiver_frequency (Frequency f)
{
#if WSJT_TRACE_CAT
  qDebug () << "Configuration::transceiver_frequency:" << f << m_->cached_rig_state_;
#endif
  m_->transceiver_frequency (f);
}

void Configuration::transceiver_tx_frequency (Frequency f)
{
#if WSJT_TRACE_CAT
  qDebug () << "Configuration::transceiver_tx_frequency:" << f << m_->cached_rig_state_;
#endif

  m_->transceiver_tx_frequency (f);
}

void Configuration::transceiver_mode (MODE mode)
{
#if WSJT_TRACE_CAT
  qDebug () << "Configuration::transceiver_mode:" << mode << m_->cached_rig_state_;
#endif

  m_->transceiver_mode (mode);
}

void Configuration::transceiver_ptt (bool on)
{
#if WSJT_TRACE_CAT
  qDebug () << "Configuration::transceiver_ptt:" << on << m_->cached_rig_state_;
#endif

  m_->transceiver_ptt (on);
}

void Configuration::transceiver_ft4_mode (bool on)
{
#if WSJT_TRACE_CAT
  qDebug () << "Configuration::transceiver_ft4_mode:" << on << m_->cached_rig_state_;
#endif

  m_->transceiver_ft4_mode (on);
}

void Configuration::sync_transceiver (bool force_signal, bool enforce_mode_and_split)
{
#if WSJT_TRACE_CAT
  qDebug () << "Configuration::sync_transceiver: force signal:" << force_signal << "enforce_mode_and_split:" << enforce_mode_and_split << m_->cached_rig_state_;
#endif

  m_->sync_transceiver (force_signal);
  if (!enforce_mode_and_split)
    {
      m_->transceiver_tx_frequency (0);
    }
}

bool Configuration::valid_udp2 () const
{
  auto server_name = m_->udp2_server_name_;
  auto port_number = m_->udp2_server_port_;
  return(!(server_name.trimmed().isEmpty() || port_number == 0));
}

namespace
{
#if defined (Q_OS_MAC)
  char const * app_root = "/../../../";
#else
  char const * app_root = "/../";
#endif
  QString doc_path ()
  {
#if CMAKE_BUILD
    if (QDir::isRelativePath (CMAKE_INSTALL_DOCDIR))
      {
        return QApplication::applicationDirPath () + app_root + CMAKE_INSTALL_DOCDIR;
      }
    return CMAKE_INSTALL_DOCDIR;
#else
    return QApplication::applicationDirPath ();
#endif
  }

  QString data_path ()
  {
#if CMAKE_BUILD
    if (QDir::isRelativePath (CMAKE_INSTALL_DATADIR))
      {
        return QApplication::applicationDirPath () + app_root + CMAKE_INSTALL_DATADIR + QChar {'/'} + CMAKE_PROJECT_NAME;
      }
    return CMAKE_INSTALL_DATADIR;
#else
    return QApplication::applicationDirPath ();
#endif
  }
}

Configuration::impl::impl (Configuration * self, QSettings * settings, QWidget * parent)
  : QDialog {parent}
  , self_ {self}
  , ui_ {new Ui::configuration_dialog}
  , settings_ {settings}
  , doc_dir_ {doc_path ()}
  , data_dir_ {data_path ()}
  , restart_sound_input_device_ {false}
  , restart_sound_output_device_ {false}
  , frequencies_ {&bands_}
  , next_frequencies_ {&bands_}
  , stations_ {&bands_}
  , next_stations_ {&bands_}
  , current_offset_ {0}
  , current_tx_offset_ {0}
  , frequency_dialog_ {new FrequencyDialog {&regions_, &modes_, this}}
  , station_dialog_ {new StationDialog {&next_stations_, &bands_, this}}
  , last_port_type_ {TransceiverFactory::Capabilities::none}
  , rig_is_dummy_ {false}
  , rig_active_ {false}
  , have_rig_ {false}
  , rig_changed_ {false}
  , rig_resolution_ {0}
  , transceiver_command_number_ {0}
  , default_audio_input_device_selected_ {false}
  , default_audio_output_device_selected_ {false}
{
  ui_->setupUi (this);

  {
    ui_->configuration_dialog_button_box->button(QDialogButtonBox::Ok)->setText(tr("&OK"));
    ui_->configuration_dialog_button_box->button(QDialogButtonBox::Cancel)->setText(tr("&Cancel"));
    // Create a temporary directory in a suitable location
    QString temp_location {QStandardPaths::writableLocation (QStandardPaths::TempLocation)};
    if (!temp_location.isEmpty ())
      {
        temp_dir_.setPath (temp_location);
      }

    bool ok {false};
    QString unique_directory {QApplication::applicationName ()};
    do
      {
        if (!temp_dir_.mkpath (unique_directory)
            || !temp_dir_.cd (unique_directory))
          {
            JTDXMessageBox::critical_message (this, "JTDX", tr ("Create temporary directory error: ") + temp_dir_.absolutePath ());
            throw std::runtime_error {"Failed to create a temporary directory"};
          }
        if (!temp_dir_.isReadable () || !(ok = QTemporaryFile {temp_dir_.absoluteFilePath ("test")}.open ()))
          {
            if (JTDXMessageBox::Cancel == JTDXMessageBox::critical_message (this, "JTDX",
                                                              tr ("Create temporary directory error:\n%1\n"
                                                                  "Another application may be locking the directory").arg (temp_dir_.absolutePath ()),"","",
                                                              JTDXMessageBox::Retry | JTDXMessageBox::Cancel))
              {
                throw std::runtime_error {"Failed to create a usable temporary directory"};
              }
            temp_dir_.cdUp ();  // revert to parent as this one is no good
          }
      }
    while (!ok);
  }

  {
    // Find a suitable data file location
    QDir data_dir {QStandardPaths::writableLocation (QStandardPaths::DataLocation)};
    if (!data_dir.mkpath ("."))
      {
        JTDXMessageBox::critical_message (this, "JTDX", tr ("Create data directory error: ") + data_dir.absolutePath ());
        throw std::runtime_error {"Failed to create data directory"};
      }

    // Make sure the default save directory exists
    QString save_dir {"save"};
    default_save_directory_ = data_dir;
    if (!default_save_directory_.mkpath (save_dir) || !default_save_directory_.cd (save_dir))
      {
        JTDXMessageBox::critical_message (this, "JTDX", tr ("Create Directory", "Cannot create directory \"") +
                               default_save_directory_.absoluteFilePath (save_dir) + "\".");
        throw std::runtime_error {"Failed to create save directory"};
      }

    // we now have a deafult save path that exists

    // make sure samples directory exists
    QString samples_dir {"samples"};
    if (!default_save_directory_.mkpath (samples_dir))
      {
        JTDXMessageBox::critical_message (this, "JTDX", tr ("Create Directory", "Cannot create directory \"") +
                               default_save_directory_.absoluteFilePath (samples_dir) + "\".");
        throw std::runtime_error {"Failed to create save directory"};
      }

    // copy in any new sample files to the sample directory
    QDir dest_dir {default_save_directory_};
    dest_dir.cd (samples_dir);
    
    QDir source_dir {":/" + samples_dir};
    source_dir.cd (save_dir);
    source_dir.cd (samples_dir);
    auto list = source_dir.entryInfoList (QStringList {{"*.wav"}}, QDir::Files | QDir::Readable);
    Q_FOREACH (auto const& item, list)
      {
        if (!dest_dir.exists (item.fileName ()))
          {
            QFile file {item.absoluteFilePath ()};
            file.copy (dest_dir.absoluteFilePath (item.fileName ()));
          }
      }
  }

  // this must be done after the default paths above are set
  read_settings ();

  //
  // validation
  //
  ui_->callsign_line_edit->setValidator (new QRegExpValidator {QRegExp {"[A-Za-z0-9/-]+"}, this});
  ui_->grid_line_edit->setValidator (new QRegExpValidator {QRegExp {"[A-Ra-r]{2,2}[0-9]{2,2}[A-Xa-x]{0,2}[0-9]{0,2}"}, this});
  ui_->logTime_line_edit->setValidator (new QRegExpValidator {QRegExp {"[0-9]+"}, this});
  ui_->content_line_edit->setValidator (new QRegExpValidator {QRegExp {"[A-Za-z0-9,]+"}, this});
  ui_->countries_line_edit->setValidator (new QRegExpValidator {QRegExp {"[A-Za-z0-9,/*]+"}, this});
  ui_->callsigns_line_edit->setValidator (new QRegExpValidator {QRegExp {"[A-Za-z0-9,]+"}, this});
  ui_->add_macro_line_edit->setValidator (new QRegExpValidator {message_alphabet, this});

  ui_->udp_server_port_spin_box->setMinimum (1);
  ui_->udp_server_port_spin_box->setMaximum (std::numeric_limits<port_type>::max ());
  ui_->tcp_server_port_spin_box->setMinimum (1);
  ui_->tcp_server_port_spin_box->setMaximum (std::numeric_limits<port_type>::max ());
  ui_->udp2_server_port_spin_box->setMinimum (1);
  ui_->udp2_server_port_spin_box->setMaximum (std::numeric_limits<port_type>::max ());

  // Dependent checkboxes 
  ui_->countryPrefix_check_box->setChecked(countryName_ && countryPrefix_);
  ui_->countryPrefix_check_box->setEnabled(countryName_);
  ui_->gridNotif_check_box->setChecked(callNotif_ && gridNotif_);
  ui_->gridNotif_check_box->setEnabled(callNotif_);
  ui_->blueMarker_check_box->setChecked(redMarker_ && blueMarker_);
  ui_->blueMarker_check_box->setEnabled(redMarker_);

  ui_->workedColor_check_box->setChecked((newCQZ_ || newITUZ_ || newDXCC_ || newGrid_ || newPx_ || newCall_) && workedColor_);
  ui_->workedColor_check_box->setEnabled(newCQZ_ || newITUZ_ || newDXCC_ || newGrid_ || newPx_ || newCall_);
  ui_->workedStriked_check_box->setChecked((newCQZ_ || newITUZ_ || newDXCC_ || newGrid_ || newPx_ || newCall_) && !workedUnderlined_ && workedStriked_);
  ui_->workedStriked_check_box->setEnabled((newCQZ_ || newITUZ_ || newDXCC_ || newGrid_ || newPx_ || newCall_) && !workedUnderlined_);
  ui_->workedUnderlined_check_box->setChecked((newCQZ_ || newITUZ_ || newDXCC_ || newGrid_ || newPx_ || newCall_) && !workedStriked_ && workedUnderlined_);
  ui_->workedUnderlined_check_box->setEnabled((newCQZ_ || newITUZ_ || newDXCC_ || newGrid_ || newPx_ || newCall_) && !workedStriked_);
  ui_->workedDontShow_check_box->setChecked((newCQZ_ || newITUZ_ || newDXCC_ || newGrid_ || newPx_ || newCall_) && workedDontShow_);
  ui_->workedDontShow_check_box->setEnabled(newCQZ_ || newITUZ_ || newDXCC_ || newGrid_ || newPx_ || newCall_);

  ui_->newCQZBand_check_box->setChecked(newCQZ_ && newCQZBand_);
  ui_->newCQZBand_check_box->setEnabled(newCQZ_);
  ui_->newITUZBand_check_box->setChecked(newITUZ_ && newITUZBand_);
  ui_->newITUZBand_check_box->setEnabled(newITUZ_);
  ui_->newDXCCBand_check_box->setChecked(newDXCC_ && newDXCCBand_);
  ui_->newDXCCBand_check_box->setEnabled(newDXCC_);
  ui_->newCallBand_check_box->setChecked(newCall_ && newCallBand_);
  ui_->newCallBand_check_box->setEnabled(newCall_);
  ui_->newPxBand_check_box->setChecked(newPx_ && newPxBand_);
  ui_->newPxBand_check_box->setEnabled(newPx_);
  ui_->newGridBand_check_box->setChecked(newGrid_ && newGridBand_);
  ui_->newGridBand_check_box->setEnabled(newGrid_);
  ui_->newCQZBandMode_check_box->setChecked(newCQZ_ && newCQZBandMode_);
  ui_->newCQZBandMode_check_box->setEnabled(newCQZ_);
  ui_->newITUZBandMode_check_box->setChecked(newITUZ_ && newITUZBandMode_);
  ui_->newITUZBandMode_check_box->setEnabled(newITUZ_);
  ui_->newDXCCBandMode_check_box->setChecked(newDXCC_ && newDXCCBandMode_);
  ui_->newDXCCBandMode_check_box->setEnabled(newDXCC_);
  ui_->newCallBandMode_check_box->setChecked(newCall_ && newCallBandMode_);
  ui_->newCallBandMode_check_box->setEnabled(newCall_);
  ui_->newPxBandMode_check_box->setChecked(newPx_ && newPxBandMode_);
  ui_->newPxBandMode_check_box->setEnabled(newPx_);
  ui_->newGridBandMode_check_box->setChecked(newGrid_ && newGridBandMode_);
  ui_->newGridBandMode_check_box->setEnabled(newGrid_);
  ui_->beep_on_newCQZ_check_box->setChecked(newCQZ_ && beepOnNewCQZ_);
  ui_->beep_on_newCQZ_check_box->setEnabled(newCQZ_);
  ui_->beep_on_newITUZ_check_box->setChecked(newITUZ_ && beepOnNewITUZ_);
  ui_->beep_on_newITUZ_check_box->setEnabled(newITUZ_);
  ui_->beep_on_newDXCC_check_box->setChecked(newDXCC_ && beepOnNewDXCC_);
  ui_->beep_on_newDXCC_check_box->setEnabled(newDXCC_);
  ui_->beep_on_newCall_check_box->setChecked(newCall_ && beepOnNewCall_);
  ui_->beep_on_newCall_check_box->setEnabled(newCall_);
  ui_->beep_on_newPx_check_box->setChecked(newPx_ && beepOnNewPx_);
  ui_->beep_on_newPx_check_box->setEnabled(newPx_);
  ui_->beep_on_newGrid_check_box->setChecked(newGrid_ && beepOnNewGrid_);
  ui_->beep_on_newGrid_check_box->setEnabled(newGrid_);
  if(content_.isEmpty ()) {
     ui_->enableContent_check_box->setChecked(false);
     ui_->enableContent_check_box->setEnabled(false);
  }
  if(countries_.isEmpty ()) {
     ui_->enableCountryFilter_check_box->setChecked(false);
     ui_->enableCountryFilter_check_box->setEnabled(false);
  }
  if(callsigns_.isEmpty ()) {
  ui_->enableCallsignFilter_check_box->setChecked(false);
  ui_->enableCallsignFilter_check_box->setEnabled(false);
  }
  //
  // assign ids to radio buttons
  //
  ui_->CAT_data_bits_button_group->setId (ui_->CAT_default_bit_radio_button, TransceiverFactory::default_data_bits);
  ui_->CAT_data_bits_button_group->setId (ui_->CAT_7_bit_radio_button, TransceiverFactory::seven_data_bits);
  ui_->CAT_data_bits_button_group->setId (ui_->CAT_8_bit_radio_button, TransceiverFactory::eight_data_bits);

  ui_->CAT_stop_bits_button_group->setId (ui_->CAT_default_stop_bit_radio_button, TransceiverFactory::default_stop_bits);
  ui_->CAT_stop_bits_button_group->setId (ui_->CAT_one_stop_bit_radio_button, TransceiverFactory::one_stop_bit);
  ui_->CAT_stop_bits_button_group->setId (ui_->CAT_two_stop_bit_radio_button, TransceiverFactory::two_stop_bits);

  ui_->CAT_handshake_button_group->setId (ui_->CAT_handshake_default_radio_button, TransceiverFactory::handshake_default);
  ui_->CAT_handshake_button_group->setId (ui_->CAT_handshake_none_radio_button, TransceiverFactory::handshake_none);
  ui_->CAT_handshake_button_group->setId (ui_->CAT_handshake_xon_radio_button, TransceiverFactory::handshake_XonXoff);
  ui_->CAT_handshake_button_group->setId (ui_->CAT_handshake_hardware_radio_button, TransceiverFactory::handshake_hardware);

  ui_->PTT_method_button_group->setId (ui_->PTT_VOX_radio_button, TransceiverFactory::PTT_method_VOX);
  ui_->PTT_method_button_group->setId (ui_->PTT_CAT_radio_button, TransceiverFactory::PTT_method_CAT);
  ui_->PTT_method_button_group->setId (ui_->PTT_DTR_radio_button, TransceiverFactory::PTT_method_DTR);
  ui_->PTT_method_button_group->setId (ui_->PTT_RTS_radio_button, TransceiverFactory::PTT_method_RTS);

  ui_->TX_audio_source_button_group->setId (ui_->TX_source_mic_radio_button, TransceiverFactory::TX_audio_source_front);
  ui_->TX_audio_source_button_group->setId (ui_->TX_source_data_radio_button, TransceiverFactory::TX_audio_source_rear);

  ui_->TX_mode_button_group->setId (ui_->mode_none_radio_button, data_mode_none);
  ui_->TX_mode_button_group->setId (ui_->mode_USB_radio_button, data_mode_USB);
  ui_->TX_mode_button_group->setId (ui_->mode_data_radio_button, data_mode_data);

  ui_->split_mode_button_group->setId (ui_->split_none_radio_button, TransceiverFactory::split_mode_none);
  ui_->split_mode_button_group->setId (ui_->split_rig_radio_button, TransceiverFactory::split_mode_rig);
  ui_->split_mode_button_group->setId (ui_->split_emulate_radio_button, TransceiverFactory::split_mode_emulate);

  //
  // setup PTT port combo box drop down content
  //
  fill_port_combo_box (ui_->PTT_port_combo_box);
  ui_->PTT_port_combo_box->addItem ("CAT");

  //
  // setup hooks to keep audio channels aligned with devices
  //
  {
    using namespace std;
    using namespace std::placeholders;

    function<void (int)> cb (bind (&Configuration::impl::update_audio_channels, this, ui_->sound_input_combo_box, _1, ui_->sound_input_channel_combo_box, false));
    connect (ui_->sound_input_combo_box, static_cast<void (QComboBox::*)(int)> (&QComboBox::currentIndexChanged), cb);
    cb = bind (&Configuration::impl::update_audio_channels, this, ui_->sound_output_combo_box, _1, ui_->sound_output_channel_combo_box, true);
    connect (ui_->sound_output_combo_box, static_cast<void (QComboBox::*)(int)> (&QComboBox::currentIndexChanged), cb);
  }

  //
  // setup macros list view
  //
  ui_->macros_list_view->setModel (&next_macros_);
  ui_->macros_list_view->setItemDelegate (new MessageItemDelegate {this});

  macro_delete_action_ = new QAction {tr ("&Delete"), ui_->macros_list_view};
  ui_->macros_list_view->insertAction (nullptr, macro_delete_action_);
  connect (macro_delete_action_, &QAction::triggered, this, &Configuration::impl::delete_macro);


  // setup IARU region combo box model
  ui_->region_combo_box->setModel (&regions_);

  //
  // setup working frequencies table model & view
  //
  frequencies_.sort (FrequencyList_v2::frequency_column);

  ui_->frequencies_table_view->setModel (&next_frequencies_);
  ui_->frequencies_table_view->horizontalHeader ()->setSectionResizeMode (QHeaderView::ResizeToContents);
  ui_->frequencies_table_view->verticalHeader ()->setSectionResizeMode (QHeaderView::ResizeToContents);
  ui_->frequencies_table_view->sortByColumn (FrequencyList_v2::frequency_column, Qt::AscendingOrder);
  ui_->frequencies_table_view->setColumnHidden (FrequencyList_v2::frequency_mhz_column, true);
  ui_->frequencies_table_view->setColumnHidden (FrequencyList_v2::mode_frequency_mhz_column, true);

  // delegates
  ui_->frequencies_table_view->setItemDelegateForColumn (FrequencyList_v2::frequency_column, new FrequencyDelegate {this});
  ui_->frequencies_table_view->setItemDelegateForColumn (FrequencyList_v2::region_column, new ForeignKeyDelegate {&regions_, 0, this});
  ui_->frequencies_table_view->setItemDelegateForColumn (FrequencyList_v2::mode_column, new ForeignKeyDelegate {&modes_, 0, this});

  // actions
  frequency_delete_action_ = new QAction {tr ("&Delete"), ui_->frequencies_table_view};
  ui_->frequencies_table_view->insertAction (nullptr, frequency_delete_action_);
  connect (frequency_delete_action_, &QAction::triggered, this, &Configuration::impl::delete_frequencies);

  frequency_insert_action_ = new QAction {tr ("&Insert ..."), ui_->frequencies_table_view};
  ui_->frequencies_table_view->insertAction (nullptr, frequency_insert_action_);
  connect (frequency_insert_action_, &QAction::triggered, this, &Configuration::impl::insert_frequency);

  load_frequencies_action_ = new QAction {tr ("&Load ..."), ui_->frequencies_table_view};
  ui_->frequencies_table_view->insertAction (nullptr, load_frequencies_action_);
  connect (load_frequencies_action_, &QAction::triggered, this, &Configuration::impl::load_frequencies);

  save_frequencies_action_ = new QAction {tr ("&Save as ..."), ui_->frequencies_table_view};
  ui_->frequencies_table_view->insertAction (nullptr, save_frequencies_action_);
  connect (save_frequencies_action_, &QAction::triggered, this, &Configuration::impl::save_frequencies);

  merge_frequencies_action_ = new QAction {tr ("&Merge ..."), ui_->frequencies_table_view};
  ui_->frequencies_table_view->insertAction (nullptr, merge_frequencies_action_);
  connect (merge_frequencies_action_, &QAction::triggered, this, &Configuration::impl::merge_frequencies);

  reset_frequencies_action_ = new QAction {tr ("&Reset"), ui_->frequencies_table_view};
  ui_->frequencies_table_view->insertAction (nullptr, reset_frequencies_action_);
  connect (reset_frequencies_action_, &QAction::triggered, this, &Configuration::impl::reset_frequencies);


  // Schedulers
  
  ui_->bandComboBox_1->setModel(&next_frequencies_);
  ui_->bandComboBox_1->setModelColumn(FrequencyList_v2::mode_frequency_mhz_column);
  ui_->bandComboBox_2->setModel(&next_frequencies_);
  ui_->bandComboBox_2->setModelColumn(FrequencyList_v2::mode_frequency_mhz_column);
  ui_->bandComboBox_3->setModel(&next_frequencies_);
  ui_->bandComboBox_3->setModelColumn(FrequencyList_v2::mode_frequency_mhz_column);
  ui_->bandComboBox_4->setModel(&next_frequencies_);
  ui_->bandComboBox_4->setModelColumn(FrequencyList_v2::mode_frequency_mhz_column);
  ui_->bandComboBox_5->setModel(&next_frequencies_);
  ui_->bandComboBox_5->setModelColumn(FrequencyList_v2::mode_frequency_mhz_column);
  //
  // setup stations table model & view
  //
  stations_.sort (StationList::band_column);

  ui_->stations_table_view->setModel (&next_stations_);
  ui_->stations_table_view->sortByColumn (StationList::band_column, Qt::AscendingOrder);

  // delegates
  auto stations_item_delegate = new QStyledItemDelegate {this};
  stations_item_delegate->setItemEditorFactory (item_editor_factory ());
  ui_->stations_table_view->setItemDelegate (stations_item_delegate);
  ui_->stations_table_view->setItemDelegateForColumn (StationList::band_column, new ForeignKeyDelegate {&bands_, &next_stations_, 0, StationList::band_column, this});

  // actions
  station_delete_action_ = new QAction {tr ("&Delete"), ui_->stations_table_view};
  ui_->stations_table_view->insertAction (nullptr, station_delete_action_);
  connect (station_delete_action_, &QAction::triggered, this, &Configuration::impl::delete_stations);

  station_insert_action_ = new QAction {tr ("&Insert ..."), ui_->stations_table_view};
  ui_->stations_table_view->insertAction (nullptr, station_insert_action_);
  connect (station_insert_action_, &QAction::triggered, this, &Configuration::impl::insert_station);

  //
  // load combo boxes with audio setup choices
  //
  default_audio_input_device_selected_ = load_audio_devices (QAudio::AudioInput, ui_->sound_input_combo_box, &audio_input_device_);
  default_audio_output_device_selected_ = load_audio_devices (QAudio::AudioOutput, ui_->sound_output_combo_box, &audio_output_device_);

  update_audio_channels (ui_->sound_input_combo_box, ui_->sound_input_combo_box->currentIndex (), ui_->sound_input_channel_combo_box, false);
  update_audio_channels (ui_->sound_output_combo_box, ui_->sound_output_combo_box->currentIndex (), ui_->sound_output_channel_combo_box, true);

  ui_->sound_input_channel_combo_box->setCurrentIndex (audio_input_channel_);
  ui_->sound_output_channel_combo_box->setCurrentIndex (audio_output_channel_);

  restart_sound_input_device_ = false;
  restart_sound_output_device_ = false;

  enumerate_rigs ();
  initialize_models ();

  transceiver_thread_ = new QThread {this};
  transceiver_thread_->start ();

}

Configuration::impl::~impl ()
{
  transceiver_thread_->quit ();
  transceiver_thread_->wait ();
  write_settings ();
  temp_dir_.removeRecursively (); // clean up temp files
}

void Configuration::impl::initialize_models ()
{
  auto pal = ui_->callsign_line_edit->palette ();
  if (my_callsign_.isEmpty ())
    {
      pal.setColor (QPalette::Base, Radio::convert_dark("#ffccff",useDarkStyle_));
    }
  else
    {
      pal.setColor (QPalette::Base, Radio::convert_dark("#ffffff",useDarkStyle_));
    }
  ui_->callsign_line_edit->setPalette (pal);
  ui_->grid_line_edit->setPalette (pal);
  ui_->callsign_line_edit->setText (my_callsign_);
  ui_->grid_line_edit->setText (my_grid_);
  ui_->logTime_line_edit->setText (timeFrom_);
  ui_->content_line_edit->setText (content_);
  ui_->countries_line_edit->setText (countries_);
  ui_->callsigns_line_edit->setText (callsigns_);
  ui_->labTx->setStyleSheet(QString("background: %1").arg(Radio::convert_dark(color_TxMsg_.name(),useDarkStyle_)));
  ui_->test_PTT_push_button->setStyleSheet(QString("QPushButton:checked { background-color: %1; border-style: outset; border-width: 1px; border-radius: 5px; border-color: %2; min-width: 5em; padding: 3px;}").arg(Radio::convert_dark("#ff0000",useDarkStyle_),Radio::convert_dark("#000000",useDarkStyle_)));
  ui_->macros_list_view->setStyleSheet(QString("QListView { show-decoration-selected: 1; } "
"QListView::item:alternate { background: %1; } "
"QListView::item:selected { border: 1px solid %2; } "
"QListView::item:selected:!active { background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1, stop: 0 %3, stop: 1 %4); } "
"QListView::item:selected:active { background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1, stop: 0 %5, stop: 1 %6); } "
"QListView::item:hover { background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1, stop: 0 %7, stop: 1 %8); }").arg(Radio::convert_dark("#eeeeee",useDarkStyle_),Radio::convert_dark("#6a6ea9",useDarkStyle_),
Radio::convert_dark("#abafe5",useDarkStyle_),Radio::convert_dark("#8588b2",useDarkStyle_),
Radio::convert_dark("#6a6ea9",useDarkStyle_),Radio::convert_dark("#888dd9",useDarkStyle_),
Radio::convert_dark("#fafbfe",useDarkStyle_),Radio::convert_dark("#dcdef1",useDarkStyle_)));
  ui_->save_path_display_label->setStyleSheet(QString("background-color: %1").arg(Radio::convert_dark("#ffffff",useDarkStyle_)));
  next_txtColor_ = txtColor_;
  next_workedColor_ = workedColor_;
  next_workedStriked_ = workedStriked_;
  next_workedUnderlined_ = workedUnderlined_;
  next_workedDontShow_ = workedDontShow_;
  next_newCQZ_ = newCQZ_;
  next_newCQZBand_ = newCQZBand_;
  next_newCQZBandMode_ = newCQZBandMode_;
  next_newITUZ_ = newITUZ_;
  next_newITUZBand_ = newITUZBand_;
  next_newITUZBandMode_ = newITUZBandMode_;
  next_newDXCC_ = newDXCC_;
  next_newDXCCBand_ = newDXCCBand_;
  next_newDXCCBandMode_ = newDXCCBandMode_;
  next_newGrid_ = newGrid_;
  next_newGridBand_ = newGridBand_;
  next_newGridBandMode_ = newGridBandMode_;
  next_newPx_ = newPx_;
  next_newPxBand_ = newPxBand_;
  next_newPxBandMode_ = newPxBandMode_;
  next_newCall_ = newCall_;
  next_newCallBand_ = newCallBand_;
  next_newCallBandMode_ = newCallBandMode_;
  next_newPotential_ = newPotential_;
  if (txtColor_){
    ui_->labCQ->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(color_CQ_.name(),useDarkStyle_),Radio::convert_dark("#ffffff",useDarkStyle_)));
    ui_->labMyCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(color_MyCall_.name(),useDarkStyle_),Radio::convert_dark("#ffffff",useDarkStyle_)));
    ui_->labStandardCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_),Radio::convert_dark("#ffffff",useDarkStyle_)));
    ui_->labNewCQZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    ui_->labNewCQZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    ui_->labNewITUZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    ui_->labNewITUZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    ui_->labNewDXCC->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    ui_->labNewDXCCBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    ui_->labNewGrid->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    ui_->labNewGridBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    ui_->labNewPx->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    ui_->labNewPxBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    ui_->labNewCall->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    ui_->labNewCallBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    if (workedColor_) {
      if (workedStriked_) {
        ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark(color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark(color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark(color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
      } else if (workedUnderlined_) {
        ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark(color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark(color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark(color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
      } else {
        ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
      }
    } else if (workedStriked_) {
      ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    } else if (workedUnderlined_) {
      ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    } else {
      ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    }
    ui_->labNewMcCQZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcCQZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcITUZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcITUZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcDXCC->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcDXCCBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcGrid->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcGridBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcPx->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcPxBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcCall->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcCallBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewScCQZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScCQZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScITUZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScITUZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScDXCC->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScDXCCBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScGrid->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScGridBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScPx->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScPxBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScCall->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScCallBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
  } else {
    ui_->labCQ->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(color_CQ_.name(),useDarkStyle_),Radio::convert_dark("#ffffff",useDarkStyle_)));
    ui_->labMyCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(color_MyCall_.name(),useDarkStyle_),Radio::convert_dark("#ffffff",useDarkStyle_)));
    ui_->labStandardCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_),Radio::convert_dark("#ffffff",useDarkStyle_)));
    ui_->labNewCQZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    ui_->labNewCQZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    ui_->labNewITUZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    ui_->labNewITUZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    ui_->labNewDXCC->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    ui_->labNewDXCCBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    ui_->labNewGrid->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    ui_->labNewGridBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    ui_->labNewPx->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    ui_->labNewPxBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    ui_->labNewCall->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    ui_->labNewCallBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
    if (workedColor_) {
      if (workedStriked_) {
        ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark(color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark(color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark(color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
      } else if (workedUnderlined_) {
        ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark(color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark(color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark(color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
      } else {
        ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
      }
    } else if (workedStriked_) {
      ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    } else if (workedUnderlined_) {
      ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    } else {
      ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    }
    ui_->labNewMcCQZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcCQZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcITUZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcITUZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcDXCC->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcDXCCBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcGrid->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcGridBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcPx->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcPxBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcCall->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcCallBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewScCQZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScCQZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScITUZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScITUZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScDXCC->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScDXCCBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScGrid->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScGridBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScPx->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScPxBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScCall->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScCallBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(color_StandardCall_.name(),useDarkStyle_)));
  }
  ui_->labNewCQZ->setVisible(newCQZ_);
  ui_->labNewCQZBand->setVisible(newCQZBandMode_ || newCQZBand_);
  ui_->labNewITUZ->setVisible(newITUZ_);
  ui_->labNewITUZBand->setVisible(newITUZBandMode_ || newITUZBand_);
  ui_->labNewDXCC->setVisible(newDXCC_);
  ui_->labNewDXCCBand->setVisible(newDXCCBandMode_ || newDXCCBand_);
  ui_->labNewGrid->setVisible(newGrid_);
  ui_->labNewGridBand->setVisible(newGridBandMode_ || newGridBand_);
  ui_->labNewPx->setVisible(newPx_);
  ui_->labNewPxBand->setVisible(newPxBandMode_ || newPxBand_);
  ui_->labNewCall->setVisible(newCall_);
  ui_->labNewCallBand->setVisible(newCallBandMode_ || newCallBand_);
  ui_->labWorkedCall->setVisible(newCQZ_ || newITUZ_ || newDXCC_ || newGrid_ || newCall_);
  ui_->labNewMcCQZ->setVisible(newCQZ_);
  ui_->labNewMcCQZBand->setVisible(newCQZBandMode_ || newCQZBand_);
  ui_->labNewMcITUZ->setVisible(newITUZ_);
  ui_->labNewMcITUZBand->setVisible(newITUZBandMode_ || newITUZBand_);
  ui_->labNewMcDXCC->setVisible(newDXCC_);
  ui_->labNewMcDXCCBand->setVisible(newDXCCBandMode_ || newDXCCBand_);
  ui_->labNewMcGrid->setVisible(newGrid_);
  ui_->labNewMcGridBand->setVisible(newGridBandMode_ || newGridBand_);
  ui_->labNewMcPx->setVisible(newPx_);
  ui_->labNewMcPxBand->setVisible(newPxBandMode_ || newPxBand_);
  ui_->labNewMcCall->setVisible(newCall_);
  ui_->labNewMcCallBand->setVisible(newCallBandMode_ || newCallBand_);
  ui_->labWorkedMcCall->setVisible(newCQZ_ || newITUZ_ || newDXCC_ || newGrid_ || newPx_ || newCall_);
  ui_->labStandardCall->setVisible(newPotential_);
//  ui_->labStandardCall->setVisible((!newCQZ_ && !newITUZ_ && !newDXCC_ && !newGrid_ && !newCall_) && newPotential_);
//  ui_->labCQ->setVisible(!newCQZ_ && !newITUZ_ && !newDXCC_ && !newGrid_ && !newCall_);
//  ui_->labMyCall->setVisible(!newCQZ_ && !newITUZ_ && !newDXCC_ && !newGrid_ && !newCall_);
  ui_->labNewScCQZ->setVisible(newCQZ_ && newPotential_);
  ui_->labNewScCQZBand->setVisible((newCQZBandMode_ || newCQZBand_) && newPotential_);
  ui_->labNewScITUZ->setVisible(newITUZ_ && newPotential_);
  ui_->labNewScITUZBand->setVisible((newITUZBandMode_ || newITUZBand_) && newPotential_);
  ui_->labNewScDXCC->setVisible(newDXCC_ && newPotential_);
  ui_->labNewScDXCCBand->setVisible((newDXCCBandMode_ || newDXCCBand_) && newPotential_);
  ui_->labNewScGrid->setVisible(newGrid_ && newPotential_);
  ui_->labNewScGridBand->setVisible((newGridBandMode_ || newGridBand_) && newPotential_);
  ui_->labNewScPx->setVisible(newPx_ && newPotential_);
  ui_->labNewScPxBand->setVisible((newPxBandMode_ || newPxBand_) && newPotential_);
  ui_->labNewScCall->setVisible(newCall_ && newPotential_);
  ui_->labNewScCallBand->setVisible((newCallBandMode_ || newCallBand_) && newPotential_);
  ui_->labWorkedScCall->setVisible((newCQZ_ || newITUZ_ || newDXCC_ || newGrid_ || newPx_ || newCall_) && newPotential_);

  ui_->CW_id_interval_spin_box->setValue (id_interval_);  
  ui_->sbNtrials->setValue (ntrials_);
  ui_->sbTxDelay->setValue (txDelay_);
  ui_->sbNtrials10->setValue (ntrials10_);
  ui_->sbNtrialsRXF10->setValue (ntrialsrxf10_);
  ui_->sbNpreampass->setValue (npreampass_);
  ui_->sbAggressive->setValue (aggressive_);
  ui_->sbHarmonics->setValue (harmonicsdepth_);
  ui_->sbNsingdecatt->setValue (nsingdecatt_);
  ui_->fMask_check_box->setChecked (fmaskact_);
  ui_->answerCQCount_checkBox->setChecked (answerCQCount_);
  ui_->answerInCallCount_checkBox->setChecked (answerInCallCount_);
  ui_->sentRReportCount_checkBox->setChecked (sentRReportCount_);
  ui_->sentRR7373Count_checkBox->setChecked (sentRR7373Count_);
  ui_->strictDirCQ_checkBox->setChecked (strictdirCQ_);
  ui_->haltTxReplyOther_checkBox->setChecked (halttxreplyother_);
  ui_->HideFree_check_box->setChecked (hidefree_);
  ui_->Hide2ndHash_check_box->setChecked (hide2ndHash_);
  ui_->ShowCQ_check_box->setChecked (showcq_);
  ui_->ShowCQRRR73_check_box->setChecked (showcqrrr73_);
  ui_->ShowCQ73_check_box->setChecked (showcq73_);
  ui_->enableContent_check_box->setChecked (enableContent_);
  ui_->enableCountryFilter_check_box->setChecked (enableCountryFilter_);
  ui_->enableCallsignFilter_check_box->setChecked (enableCallsignFilter_);
  ui_->S_meter_check_box->setChecked (do_snr_);
  ui_->output_power_check_box->setChecked (do_pwr_);
  ui_->rig_power_check_box->setChecked (rig_power_);
  ui_->sbTopFreq->setValue (ntopfreq65_);
  ui_->sbAnswerCQCounter->setValue (nAnswerCQCounter_);
  ui_->sbAnswerInCallCounter->setValue (nAnswerInCallCounter_);
  ui_->sbSentRReportCounter->setValue (nSentRReportCounter_);
  ui_->sbSentRR7373Counter->setValue (nSentRR7373Counter_);
  ui_->label_11->setText ("<a><img src=\":/decpasses.png\" height=\"464\" /></a>");
  ui_->label_13->setText ("<a><img src=\":/dtrange.png\" height=\"165\" /></a>");

  ui_->PTT_method_button_group->button (rig_params_.ptt_type)->setChecked (true);
  ui_->save_path_display_label->setText (save_directory_.absolutePath ());
  ui_->CW_id_after_73_check_box->setChecked (id_after_73_);
  ui_->tx_QSY_check_box->setChecked (tx_QSY_allowed_);
  ui_->psk_reporter_check_box->setChecked (spot_to_psk_reporter_);
  ui_->dxsummit_check_box->setChecked (spot_to_dxsummit_);
  ui_->preventFalseUDP_check_box->setChecked (prevent_spotting_false_);
  ui_->filterUDP_check_box->setChecked (filterUDP_);
  ui_->eqsluser_edit->setText (eqsl_username_);
  ui_->eqslpasswd_edit->setText (eqsl_passwd_);
  ui_->eqslnick_edit->setText (eqsl_nickname_);
  if(!eqsl_username_.isEmpty () && !eqsl_passwd_.isEmpty () && !eqsl_nickname_.isEmpty ()) {
    ui_->eqsl_check_box->setEnabled (true);
    ui_->eqsl_check_box->setChecked (send_to_eqsl_);
  }
  ui_->UseSched_check_box->setChecked (usesched_);
  ui_->hhComboBox_1->setCurrentText (sched_hh_1_);
  ui_->mmComboBox_1->setCurrentText (sched_mm_1_);
  ui_->bandComboBox_1->setCurrentText (sched_band_1_);
  ui_->band_mix_check_box_1->setChecked (sched_mix_1_);
  ui_->hhComboBox_2->setCurrentText (sched_hh_2_);
  ui_->mmComboBox_2->setCurrentText (sched_mm_2_);
  ui_->bandComboBox_2->setCurrentText (sched_band_2_);
  ui_->band_mix_check_box_2->setChecked (sched_mix_2_);
  ui_->hhComboBox_3->setCurrentText (sched_hh_3_);
  ui_->mmComboBox_3->setCurrentText (sched_mm_3_);
  ui_->bandComboBox_3->setCurrentText (sched_band_3_);
  ui_->band_mix_check_box_3->setChecked (sched_mix_3_);
  ui_->hhComboBox_4->setCurrentText (sched_hh_4_);
  ui_->mmComboBox_4->setCurrentText (sched_mm_4_);
  ui_->bandComboBox_4->setCurrentText (sched_band_4_);
  ui_->band_mix_check_box_4->setChecked (sched_mix_4_);
  ui_->hhComboBox_5->setCurrentText (sched_hh_5_);
  ui_->mmComboBox_5->setCurrentText (sched_mm_5_);
  ui_->bandComboBox_5->setCurrentText (sched_band_5_);
  ui_->band_mix_check_box_5->setChecked (sched_mix_5_);
  ui_->monitor_off_check_box->setChecked (monitor_off_at_startup_);
  ui_->monitor_last_used_check_box->setChecked (monitor_last_used_);
  ui_->log_as_RTTY_check_box->setChecked (log_as_RTTY_);
  ui_->report_in_comments_check_box->setChecked (report_in_comments_);
  ui_->prompt_to_log_check_box->setChecked (prompt_to_log_);
  ui_->autolog_check_box->setChecked (autolog_);
  ui_->insert_blank_check_box->setChecked (insert_blank_);
  ui_->useDarkStyle_check_box->setChecked (useDarkStyle_);
  ui_->countryName_check_box->setChecked (countryName_);
  ui_->countryPrefix_check_box->setChecked (countryName_ && countryPrefix_);
  ui_->callNotif_check_box->setChecked (callNotif_);
  ui_->gridNotif_check_box->setChecked (gridNotif_ && callNotif_);
  ui_->otherMessagesMarker_check_box->setChecked (otherMessagesMarker_);
  ui_->RR73_marker_check_box->setChecked (RR73Marker_);
  ui_->redMarker_check_box->setChecked (redMarker_);
  ui_->blueMarker_check_box->setChecked (redMarker_ && blueMarker_);
  ui_->txtColor_check_box->setChecked (txtColor_);
  ui_->workedColor_check_box->setChecked (workedColor_ && (newCQZ_ || newITUZ_ || newDXCC_ || newCall_ || newPx_ || newGrid_));
  ui_->workedStriked_check_box->setChecked (!workedUnderlined_ && workedStriked_ && (newCQZ_ || newITUZ_ || newDXCC_ || newCall_ || newPx_ || newGrid_));
  ui_->workedUnderlined_check_box->setChecked (!workedStriked_ && workedUnderlined_ && (newCQZ_ || newITUZ_ || newDXCC_ || newCall_ || newPx_ || newGrid_));
  ui_->workedDontShow_check_box->setChecked (workedDontShow_ && (newCQZ_ || newITUZ_ || newDXCC_ || newCall_ || newPx_ || newGrid_));
  ui_->newCQZ_check_box->setChecked (newCQZ_);
  ui_->newCQZBand_check_box->setChecked (newCQZBand_ && newCQZ_);
  ui_->newCQZBandMode_check_box->setChecked (newCQZBandMode_ && newCQZ_);
  ui_->newITUZ_check_box->setChecked (newITUZ_);
  ui_->newITUZBand_check_box->setChecked (newITUZBand_ && newITUZ_);
  ui_->newITUZBandMode_check_box->setChecked (newITUZBandMode_ && newITUZ_);
  ui_->newDXCC_check_box->setChecked (newDXCC_);
  ui_->newDXCCBand_check_box->setChecked (newDXCCBand_ && newDXCC_);
  ui_->newDXCCBandMode_check_box->setChecked (newDXCCBandMode_ && newDXCC_);
  ui_->newCall_check_box->setChecked (newCall_);
  ui_->newCallBand_check_box->setChecked (newCallBand_ && newCall_);
  ui_->newCallBandMode_check_box->setChecked (newCallBandMode_ && newCall_);
  ui_->newPx_check_box->setChecked (newPx_);
  ui_->newPxBand_check_box->setChecked (newPxBand_ && newPx_);
  ui_->newPxBandMode_check_box->setChecked (newPxBandMode_ && newPx_);
  ui_->newGrid_check_box->setChecked (newGrid_);
  ui_->newGridBand_check_box->setChecked (newGridBand_ && newGrid_);
  ui_->newGridBandMode_check_box->setChecked (newGridBandMode_ && newGrid_);
  ui_->newPotential_check_box->setChecked (newPotential_);
  ui_->Africa_check_box->setChecked (hideAfrica_);
  ui_->Antarctica_check_box->setChecked (hideAntarctica_);
  ui_->Asia_check_box->setChecked (hideAsia_);
  ui_->Europe_check_box->setChecked (hideEurope_);
  ui_->Oceania_check_box->setChecked (hideOceania_);
  ui_->NAmerica_check_box->setChecked (hideNAmerica_);
  ui_->SAmerica_check_box->setChecked (hideSAmerica_);
  ui_->clear_DX_check_box->setChecked (clear_DX_);
  ui_->clear_DX_exit_check_box->setChecked (clear_DX_exit_);
  ui_->miles_check_box->setChecked (miles_);
  ui_->tx_watchdog_spin_box->setValue (watchdog_);
  ui_->tune_timer_spin_box->setValue (tunetimer_);
  ui_->TX_messages_check_box->setChecked (TX_messages_);
  ui_->hide_TX_messages_check_box->setChecked (hide_TX_messages_);
  ui_->decode_at_52s_check_box->setChecked(decode_at_52s_);
  ui_->beep_on_my_call_check_box->setChecked(beepOnMyCall_);
  ui_->beep_on_newCQZ_check_box->setChecked(beepOnNewCQZ_ && newCQZ_);
  ui_->beep_on_newITUZ_check_box->setChecked(beepOnNewITUZ_ && newITUZ_);
  ui_->beep_on_newDXCC_check_box->setChecked(beepOnNewDXCC_ && newDXCC_);
  ui_->beep_on_newGrid_check_box->setChecked(beepOnNewGrid_ && newGrid_);
  ui_->beep_on_newPx_check_box->setChecked(beepOnNewPx_ && newPx_);
  ui_->beep_on_newCall_check_box->setChecked(beepOnNewCall_ && newCall_);
  ui_->beep_on_firstMsg_check_box->setChecked(beepOnFirstMsg_);
  ui_->type_2_msg_gen_combo_box->setCurrentIndex (type_2_msg_gen_);
  ui_->rig_combo_box->setCurrentText (rig_params_.rig_name);
  ui_->TX_mode_button_group->button (data_mode_)->setChecked (true);
  ui_->split_mode_button_group->button (rig_params_.split_mode)->setChecked (true);
  ui_->CAT_serial_baud_combo_box->setCurrentText (QString::number (rig_params_.baud));
  ui_->CAT_data_bits_button_group->button (rig_params_.data_bits)->setChecked (true);
  ui_->CAT_stop_bits_button_group->button (rig_params_.stop_bits)->setChecked (true);
  ui_->CAT_handshake_button_group->button (rig_params_.handshake)->setChecked (true);
  ui_->checkBoxPwrBandTxMemory->setChecked(pwrBandTxMemory_);
  ui_->checkBoxPwrBandTuneMemory->setChecked(pwrBandTuneMemory_);  
  if (rig_params_.force_dtr)
    {
      ui_->force_DTR_combo_box->setCurrentIndex (rig_params_.dtr_high ? 1 : 2);
    }
  else
    {
      ui_->force_DTR_combo_box->setCurrentIndex (0);
    }
  if (rig_params_.force_rts)
    {
      ui_->force_RTS_combo_box->setCurrentIndex (rig_params_.rts_high ? 1 : 2);
    }
  else
    {
      ui_->force_RTS_combo_box->setCurrentIndex (0);
    }
  ui_->TX_audio_source_button_group->button (rig_params_.audio_source)->setChecked (true);
  ui_->CAT_poll_interval_spin_box->setValue (rig_params_.poll_interval);
  ui_->udp_server_line_edit->setText (udp_server_name_);
  ui_->udp_server_port_spin_box->setValue (udp_server_port_);
  ui_->accept_udp_requests_check_box->setChecked (accept_udp_requests_);
  ui_->udpWindowToFront->setChecked(udpWindowToFront_);
  ui_->udpWindowRestore->setChecked(udpWindowRestore_);
  ui_->udp1_adif_enable_check_box->setChecked (enable_udp1_adif_sending_);
  ui_->udp2_server_line_edit->setText (udp2_server_name_);
  ui_->udp2_server_port_spin_box->setValue (udp2_server_port_);
  ui_->udp2_enable_check_box->setChecked (enable_udp2_broadcast_);
  ui_->tcp_server_line_edit->setText (tcp_server_name_);
  ui_->tcp_server_port_spin_box->setValue (tcp_server_port_);
  ui_->TCP_checkBox->setChecked (enable_tcp_connection_);
  ui_->write_decoded_check_box->setChecked (write_decoded_);
  ui_->write_decoded_debug_check_box->setChecked (write_decoded_debug_);

  ui_->calibration_intercept_spin_box->setValue (frequency_calibration_intercept_);
  ui_->calibration_slope_ppm_spin_box->setValue (frequency_calibration_slope_ppm_);

  if (rig_params_.ptt_port.isEmpty ())
    {
      if (ui_->PTT_port_combo_box->count ())
        {
          ui_->PTT_port_combo_box->setCurrentText (ui_->PTT_port_combo_box->itemText (0));
        }
    }
  else
    {
      ui_->PTT_port_combo_box->setCurrentText (rig_params_.ptt_port);
    }
  ui_->region_combo_box->setCurrentIndex (region_);
  next_macros_.setStringList (macros_.stringList ());
  next_frequencies_.frequency_list (frequencies_.frequency_list ());
  next_stations_.station_list (stations_.station_list ());

  set_rig_invariants ();
}

void Configuration::impl::done (int r)
{
  // do this here since window is still on screen at this point
  SettingsGroup g {settings_, "Configuration"};
  settings_->setValue ("window/geometry", saveGeometry ());

  QDialog::done (r);
}

void Configuration::impl::read_settings ()
{
  SettingsGroup g {settings_, "Configuration"};
  restoreGeometry (settings_->value ("window/geometry").toByteArray ());

  my_callsign_ = settings_->value ("MyCall", "").toString ();
  my_grid_ = settings_->value ("MyGrid", "").toString ();

  timeFrom_ = settings_->value ("timeFromLogFiltering", "").toString ();
  if(!timeFrom_.isEmpty()) {
    QRegularExpression timefrom_re("^[0-9]{14,14}$"); QRegularExpressionMatch match = timefrom_re.match(timeFrom_);
    bool hasMatch = match.hasMatch(); if(!hasMatch) timeFrom_="";
  }
  
  content_ = settings_->value ("Content", "").toString ();
  countries_= settings_->value ("CountryFilterList", "").toString ();
  callsigns_= settings_->value ("CallsignFilterList", "").toString ();
  next_color_CQ_ = color_CQ_ = settings_->value("colorCQ","#000000").toString();
  next_color_MyCall_ = color_MyCall_ = settings_->value("colorMyCall","#f00000").toString();
  next_color_StandardCall_ = color_StandardCall_ = settings_->value("colorStandardCall","#707070").toString();
  next_color_TxMsg_ = color_TxMsg_ = settings_->value("colorTxMsg","#ffff00").toString();
  next_color_NewCQZ_ = color_NewCQZ_ = settings_->value("colorNewCQZ","#c08000").toString();
  next_color_NewCQZBand_ = color_NewCQZBand_ = settings_->value("colorNewCQZBand","#c0a080").toString();
  next_color_NewITUZ_ = color_NewITUZ_ = settings_->value("colorNewITUZ","#90b000").toString();
  next_color_NewITUZBand_ = color_NewITUZBand_ = settings_->value("colorNewITUZBand","#c0d0a0").toString();
  next_color_NewDXCC_ = color_NewDXCC_ = settings_->value("colorNewDXCC","#c000c0").toString();
  next_color_NewDXCCBand_ = color_NewDXCCBand_ = settings_->value("colorNewDXCCBand","#d080d0").toString();
  next_color_NewGrid_ = color_NewGrid_ = settings_->value("colorNewGrid","#00a0a0").toString();
  next_color_NewGridBand_ = color_NewGridBand_ = settings_->value("colorNewGridBand","#80d0d0").toString();
  next_color_NewPx_ = color_NewPx_ = settings_->value("colorNewPx","#00a040").toString();
  next_color_NewPxBand_ = color_NewPxBand_ = settings_->value("colorNewPxBand","#50e090").toString();
  next_color_NewCall_ = color_NewCall_ = settings_->value("colorNewCall","#a0a030").toString();
  next_color_NewCallBand_ = color_NewCallBand_ = settings_->value("colorNewCallBand","#e0e070").toString();
  next_color_WorkedCall_ = color_WorkedCall_ = settings_->value("colorWorkedCall","#00ff00").toString();
  useDarkStyle_ = settings_->value ("UseDarkStyle", false).toBool ();

  next_font_.fromString (settings_->value ("Font", QGuiApplication::font ().toString ()).toString ());
  if (next_font_ != font_ || useDarkStyle_)
    {
      font_ = next_font_;
      set_application_font (font_);
    }

  if (next_decoded_text_font_.fromString (settings_->value ("DecodedTextFont", "Courier, 10").toString ())
      && next_decoded_text_font_ != decoded_text_font_)
    {
      decoded_text_font_ = next_decoded_text_font_;
      Q_EMIT self_->decoded_text_font_changed (decoded_text_font_);
    }
  else
    {
      next_decoded_text_font_ = decoded_text_font_;
    }

  id_interval_ = settings_->value ("IDint", 0).toInt (); if(!(id_interval_>=0 && id_interval_<=99)) id_interval_=0;
  ntrials_ = settings_->value ("nTrials", 3).toInt (); if(!(ntrials_>=1 && ntrials_<=8)) ntrials_=3;

  qint32 ntxDelay=settings_->value ("TxDelayInt",100).toInt();
  txDelay_ = static_cast<double>(ntxDelay/1000.); if(!(txDelay_>-0.001 && txDelay_<0.501)) txDelay_=0.1;
  ntrials10_ = settings_->value ("nTrialsT10", 1).toInt (); if(!(ntrials10_>=1 && ntrials10_<=8)) ntrials10_=1;
  ntrialsrxf10_ = settings_->value ("nTrialsRxFreqT10", 1).toInt (); if(!(ntrialsrxf10_>=1 && ntrialsrxf10_<=3)) ntrialsrxf10_=1;
  npreampass_ = settings_->value ("nPreampass", 4).toInt (); if(!(npreampass_>=2 && npreampass_<=4)) npreampass_=4;
  aggressive_ = settings_->value ("Aggressive", 1).toInt (); if(!(aggressive_>=1 && aggressive_<=5)) aggressive_=1;
  harmonicsdepth_ = settings_->value ("HarmonicsDecodingDepth", 0).toInt (); if(!(harmonicsdepth_>=0 && harmonicsdepth_<=4)) harmonicsdepth_=0;
  ntopfreq65_ = settings_->value ("TopFrequencyJT65", 2700).toInt (); if(!(ntopfreq65_>=100 && ntopfreq65_<=5000)) ntopfreq65_=2700;
  nAnswerCQCounter_ = settings_->value ("SeqAnswerCQCounterValue", 2).toInt (); if(!(nAnswerCQCounter_>=1 && nAnswerCQCounter_<=5)) nAnswerCQCounter_=2;
  nAnswerInCallCounter_ = settings_->value ("SeqAnswerInCallCounterValue", 2).toInt (); if(!(nAnswerInCallCounter_>=1 && nAnswerInCallCounter_<=5)) nAnswerInCallCounter_=2;
  nSentRReportCounter_ = settings_->value ("SeqSentRReportCounterValue", 3).toInt (); if(!(nSentRReportCounter_>=1 && nSentRReportCounter_<=5)) nSentRReportCounter_=3;
  nSentRR7373Counter_ = settings_->value ("SeqSentRR7373CounterValue", 2).toInt (); if(!(nSentRR7373Counter_>=1 && nSentRR7373Counter_<=5)) nSentRR7373Counter_=2;
  nsingdecatt_ = settings_->value ("nSingleDecodeAttempts", 1).toInt (); if(!(nsingdecatt_>=1 && nsingdecatt_<=3)) nsingdecatt_=1;
  fmaskact_ = settings_->value ("FMaskDecoding", false).toBool ();
  answerCQCount_ = settings_->value ("SeqAnswerCQCount", false).toBool ();
  answerInCallCount_ = settings_->value ("SeqAnswerInCallCount", false).toBool ();
  sentRReportCount_ = settings_->value ("SeqSentRReportCount", false).toBool ();
  sentRR7373Count_ = settings_->value ("SeqSentRR7373Count", false).toBool ();
  strictdirCQ_ = settings_->value ("StrictDirectionalCQ", false).toBool ();
  halttxreplyother_ = settings_->value ("SeqHaltTxReplyOther", true).toBool ();

  if(settings_->value ("HideFreeMsgs").toString()=="false" || settings_->value ("HideFreeMsgs").toString()=="true")
    hidefree_ = settings_->value ("HideFreeMsgs").toBool ();
  else hidefree_ = false;

  hide2ndHash_ = settings_->value ("HideMsgsWith2ndCallAsHash", false).toBool ();

  if(settings_->value ("ShowCQMsgsOnly").toString()=="false" || settings_->value ("ShowCQMsgsOnly").toString()=="true")
    showcq_ = settings_->value ("ShowCQMsgsOnly").toBool ();
  else showcq_ = false;

  if(settings_->value ("ShowCQRRR73MsgsOnly").toString()=="false" || settings_->value ("ShowCQRRR73MsgsOnly").toString()=="true")
    showcqrrr73_ = settings_->value ("ShowCQRRR73MsgsOnly").toBool ();
  else showcqrrr73_ = false;

  if(settings_->value ("ShowCQ73MsgsOnly").toString()=="false" || settings_->value ("ShowCQ73MsgsOnly").toString()=="true")
    showcq73_ = settings_->value ("ShowCQ73MsgsOnly").toBool ();
  else showcq73_ = false;

  enableContent_ = settings_->value ("EnableContent", false).toBool ();

  if(settings_->value ("EnableCountryFilter").toString()=="false" || settings_->value ("EnableCountryFilter").toString()=="true")
    enableCountryFilter_ = settings_->value ("EnableCountryFilter").toBool ();
  else enableCountryFilter_ = false;

  if(settings_->value ("EnableCallsignFilter").toString()=="false" || settings_->value ("EnableCallsignFilter").toString()=="true")
    enableCallsignFilter_ = settings_->value ("EnableCallsignFilter", false).toBool ();
  else enableCallsignFilter_ = false;

  do_snr_ = settings_->value ("CATRequestSNR", false).toBool ();
  do_pwr_ = settings_->value ("CATRequestPower", false).toBool ();
  rig_power_ = settings_->value ("RigPower", false).toBool ();

  if(settings_->value ("hideAfrica").toString()=="false" || settings_->value ("hideAfrica").toString()=="true")
    hideAfrica_ = settings_->value ("hideAfrica").toBool ();
  else hideAfrica_ = false;

  if(settings_->value ("hideAntarctica").toString()=="false" || settings_->value ("hideAntarctica").toString()=="true")
    hideAntarctica_ = settings_->value ("hideAntarctica").toBool ();
  else hideAntarctica_ = false;

  if(settings_->value ("hideAsia").toString()=="false" || settings_->value ("hideAsia").toString()=="true")
    hideAsia_ = settings_->value ("hideAsia").toBool ();
  else hideAsia_ = false;

  if(settings_->value ("hideEurope").toString()=="false" || settings_->value ("hideEurope").toString()=="true")
    hideEurope_ = settings_->value ("hideEurope").toBool ();
  else hideEurope_ = false;

  if(settings_->value ("hideOceania").toString()=="false" || settings_->value ("hideOceania").toString()=="true")
    hideOceania_ = settings_->value ("hideOceania").toBool ();
  else hideOceania_ = false;

  if(settings_->value ("hideNAmerica").toString()=="false" || settings_->value ("hideNAmerica").toString()=="true")
    hideNAmerica_ = settings_->value ("hideNAmerica").toBool ();
  else hideNAmerica_ = false;

  if(settings_->value ("hideSAmerica").toString()=="false" || settings_->value ("hideSAmerica").toString()=="true")
    hideSAmerica_ = settings_->value ("hideSAmerica").toBool ();
  else hideSAmerica_ = false;

  save_directory_.setPath (settings_->value ("SaveDir", default_save_directory_.absolutePath ()).toString ());

  {
    //
    // retrieve audio input device
    //
    auto saved_name = settings_->value ("SoundInName").toString ();

    // deal with special Windows default audio devices
    auto default_device = QAudioDeviceInfo::defaultInputDevice ();
    if (saved_name == default_device.deviceName ())
      {
        audio_input_device_ = default_device;
        default_audio_input_device_selected_ = true;
      }
    else
      {
        default_audio_input_device_selected_ = false;
        Q_FOREACH (auto const& p, QAudioDeviceInfo::availableDevices (QAudio::AudioInput)) // available audio input devices
          {
            if (p.deviceName () == saved_name)
              {
                audio_input_device_ = p;
              }
          }
      }
  }

  {
    //
    // retrieve audio output device
    //
    auto saved_name = settings_->value("SoundOutName").toString();

    // deal with special Windows default audio devices
    auto default_device = QAudioDeviceInfo::defaultOutputDevice ();
    if (saved_name == default_device.deviceName ())
      {
        audio_output_device_ = default_device;
        default_audio_output_device_selected_ = true;
      }
    else
      {
        default_audio_output_device_selected_ = false;
        Q_FOREACH (auto const& p, QAudioDeviceInfo::availableDevices (QAudio::AudioOutput)) // available audio output devices
          {
            if (p.deviceName () == saved_name)
              {
                audio_output_device_ = p;
              }
          }
      }
  }

  // retrieve audio channel info
  audio_input_channel_ = AudioDevice::fromString (settings_->value ("AudioInputChannel", "Mono").toString ());
  audio_output_channel_ = AudioDevice::fromString (settings_->value ("AudioOutputChannel", "Mono").toString ());

  type_2_msg_gen_ = settings_->value ("Type2MsgGen", QVariant::fromValue (Configuration::type_2_msg_3_full)).value<Configuration::Type2MsgGen> ();

  if(settings_->value ("MonitorOFF").toString()=="false" || settings_->value ("MonitorOFF").toString()=="true")
    monitor_off_at_startup_ = settings_->value ("MonitorOFF").toBool ();
  else monitor_off_at_startup_ = false;

  monitor_last_used_ = settings_->value ("MonitorLastUsed", false).toBool ();
  spot_to_psk_reporter_ = settings_->value ("PSKReporter", false).toBool ();
  spot_to_dxsummit_ = settings_->value ("AllowSpotsDXSummit", false).toBool ();
  prevent_spotting_false_ = settings_->value ("preventFalseUDPspots", true).toBool ();

  if(settings_->value ("ApplyFiltersToUDPmessages").toString()=="false" || settings_->value ("ApplyFiltersToUDPmessages").toString()=="true")
    filterUDP_ = settings_->value ("ApplyFiltersToUDPmessages").toBool ();
  else filterUDP_ = false;

  eqsl_username_ = settings_->value ("EQSLUser", "").toString ();
  eqsl_passwd_ = settings_->value ("EQSLPasswd", "").toString ();
  eqsl_nickname_ = settings_->value ("EQSLNick", "").toString ();
  send_to_eqsl_ = settings_->value ("EQSLSend", false).toBool ();
  if(eqsl_username_.isEmpty () || eqsl_passwd_.isEmpty () || eqsl_nickname_.isEmpty ()) {
    ui_->eqsl_check_box->setChecked (false); ui_->eqsl_check_box->setEnabled (false); send_to_eqsl_=false; 
  }
  usesched_ = settings_->value ("UseSchedBands", false).toBool ();
  sched_hh_1_ = settings_->value ("Sched_hh_1", "").toString ();
  sched_mm_1_ = settings_->value ("Sched_mm_1", "").toString ();
  sched_band_1_ = settings_->value ("Sched_band_1", "").toString ();
  sched_mix_1_ = settings_->value ("Sched_mix_1", false).toBool ();
  sched_hh_2_ = settings_->value ("Sched_hh_2", "").toString ();
  sched_mm_2_ = settings_->value ("Sched_mm_2", "").toString ();
  sched_band_2_ = settings_->value ("Sched_band_2", "").toString ();
  sched_mix_2_ = settings_->value ("Sched_mix_2", false).toBool ();
  sched_hh_3_ = settings_->value ("Sched_hh_3", "").toString ();
  sched_mm_3_ = settings_->value ("Sched_mm_3", "").toString ();
  sched_band_3_ = settings_->value ("Sched_band_3", "").toString ();
  sched_mix_3_ = settings_->value ("Sched_mix_3", false).toBool ();
  sched_hh_4_ = settings_->value ("Sched_hh_4", "").toString ();
  sched_mm_4_ = settings_->value ("Sched_mm_4", "").toString ();
  sched_band_4_ = settings_->value ("Sched_band_4", "").toString ();
  sched_mix_4_ = settings_->value ("Sched_mix_4", false).toBool ();
  sched_hh_5_ = settings_->value ("Sched_hh_5", "").toString ();
  sched_mm_5_ = settings_->value ("Sched_mm_5", "").toString ();
  sched_band_5_ = settings_->value ("Sched_band_5", "").toString ();
  sched_mix_5_ = settings_->value ("Sched_mix_5", false).toBool ();

//check scheduler settings for consistency
  if(sched_hh_1_.isEmpty ()) { sched_mm_1_=""; ui_->mmComboBox_1->setEnabled(false); }
  if(sched_mm_1_.isEmpty ()) { sched_band_1_=""; ui_->bandComboBox_1->setEnabled(false); }
  if(sched_band_1_.isEmpty ()) { ui_->band_mix_check_box_1->setEnabled(false); ui_->band_mix_check_box_1->setChecked (false); sched_hh_2_=""; ui_->hhComboBox_2->setEnabled(false); }

  if(sched_hh_2_.isEmpty ()) { sched_mm_2_=""; ui_->mmComboBox_2->setEnabled(false); }
  if(sched_mm_2_.isEmpty ()) { sched_band_2_=""; ui_->bandComboBox_2->setEnabled(false); }
  if(sched_band_2_.isEmpty ()) { ui_->band_mix_check_box_2->setEnabled(false); ui_->band_mix_check_box_2->setChecked (false); sched_hh_3_=""; ui_->hhComboBox_3->setEnabled(false); }

  if(sched_hh_3_.isEmpty ()) { sched_mm_3_=""; ui_->mmComboBox_3->setEnabled(false); }
  if(sched_mm_3_.isEmpty ()) { sched_band_3_=""; ui_->bandComboBox_3->setEnabled(false); }
  if(sched_band_3_.isEmpty ()) { ui_->band_mix_check_box_3->setEnabled(false); ui_->band_mix_check_box_3->setChecked (false); sched_hh_4_=""; ui_->hhComboBox_4->setEnabled(false); }

  if(sched_hh_4_.isEmpty ()) { sched_mm_4_=""; ui_->mmComboBox_4->setEnabled(false); }
  if(sched_mm_4_.isEmpty ()) { sched_band_4_=""; ui_->bandComboBox_4->setEnabled(false); }
  if(sched_band_4_.isEmpty ()) { ui_->band_mix_check_box_4->setEnabled(false); ui_->band_mix_check_box_4->setChecked (false); sched_hh_5_=""; ui_->hhComboBox_5->setEnabled(false); }

  if(sched_hh_5_.isEmpty ()) { sched_mm_5_=""; ui_->mmComboBox_5->setEnabled(false); }
  if(sched_mm_5_.isEmpty ()) { sched_band_5_=""; ui_->bandComboBox_5->setEnabled(false); }
  if(sched_band_5_.isEmpty ()) { ui_->band_mix_check_box_5->setEnabled(false); ui_->band_mix_check_box_5->setChecked (false); }

  if(settings_->value ("After73").toString()=="false" || settings_->value ("After73").toString()=="true")
    id_after_73_ = settings_->value ("After73").toBool ();
  else id_after_73_ = false;

  if(settings_->value ("TxQSYAllowed").toString()=="false" || settings_->value ("TxQSYAllowed").toString()=="true")
    tx_QSY_allowed_ = settings_->value ("TxQSYAllowed").toBool ();
  else tx_QSY_allowed_ = false;

  macros_.setStringList (settings_->value ("Macros", QStringList {"@ TNX 73","TU ^ 73","@ &","@ #","@ R#"}).toStringList ());

  region_ = settings_->value ("Region", QVariant::fromValue (IARURegions::ALL)).value<IARURegions::Region> ();

  if (settings_->contains ("FrequenciesForModes"))
    {
      auto const& v = settings_->value ("FrequenciesForModes");
      if (v.isValid ())
        {
          frequencies_.frequency_list (v.value<FrequencyList_v2::FrequencyItems> ());
        }
      else
        {
          frequencies_.reset_to_defaults ();
        }
    }
  else
    {
      frequencies_.reset_to_defaults ();
    }

  stations_.station_list (settings_->value ("stations").value<StationList::Stations> ());

  log_as_RTTY_ = settings_->value ("toRTTY", false).toBool ();
  report_in_comments_ = settings_->value("dBtoComments", false).toBool ();
  rig_params_.rig_name = settings_->value ("Rig", TransceiverFactory::basic_transceiver_name_).toString ();
  rig_is_dummy_ = TransceiverFactory::basic_transceiver_name_ == rig_params_.rig_name;
  rig_params_.network_port = settings_->value ("CATNetworkPort").toString ();
  rig_params_.usb_port = settings_->value ("CATUSBPort").toString ();
  rig_params_.serial_port = settings_->value ("CATSerialPort").toString ();
  rig_params_.baud = settings_->value ("CATSerialRate", 4800).toInt ();
  rig_params_.data_bits = settings_->value ("CATDataBits", QVariant::fromValue (TransceiverFactory::default_data_bits)).value<TransceiverFactory::DataBits> ();
  rig_params_.stop_bits = settings_->value ("CATStopBits", QVariant::fromValue (TransceiverFactory::default_stop_bits)).value<TransceiverFactory::StopBits> ();
  rig_params_.handshake = settings_->value ("CATHandshake", QVariant::fromValue (TransceiverFactory::handshake_default)).value<TransceiverFactory::Handshake> ();
  rig_params_.force_dtr = settings_->value ("CATForceDTR", false).toBool ();
  rig_params_.dtr_high = settings_->value ("DTR", false).toBool ();
  rig_params_.force_rts = settings_->value ("CATForceRTS", false).toBool ();
  rig_params_.rts_high = settings_->value ("RTS", false).toBool ();
  rig_params_.ptt_type = settings_->value ("PTTMethod", QVariant::fromValue (TransceiverFactory::PTT_method_VOX)).value<TransceiverFactory::PTTMethod> ();
  rig_params_.audio_source = settings_->value ("TXAudioSource", QVariant::fromValue (TransceiverFactory::TX_audio_source_front)).value<TransceiverFactory::TXAudioSource> ();
  rig_params_.ptt_port = settings_->value ("PTTport").toString ();
  data_mode_ = settings_->value ("DataMode", QVariant::fromValue (data_mode_none)).value<Configuration::DataMode> ();
  prompt_to_log_ = settings_->value ("PromptToLog", false).toBool ();
  autolog_ = settings_->value ("AutoQSOLogging", false).toBool ();
  content_ = settings_->value ("Content", "AVI,CMD,GIF,HTML,HYBRID,IMAGE,JOINT,JPG,MP4,PHOTO").toString ();
  countries_ = settings_->value ("CountryFilterList", "").toString ();
  callsigns_ = settings_->value ("CallsignFilterList", "").toString ();
  insert_blank_ = settings_->value ("InsertBlank", false).toBool ();
  countryName_ = settings_->value ("countryName", true).toBool ();
  countryPrefix_ = settings_->value ("countryPrefix", false).toBool ();

  if(settings_->value ("callsignLogFiltering").toString()=="false" || settings_->value ("callsignLogFiltering").toString()=="true")
    callNotif_ = settings_->value ("callsignLogFiltering").toBool ();
  else callNotif_ = false;

  if(settings_->value ("gridLogFiltering").toString()=="false" || settings_->value ("gridLogFiltering").toString()=="true")
    gridNotif_ = settings_->value ("gridLogFiltering").toBool ();
  else gridNotif_ = false;

  next_txtColor_ = txtColor_ = settings_->value ("txtColor", false).toBool ();
  next_workedColor_ = workedColor_ = settings_->value ("workedColor", false).toBool ();
  next_workedStriked_ = workedStriked_ = settings_->value ("workedStriked", false).toBool ();
  next_workedUnderlined_ = workedUnderlined_ = settings_->value ("workedUnderlined", true).toBool ();

  if(settings_->value ("workedDontShow").toString()=="false" || settings_->value ("workedDontShow").toString()=="true")
    next_workedDontShow_ = workedDontShow_ = settings_->value ("workedDontShow").toBool ();
  else next_workedDontShow_ = false;

  next_newCQZ_ = newCQZ_ = settings_->value ("newCQZ", true).toBool ();
  next_newCQZBand_ = newCQZBand_ = settings_->value ("newCQZBand", false).toBool ();
  next_newCQZBandMode_ = newCQZBandMode_ = settings_->value ("newCQZBandMode", false).toBool ();
  next_newITUZ_ = newITUZ_ = settings_->value ("newITUZ", true).toBool ();
  next_newITUZBand_ = newITUZBand_ = settings_->value ("newITUZBand", false).toBool ();
  next_newITUZBandMode_ = newITUZBandMode_ = settings_->value ("newITUZBandMode", false).toBool ();
  next_newDXCC_ = newDXCC_ = settings_->value ("newDXCC", true).toBool ();
  next_newDXCCBand_ = newDXCCBand_ = settings_->value ("newDXCCBand", false).toBool ();
  next_newDXCCBandMode_ = newDXCCBandMode_ = settings_->value ("newDXCCBandMode", false).toBool ();
  next_newGrid_ = newGrid_ = settings_->value ("newGrid", false).toBool ();
  next_newGridBand_ = newGridBand_ = settings_->value ("newGridBand", false).toBool ();
  next_newGridBandMode_ = newGridBandMode_ = settings_->value ("newGridBandMode", false).toBool ();
  next_newPx_ = newPx_ = settings_->value ("newPx", false).toBool ();
  next_newPxBand_ = newPxBand_ = settings_->value ("newPxBand", false).toBool ();
  next_newPxBandMode_ = newPxBandMode_ = settings_->value ("newPxBandMode", false).toBool ();
  next_newCall_ = newCall_ = settings_->value ("newCall", true).toBool ();
  next_newCallBand_ = newCallBand_ = settings_->value ("newCallBand", false).toBool ();
  next_newCallBandMode_ = newCallBandMode_ = settings_->value ("newCallBandMode", false).toBool ();
  next_newPotential_ = newPotential_ = settings_->value ("newPotential", false).toBool ();
  otherMessagesMarker_ = settings_->value ("OtherStandardMessagesMarker", true).toBool () && !newPotential_;
  RR73Marker_= settings_->value ("73RR73Marker", false).toBool ();
  on_RR73_marker_check_box_clicked(RR73Marker_);
  redMarker_ = settings_->value ("redMarker", true).toBool ();
  blueMarker_ = settings_->value ("blueMarker", false).toBool ();
  clear_DX_ = settings_->value ("ClearCallGrid", false).toBool ();
  clear_DX_exit_ = settings_->value ("ClearCallGridExit", false).toBool ();
  miles_ = settings_->value ("Miles", false).toBool ();
  watchdog_ = settings_->value ("TxWatchdogTimer", 6).toInt (); if(!(watchdog_>=0 && watchdog_<=99)) watchdog_=6;
  tunetimer_ = settings_->value ("TuneTimer", 30).toInt (); if(!(tunetimer_>=0 && tunetimer_<=300)) tunetimer_=30;
  TX_messages_ = settings_->value ("Tx2QSO", true).toBool ();
  hide_TX_messages_ = settings_->value ("HideTxMessages", false).toBool ();
  decode_at_52s_ = settings_->value("Decode52",false).toBool ();
  beepOnMyCall_ = settings_->value("BeepOnMyCall", false).toBool();
  beepOnNewCQZ_ = settings_->value("BeepOnNewCQZ", false).toBool();
  beepOnNewITUZ_ = settings_->value("BeepOnNewITUZ", false).toBool();
  beepOnNewDXCC_ = settings_->value("BeepOnNewDXCC", false).toBool();
  beepOnNewGrid_ = settings_->value("BeepOnNewGrid", false).toBool();
  beepOnNewPx_ = settings_->value("BeepOnNewPx", false).toBool();
  beepOnNewCall_ = settings_->value("BeepOnNewCall", false).toBool();
  beepOnFirstMsg_ = settings_->value("BeepOnFirstMsg", false).toBool();
  rig_params_.poll_interval = settings_->value ("Polling", 1).toInt (); if(!(rig_params_.poll_interval>=1 && rig_params_.poll_interval<=999)) rig_params_.poll_interval=1;
  rig_params_.split_mode = settings_->value ("SplitMode", QVariant::fromValue (TransceiverFactory::split_mode_none)).value<TransceiverFactory::SplitMode> ();
  udp_server_name_ = settings_->value ("UDPServer", "127.0.0.1").toString ();
  udp_server_port_ = settings_->value ("UDPServerPort", 2237).toUInt ();
  udp2_server_name_ = settings_->value ("UDP2Server", "127.0.0.1").toString ();
  udp2_server_port_ = settings_->value ("UDP2ServerPort", 2333).toUInt ();
  tcp_server_name_ = settings_->value ("TCPServer", "127.0.0.1").toString ();
  tcp_server_port_ = settings_->value ("TCPServerPort", 52001).toUInt ();
  accept_udp_requests_ = settings_->value ("AcceptUDPRequests", false).toBool ();

  if(settings_->value ("EnableUDP1adifSending").toString()=="false" || settings_->value ("EnableUDP1adifSending").toString()=="true")
    enable_udp1_adif_sending_ = settings_->value("EnableUDP1adifSending").toBool ();
  else enable_udp1_adif_sending_ = false;
  if(settings_->value ("EnableUDP2adifBroadcast").toString()=="false" || settings_->value ("EnableUDP2adifBroadcast").toString()=="true")
    enable_udp2_broadcast_ = settings_->value("EnableUDP2adifBroadcast").toBool ();
  else enable_udp2_broadcast_ = false;
  if(settings_->value ("EnableTCPConnection").toString()=="false" || settings_->value ("EnableTCPConnection").toString()=="true")
    enable_tcp_connection_ = settings_->value("EnableTCPConnection").toBool ();
  else enable_tcp_connection_ = false;

  write_decoded_ = settings_->value ("WriteDecodedALLTXT", true).toBool ();
  write_decoded_debug_ = settings_->value ("WriteDecodedDebugALLTXT", false).toBool ();
  udpWindowToFront_ = settings_->value ("udpWindowToFront",false).toBool ();
  udpWindowRestore_ = settings_->value ("udpWindowRestore",false).toBool ();
  frequency_calibration_intercept_ = settings_->value ("CalibrationIntercept", 0.).toDouble ();
  frequency_calibration_slope_ppm_ = settings_->value ("CalibrationSlopePPM", 0.).toDouble ();

  if(settings_->value ("pwrBandTxMemory").toString()=="false" || settings_->value ("pwrBandTxMemory").toString()=="true")
    pwrBandTxMemory_ = settings_->value("pwrBandTxMemory").toBool ();
  else pwrBandTxMemory_ = false;

  if(settings_->value ("pwrBandTuneMemory").toString()=="false" || settings_->value ("pwrBandTuneMemory").toString()=="true")
    pwrBandTuneMemory_ = settings_->value("pwrBandTuneMemory").toBool ();
  else pwrBandTuneMemory_ = false;
}

void Configuration::add_callsign_hideFilter (QString basecall)
{
  QString curcallsigns = m_->callsigns_;
  if(curcallsigns.isEmpty ()) { m_->callsigns_ = basecall; m_->enableCallsignFilter_ = true;}
  else { if(!curcallsigns.contains ("," + basecall + ",") && !curcallsigns.endsWith (basecall) && !curcallsigns.startsWith (basecall)) m_->callsigns_ = curcallsigns + "," + basecall; }
}

void Configuration::set_jtdxtime (JTDXDateTime * jtdxtime)
{
  m_->jtdxtime_ = jtdxtime;
}

void Configuration::impl::write_settings ()
{
  SettingsGroup g {settings_, "Configuration"};

  settings_->setValue ("MyCall", my_callsign_);
  settings_->setValue ("MyGrid", my_grid_);
  settings_->setValue ("timeFromLogFiltering", timeFrom_);
  settings_->setValue ("Content", content_);
  settings_->setValue ("CountryFilterList", countries_);
  settings_->setValue ("CallsignFilterList", callsigns_);
  settings_->setValue("colorCQ",color_CQ_);
  settings_->setValue("colorMyCall",color_MyCall_);
  settings_->setValue("colorTxMsg",color_TxMsg_);
  settings_->setValue("colorNewCQZ",color_NewCQZ_);
  settings_->setValue("colorNewCQZBand",color_NewCQZBand_);
  settings_->setValue("colorNewITUZ",color_NewITUZ_);
  settings_->setValue("colorNewITUZBand",color_NewITUZBand_);
  settings_->setValue("colorNewDXCC",color_NewDXCC_);
  settings_->setValue("colorNewDXCCBand",color_NewDXCCBand_);
  settings_->setValue("colorNewGrid",color_NewGrid_);
  settings_->setValue("colorNewGridBand",color_NewGridBand_);
  settings_->setValue("colorNewPx",color_NewPx_);
  settings_->setValue("colorNewPxBand",color_NewPxBand_);
  settings_->setValue("colorNewCall",color_NewCall_);
  settings_->setValue("colorNewCallBand",color_NewCallBand_);
  settings_->setValue("colorStandardCall",color_StandardCall_);
  settings_->setValue("colorWorkedCall",color_WorkedCall_);
  settings_->setValue ("Font", font_.toString ());
  settings_->setValue ("DecodedTextFont", decoded_text_font_.toString ());
  settings_->setValue ("IDint", id_interval_);
  settings_->setValue ("nTrials", ntrials_);
  int ntxDelay=1000*txDelay_;
  settings_->setValue ("TxDelayInt", ntxDelay);
  settings_->setValue ("nTrialsT10", ntrials10_);
  settings_->setValue ("nTrialsRxFreqT10", ntrialsrxf10_);
  settings_->setValue ("nPreampass", npreampass_);
  settings_->setValue ("Aggressive", aggressive_);
  settings_->setValue ("HarmonicsDecodingDepth", harmonicsdepth_);
  settings_->setValue ("TopFrequencyJT65", ntopfreq65_);
  settings_->setValue ("SeqAnswerCQCounterValue", nAnswerCQCounter_);
  settings_->setValue ("SeqAnswerInCallCounterValue", nAnswerInCallCounter_);
  settings_->setValue ("SeqSentRReportCounterValue", nSentRReportCounter_);
  settings_->setValue ("SeqSentRR7373CounterValue", nSentRR7373Counter_);
  settings_->setValue ("FMaskDecoding", fmaskact_);
  settings_->setValue ("SeqAnswerCQCount", answerCQCount_);
  settings_->setValue ("SeqAnswerInCallCount", answerInCallCount_);
  settings_->setValue ("SeqSentRReportCount", sentRReportCount_);
  settings_->setValue ("SeqSentRR7373Count", sentRR7373Count_);
  settings_->setValue ("StrictDirectionalCQ", strictdirCQ_);
  settings_->setValue ("SeqHaltTxReplyOther", halttxreplyother_);
  settings_->setValue ("HideFreeMsgs", hidefree_);
  settings_->setValue ("HideMsgsWith2ndCallAsHash", hide2ndHash_);
  settings_->setValue ("ShowCQMsgsOnly", showcq_);
  settings_->setValue ("ShowCQRRR73MsgsOnly", showcqrrr73_);
  settings_->setValue ("ShowCQ73MsgsOnly", showcq73_);
  settings_->setValue ("EnableContent", enableContent_);
  settings_->setValue ("EnableCountryFilter", enableCountryFilter_);
  settings_->setValue ("EnableCallsignFilter", enableCallsignFilter_);
  settings_->setValue ("CATRequestSNR", do_snr_);
  settings_->setValue ("CATRequestPower", do_pwr_);
  settings_->setValue ("RigPower", rig_power_);
  settings_->setValue ("hideAfrica", hideAfrica_);
  settings_->setValue ("hideAntarctica", hideAntarctica_);
  settings_->setValue ("hideAsia", hideAsia_);
  settings_->setValue ("hideEurope", hideEurope_);
  settings_->setValue ("hideOceania", hideOceania_);
  settings_->setValue ("hideNAmerica", hideNAmerica_);
  settings_->setValue ("hideSAmerica", hideSAmerica_);
  settings_->setValue ("nSingleDecodeAttempts", nsingdecatt_);
  settings_->setValue ("PTTMethod", QVariant::fromValue (rig_params_.ptt_type));
  settings_->setValue ("PTTport", rig_params_.ptt_port);
  settings_->setValue ("SaveDir", save_directory_.absolutePath ());

  if (default_audio_input_device_selected_)
    {
      settings_->setValue ("SoundInName", QAudioDeviceInfo::defaultInputDevice ().deviceName ());
    }
  else
    {
      settings_->setValue ("SoundInName", audio_input_device_.deviceName ());
    }

  if (default_audio_output_device_selected_)
    {
      settings_->setValue ("SoundOutName", QAudioDeviceInfo::defaultOutputDevice ().deviceName ());
    }
  else
    {
      settings_->setValue ("SoundOutName", audio_output_device_.deviceName ());
    }

  settings_->setValue ("AudioInputChannel", AudioDevice::toString (audio_input_channel_));
  settings_->setValue ("AudioOutputChannel", AudioDevice::toString (audio_output_channel_));
  settings_->setValue ("Type2MsgGen", QVariant::fromValue (type_2_msg_gen_));
  settings_->setValue ("MonitorOFF", monitor_off_at_startup_);
  settings_->setValue ("MonitorLastUsed", monitor_last_used_);
  settings_->setValue ("PSKReporter", spot_to_psk_reporter_);
  settings_->setValue ("AllowSpotsDXSummit", spot_to_dxsummit_);
  settings_->setValue ("preventFalseUDPspots", prevent_spotting_false_);
  settings_->setValue ("ApplyFiltersToUDPmessages", filterUDP_);
  settings_->setValue ("EQSLSend", send_to_eqsl_);
  settings_->setValue ("EQSLUser", eqsl_username_);
  settings_->setValue ("EQSLPasswd", eqsl_passwd_);
  settings_->setValue ("EQSLNick", eqsl_nickname_);
  settings_->setValue ("UseSchedBands", usesched_);
  settings_->setValue ("Sched_hh_1", sched_hh_1_);
  settings_->setValue ("Sched_mm_1", sched_mm_1_);
  settings_->setValue ("Sched_band_1", sched_band_1_);
  settings_->setValue ("Sched_mix_1", sched_mix_1_);
  settings_->setValue ("Sched_hh_2", sched_hh_2_);
  settings_->setValue ("Sched_mm_2", sched_mm_2_);
  settings_->setValue ("Sched_band_2", sched_band_2_);
  settings_->setValue ("Sched_mix_2", sched_mix_2_);
  settings_->setValue ("Sched_hh_3", sched_hh_3_);
  settings_->setValue ("Sched_mm_3", sched_mm_3_);
  settings_->setValue ("Sched_band_3", sched_band_3_);
  settings_->setValue ("Sched_mix_3", sched_mix_3_);
  settings_->setValue ("Sched_hh_4", sched_hh_4_);
  settings_->setValue ("Sched_mm_4", sched_mm_4_);
  settings_->setValue ("Sched_band_4", sched_band_4_);
  settings_->setValue ("Sched_mix_4", sched_mix_4_);
  settings_->setValue ("Sched_hh_5", sched_hh_5_);
  settings_->setValue ("Sched_mm_5", sched_mm_5_);
  settings_->setValue ("Sched_band_5", sched_band_5_);
  settings_->setValue ("Sched_mix_5", sched_mix_5_);
  settings_->setValue ("After73", id_after_73_);
  settings_->setValue ("TxQSYAllowed", tx_QSY_allowed_);
  settings_->setValue ("Macros", macros_.stringList ());
  settings_->setValue ("FrequenciesForModes", QVariant::fromValue (frequencies_.frequency_list ()));
  settings_->setValue ("stations", QVariant::fromValue (stations_.station_list ()));
  settings_->setValue ("toRTTY", log_as_RTTY_);
  settings_->setValue ("dBtoComments", report_in_comments_);
  settings_->setValue ("Rig", rig_params_.rig_name);
  settings_->setValue ("CATNetworkPort", rig_params_.network_port);
  settings_->setValue ("CATUSBPort", rig_params_.usb_port);
  settings_->setValue ("CATSerialPort", rig_params_.serial_port);
  settings_->setValue ("CATSerialRate", rig_params_.baud);
  settings_->setValue ("CATDataBits", QVariant::fromValue (rig_params_.data_bits));
  settings_->setValue ("CATStopBits", QVariant::fromValue (rig_params_.stop_bits));
  settings_->setValue ("CATHandshake", QVariant::fromValue (rig_params_.handshake));
  settings_->setValue ("DataMode", QVariant::fromValue (data_mode_));
  settings_->setValue ("PromptToLog", prompt_to_log_);
  settings_->setValue ("AutoQSOLogging", autolog_);
  settings_->setValue ("InsertBlank", insert_blank_);
  settings_->setValue ("UseDarkStyle", useDarkStyle_);
  settings_->setValue ("countryName", countryName_);
  settings_->setValue ("countryPrefix", countryPrefix_);
  settings_->setValue ("callsignLogFiltering", callNotif_);
  settings_->setValue ("gridLogFiltering", gridNotif_);
  settings_->setValue ("OtherStandardMessagesMarker", otherMessagesMarker_);
  settings_->setValue ("73RR73Marker", RR73Marker_);
  settings_->setValue ("redMarker", redMarker_);
  settings_->setValue ("blueMarker", blueMarker_);
  settings_->setValue ("txtColor", txtColor_);
  settings_->setValue ("workedColor", workedColor_);
  settings_->setValue ("workedStriked", workedStriked_);
  settings_->setValue ("workedUnderlined", workedUnderlined_);
  settings_->setValue ("workedDontShow", workedDontShow_);
  settings_->setValue ("newCQZ", newCQZ_);
  settings_->setValue ("newCQZBand", newCQZBand_);
  settings_->setValue ("newCQZBandMode", newCQZBandMode_);
  settings_->setValue ("newITUZ", newITUZ_);
  settings_->setValue ("newITUZBand", newITUZBand_);
  settings_->setValue ("newITUZBandMode", newITUZBandMode_);
  settings_->setValue ("newDXCC", newDXCC_);
  settings_->setValue ("newDXCCBand", newDXCCBand_);
  settings_->setValue ("newDXCCBandMode", newDXCCBandMode_);
  settings_->setValue ("newCall", newCall_);
  settings_->setValue ("newCallBand", newCallBand_);
  settings_->setValue ("newCallBandMode", newCallBandMode_);
  settings_->setValue ("newPx", newPx_);
  settings_->setValue ("newPxBand", newPxBand_);
  settings_->setValue ("newPxBandMode", newPxBandMode_);
  settings_->setValue ("newGrid", newGrid_);
  settings_->setValue ("newGridBand", newGridBand_);
  settings_->setValue ("newGridBandMode", newGridBandMode_);
  settings_->setValue ("newPotential", newPotential_);
  settings_->setValue ("ClearCallGrid", clear_DX_);
  settings_->setValue ("ClearCallGridExit", clear_DX_exit_);
  settings_->setValue ("Miles", miles_);
  settings_->setValue ("TxWatchdogTimer", watchdog_);
  settings_->setValue ("TuneTimer", tunetimer_);
  settings_->setValue ("Tx2QSO", TX_messages_);
  settings_->setValue ("HideTxMessages", hide_TX_messages_);
  settings_->setValue ("CATForceDTR", rig_params_.force_dtr);
  settings_->setValue ("DTR", rig_params_.dtr_high);
  settings_->setValue ("CATForceRTS", rig_params_.force_rts);
  settings_->setValue ("RTS", rig_params_.rts_high);
  settings_->setValue ("TXAudioSource", QVariant::fromValue (rig_params_.audio_source));
  settings_->setValue ("Polling", rig_params_.poll_interval);
  settings_->setValue ("SplitMode", QVariant::fromValue (rig_params_.split_mode));
  settings_->setValue ("Decode52", decode_at_52s_);
  settings_->setValue ("BeepOnMyCall", beepOnMyCall_);
  settings_->setValue ("BeepOnNewCQZ", beepOnNewCQZ_);
  settings_->setValue ("BeepOnNewITUZ", beepOnNewITUZ_);
  settings_->setValue ("BeepOnNewDXCC", beepOnNewDXCC_);
  settings_->setValue ("BeepOnNewGrid", beepOnNewGrid_);
  settings_->setValue ("BeepOnNewPx", beepOnNewPx_);
  settings_->setValue ("BeepOnNewCall", beepOnNewCall_);
  settings_->setValue ("BeepOnFirstMsg", beepOnFirstMsg_);
  settings_->setValue ("UDPServer", udp_server_name_);
  settings_->setValue ("UDPServerPort", udp_server_port_);
  settings_->setValue ("UDP2Server", udp2_server_name_);
  settings_->setValue ("UDP2ServerPort", udp2_server_port_);
  settings_->setValue ("TCPServer", tcp_server_name_);
  settings_->setValue ("TCPServerPort", tcp_server_port_);
  settings_->setValue ("AcceptUDPRequests", accept_udp_requests_);
  settings_->setValue ("EnableUDP1adifSending", enable_udp1_adif_sending_);
  settings_->setValue ("EnableUDP2adifBroadcast", enable_udp2_broadcast_);
  settings_->setValue ("EnableTCPConnection", enable_tcp_connection_);
  settings_->setValue ("WriteDecodedALLTXT", write_decoded_);
  settings_->setValue ("WriteDecodedDebugALLTXT", write_decoded_debug_);
  settings_->setValue ("udpWindowToFront", udpWindowToFront_);
  settings_->setValue ("udpWindowRestore", udpWindowRestore_);
  settings_->setValue ("CalibrationIntercept", frequency_calibration_intercept_);
  settings_->setValue ("CalibrationSlopePPM", frequency_calibration_slope_ppm_);
  settings_->setValue ("pwrBandTxMemory", pwrBandTxMemory_);
  settings_->setValue ("pwrBandTuneMemory", pwrBandTuneMemory_);
  settings_->setValue ("Region", QVariant::fromValue (region_));  
}

void Configuration::impl::set_rig_invariants ()
{
  auto const& rig = ui_->rig_combo_box->currentText ();
  auto const& ptt_port = ui_->PTT_port_combo_box->currentText ();
  auto ptt_method = static_cast<TransceiverFactory::PTTMethod> (ui_->PTT_method_button_group->checkedId ());

  auto CAT_PTT_enabled = transceiver_factory_.has_CAT_PTT (rig);
  auto CAT_indirect_serial_PTT = transceiver_factory_.has_CAT_indirect_serial_PTT (rig);
  auto asynchronous_CAT = transceiver_factory_.has_asynchronous_CAT (rig);
  auto is_hw_handshake = ui_->CAT_handshake_group_box->isEnabled ()
    && TransceiverFactory::handshake_hardware == static_cast<TransceiverFactory::Handshake> (ui_->CAT_handshake_button_group->checkedId ());

  ui_->test_CAT_push_button->setStyleSheet ({});

  ui_->CAT_poll_interval_label->setEnabled (!asynchronous_CAT);
  ui_->CAT_poll_interval_spin_box->setEnabled (!asynchronous_CAT);

  auto port_type = transceiver_factory_.CAT_port_type (rig);

  bool is_serial_CAT (TransceiverFactory::Capabilities::serial == port_type);
  auto const& cat_port = ui_->CAT_port_combo_box->currentText ();

  // only enable CAT option if transceiver has CAT PTT
  ui_->PTT_CAT_radio_button->setEnabled (CAT_PTT_enabled);

  auto enable_ptt_port = TransceiverFactory::PTT_method_CAT != ptt_method && TransceiverFactory::PTT_method_VOX != ptt_method;
  ui_->PTT_port_combo_box->setEnabled (enable_ptt_port);
  ui_->PTT_port_label->setEnabled (enable_ptt_port);

  if (CAT_indirect_serial_PTT)
    {
      ui_->PTT_port_combo_box->setItemData (ui_->PTT_port_combo_box->findText ("CAT")
                                            , combo_box_item_enabled, Qt::UserRole - 1);
    }
  else
    {
      ui_->PTT_port_combo_box->setItemData (ui_->PTT_port_combo_box->findText ("CAT")
                                            , combo_box_item_disabled, Qt::UserRole - 1);
      if ("CAT" == ui_->PTT_port_combo_box->currentText () && ui_->PTT_port_combo_box->currentIndex () > 0)
        {
          ui_->PTT_port_combo_box->setCurrentIndex (ui_->PTT_port_combo_box->currentIndex () - 1);
        }
    }
  ui_->PTT_RTS_radio_button->setEnabled (!(is_serial_CAT && ptt_port == cat_port && is_hw_handshake));

  if (TransceiverFactory::basic_transceiver_name_ == rig)
    {
      // makes no sense with rig as "None"
      ui_->monitor_last_used_check_box->setEnabled (false);

      ui_->CAT_control_group_box->setEnabled (false);
      ui_->test_CAT_push_button->setEnabled (false);
      ui_->test_PTT_push_button->setEnabled (TransceiverFactory::PTT_method_DTR == ptt_method
                                             || TransceiverFactory::PTT_method_RTS == ptt_method);
      ui_->TX_audio_source_group_box->setEnabled (false);
      ui_->rig_power_check_box->setChecked (false);
      ui_->rig_power_check_box->setEnabled (false);
      ui_->S_meter_check_box->setChecked (false);
      ui_->S_meter_check_box->setEnabled (false);
      ui_->output_power_check_box->setChecked (false);
      ui_->output_power_check_box->setEnabled (false);
    }
  else
    {
      ui_->monitor_last_used_check_box->setEnabled (true);
      ui_->CAT_control_group_box->setEnabled (true);
      if(!ui_->rig_combo_box->currentText().startsWith("OmniRig") && !ui_->rig_combo_box->currentText().startsWith("DX Lab") &&
         !ui_->rig_combo_box->currentText().startsWith("Ham Radio") && !ui_->rig_combo_box->currentText().startsWith("Kenwood TS-480") &&
         !ui_->rig_combo_box->currentText().startsWith("Kenwood TS-850") &&
         !ui_->rig_combo_box->currentText().startsWith("Kenwood TS-870")) {
         ui_->rig_power_check_box->setEnabled (true);
         ui_->S_meter_check_box->setEnabled (true);
         ui_->output_power_check_box->setEnabled (true);
      }
      ui_->test_CAT_push_button->setEnabled (true);
      ui_->test_PTT_push_button->setEnabled (false);
      ui_->TX_audio_source_group_box->setEnabled (transceiver_factory_.has_CAT_PTT_mic_data (rig) && TransceiverFactory::PTT_method_CAT == ptt_method);
      if (port_type != last_port_type_)
        {
          last_port_type_ = port_type;
          switch (port_type)
            {
            case TransceiverFactory::Capabilities::serial:
              fill_port_combo_box (ui_->CAT_port_combo_box);
              ui_->CAT_port_combo_box->setCurrentText (rig_params_.serial_port);
              if (ui_->CAT_port_combo_box->currentText ().isEmpty () && ui_->CAT_port_combo_box->count ())
                {
                  ui_->CAT_port_combo_box->setCurrentText (ui_->CAT_port_combo_box->itemText (0));
                }
              ui_->CAT_port_label->setText (tr ("Serial Port:"));
              ui_->CAT_port_combo_box->setToolTip (tr ("Serial port used for CAT control"));
              ui_->CAT_port_combo_box->setEnabled (true);
              break;

            case TransceiverFactory::Capabilities::network:
              ui_->CAT_port_combo_box->clear ();
              ui_->CAT_port_combo_box->setCurrentText (rig_params_.network_port);
              ui_->CAT_port_label->setText (tr ("Network Server:"));
              ui_->CAT_port_combo_box->setToolTip (tr ("Optional hostname and port of network service.\n"
                                                       "Leave blank for a sensible default on this machine.\n"
                                                       "Formats:\n"
                                                       "\thostname:port\n"
                                                       "\tIPv4-address:port\n"
                                                       "\t[IPv6-address]:port"));
              ui_->CAT_port_combo_box->setEnabled (true);
              break;

            case TransceiverFactory::Capabilities::usb:
              ui_->CAT_port_combo_box->clear ();
              ui_->CAT_port_combo_box->setCurrentText (rig_params_.usb_port);
              ui_->CAT_port_label->setText (tr ("USB Device:"));
              ui_->CAT_port_combo_box->setToolTip (tr ("Optional device identification.\n"
                                                       "Leave blank for a sensible default for the rig.\n"
                                                       "Format:\n"
                                                       "\t[VID[:PID[:VENDOR[:PRODUCT]]]]"));
              ui_->CAT_port_combo_box->setEnabled (true);
              break;

            default:
              ui_->CAT_port_combo_box->clear ();
              ui_->CAT_port_combo_box->setEnabled (false);
              break;
            }
        }
      ui_->CAT_serial_port_parameters_group_box->setEnabled (is_serial_CAT);
      ui_->force_DTR_combo_box->setEnabled (is_serial_CAT
                                            && (cat_port != ptt_port
                                                || !ui_->PTT_DTR_radio_button->isEnabled ()
                                                || !ui_->PTT_DTR_radio_button->isChecked ()));
      ui_->force_RTS_combo_box->setEnabled (is_serial_CAT
                                            && !is_hw_handshake
                                            && (cat_port != ptt_port
                                                || !ui_->PTT_RTS_radio_button->isEnabled ()
                                                || !ui_->PTT_RTS_radio_button->isChecked ()));
    }
  ui_->mode_group_box->setEnabled (WSJT_RIG_NONE_CAN_SPLIT
                                   || TransceiverFactory::basic_transceiver_name_ != rig);
  ui_->split_operation_group_box->setEnabled (WSJT_RIG_NONE_CAN_SPLIT
                                              || TransceiverFactory::basic_transceiver_name_ != rig);
}

bool Configuration::impl::validate ()
{
  if (ui_->sound_input_combo_box->currentIndex () < 0
      && !QAudioDeviceInfo::availableDevices (QAudio::AudioInput).empty ())
    {
      message_box_critical (tr ("Invalid audio input device"));
      return false;
    }

  if (ui_->sound_output_combo_box->currentIndex () < 0
      && !QAudioDeviceInfo::availableDevices (QAudio::AudioOutput).empty ())
    {
      message_box_critical (tr ("Invalid audio output device"));
      return false;
    }

  if (!ui_->PTT_method_button_group->checkedButton ()->isEnabled ())
    {
      message_box_critical (tr ("Invalid PTT method"));
      return false;
    }

  auto ptt_method = static_cast<TransceiverFactory::PTTMethod> (ui_->PTT_method_button_group->checkedId ());
  auto ptt_port = ui_->PTT_port_combo_box->currentText ();
  if ((TransceiverFactory::PTT_method_DTR == ptt_method || TransceiverFactory::PTT_method_RTS == ptt_method)
      && (ptt_port.isEmpty ()
          || combo_box_item_disabled == ui_->PTT_port_combo_box->itemData (ui_->PTT_port_combo_box->findText (ptt_port), Qt::UserRole - 1)))
    {
      message_box_critical (tr ("Invalid PTT port"));
      return false;
    }

  return true;
}

int Configuration::impl::exec ()
{
  // macros can be modified in the main window
  next_macros_.setStringList (macros_.stringList ());

  have_rig_ = rig_active_;	// record that we started with a rig open
  saved_rig_params_ = rig_params_; // used to detect changes that
                                   // require the Transceiver to be
                                   // re-opened
  rig_changed_ = false;

  initialize_models ();
  return QDialog::exec();
}

TransceiverFactory::ParameterPack Configuration::impl::gather_rig_data ()
{
  TransceiverFactory::ParameterPack result;
  result.rig_name = ui_->rig_combo_box->currentText ();

  switch (transceiver_factory_.CAT_port_type (result.rig_name))
    {
    case TransceiverFactory::Capabilities::network:
      result.network_port = ui_->CAT_port_combo_box->currentText ();
      result.usb_port = rig_params_.usb_port;
      result.serial_port = rig_params_.serial_port;
      break;

    case TransceiverFactory::Capabilities::usb:
      result.usb_port = ui_->CAT_port_combo_box->currentText ();
      result.network_port = rig_params_.network_port;
      result.serial_port = rig_params_.serial_port;
      break;

    default:
      result.serial_port = ui_->CAT_port_combo_box->currentText ();
      result.network_port = rig_params_.network_port;
      result.usb_port = rig_params_.usb_port;
      break;
    }

  result.do_snr = ui_->S_meter_check_box->isChecked ();
  result.do_pwr = ui_->output_power_check_box->isChecked ();
  result.rig_power = ui_->rig_power_check_box->isChecked ();
  result.baud = ui_->CAT_serial_baud_combo_box->currentText ().toInt ();
  result.data_bits = static_cast<TransceiverFactory::DataBits> (ui_->CAT_data_bits_button_group->checkedId ());
  result.stop_bits = static_cast<TransceiverFactory::StopBits> (ui_->CAT_stop_bits_button_group->checkedId ());
  result.handshake = static_cast<TransceiverFactory::Handshake> (ui_->CAT_handshake_button_group->checkedId ());
  result.force_dtr = ui_->force_DTR_combo_box->isEnabled () && ui_->force_DTR_combo_box->currentIndex () > 0;
  result.dtr_high = ui_->force_DTR_combo_box->isEnabled () && 1 == ui_->force_DTR_combo_box->currentIndex ();
  result.force_rts = ui_->force_RTS_combo_box->isEnabled () && ui_->force_RTS_combo_box->currentIndex () > 0;
  result.rts_high = ui_->force_RTS_combo_box->isEnabled () && 1 == ui_->force_RTS_combo_box->currentIndex ();
  result.poll_interval = ui_->CAT_poll_interval_spin_box->value ();
  result.ptt_type = static_cast<TransceiverFactory::PTTMethod> (ui_->PTT_method_button_group->checkedId ());
  result.ptt_port = ui_->PTT_port_combo_box->currentText ();
  result.audio_source = static_cast<TransceiverFactory::TXAudioSource> (ui_->TX_audio_source_button_group->checkedId ());
  result.split_mode = static_cast<TransceiverFactory::SplitMode> (ui_->split_mode_button_group->checkedId ());
  return result;
}

void Configuration::impl::accept ()
{
  // Called when OK button is clicked.

  if (!validate ())
    {
      return;			// not accepting
    }

  // extract all rig related configuration parameters into temporary
  // structure for checking if the rig needs re-opening without
  // actually updating our live state
  auto temp_rig_params = gather_rig_data ();

  // open_rig() uses values from models so we use it to validate the
  // Transceiver settings before agreeing to accept the configuration
  if (temp_rig_params != rig_params_ && !open_rig ())
    {
      return;			// not accepting
    }

  QDialog::accept();            // do this before accessing custom
                                // models so that any changes in
                                // delegates in views get flushed to
                                // the underlying models before we
                                // access them

  sync_transceiver (true);	// force an update

  //
  // from here on we are bound to accept the new configuration
  // parameters so extract values from models and make them live
  //

  if (next_font_ != font_ || useDarkStyle_ != ui_->useDarkStyle_check_box->isChecked ())
    {
      useDarkStyle_ = ui_->useDarkStyle_check_box->isChecked ();
      font_ = next_font_;
      set_application_font (font_);
    }

  if (next_decoded_text_font_ != decoded_text_font_)
    {
      decoded_text_font_ = next_decoded_text_font_;
      Q_EMIT self_->decoded_text_font_changed (decoded_text_font_);
    }

  color_CQ_ = next_color_CQ_;
  color_MyCall_ = next_color_MyCall_;
  color_TxMsg_ = next_color_TxMsg_;
  color_NewCQZ_ = next_color_NewCQZ_;
  color_NewCQZBand_ = next_color_NewCQZBand_;
  color_NewITUZ_ = next_color_NewITUZ_;
  color_NewITUZBand_ = next_color_NewITUZBand_;
  color_NewDXCC_ = next_color_NewDXCC_;
  color_NewDXCCBand_ = next_color_NewDXCCBand_;
  color_NewGrid_ = next_color_NewGrid_;
  color_NewGridBand_ = next_color_NewGridBand_;
  color_NewPx_ = next_color_NewPx_;
  color_NewPxBand_ = next_color_NewPxBand_;
  color_NewCall_ = next_color_NewCall_;
  color_NewCallBand_ = next_color_NewCallBand_;
  color_StandardCall_ = next_color_StandardCall_;
  color_WorkedCall_ = next_color_WorkedCall_;

  rig_params_ = temp_rig_params; // now we can go live with the rig
                                 // related configuration parameters
  rig_is_dummy_ = TransceiverFactory::basic_transceiver_name_ == rig_params_.rig_name;

  // Check to see whether SoundInThread must be restarted,
  // and save user parameters.
  {
    auto const& device_name = ui_->sound_input_combo_box->currentText ();
    if (device_name != audio_input_device_.deviceName ())
      {
        auto const& default_device = QAudioDeviceInfo::defaultInputDevice ();
        if (device_name == default_device.deviceName ())
          {
            audio_input_device_ = default_device;
          }
        else
          {
            bool found {false};
            Q_FOREACH (auto const& d, QAudioDeviceInfo::availableDevices (QAudio::AudioInput))
              {
                if (device_name == d.deviceName ())
                  {
                    audio_input_device_ = d;
                    found = true;
                  }
              }
            if (!found)
              {
                audio_input_device_ = default_device;
              }
          }
        restart_sound_input_device_ = true;
      }
  }

  {
    auto const& device_name = ui_->sound_output_combo_box->currentText ();
    if (device_name != audio_output_device_.deviceName ())
      {
        auto const& default_device = QAudioDeviceInfo::defaultOutputDevice ();
        if (device_name == default_device.deviceName ())
          {
            audio_output_device_ = default_device;
          }
        else
          {
            bool found {false};
            Q_FOREACH (auto const& d, QAudioDeviceInfo::availableDevices (QAudio::AudioOutput))
              {
                if (device_name == d.deviceName ())
                  {
                    audio_output_device_ = d;
                    found = true;
                  }
              }
            if (!found)
              {
                audio_output_device_ = default_device;
              }
          }
        restart_sound_output_device_ = true;
      }
  }

  if (audio_input_channel_ != static_cast<AudioDevice::Channel> (ui_->sound_input_channel_combo_box->currentIndex ()))
    {
      audio_input_channel_ = static_cast<AudioDevice::Channel> (ui_->sound_input_channel_combo_box->currentIndex ());
      restart_sound_input_device_ = true;
    }
  Q_ASSERT (audio_input_channel_ <= AudioDevice::Right);

  if (audio_output_channel_ != static_cast<AudioDevice::Channel> (ui_->sound_output_channel_combo_box->currentIndex ()))
    {
      audio_output_channel_ = static_cast<AudioDevice::Channel> (ui_->sound_output_channel_combo_box->currentIndex ());
      restart_sound_output_device_ = true;
    }
  Q_ASSERT (audio_output_channel_ <= AudioDevice::Both);

  my_callsign_ = ui_->callsign_line_edit->text ();
  my_grid_ = ui_->grid_line_edit->text ();
  timeFrom_ = ui_->logTime_line_edit->text ();
  content_ = ui_->content_line_edit->text ();
  countries_ = ui_->countries_line_edit->text ();
  callsigns_ = ui_->callsigns_line_edit->text ();
  spot_to_psk_reporter_ = ui_->psk_reporter_check_box->isChecked ();
  spot_to_dxsummit_ = ui_->dxsummit_check_box->isChecked ();
  prevent_spotting_false_ = ui_->preventFalseUDP_check_box->isChecked ();
  filterUDP_ =  ui_->filterUDP_check_box->isChecked ();
  if(!ui_->eqsluser_edit->text ().isEmpty () && !ui_->eqslpasswd_edit->text ().isEmpty () && !ui_->eqslnick_edit->text ().isEmpty ()) send_to_eqsl_ = ui_->eqsl_check_box->isChecked ();
  else send_to_eqsl_ = false;
  eqsl_username_ = ui_->eqsluser_edit->text ();
  eqsl_passwd_ = ui_->eqslpasswd_edit->text ();
  eqsl_nickname_ = ui_->eqslnick_edit->text ();
  usesched_ = ui_->UseSched_check_box->isChecked ();
  sched_hh_1_ = ui_->hhComboBox_1->currentText ();
  sched_mm_1_ = ui_->mmComboBox_1->currentText ();
  sched_band_1_ = ui_->bandComboBox_1->currentText ();
  sched_mix_1_ = ui_->band_mix_check_box_1->isChecked ();
  sched_hh_2_ = ui_->hhComboBox_2->currentText ();
  sched_mm_2_ = ui_->mmComboBox_2->currentText ();
  sched_band_2_ = ui_->bandComboBox_2->currentText ();
  sched_mix_2_ = ui_->band_mix_check_box_2->isChecked ();
  sched_hh_3_ = ui_->hhComboBox_3->currentText ();
  sched_mm_3_ = ui_->mmComboBox_3->currentText ();
  sched_band_3_ = ui_->bandComboBox_3->currentText ();
  sched_mix_3_ = ui_->band_mix_check_box_3->isChecked ();
  sched_hh_4_ = ui_->hhComboBox_4->currentText ();
  sched_mm_4_ = ui_->mmComboBox_4->currentText ();
  sched_band_4_ = ui_->bandComboBox_4->currentText ();
  sched_mix_4_ = ui_->band_mix_check_box_4->isChecked ();
  sched_hh_5_ = ui_->hhComboBox_5->currentText ();
  sched_mm_5_ = ui_->mmComboBox_5->currentText ();
  sched_band_5_ = ui_->bandComboBox_5->currentText ();
  sched_mix_5_ = ui_->band_mix_check_box_5->isChecked ();
  id_interval_ = ui_->CW_id_interval_spin_box->value ();
  ntrials_ = ui_->sbNtrials->value ();
  txDelay_ = ui_->sbTxDelay->value ();
  ntrials10_ = ui_->sbNtrials10->value ();
  ntrialsrxf10_ = ui_->sbNtrialsRXF10->value ();
  npreampass_= ui_->sbNpreampass->value ();
  aggressive_= ui_->sbAggressive->value ();
  harmonicsdepth_= ui_->sbHarmonics->value ();
  nsingdecatt_ = ui_->sbNsingdecatt->value ();
  ntopfreq65_ = ui_->sbTopFreq->value ();
  nAnswerCQCounter_ = ui_->sbAnswerCQCounter->value ();
  nAnswerInCallCounter_ = ui_->sbAnswerInCallCounter->value ();
  nSentRReportCounter_ = ui_->sbSentRReportCounter->value ();
  nSentRR7373Counter_ = ui_->sbSentRR7373Counter->value ();
  fmaskact_ = ui_->fMask_check_box->isChecked ();
  answerCQCount_ = ui_->answerCQCount_checkBox->isChecked ();
  answerInCallCount_ = ui_->answerInCallCount_checkBox->isChecked ();
  sentRReportCount_ = ui_->sentRReportCount_checkBox->isChecked ();
  sentRR7373Count_ = ui_->sentRR7373Count_checkBox->isChecked ();
  strictdirCQ_ = ui_->strictDirCQ_checkBox->isChecked ();
  halttxreplyother_ = ui_->haltTxReplyOther_checkBox->isChecked ();
  hidefree_ = ui_->HideFree_check_box->isChecked ();
  hide2ndHash_ = ui_->Hide2ndHash_check_box->isChecked ();
  showcq_ = ui_->ShowCQ_check_box->isChecked ();
  showcqrrr73_ = ui_->ShowCQRRR73_check_box->isChecked ();
  showcq73_ = ui_->ShowCQ73_check_box->isChecked ();
  enableContent_ = ui_->enableContent_check_box->isChecked ();
  enableCountryFilter_ = ui_->enableCountryFilter_check_box->isChecked ();
  enableCallsignFilter_ = ui_->enableCallsignFilter_check_box->isChecked ();
  do_snr_ = ui_->S_meter_check_box->isChecked ();
  do_pwr_ = ui_->output_power_check_box->isChecked ();
  rig_power_ = ui_->rig_power_check_box->isChecked ();
  id_after_73_ = ui_->CW_id_after_73_check_box->isChecked ();
  tx_QSY_allowed_ = ui_->tx_QSY_check_box->isChecked ();
  monitor_off_at_startup_ = ui_->monitor_off_check_box->isChecked ();
  monitor_last_used_ = ui_->monitor_last_used_check_box->isChecked ();
  type_2_msg_gen_ = static_cast<Type2MsgGen> (ui_->type_2_msg_gen_combo_box->currentIndex ());
  log_as_RTTY_ = ui_->log_as_RTTY_check_box->isChecked ();
  report_in_comments_ = ui_->report_in_comments_check_box->isChecked ();
  prompt_to_log_ = ui_->prompt_to_log_check_box->isChecked ();
  autolog_ = ui_->autolog_check_box->isChecked ();
  insert_blank_ = ui_->insert_blank_check_box->isChecked ();
  watchdog_ = ui_->tx_watchdog_spin_box->value ();
  tunetimer_= ui_->tune_timer_spin_box->value ();
  countryName_ = ui_->countryName_check_box->isChecked ();
  countryPrefix_ = ui_->countryPrefix_check_box->isChecked ();
  callNotif_ = ui_->callNotif_check_box->isChecked ();
  gridNotif_ = ui_->gridNotif_check_box->isChecked ();
  otherMessagesMarker_ = ui_->otherMessagesMarker_check_box->isChecked ();
  RR73Marker_ = ui_->RR73_marker_check_box->isChecked ();
  redMarker_ = ui_->redMarker_check_box->isChecked ();
  blueMarker_ = ui_->blueMarker_check_box->isChecked ();
  txtColor_ = ui_->txtColor_check_box->isChecked ();
  workedColor_ = ui_->workedColor_check_box->isChecked ();
  workedStriked_ = ui_->workedStriked_check_box->isChecked ();
  workedUnderlined_ = ui_->workedUnderlined_check_box->isChecked ();
  workedDontShow_ = ui_->workedDontShow_check_box->isChecked ();
  newCQZ_ = ui_->newCQZ_check_box->isChecked ();
  newCQZBand_ = ui_->newCQZBand_check_box->isChecked ();
  newCQZBandMode_ = ui_->newCQZBandMode_check_box->isChecked ();
  newITUZ_ = ui_->newITUZ_check_box->isChecked ();
  newITUZBand_ = ui_->newITUZBand_check_box->isChecked ();
  newITUZBandMode_ = ui_->newITUZBandMode_check_box->isChecked ();
  newDXCC_ = ui_->newDXCC_check_box->isChecked ();
  newDXCCBand_ = ui_->newDXCCBand_check_box->isChecked ();
  newDXCCBandMode_ = ui_->newDXCCBandMode_check_box->isChecked ();
  newCall_ = ui_->newCall_check_box->isChecked ();
  newCallBand_ = ui_->newCallBand_check_box->isChecked ();
  newCallBandMode_ = ui_->newCallBandMode_check_box->isChecked ();
  newPx_ = ui_->newPx_check_box->isChecked ();
  newPxBand_ = ui_->newPxBand_check_box->isChecked ();
  newPxBandMode_ = ui_->newPxBandMode_check_box->isChecked ();
  newGrid_ = ui_->newGrid_check_box->isChecked ();
  newGridBand_ = ui_->newGridBand_check_box->isChecked ();
  newGridBandMode_ = ui_->newGridBandMode_check_box->isChecked ();
  newPotential_ = ui_->newPotential_check_box->isChecked ();
  hideAfrica_= ui_->Africa_check_box->isChecked ();
  hideAntarctica_= ui_->Antarctica_check_box->isChecked ();
  hideAsia_= ui_->Asia_check_box->isChecked ();
  hideEurope_= ui_->Europe_check_box->isChecked ();
  hideOceania_= ui_->Oceania_check_box->isChecked ();
  hideNAmerica_= ui_->NAmerica_check_box->isChecked ();
  hideSAmerica_= ui_->SAmerica_check_box->isChecked ();
  clear_DX_ = ui_->clear_DX_check_box->isChecked ();
  clear_DX_exit_ = ui_->clear_DX_exit_check_box->isChecked ();
  miles_ = ui_->miles_check_box->isChecked ();
  TX_messages_ = ui_->TX_messages_check_box->isChecked ();
  hide_TX_messages_ = ui_->hide_TX_messages_check_box->isChecked ();
  data_mode_ = static_cast<DataMode> (ui_->TX_mode_button_group->checkedId ());
  save_directory_.setPath (ui_->save_path_display_label->text ());
  decode_at_52s_ = ui_->decode_at_52s_check_box->isChecked ();
  beepOnMyCall_ = ui_->beep_on_my_call_check_box->isChecked();
  beepOnNewCQZ_ = ui_->beep_on_newCQZ_check_box->isChecked();
  beepOnNewITUZ_ = ui_->beep_on_newITUZ_check_box->isChecked();
  beepOnNewDXCC_ = ui_->beep_on_newDXCC_check_box->isChecked();
  beepOnNewGrid_ = ui_->beep_on_newGrid_check_box->isChecked();
  beepOnNewPx_ = ui_->beep_on_newPx_check_box->isChecked();
  beepOnNewCall_ = ui_->beep_on_newCall_check_box->isChecked();
  beepOnFirstMsg_ = ui_->beep_on_firstMsg_check_box->isChecked();
  frequency_calibration_intercept_ = ui_->calibration_intercept_spin_box->value ();
  frequency_calibration_slope_ppm_ = ui_->calibration_slope_ppm_spin_box->value ();
  pwrBandTxMemory_ = ui_->checkBoxPwrBandTxMemory->isChecked ();
  pwrBandTuneMemory_ = ui_->checkBoxPwrBandTuneMemory->isChecked ();  

  auto new_server = ui_->udp_server_line_edit->text ();
  if (new_server != udp_server_name_)
    {
      udp_server_name_ = new_server;
      Q_EMIT self_->udp_server_changed (new_server);
    }

  auto new_port = ui_->udp_server_port_spin_box->value ();
  if (new_port != udp_server_port_)
    {
      udp_server_port_ = new_port;
      Q_EMIT self_->udp_server_port_changed (new_port);
    }

  auto new_tcpserver = ui_->tcp_server_line_edit->text ();
  if (new_tcpserver != tcp_server_name_)
    {
      tcp_server_name_ = new_tcpserver;
      Q_EMIT self_->tcp_server_changed (new_tcpserver);
    }

  auto new_tcpport = ui_->tcp_server_port_spin_box->value ();
  if (new_tcpport != tcp_server_port_)
    {
      tcp_server_port_ = new_tcpport;
      Q_EMIT self_->tcp_server_port_changed (new_tcpport);
    }

  accept_udp_requests_ = ui_->accept_udp_requests_check_box->isChecked ();
  enable_tcp_connection_ = ui_->TCP_checkBox->isChecked ();
  write_decoded_ = ui_->write_decoded_check_box->isChecked ();
  write_decoded_debug_ = ui_->write_decoded_debug_check_box->isChecked ();
  udpWindowToFront_ = ui_->udpWindowToFront->isChecked ();
  udpWindowRestore_ = ui_->udpWindowRestore->isChecked ();
  enable_udp1_adif_sending_ = ui_->udp1_adif_enable_check_box->isChecked ();
  udp2_server_name_ = ui_->udp2_server_line_edit->text ();
  udp2_server_port_ = ui_->udp2_server_port_spin_box->value ();
  enable_udp2_broadcast_ = ui_->udp2_enable_check_box->isChecked ();

  if (macros_.stringList () != next_macros_.stringList ())
    {
      macros_.setStringList (next_macros_.stringList ());
    }

  region_ = IARURegions::value (ui_->region_combo_box->currentText ());

  if (frequencies_.frequency_list () != next_frequencies_.frequency_list ())
    {
      frequencies_.frequency_list (next_frequencies_.frequency_list ());
      frequencies_.sort (FrequencyList_v2::frequency_column);
    }

  if (stations_.station_list () != next_stations_.station_list ())
    {
      stations_.station_list(next_stations_.station_list ());
      stations_.sort (StationList::band_column);
    }
 
  write_settings ();		// make visible to all
}

void Configuration::impl::reject ()
{
  on_RR73_marker_check_box_clicked(RR73Marker_);
  initialize_models ();		// reverts to settings as at exec ()

  // check if the Transceiver instance changed, in which case we need
  // to re open any prior Transceiver type
  if (rig_changed_)
    {
      if (have_rig_)
        {
          // we have to do this since the rig has been opened since we
          // were exec'ed even though it might fail
          open_rig ();
        }
      else
        {
          close_rig ();
        }
    }

  QDialog::reject ();
}

void Configuration::impl::message_box_critical (QString const& reason, QString const& detail)
{
/*  QPushButton::tr("OK");
  QPushButton::tr("Save");
  QPushButton::tr("Save All");
  QPushButton::tr("Open");
  QPushButton::tr("&Yes");
  QPushButton::tr("Yes to &All");
  QPushButton::tr("&No");
  QPushButton::tr("N&o to All");
  QPushButton::tr("Abort");
  QPushButton::tr("Retry");
  QPushButton::tr("Ignore");
  QPushButton::tr("Close");
  QPushButton::tr("Cancel");
  QPushButton::tr("Discard");
  QPushButton::tr("Help");
  QPushButton::tr("Apply");
  QPushButton::tr("Reset");
  QPushButton::tr("Restore Defaults");*/

  JTDXMessageBox mb;
  mb.setText (reason);
  if (!detail.isEmpty ())
    {
      mb.setDetailedText (detail);
    }
  mb.setStandardButtons (JTDXMessageBox::Ok);
  mb.setDefaultButton (JTDXMessageBox::Ok);
  mb.setIcon (JTDXMessageBox::Critical);
  mb.translate_buttons();
  mb.exec ();
}

void Configuration::impl::on_eqsluser_edit_textEdited(const QString &user)
{
  ui_->eqsl_check_box->setEnabled (!user.isEmpty () && !ui_->eqslpasswd_edit->text ().isEmpty () && !ui_->eqslnick_edit->text ().isEmpty ());  
  ui_->eqsl_check_box->setChecked ((!ui_->eqsluser_edit->text ().isEmpty () && !ui_->eqslpasswd_edit->text ().isEmpty () && !ui_->eqslnick_edit->text ().isEmpty ()) && send_to_eqsl_);  
}

void Configuration::impl::on_eqslpasswd_edit_textEdited(const QString &passwd)
{
  ui_->eqsl_check_box->setEnabled (!ui_->eqsluser_edit->text ().isEmpty () && !passwd.isEmpty () && !ui_->eqslnick_edit->text ().isEmpty ());  
  ui_->eqsl_check_box->setChecked ((!ui_->eqsluser_edit->text ().isEmpty () && !passwd.isEmpty () && !ui_->eqslnick_edit->text ().isEmpty ()) && send_to_eqsl_);  
}

void Configuration::impl::on_eqslnick_edit_textEdited(const QString &nick)
{
  ui_->eqsl_check_box->setEnabled (!ui_->eqsluser_edit->text ().isEmpty () && !ui_->eqslpasswd_edit->text ().isEmpty () && !nick.isEmpty ());  
  ui_->eqsl_check_box->setChecked ((!ui_->eqsluser_edit->text ().isEmpty () && !ui_->eqslpasswd_edit->text ().isEmpty () && !nick.isEmpty ()) && send_to_eqsl_);  
}

void Configuration::impl::on_font_push_button_clicked ()
{
  next_font_ = QFontDialog::getFont (0, next_font_, this);
}

void Configuration::impl::on_countryName_check_box_clicked(bool checked)
{
  ui_->countryPrefix_check_box->setChecked(checked && countryPrefix_);
  ui_->countryPrefix_check_box->setEnabled(checked);
}

void Configuration::impl::on_callNotif_check_box_clicked(bool checked)
{
  ui_->gridNotif_check_box->setChecked(checked && gridNotif_);
  ui_->gridNotif_check_box->setEnabled(checked);
}

void Configuration::impl::on_otherMessagesMarker_check_box_clicked(bool checked)
{
   if(checked) ui_->newPotential_check_box->setChecked(false);
}

void Configuration::impl::on_RR73_marker_check_box_clicked(bool checked)
{
   if(checked) ui_->pbCQmsg->setText(tr("CQ/73 in message"));
   else ui_->pbCQmsg->setText(tr("CQ in message"));
}

void Configuration::impl::on_redMarker_check_box_clicked(bool checked)
{
  ui_->blueMarker_check_box->setChecked(checked && blueMarker_);
  ui_->blueMarker_check_box->setEnabled(checked);
}

void Configuration::impl::on_ShowCQ_check_box_clicked(bool checked)
{
  if(checked) {
    ui_->ShowCQ73_check_box->setChecked(false);
    ui_->ShowCQRRR73_check_box->setChecked(false);
  }
}

void Configuration::impl::on_ShowCQRRR73_check_box_clicked(bool checked)
{
  if(checked) {
    ui_->ShowCQ_check_box->setChecked(false);
    ui_->ShowCQ73_check_box->setChecked(false);
  }
}

void Configuration::impl::on_ShowCQ73_check_box_clicked(bool checked)
{
  if(checked) {
    ui_->ShowCQ_check_box->setChecked(false);
    ui_->ShowCQRRR73_check_box->setChecked(false);
  }
}

void Configuration::impl::on_prompt_to_log_check_box_clicked(bool checked)
{
  if(checked) ui_->autolog_check_box->setChecked(false);
}

void Configuration::impl::on_autolog_check_box_clicked(bool checked)
{
  if(checked) ui_->prompt_to_log_check_box->setChecked(false);
}

void Configuration::impl::on_write_decoded_check_box_clicked(bool checked)
{
  if(checked) ui_->write_decoded_debug_check_box->setChecked(false);
}

void Configuration::impl::on_write_decoded_debug_check_box_clicked(bool checked)
{
  if(checked) ui_->write_decoded_check_box->setChecked(false);
}

void Configuration::impl::on_txtColor_check_box_clicked(bool checked)
{
  next_txtColor_ = checked;
  if (next_txtColor_){
    ui_->labCQ->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_),Radio::convert_dark("#ffffff",useDarkStyle_)));
    ui_->labMyCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_),Radio::convert_dark("#ffffff",useDarkStyle_)));
    ui_->labStandardCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_),Radio::convert_dark("#ffffff",useDarkStyle_)));
    ui_->labNewCQZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    ui_->labNewCQZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    ui_->labNewITUZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    ui_->labNewITUZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    ui_->labNewDXCC->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    ui_->labNewDXCCBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    ui_->labNewGrid->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    ui_->labNewGridBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    ui_->labNewPx->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    ui_->labNewPxBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    ui_->labNewCall->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    ui_->labNewCallBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    if (next_workedColor_) {
      if (next_workedStriked_) {
        ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else if (next_workedUnderlined_) {
        ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else {
        ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      }
    } else if (next_workedStriked_) {
      ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    } else if (next_workedUnderlined_) {
      ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    } else {
      ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    }
    ui_->labNewMcCQZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcCQZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcITUZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcITUZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcDXCC->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcDXCCBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcGrid->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcGridBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcPx->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcPxBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcCall->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcCallBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewScCQZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScCQZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScITUZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScITUZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScDXCC->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScDXCCBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScGrid->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScGridBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScPx->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScPxBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScCall->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScCallBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
  } else {
    ui_->labCQ->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_),Radio::convert_dark("#ffffff",useDarkStyle_)));
    ui_->labMyCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_),Radio::convert_dark("#ffffff",useDarkStyle_)));
    ui_->labStandardCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_),Radio::convert_dark("#ffffff",useDarkStyle_)));
    ui_->labNewCQZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    ui_->labNewCQZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    ui_->labNewITUZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    ui_->labNewITUZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    ui_->labNewDXCC->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    ui_->labNewDXCCBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    ui_->labNewGrid->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    ui_->labNewGridBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    ui_->labNewPx->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    ui_->labNewPxBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    ui_->labNewCall->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    ui_->labNewCallBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
    if (next_workedColor_) {
      if (next_workedStriked_) {
        ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else if (next_workedUnderlined_) {
        ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else {
        ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      }
    } else if (next_workedStriked_) {
      ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    } else if (next_workedUnderlined_) {
      ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    } else {
      ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    }
    ui_->labNewMcCQZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcCQZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcITUZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcITUZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcDXCC->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcDXCCBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcGrid->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcGridBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcPx->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcPxBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcCall->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewMcCallBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
    ui_->labNewScCQZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScCQZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScITUZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScITUZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScDXCC->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScDXCCBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScGrid->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScGridBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScPx->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScPxBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScCall->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    ui_->labNewScCallBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
  }
}

void Configuration::impl::on_workedColor_check_box_clicked(bool checked)
{
  next_workedColor_ = checked;
  if (next_txtColor_) {
    if (next_workedColor_) {
      if (next_workedStriked_) {
        ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else if (next_workedUnderlined_) {
        ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else {
        ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      }
    } else if (next_workedStriked_) {
      ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    } else if (next_workedUnderlined_) {
      ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    } else {
      ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    }
  } else {
    if (next_workedColor_) {
      if (next_workedStriked_) {
        ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else if (next_workedUnderlined_) {
        ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else {
        ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      }
    } else if (next_workedStriked_) {
      ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    } else if (next_workedUnderlined_) {
      ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    } else {
      ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    }
  }
}

void Configuration::impl::on_workedDontShow_check_box_clicked(bool checked)
{
  next_workedDontShow_ = checked;
}

void Configuration::impl::on_workedStriked_check_box_clicked(bool checked)
{
  next_workedStriked_ = checked;
  ui_->workedUnderlined_check_box->setChecked(!checked && workedUnderlined_);
  ui_->workedUnderlined_check_box->setEnabled(!checked);
  if (next_txtColor_) {
    if (next_workedColor_) {
      if (next_workedStriked_) {
        ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else if (next_workedUnderlined_) {
        ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else {
        ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      }
    } else if (next_workedStriked_) {
      ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    } else if (next_workedUnderlined_) {
      ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    } else {
      ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    }
  } else {
    if (next_workedColor_) {
      if (next_workedStriked_) {
        ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else if (next_workedUnderlined_) {
        ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else {
        ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      }
    } else if (next_workedStriked_) {
      ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    } else if (next_workedUnderlined_) {
      ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    } else {
      ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    }
  }
}

void Configuration::impl::on_workedUnderlined_check_box_clicked(bool checked)
{
  next_workedUnderlined_ = checked;
  ui_->workedStriked_check_box->setChecked(!checked && workedStriked_);
  ui_->workedStriked_check_box->setEnabled(!checked);
  if (next_txtColor_) {
    if (next_workedColor_) {
      if (next_workedStriked_) {
        ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else if (next_workedUnderlined_) {
        ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else {
        ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      }
    } else if (next_workedStriked_) {
      ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    } else if (next_workedUnderlined_) {
      ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    } else {
      ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    }
  } else {
    if (next_workedColor_) {
      if (next_workedStriked_) {
        ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else if (next_workedUnderlined_) {
        ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else {
        ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      }
    } else if (next_workedStriked_) {
      ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    } else if (next_workedUnderlined_) {
      ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    } else {
      ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
      ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
      ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
    }
  }
}

void Configuration::impl::on_newPotential_check_box_clicked(bool checked)
{
  next_newPotential_ = checked;
  ui_->labStandardCall->setVisible(checked);
  ui_->labNewScCQZ->setVisible(next_newCQZ_ && checked);
  ui_->labNewScCQZBand->setVisible((next_newCQZBandMode_ || next_newCQZBand_) && checked);
  ui_->labNewScITUZ->setVisible(next_newITUZ_ && checked);
  ui_->labNewScITUZBand->setVisible((next_newITUZBandMode_ || next_newITUZBand_) && checked);
  ui_->labNewScDXCC->setVisible(next_newDXCC_ && checked);
  ui_->labNewScDXCCBand->setVisible((next_newDXCCBandMode_ || next_newDXCCBand_) && checked);
  ui_->labNewScGrid->setVisible(next_newGrid_ && checked);
  ui_->labNewScGridBand->setVisible((next_newGridBandMode_ || next_newGridBand_) && checked);
  ui_->labNewScPx->setVisible(next_newPx_ && checked);
  ui_->labNewScPxBand->setVisible((next_newPxBandMode_ || next_newPxBand_) && checked);
  ui_->labNewScCall->setVisible(next_newCall_ && checked);
  ui_->labNewScCallBand->setVisible((next_newCallBandMode_ || next_newCallBand_) && checked);
  ui_->labWorkedScCall->setVisible((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && checked);
  if(checked) ui_->otherMessagesMarker_check_box->setChecked(false);
}

void Configuration::impl::on_newCQZ_check_box_clicked(bool checked)
{
  next_newCQZ_ = checked;
  ui_->newCQZBand_check_box->setChecked(checked && newCQZBand_);
  next_newCQZBand_ = checked && newCQZBand_;
  ui_->newCQZBand_check_box->setEnabled(checked);
  ui_->beep_on_newCQZ_check_box->setChecked(checked && beepOnNewCQZ_);
  ui_->beep_on_newCQZ_check_box->setEnabled(checked);
  ui_->newCQZBandMode_check_box->setChecked(checked && newCQZBandMode_);
  next_newCQZBandMode_ = checked && newCQZBandMode_;
  ui_->newCQZBandMode_check_box->setEnabled(checked);
  ui_->workedColor_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && next_workedColor_);
  ui_->workedColor_check_box->setEnabled(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->workedStriked_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedUnderlined_ && next_workedStriked_);
  ui_->workedStriked_check_box->setEnabled((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedUnderlined_);
  ui_->workedUnderlined_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedStriked_ && next_workedUnderlined_);
  ui_->workedUnderlined_check_box->setEnabled((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedStriked_);
  ui_->workedDontShow_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && next_workedDontShow_);
  ui_->workedDontShow_check_box->setEnabled(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->labNewCQZ->setVisible(next_newCQZ_);
  ui_->labNewMcCQZ->setVisible(next_newCQZ_);
  ui_->labNewScCQZ->setVisible(next_newCQZ_ && next_newPotential_);
  ui_->labNewCQZBand->setVisible(next_newCQZBand_ || next_newCQZBandMode_);
  ui_->labNewMcCQZBand->setVisible(next_newCQZBand_ || next_newCQZBandMode_);
  ui_->labNewScCQZBand->setVisible((next_newCQZBand_ || next_newCQZBandMode_) && next_newPotential_);
  ui_->labWorkedCall->setVisible(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->labWorkedMcCall->setVisible(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->labWorkedScCall->setVisible((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && next_newPotential_);
}

void Configuration::impl::on_newITUZ_check_box_clicked(bool checked)
{
  next_newITUZ_ = checked;
  ui_->newITUZBand_check_box->setChecked(checked && newITUZBand_);
  next_newITUZBand_ = checked && newITUZBand_;
  ui_->newITUZBand_check_box->setEnabled(checked);
  ui_->beep_on_newITUZ_check_box->setChecked(checked && beepOnNewITUZ_);
  ui_->beep_on_newITUZ_check_box->setEnabled(checked);
  ui_->newITUZBandMode_check_box->setChecked(checked && newITUZBandMode_);
  next_newITUZBandMode_ = checked && newITUZBandMode_;
  ui_->newITUZBandMode_check_box->setEnabled(checked);
  ui_->workedColor_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && next_workedColor_);
  ui_->workedColor_check_box->setEnabled(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->workedStriked_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedUnderlined_ && next_workedStriked_);
  ui_->workedStriked_check_box->setEnabled((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedUnderlined_);
  ui_->workedUnderlined_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedStriked_ && next_workedUnderlined_);
  ui_->workedUnderlined_check_box->setEnabled((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedStriked_);
  ui_->workedDontShow_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && next_workedDontShow_);
  ui_->workedDontShow_check_box->setEnabled(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->labNewITUZ->setVisible(next_newITUZ_);
  ui_->labNewMcITUZ->setVisible(next_newITUZ_);
  ui_->labNewScITUZ->setVisible(next_newITUZ_ && next_newPotential_);
  ui_->labNewITUZBand->setVisible(next_newITUZBand_ || next_newITUZBandMode_);
  ui_->labNewMcITUZBand->setVisible(next_newITUZBand_ || next_newITUZBandMode_);
  ui_->labNewScITUZBand->setVisible((next_newITUZBand_ || next_newITUZBandMode_) && next_newPotential_);
  ui_->labWorkedCall->setVisible(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->labWorkedMcCall->setVisible(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->labWorkedScCall->setVisible((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && next_newPotential_);
}

void Configuration::impl::on_newDXCC_check_box_clicked(bool checked)
{
  next_newDXCC_ = checked;
  ui_->newDXCCBand_check_box->setChecked(checked && newDXCCBand_);
  next_newDXCCBand_ = checked && newDXCCBand_;
  ui_->newDXCCBand_check_box->setEnabled(checked);
  ui_->beep_on_newDXCC_check_box->setChecked(checked && beepOnNewDXCC_);
  ui_->beep_on_newDXCC_check_box->setEnabled(checked);
  ui_->newDXCCBandMode_check_box->setChecked(checked && newDXCCBandMode_);
  next_newDXCCBandMode_ = checked && newDXCCBandMode_;
  ui_->newDXCCBandMode_check_box->setEnabled(checked);
  ui_->workedColor_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && next_workedColor_);
  ui_->workedColor_check_box->setEnabled(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->workedStriked_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedUnderlined_ && next_workedStriked_);
  ui_->workedStriked_check_box->setEnabled((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedUnderlined_);
  ui_->workedUnderlined_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedStriked_ && next_workedUnderlined_);
  ui_->workedUnderlined_check_box->setEnabled((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedStriked_);
  ui_->workedDontShow_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && next_workedDontShow_);
  ui_->workedDontShow_check_box->setEnabled(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->labNewDXCC->setVisible(next_newDXCC_);
  ui_->labNewMcDXCC->setVisible(next_newDXCC_);
  ui_->labNewScDXCC->setVisible(next_newDXCC_ && next_newPotential_);
  ui_->labNewDXCCBand->setVisible(next_newDXCCBand_ || next_newDXCCBandMode_);
  ui_->labNewMcDXCCBand->setVisible(next_newDXCCBand_ || next_newDXCCBandMode_);
  ui_->labNewScDXCCBand->setVisible((next_newDXCCBand_ || next_newDXCCBandMode_) && next_newPotential_);
  ui_->labWorkedCall->setVisible(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->labWorkedMcCall->setVisible(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->labWorkedScCall->setVisible((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && next_newPotential_);
}

void Configuration::impl::on_newCall_check_box_clicked(bool checked)
{
  next_newCall_ = checked;
  ui_->newCallBand_check_box->setChecked(checked && newCallBand_);
  next_newCallBand_ = checked && newCallBand_;
  ui_->newCallBand_check_box->setEnabled(checked);
  ui_->beep_on_newCall_check_box->setChecked(checked && beepOnNewCall_);
  ui_->beep_on_newCall_check_box->setEnabled(checked);
  ui_->newCallBandMode_check_box->setChecked(checked && newCallBandMode_);
  next_newCallBandMode_ = checked && newCallBandMode_;
  ui_->newCallBandMode_check_box->setEnabled(checked);
  ui_->workedColor_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && next_workedColor_);
  ui_->workedColor_check_box->setEnabled(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->workedStriked_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedUnderlined_ && next_workedStriked_);
  ui_->workedStriked_check_box->setEnabled((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedUnderlined_);
  ui_->workedUnderlined_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedStriked_ && next_workedUnderlined_);
  ui_->workedUnderlined_check_box->setEnabled((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedStriked_);
  ui_->workedDontShow_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && next_workedDontShow_);
  ui_->workedDontShow_check_box->setEnabled(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->labNewCall->setVisible(next_newCall_);
  ui_->labNewMcCall->setVisible(next_newCall_);
  ui_->labNewScCall->setVisible(next_newCall_ && next_newPotential_);
  ui_->labNewCallBand->setVisible(next_newCallBand_ || next_newCallBandMode_);
  ui_->labNewMcCallBand->setVisible(next_newCallBand_ || next_newCallBandMode_);
  ui_->labNewScCallBand->setVisible((next_newCallBand_ || next_newCallBandMode_) && next_newPotential_);
  ui_->labWorkedCall->setVisible(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->labWorkedMcCall->setVisible(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->labWorkedScCall->setVisible((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && next_newPotential_);
}

void Configuration::impl::on_newPx_check_box_clicked(bool checked)
{
  next_newPx_ = checked;
  ui_->newPxBand_check_box->setChecked(checked && newPxBand_);
  next_newPxBand_ = checked && newPxBand_;
  ui_->newPxBand_check_box->setEnabled(checked);
  ui_->beep_on_newPx_check_box->setChecked(checked && beepOnNewPx_);
  ui_->beep_on_newPx_check_box->setEnabled(checked);
  ui_->newPxBandMode_check_box->setChecked(checked && newPxBandMode_);
  next_newPxBandMode_ = checked && newPxBandMode_;
  ui_->newPxBandMode_check_box->setEnabled(checked);
  ui_->workedColor_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && next_workedColor_);
  ui_->workedColor_check_box->setEnabled(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->workedStriked_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedUnderlined_ && next_workedStriked_);
  ui_->workedStriked_check_box->setEnabled((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedUnderlined_);
  ui_->workedUnderlined_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedStriked_ && next_workedUnderlined_);
  ui_->workedUnderlined_check_box->setEnabled((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedStriked_);
  ui_->workedDontShow_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && next_workedDontShow_);
  ui_->workedDontShow_check_box->setEnabled(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->labNewPx->setVisible(next_newPx_);
  ui_->labNewMcPx->setVisible(next_newPx_);
  ui_->labNewScPx->setVisible(next_newPx_ && next_newPotential_);
  ui_->labNewPxBand->setVisible(next_newPxBand_ || next_newPxBandMode_);
  ui_->labNewMcPxBand->setVisible(next_newPxBand_ || next_newPxBandMode_);
  ui_->labNewScPxBand->setVisible((next_newPxBand_ || next_newPxBandMode_) && next_newPotential_);
  ui_->labWorkedCall->setVisible(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->labWorkedMcCall->setVisible(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->labWorkedScCall->setVisible((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && next_newPotential_);
}

void Configuration::impl::on_newGrid_check_box_clicked(bool checked)
{
  next_newGrid_ = checked;
  ui_->newGridBand_check_box->setChecked(checked && newGridBand_);
  next_newGridBand_ = checked && newGridBand_;
  ui_->newGridBand_check_box->setEnabled(checked);
  ui_->beep_on_newGrid_check_box->setChecked(checked && beepOnNewGrid_);
  ui_->beep_on_newGrid_check_box->setEnabled(checked);
  ui_->newGridBandMode_check_box->setChecked(checked && newGridBandMode_);
  next_newGridBandMode_ = checked && newGridBandMode_;
  ui_->newGridBandMode_check_box->setEnabled(checked);
  ui_->workedColor_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && next_workedColor_);
  ui_->workedColor_check_box->setEnabled(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->workedStriked_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedUnderlined_ && next_workedStriked_);
  ui_->workedStriked_check_box->setEnabled((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedUnderlined_);
  ui_->workedUnderlined_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedStriked_ && next_workedUnderlined_);
  ui_->workedUnderlined_check_box->setEnabled((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && !next_workedStriked_);
  ui_->workedDontShow_check_box->setChecked((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && next_workedDontShow_);
  ui_->workedDontShow_check_box->setEnabled(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->labNewGrid->setVisible(next_newGrid_);
  ui_->labNewMcGrid->setVisible(next_newGrid_);
  ui_->labNewScGrid->setVisible(next_newGrid_ && next_newPotential_);
  ui_->labNewGridBand->setVisible(next_newGridBand_ || next_newGridBandMode_);
  ui_->labNewMcGridBand->setVisible(next_newGridBand_ || next_newGridBandMode_);
  ui_->labNewScGridBand->setVisible((next_newGridBand_ || next_newGridBandMode_) && next_newPotential_);
  ui_->labWorkedCall->setVisible(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->labWorkedMcCall->setVisible(next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_);
  ui_->labWorkedScCall->setVisible((next_newCQZ_ || next_newITUZ_ || next_newDXCC_ || next_newGrid_ || next_newPx_ || next_newCall_) && next_newPotential_);
}

void Configuration::impl::on_newCQZBand_check_box_clicked(bool checked)
{
  next_newCQZBand_ = checked;
  ui_->labNewCQZBand->setVisible(next_newCQZBand_ || next_newCQZBandMode_);
  ui_->labNewMcCQZBand->setVisible(next_newCQZBand_ || next_newCQZBandMode_);
  ui_->labNewScCQZBand->setVisible((next_newCQZBand_ || next_newCQZBandMode_) && next_newPotential_);
}

void Configuration::impl::on_newITUZBand_check_box_clicked(bool checked)
{
  next_newITUZBand_ = checked;
  ui_->labNewITUZBand->setVisible(next_newITUZBand_ || next_newITUZBandMode_);
  ui_->labNewMcITUZBand->setVisible(next_newITUZBand_ || next_newITUZBandMode_);
  ui_->labNewScITUZBand->setVisible((next_newITUZBand_ || next_newITUZBandMode_) && next_newPotential_);
}

void Configuration::impl::on_newDXCCBand_check_box_clicked(bool checked)
{
  next_newDXCCBand_ = checked;
  ui_->labNewDXCCBand->setVisible(next_newDXCCBand_ || next_newDXCCBandMode_);
  ui_->labNewMcDXCCBand->setVisible(next_newDXCCBand_ || next_newDXCCBandMode_);
  ui_->labNewScDXCCBand->setVisible((next_newDXCCBand_ || next_newDXCCBandMode_) && next_newPotential_);
}

void Configuration::impl::on_newCallBand_check_box_clicked(bool checked)
{
  next_newCallBand_ = checked;
  ui_->labNewCallBand->setVisible(next_newCallBand_ || next_newCallBandMode_);
  ui_->labNewMcCallBand->setVisible(next_newCallBand_ || next_newCallBandMode_);
  ui_->labNewScCallBand->setVisible((next_newCallBand_ || next_newCallBandMode_) && next_newPotential_);
}

void Configuration::impl::on_newPxBand_check_box_clicked(bool checked)
{
  next_newPxBand_ = checked;
  ui_->labNewPxBand->setVisible(next_newPxBand_ || next_newPxBandMode_);
  ui_->labNewMcPxBand->setVisible(next_newPxBand_ || next_newPxBandMode_);
  ui_->labNewScPxBand->setVisible((next_newPxBand_ || next_newPxBandMode_) && next_newPotential_);
}

void Configuration::impl::on_newGridBand_check_box_clicked(bool checked)
{
  next_newGridBand_ = checked;
  ui_->labNewGridBand->setVisible(next_newGridBand_ || next_newGridBandMode_);
  ui_->labNewMcGridBand->setVisible(next_newGridBand_ || next_newGridBandMode_);
  ui_->labNewScGridBand->setVisible((next_newGridBand_ || next_newGridBandMode_) && next_newPotential_);
}

void Configuration::impl::on_newCQZBandMode_check_box_clicked(bool checked)
{
  next_newCQZBandMode_ = checked;
  ui_->labNewCQZBand->setVisible(next_newCQZBand_ || next_newCQZBandMode_);
  ui_->labNewMcCQZBand->setVisible(next_newCQZBand_ || next_newCQZBandMode_);
  ui_->labNewScCQZBand->setVisible((next_newCQZBand_ || next_newCQZBandMode_) && next_newPotential_);
}

void Configuration::impl::on_newITUZBandMode_check_box_clicked(bool checked)
{
  next_newITUZBandMode_ = checked;
  ui_->labNewITUZBand->setVisible(next_newITUZBand_ || next_newITUZBandMode_);
  ui_->labNewMcITUZBand->setVisible(next_newITUZBand_ || next_newITUZBandMode_);
  ui_->labNewScITUZBand->setVisible((next_newITUZBand_ || next_newITUZBandMode_) && next_newPotential_);
}

void Configuration::impl::on_newDXCCBandMode_check_box_clicked(bool checked)
{
  next_newDXCCBandMode_ = checked;
  ui_->labNewDXCCBand->setVisible(next_newDXCCBand_ || next_newDXCCBandMode_);
  ui_->labNewMcDXCCBand->setVisible(next_newDXCCBand_ || next_newDXCCBandMode_);
  ui_->labNewScDXCCBand->setVisible((next_newDXCCBand_ || next_newDXCCBandMode_) && next_newPotential_);
}

void Configuration::impl::on_newCallBandMode_check_box_clicked(bool checked)
{
  next_newCallBandMode_ = checked;
  ui_->labNewCallBand->setVisible(next_newCallBand_ || next_newCallBandMode_);
  ui_->labNewMcCallBand->setVisible(next_newCallBand_ || next_newCallBandMode_);
  ui_->labNewScCallBand->setVisible((next_newCallBand_ || next_newCallBandMode_) && next_newPotential_);
}

void Configuration::impl::on_newPxBandMode_check_box_clicked(bool checked)
{
  next_newPxBandMode_ = checked;
  ui_->labNewPxBand->setVisible(next_newPxBand_ || next_newPxBandMode_);
  ui_->labNewMcPxBand->setVisible(next_newPxBand_ || next_newPxBandMode_);
  ui_->labNewScPxBand->setVisible((next_newPxBand_ || next_newPxBandMode_) && next_newPotential_);
}

void Configuration::impl::on_newGridBandMode_check_box_clicked(bool checked)
{
  next_newGridBandMode_ = checked;
  ui_->labNewGridBand->setVisible(next_newGridBand_ || next_newGridBandMode_);
  ui_->labNewMcGridBand->setVisible(next_newGridBand_ || next_newGridBandMode_);
  ui_->labNewScGridBand->setVisible((next_newGridBand_ || next_newGridBandMode_) && next_newPotential_);
}

void Configuration::impl::on_pbCQmsg_clicked()
{
  auto new_color = QColorDialog::getColor(QColor(Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)), this, "CQ Messages Color");
  if (new_color.isValid ())
    {
      next_color_CQ_ = QColor(Radio::convert_dark(new_color.name(),useDarkStyle_));
      if (next_txtColor_) {
        ui_->labCQ->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_),Radio::convert_dark("#ffffff",useDarkStyle_)));
        ui_->labNewCQZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewCQZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewITUZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewITUZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewDXCC->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewDXCCBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewGrid->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewGridBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewPx->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewPxBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewCall->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewCallBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        if (next_workedColor_) {
          if (next_workedStriked_) {
            ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
          } else if (next_workedUnderlined_) {
            ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
          } else {
            ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
          }
        } else if (next_workedStriked_) {
          ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        } else if (next_workedUnderlined_) {
          ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        } else {
          ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        }
      } else {      
        ui_->labCQ->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_),Radio::convert_dark("#ffffff",useDarkStyle_)));
        ui_->labNewCQZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewCQZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewITUZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewITUZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewDXCC->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewDXCCBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewGrid->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewGridBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewPx->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewPxBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewCall->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewCallBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        if (next_workedColor_) {
          if (next_workedStriked_) {
            ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
          } else if (next_workedUnderlined_) {
            ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
          } else {
            ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
          }
        } else if (next_workedStriked_) {
          ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        } else if (next_workedUnderlined_) {
          ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        } else {
          ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        }
      }
    }
}

void Configuration::impl::on_pbMyCall_clicked()
{
  auto new_color = QColorDialog::getColor(QColor(Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)), this, "My Call Messages Color");
  if (new_color.isValid ())
    {
      next_color_MyCall_ = QColor(Radio::convert_dark(new_color.name(),useDarkStyle_));
      if (next_txtColor_) {
        ui_->labMyCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_),Radio::convert_dark("#ffffff",useDarkStyle_)));
        ui_->labNewMcCQZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewMcCQZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewMcITUZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewMcITUZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewMcDXCC->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewMcDXCCBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewMcGrid->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewMcGridBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewMcPx->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewMcPxBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewMcCall->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewMcCallBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        if (next_workedColor_) {
          if (next_workedStriked_) {
            ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
          } else if (next_workedUnderlined_) {
            ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
          } else {
            ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
          }
        } else if (next_workedStriked_) {
          ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        } else if (next_workedUnderlined_) {
          ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        } else {
          ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        }
      } else {      
        ui_->labMyCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_),Radio::convert_dark("#ffffff",useDarkStyle_)));
        ui_->labNewMcCQZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewMcCQZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewMcITUZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewMcITUZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewMcDXCC->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewMcDXCCBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewMcGrid->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewMcGridBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewMcPx->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewMcPxBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewMcCall->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewMcCallBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        if (next_workedColor_) {
          if (next_workedStriked_) {
            ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
          } else if (next_workedUnderlined_) {
            ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
          } else {
            ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
          }
        } else if (next_workedStriked_) {
          ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        } else if (next_workedUnderlined_) {
          ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        } else {
          ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        }
      }
    }
}

void Configuration::impl::on_pbStandardCall_clicked()
{
  auto new_color = QColorDialog::getColor(QColor(Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)), this, "Standard Messages Color");
  if (new_color.isValid ())
    {
      next_color_StandardCall_ = QColor(Radio::convert_dark(new_color.name(),useDarkStyle_));
      if (next_txtColor_) {
        ui_->labStandardCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_),Radio::convert_dark("#ffffff",useDarkStyle_)));
        ui_->labNewScCQZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        ui_->labNewScCQZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        ui_->labNewScITUZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        ui_->labNewScITUZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        ui_->labNewScDXCC->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        ui_->labNewScDXCCBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        ui_->labNewScGrid->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        ui_->labNewScGridBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        ui_->labNewScPx->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        ui_->labNewScPxBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        ui_->labNewScCall->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        ui_->labNewScCallBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        if (next_workedColor_) {
          if (next_workedStriked_) {
            ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
          } else if (next_workedUnderlined_) {
            ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
          } else {
            ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
          }
        } else if (next_workedStriked_) {
          ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        } else if (next_workedUnderlined_) {
          ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        } else {
          ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        }
      } else {
        ui_->labStandardCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_),Radio::convert_dark("#ffffff",useDarkStyle_)));
        ui_->labNewScCQZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        ui_->labNewScCQZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        ui_->labNewScITUZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        ui_->labNewScITUZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        ui_->labNewScDXCC->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        ui_->labNewScDXCCBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        ui_->labNewScGrid->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        ui_->labNewScGridBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        ui_->labNewScPx->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        ui_->labNewScPxBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        ui_->labNewScCall->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        ui_->labNewScCallBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        if (next_workedColor_) {
          if (next_workedStriked_) {
            ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
          } else if (next_workedUnderlined_) {
            ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
          } else {
            ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
          }
        } else if (next_workedStriked_) {
          ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        } else if (next_workedUnderlined_) {
          ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        } else {
          ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        }
      }
    }
}

void Configuration::impl::on_pbTxMsg_clicked()
{
  auto new_color = QColorDialog::getColor(QColor(Radio::convert_dark(next_color_TxMsg_.name(),useDarkStyle_)), this, "Tx Messages Color");
  if (new_color.isValid ())
    {
      next_color_TxMsg_ = QColor(Radio::convert_dark(new_color.name(),useDarkStyle_));
      ui_->labTx->setStyleSheet(QString("background: %1").arg(Radio::convert_dark(next_color_TxMsg_.name(),useDarkStyle_)));
    }
}

void Configuration::impl::on_pbNewCQZ_clicked()
{
  auto new_color = QColorDialog::getColor(QColor(Radio::convert_dark(next_color_NewCQZ_.name(),useDarkStyle_)), this, "New CQZ Messages Color");
  if (new_color.isValid ())
    {
      next_color_NewCQZ_ = QColor(Radio::convert_dark(new_color.name(),useDarkStyle_));
      if (next_txtColor_) {
        ui_->labNewCQZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcCQZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScCQZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else {
        ui_->labNewCQZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcCQZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScCQZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCQZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      }
    }
}

void Configuration::impl::on_pbNewCQZBand_clicked()
{
  auto new_color = QColorDialog::getColor(QColor(Radio::convert_dark(next_color_NewCQZBand_.name(),useDarkStyle_)), this, "New CQZ on Band/Mode Messages Color");
  if (new_color.isValid ())
    {
      next_color_NewCQZBand_ = QColor(Radio::convert_dark(new_color.name(),useDarkStyle_));
      if (next_txtColor_) {
        ui_->labNewCQZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcCQZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScCQZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else {
        ui_->labNewCQZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcCQZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScCQZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCQZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      }
    }
}

void Configuration::impl::on_pbNewITUZ_clicked()
{
  auto new_color = QColorDialog::getColor(QColor(Radio::convert_dark(next_color_NewITUZ_.name(),useDarkStyle_)), this, "New ITUZ Messages Color");
  if (new_color.isValid ())
    {
      next_color_NewITUZ_ = QColor(Radio::convert_dark(new_color.name(),useDarkStyle_));
      if (next_txtColor_) {
        ui_->labNewITUZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcITUZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScITUZ->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else {
        ui_->labNewITUZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcITUZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScITUZ->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewITUZ_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      }
    }
}

void Configuration::impl::on_pbNewITUZBand_clicked()
{
  auto new_color = QColorDialog::getColor(QColor(Radio::convert_dark(next_color_NewITUZBand_.name(),useDarkStyle_)), this, "New ITUZ on Band/Mode Messages Color");
  if (new_color.isValid ())
    {
      next_color_NewITUZBand_ = QColor(Radio::convert_dark(new_color.name(),useDarkStyle_));
      if (next_txtColor_) {
        ui_->labNewITUZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcITUZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScITUZBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else {
        ui_->labNewITUZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcITUZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScITUZBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewITUZBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      }
    }
}

void Configuration::impl::on_pbNewDXCC_clicked()
{
  auto new_color = QColorDialog::getColor(QColor(Radio::convert_dark(next_color_NewDXCC_.name(),useDarkStyle_)), this, "New DXCC Messages Color");
  if (new_color.isValid ())
    {
      next_color_NewDXCC_ = QColor(Radio::convert_dark(new_color.name(),useDarkStyle_));
      if (next_txtColor_) {
        ui_->labNewDXCC->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcDXCC->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScDXCC->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else {
        ui_->labNewDXCC->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcDXCC->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScDXCC->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewDXCC_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      }
    }
}

void Configuration::impl::on_pbNewDXCCBand_clicked()
{
  auto new_color = QColorDialog::getColor(QColor(Radio::convert_dark(next_color_NewDXCCBand_.name(),useDarkStyle_)), this, "New DXCC on Band/Mode Messages Color");
  if (new_color.isValid ())
    {
      next_color_NewDXCCBand_ = QColor(Radio::convert_dark(new_color.name(),useDarkStyle_));
      if (next_txtColor_) {
        ui_->labNewDXCCBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcDXCCBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScDXCCBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else {
        ui_->labNewDXCCBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcDXCCBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScDXCCBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewDXCCBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      }
    }
}

void Configuration::impl::on_pbNewGrid_clicked()
{
  auto new_color = QColorDialog::getColor(QColor(Radio::convert_dark(next_color_NewGrid_.name(),useDarkStyle_)), this, "New Grid Messages Color");
  if (new_color.isValid ())
    {
      next_color_NewGrid_ = QColor(Radio::convert_dark(new_color.name(),useDarkStyle_));
      if (next_txtColor_) {
        ui_->labNewGrid->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcGrid->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScGrid->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else {
        ui_->labNewGrid->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcGrid->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScGrid->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewGrid_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      }
    }
}

void Configuration::impl::on_pbNewGridBand_clicked()
{
  auto new_color = QColorDialog::getColor(QColor(Radio::convert_dark(next_color_NewGridBand_.name(),useDarkStyle_)), this, "New Grid on Band/Mode Messages Color");
  if (new_color.isValid ())
    {
      next_color_NewGridBand_ = QColor(Radio::convert_dark(new_color.name(),useDarkStyle_));
      if (next_txtColor_) {
        ui_->labNewGridBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcGridBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScGridBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else {
        ui_->labNewGridBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcGridBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScGridBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewGridBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      }
    }
}

void Configuration::impl::on_pbNewPx_clicked()
{
  auto new_color = QColorDialog::getColor(QColor(Radio::convert_dark(next_color_NewPx_.name(),useDarkStyle_)), this, "New Prefix Messages Color");
  if (new_color.isValid ())
    {
      next_color_NewPx_ = QColor(Radio::convert_dark(new_color.name(),useDarkStyle_));
      if (next_txtColor_) {
        ui_->labNewPx->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcPx->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScPx->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else {
        ui_->labNewPx->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcPx->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScPx->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewPx_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      }
    }
}

void Configuration::impl::on_pbNewPxBand_clicked()
{
  auto new_color = QColorDialog::getColor(QColor(Radio::convert_dark(next_color_NewPxBand_.name(),useDarkStyle_)), this, "New Prefix on Band/Mode Messages Color");
  if (new_color.isValid ())
    {
      next_color_NewPxBand_ = QColor(Radio::convert_dark(new_color.name(),useDarkStyle_));
      if (next_txtColor_) {
        ui_->labNewPxBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcPxBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScPxBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else {
        ui_->labNewPxBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcPxBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScPxBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewPxBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      }
    }
}

void Configuration::impl::on_pbNewCall_clicked()
{
  auto new_color = QColorDialog::getColor(QColor(Radio::convert_dark(next_color_NewCall_.name(),useDarkStyle_)), this, "New Call Messages Color");
  if (new_color.isValid ())
    {
      next_color_NewCall_ = QColor(Radio::convert_dark(new_color.name(),useDarkStyle_));
      if (next_txtColor_) {
        ui_->labNewCall->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcCall->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScCall->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else {
        ui_->labNewCall->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcCall->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScCall->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      }
    }
}

void Configuration::impl::on_pbNewCallBand_clicked()
{
  auto new_color = QColorDialog::getColor(QColor(Radio::convert_dark(next_color_NewCallBand_.name(),useDarkStyle_)), this, "New Call on Band/Mode Messages Color");
  if (new_color.isValid ())
    {
      next_color_NewCallBand_ = QColor(Radio::convert_dark(new_color.name(),useDarkStyle_));
      if (next_txtColor_) {
        ui_->labNewCallBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcCallBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScCallBand->setStyleSheet(QString("font-weight: bold;background: %2;color: %1").arg(Radio::convert_dark(next_color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      } else {
        ui_->labNewCallBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
        ui_->labNewMcCallBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
        ui_->labNewScCallBand->setStyleSheet(QString("font-weight: bold;background: %1;color: %2").arg(Radio::convert_dark(next_color_NewCallBand_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
      }
    }
}

void Configuration::impl::on_pbWorkedCall_clicked()
{
  auto new_color = QColorDialog::getColor(QColor(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_)), this, "Worked Call Color");
  if (new_color.isValid ())
    {
      next_color_WorkedCall_ = QColor(Radio::convert_dark(new_color.name(),useDarkStyle_));
      if (next_txtColor_) {
        if (next_workedColor_) {
          if (next_workedStriked_) {
            ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
            ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
            ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
          } else if (next_workedUnderlined_) {
            ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
            ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
            ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
          } else {
            ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
            ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
            ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
          }
        } else if (next_workedStriked_) {
          ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
          ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
          ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        } else if (next_workedUnderlined_) {
          ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
          ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
          ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        } else {
          ui_->labWorkedCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
          ui_->labWorkedMcCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
          ui_->labWorkedScCall->setStyleSheet(QString("background: %2;color: %1").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        }
      } else {
        if (next_workedColor_) {
          if (next_workedStriked_) {
            ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
            ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
            ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
          } else if (next_workedUnderlined_) {
            ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
            ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
            ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
          } else {
            ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
            ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
            ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark(next_color_WorkedCall_.name(),useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
          }
        } else if (next_workedStriked_) {
          ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
          ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
          ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: line-through").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        } else if (next_workedUnderlined_) {
          ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
          ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
          ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2;text-decoration: underline").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        } else {
          ui_->labWorkedCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_CQ_.name(),useDarkStyle_)));
          ui_->labWorkedMcCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_MyCall_.name(),useDarkStyle_)));
          ui_->labWorkedScCall->setStyleSheet(QString("background: %1;color: %2").arg(Radio::convert_dark("#ffffff",useDarkStyle_),Radio::convert_dark(next_color_StandardCall_.name(),useDarkStyle_)));
        }
      }
    }
}

void Configuration::impl::on_decoded_text_font_push_button_clicked ()
{
  next_decoded_text_font_ = QFontDialog::getFont (0, decoded_text_font_ , this
                                                  , tr ("JTDX Decoded Text Font Chooser")
#if QT_VERSION >= 0x050201
                                                  , QFontDialog::MonospacedFonts
#endif
                                                  );
}

void Configuration::impl::on_PTT_port_combo_box_activated (int /* index */)
{
  set_rig_invariants ();
}

void Configuration::impl::on_CAT_port_combo_box_activated (int /* index */)
{
  set_rig_invariants ();
}

void Configuration::impl::on_CAT_serial_baud_combo_box_currentIndexChanged (int /* index */)
{
  set_rig_invariants ();
}

void Configuration::impl::on_CAT_handshake_button_group_buttonClicked (int /* id */)
{
  set_rig_invariants ();
}

void Configuration::impl::on_rig_combo_box_currentIndexChanged (int /* index */)
{
  set_rig_invariants ();
  if(ui_->rig_combo_box->currentText().startsWith("OmniRig") || ui_->rig_combo_box->currentText().startsWith("DX Lab") ||
     ui_->rig_combo_box->currentText().startsWith("Ham Radio") || ui_->rig_combo_box->currentText().startsWith("Kenwood TS-480") ||
     ui_->rig_combo_box->currentText().startsWith("Kenwood TS-850") ||
     ui_->rig_combo_box->currentText().startsWith("Kenwood TS-870")) {
     ui_->rig_power_check_box->setChecked (false); ui_->rig_power_check_box->setEnabled (false);
     ui_->S_meter_check_box->setChecked (false); ui_->S_meter_check_box->setEnabled (false);
     ui_->output_power_check_box->setChecked (false); ui_->output_power_check_box->setEnabled (false);
  }
}

void Configuration::impl::on_CAT_data_bits_button_group_buttonClicked (int /* id */)
{
  set_rig_invariants ();
}

void Configuration::impl::on_CAT_stop_bits_button_group_buttonClicked (int /* id */)
{
  set_rig_invariants ();
}

void Configuration::impl::on_CAT_poll_interval_spin_box_valueChanged (int /* value */)
{
  set_rig_invariants ();
}

void Configuration::impl::on_split_mode_button_group_buttonClicked (int /* id */)
{
  set_rig_invariants ();
}

void Configuration::impl::on_test_CAT_push_button_clicked ()
{
  if (!validate ())
    {
      return;
    }

  ui_->test_CAT_push_button->setStyleSheet ({});
  if (open_rig (true))
    {
      //Q_EMIT sync (true);
    }

  set_rig_invariants ();
}

void Configuration::impl::on_test_PTT_push_button_clicked (bool checked)
{
  ui_->test_PTT_push_button->setChecked (!checked); // let status
                                                    // update check us
  if (!validate ())
    {
      return;
    }

  if (open_rig ())
    {
      Q_EMIT self_->transceiver_ptt (checked);
    }
}

void Configuration::impl::on_force_DTR_combo_box_currentIndexChanged (int /* index */)
{
  set_rig_invariants ();
}

void Configuration::impl::on_force_RTS_combo_box_currentIndexChanged (int /* index */)
{
  set_rig_invariants ();
}

void Configuration::impl::on_PTT_method_button_group_buttonClicked (int /* id */)
{
  set_rig_invariants ();
}

void Configuration::impl::on_callsign_line_edit_textChanged ()
{
  auto pos = ui_->callsign_line_edit->cursorPosition (); ui_->callsign_line_edit->setText (ui_->callsign_line_edit->text ().toUpper ());
  ui_->callsign_line_edit->setCursorPosition (pos);
}

void Configuration::impl::on_grid_line_edit_textChanged ()
{
  auto pos = ui_->grid_line_edit->cursorPosition (); auto text = ui_->grid_line_edit->text ();
  ui_->grid_line_edit->setText (text.left (4).toUpper () + text.mid (4).toLower ()); ui_->grid_line_edit->setCursorPosition (pos);
}

void Configuration::impl::on_content_line_edit_textChanged (QString const& text)
{
  auto pos = ui_->content_line_edit->cursorPosition (); ui_->content_line_edit->setText(text.toUpper ()); ui_->content_line_edit->setCursorPosition (pos);
  if(text.isEmpty ()) ui_->enableContent_check_box->setChecked(false);
  ui_->enableContent_check_box->setEnabled(!text.isEmpty ());
}

void Configuration::impl::on_countries_line_edit_textChanged (QString const& text)
{
  auto pos = ui_->countries_line_edit->cursorPosition (); ui_->countries_line_edit->setText(text.toUpper ()); ui_->countries_line_edit->setCursorPosition (pos);
  ui_->enableCountryFilter_check_box->setChecked(!text.isEmpty ());
  ui_->enableCountryFilter_check_box->setEnabled(!text.isEmpty ());
}

void Configuration::impl::on_callsigns_line_edit_textChanged (QString const& text)
{
  auto pos = ui_->callsigns_line_edit->cursorPosition (); ui_->callsigns_line_edit->setText(text.toUpper ()); ui_->callsigns_line_edit->setCursorPosition (pos);
  ui_->enableCallsignFilter_check_box->setChecked(!text.isEmpty ());
  ui_->enableCallsignFilter_check_box->setEnabled(!text.isEmpty ());
}

void Configuration::impl::on_sound_input_combo_box_currentTextChanged (QString const& text)
{
  default_audio_input_device_selected_ = QAudioDeviceInfo::defaultInputDevice ().deviceName () == text;
}

void Configuration::impl::on_sound_output_combo_box_currentTextChanged (QString const& text)
{
  default_audio_output_device_selected_ = QAudioDeviceInfo::defaultOutputDevice ().deviceName () == text;
}

void Configuration::impl::on_hhComboBox_1_currentIndexChanged(int index)
{
  if (index > 0) {
     ui_->mmComboBox_1->setEnabled(true);
     ui_->mmComboBox_1->setCurrentText (sched_mm_1_);
  } else {
     ui_->mmComboBox_1->setEnabled(false);
     ui_->mmComboBox_1->setCurrentIndex(0);
     sched_mm_1_="";
  }
//  printf("hh1 changed %d\n",index);
}

void Configuration::impl::on_mmComboBox_1_currentIndexChanged(int index)
{
  if (index > 0) {
     ui_->bandComboBox_1->setEnabled(true);
     ui_->bandComboBox_1->setCurrentText (sched_band_1_);
  } else {
     ui_->bandComboBox_1->setEnabled(false);
     ui_->bandComboBox_1->setCurrentIndex(-1);
     sched_band_1_="";
  }
//  printf("mm1 changed %d\n",index);
}

void Configuration::impl::on_bandComboBox_1_currentIndexChanged(QString const& text)
{
  if (!text.isEmpty ()){
      ui_->bandComboBox_1->setCurrentText(text);
      ui_->hhComboBox_2->setEnabled(true);
      ui_->hhComboBox_2->setCurrentText (sched_hh_2_);
      ui_->UseSched_check_box->setEnabled(true);
      ui_->UseSched_check_box->setChecked (usesched_);
      if (text.right(4) == "JT65") {
        ui_->band_mix_check_box_1->setEnabled(true);
        ui_->band_mix_check_box_1->setChecked (sched_mix_1_);
      } else {
        ui_->band_mix_check_box_1->setEnabled(false);
        ui_->band_mix_check_box_1->setChecked (false);
      }
  } else if (ui_->bandComboBox_1->isEnabled()) {
      ui_->bandComboBox_1->setCurrentText(sched_band_1_);
      if (!sched_band_1_.isEmpty ()){
        ui_->hhComboBox_2->setEnabled(true);
        ui_->hhComboBox_2->setCurrentText (sched_hh_2_);
        ui_->UseSched_check_box->setEnabled(true);
        ui_->UseSched_check_box->setChecked (usesched_);
        if (sched_band_1_.right(4) == "JT65") {
          ui_->band_mix_check_box_1->setEnabled(true);
          ui_->band_mix_check_box_1->setChecked (sched_mix_1_);
        } else {
          ui_->band_mix_check_box_1->setEnabled(false);
          ui_->band_mix_check_box_1->setChecked (false);
        }
      } else {
        ui_->hhComboBox_2->setEnabled(false);
        ui_->hhComboBox_2->setCurrentIndex(0);
        sched_hh_2_="";
        ui_->UseSched_check_box->setEnabled(false);
        ui_->UseSched_check_box->setChecked (false);
        ui_->band_mix_check_box_1->setEnabled(false);
        ui_->band_mix_check_box_1->setChecked (false);
      }
  } 
  else {
      ui_->bandComboBox_1->setCurrentText("");
      sched_band_1_="";
      ui_->hhComboBox_2->setEnabled(false);
      ui_->hhComboBox_2->setCurrentIndex(0);
      sched_hh_2_="";
      ui_->UseSched_check_box->setEnabled(false);
      ui_->UseSched_check_box->setChecked (false);
      ui_->band_mix_check_box_1->setEnabled(false);
      ui_->band_mix_check_box_1->setChecked (false);
  }
//  printf("band1 changed %s:%s\n",text.toStdString().c_str(),ui_->bandComboBox_1->currentText().toStdString().c_str());
}

void Configuration::impl::on_hhComboBox_2_currentIndexChanged(int index)
{
  if (index > 0) {
     ui_->mmComboBox_2->setEnabled(true);
     ui_->mmComboBox_2->setCurrentText (sched_mm_2_);
  } else {
     ui_->mmComboBox_2->setEnabled(false);
     ui_->mmComboBox_2->setCurrentIndex(0);
     sched_mm_2_="";
  }
//  printf("hh2 changed %d\n",index);
}

void Configuration::impl::on_mmComboBox_2_currentIndexChanged(int index)
{
  if (index > 0) {
     ui_->bandComboBox_2->setEnabled(true);
     ui_->bandComboBox_2->setCurrentText (sched_band_2_);
  } else {
     ui_->bandComboBox_2->setEnabled(false);
     ui_->bandComboBox_2->setCurrentIndex(-1);
     sched_band_2_="";
  }
//  printf("mm2 changed %d\n",index);
}

void Configuration::impl::on_bandComboBox_2_currentIndexChanged(QString const& text)
{
  if (!text.isEmpty ()){
      ui_->bandComboBox_2->setCurrentText(text);
      ui_->hhComboBox_3->setEnabled(true);
      ui_->hhComboBox_3->setCurrentText (sched_hh_3_);
      if (text.right(4) == "JT65") {
        ui_->band_mix_check_box_2->setEnabled(true);
        ui_->band_mix_check_box_2->setChecked (sched_mix_2_);
      } else {
        ui_->band_mix_check_box_2->setEnabled(false);
        ui_->band_mix_check_box_2->setChecked (false);
      }
  } else if (ui_->bandComboBox_2->isEnabled()) {
      ui_->bandComboBox_2->setCurrentText(sched_band_2_);
      if (!sched_band_2_.isEmpty ()){
        ui_->hhComboBox_3->setEnabled(true);
        ui_->hhComboBox_3->setCurrentText (sched_hh_3_);
        if (sched_band_2_.right(4) == "JT65") {
          ui_->band_mix_check_box_2->setEnabled(true);
          ui_->band_mix_check_box_2->setChecked (sched_mix_2_);
        } else {
          ui_->band_mix_check_box_2->setEnabled(false);
          ui_->band_mix_check_box_2->setChecked (false);
        }
      } else {
        ui_->hhComboBox_3->setEnabled(false);
        ui_->hhComboBox_3->setCurrentIndex(0);
        sched_hh_3_="";
        ui_->band_mix_check_box_2->setEnabled(false);
        ui_->band_mix_check_box_2->setChecked (false);
      }
  } 
  else {
      ui_->bandComboBox_2->setCurrentText("");
      sched_band_2_="";
      ui_->hhComboBox_3->setEnabled(false);
      ui_->hhComboBox_3->setCurrentIndex(0);
      sched_hh_3_="";
      ui_->band_mix_check_box_2->setEnabled(false);
      ui_->band_mix_check_box_2->setChecked (false);
  }
//  printf("band2 changed %s:%s\n",text.toStdString().c_str(),ui_->bandComboBox_2->currentText().toStdString().c_str());
}

void Configuration::impl::on_hhComboBox_3_currentIndexChanged(int index)
{
  if (index > 0) {
     ui_->mmComboBox_3->setEnabled(true);
     ui_->mmComboBox_3->setCurrentText (sched_mm_3_);
  } else {
     ui_->mmComboBox_3->setEnabled(false);
     ui_->mmComboBox_3->setCurrentIndex(0);
     sched_mm_3_="";
  }
//  printf("hh3 changed %d\n",index);
}

void Configuration::impl::on_mmComboBox_3_currentIndexChanged(int index)
{
  if (index > 0) {
     ui_->bandComboBox_3->setEnabled(true);
     ui_->bandComboBox_3->setCurrentText (sched_band_3_);
  } else {
     ui_->bandComboBox_3->setEnabled(false);
     ui_->bandComboBox_3->setCurrentIndex(-1);
     sched_band_3_="";
  }
//  printf("mm3 changed %d\n",index);
}

void Configuration::impl::on_bandComboBox_3_currentIndexChanged(QString const& text)
{
  if (!text.isEmpty ()){
      ui_->bandComboBox_3->setCurrentText(text);
      ui_->hhComboBox_4->setEnabled(true);
      ui_->hhComboBox_4->setCurrentText (sched_hh_4_);
      if (text.right(4) == "JT65") {
        ui_->band_mix_check_box_3->setEnabled(true);
        ui_->band_mix_check_box_3->setChecked (sched_mix_3_);
      } else {
        ui_->band_mix_check_box_3->setEnabled(false);
        ui_->band_mix_check_box_3->setChecked (false);
      }
  } else if (ui_->bandComboBox_3->isEnabled()) {
      ui_->bandComboBox_3->setCurrentText(sched_band_3_);
      if (!sched_band_3_.isEmpty ()){
        ui_->hhComboBox_4->setEnabled(true);
        ui_->hhComboBox_4->setCurrentText (sched_hh_4_);
        if (sched_band_3_.right(4) == "JT65") {
          ui_->band_mix_check_box_3->setEnabled(true);
          ui_->band_mix_check_box_3->setChecked (sched_mix_3_);
        } else {
          ui_->band_mix_check_box_3->setEnabled(false);
          ui_->band_mix_check_box_3->setChecked (false);
        }
      } else {
        ui_->hhComboBox_4->setEnabled(false);
        ui_->hhComboBox_4->setCurrentIndex(0);
        sched_hh_4_="";
        ui_->band_mix_check_box_3->setEnabled(false);
        ui_->band_mix_check_box_3->setChecked (false);
      }
  } 
  else {
      ui_->bandComboBox_3->setCurrentText("");
      sched_band_3_="";
      ui_->hhComboBox_4->setEnabled(false);
      ui_->hhComboBox_4->setCurrentIndex(0);
      sched_hh_4_="";
      ui_->band_mix_check_box_3->setEnabled(false);
      ui_->band_mix_check_box_3->setChecked (false);
  }
//  printf("band3 changed %s:%s\n",text.toStdString().c_str(),ui_->bandComboBox_3->currentText().toStdString().c_str());
}

void Configuration::impl::on_hhComboBox_4_currentIndexChanged(int index)
{
  if (index > 0) {
     ui_->mmComboBox_4->setEnabled(true);
     ui_->mmComboBox_4->setCurrentText (sched_mm_4_);
  } else {
     ui_->mmComboBox_4->setEnabled(false);
     ui_->mmComboBox_4->setCurrentIndex(0);
     sched_mm_4_="";
  }
//  printf("hh4 changed %d\n",index);
}

void Configuration::impl::on_mmComboBox_4_currentIndexChanged(int index)
{
  if (index > 0) {
     ui_->bandComboBox_4->setEnabled(true);
     ui_->bandComboBox_4->setCurrentText (sched_band_4_);
  } else {
     ui_->bandComboBox_4->setEnabled(false);
     ui_->bandComboBox_4->setCurrentIndex(-1);
     sched_band_4_="";
  }
//  printf("mm4 changed %d\n",index);
}

void Configuration::impl::on_bandComboBox_4_currentIndexChanged(QString const& text)
{
  if (!text.isEmpty ()){
      ui_->bandComboBox_4->setCurrentText(text);
      ui_->hhComboBox_5->setEnabled(true);
      ui_->hhComboBox_5->setCurrentText (sched_hh_5_);
      if (text.right(4) == "JT65") {
        ui_->band_mix_check_box_4->setEnabled(true);
        ui_->band_mix_check_box_4->setChecked (sched_mix_4_);
      } else {
        ui_->band_mix_check_box_4->setEnabled(false);
        ui_->band_mix_check_box_4->setChecked (false);
      }
  } else if (ui_->bandComboBox_4->isEnabled()) {
      ui_->bandComboBox_4->setCurrentText(sched_band_4_);
      if (!sched_band_4_.isEmpty ()){
        ui_->hhComboBox_5->setEnabled(true);
        ui_->hhComboBox_5->setCurrentText (sched_hh_5_);
        if (sched_band_4_.right(4) == "JT65") {
          ui_->band_mix_check_box_4->setEnabled(true);
          ui_->band_mix_check_box_4->setChecked (sched_mix_4_);
        } else {
          ui_->band_mix_check_box_4->setEnabled(false);
          ui_->band_mix_check_box_4->setChecked (false);
        }
      } else {
        ui_->hhComboBox_5->setEnabled(false);
        ui_->hhComboBox_5->setCurrentIndex(0);
        sched_hh_5_="";
        ui_->band_mix_check_box_4->setEnabled(false);
        ui_->band_mix_check_box_4->setChecked (false);
      }
  } 
  else {
      ui_->bandComboBox_4->setCurrentText("");
      sched_band_4_="";
      ui_->hhComboBox_5->setEnabled(false);
      ui_->hhComboBox_5->setCurrentIndex(0);
      sched_hh_5_="";
      ui_->band_mix_check_box_4->setEnabled(false);
      ui_->band_mix_check_box_4->setChecked (false);
  }
//  printf("band4 changed %s:%s\n",text.toStdString().c_str(),ui_->bandComboBox_4->currentText().toStdString().c_str());
}

void Configuration::impl::on_hhComboBox_5_currentIndexChanged(int index)
{
  if (index > 0) {
     ui_->mmComboBox_5->setEnabled(true);
     ui_->mmComboBox_5->setCurrentText (sched_mm_5_);
  } else {
     ui_->mmComboBox_5->setEnabled(false);
     ui_->mmComboBox_5->setCurrentIndex(0);
     sched_mm_5_="";
  }
//  printf("hh5 changed %d\n",index);
}

void Configuration::impl::on_mmComboBox_5_currentIndexChanged(int index)
{
  if (index > 0) {
     ui_->bandComboBox_5->setEnabled(true);
     ui_->bandComboBox_5->setCurrentText (sched_band_5_);
  } else {
     ui_->bandComboBox_5->setEnabled(false);
     ui_->bandComboBox_5->setCurrentIndex(-1);
     sched_band_5_="";
  }
//  printf("mm5 changed %d\n",index);
}

void Configuration::impl::on_bandComboBox_5_currentIndexChanged(QString const& text)
{
  if (!text.isEmpty ()){
      ui_->bandComboBox_5->setCurrentText(text);
      if (text.right(4) == "JT65") {
        ui_->band_mix_check_box_5->setEnabled(true);
        ui_->band_mix_check_box_5->setChecked (sched_mix_5_);
      } else {
        ui_->band_mix_check_box_5->setEnabled(false);
        ui_->band_mix_check_box_5->setChecked (false);
      }
  } else if (ui_->bandComboBox_5->isEnabled()) {
      ui_->bandComboBox_5->setCurrentText(sched_band_5_);
      if (!sched_band_5_.isEmpty ()){
        if (sched_band_5_.right(4) == "JT65") {
          ui_->band_mix_check_box_5->setEnabled(true);
          ui_->band_mix_check_box_5->setChecked (sched_mix_5_);
        } else {
          ui_->band_mix_check_box_5->setEnabled(false);
          ui_->band_mix_check_box_5->setChecked (false);
        }
      } else {
        ui_->band_mix_check_box_5->setEnabled(false);
        ui_->band_mix_check_box_5->setChecked (false);
      }
  } 
  else {
      ui_->bandComboBox_5->setCurrentText("");
      sched_band_5_="";
      ui_->band_mix_check_box_5->setEnabled(false);
      ui_->band_mix_check_box_5->setChecked (false);
  }
//  printf("band5 changed %s:%s\n",text.toStdString().c_str(),ui_->bandComboBox_5->currentText().toStdString().c_str());
}

void Configuration::impl::on_add_macro_line_edit_editingFinished ()
{
  ui_->add_macro_line_edit->setText (ui_->add_macro_line_edit->text ().toUpper ());
}

void Configuration::impl::on_delete_macro_push_button_clicked (bool /* checked */)
{
  auto selection_model = ui_->macros_list_view->selectionModel ();
  if (selection_model->hasSelection ())
    {
      // delete all selected items
      delete_selected_macros (selection_model->selectedRows ());
    }
}

void Configuration::impl::delete_macro ()
{
  auto selection_model = ui_->macros_list_view->selectionModel ();
  if (!selection_model->hasSelection ())
    {
      // delete item under cursor if any
      auto index = selection_model->currentIndex ();
      if (index.isValid ())
        {
          next_macros_.removeRow (index.row ());
        }
    }
  else
    {
      // delete the whole selection
      delete_selected_macros (selection_model->selectedRows ());
    }
}

void Configuration::impl::delete_selected_macros (QModelIndexList selected_rows)
{
  // sort in reverse row order so that we can delete without changing
  // indices underneath us
  std::sort (selected_rows.begin (), selected_rows.end (), [] (QModelIndex const& lhs, QModelIndex const& rhs)
         {
           return rhs.row () < lhs.row (); // reverse row ordering
         });

  // now delete them
  Q_FOREACH (auto index, selected_rows)
    {
      next_macros_.removeRow (index.row ());
    }
}

void Configuration::impl::on_add_macro_push_button_clicked (bool /* checked */)
{
  if (next_macros_.insertRow (next_macros_.rowCount ()))
    {
      auto index = next_macros_.index (next_macros_.rowCount () - 1);
      ui_->macros_list_view->setCurrentIndex (index);
      next_macros_.setData (index, ui_->add_macro_line_edit->text ());
      ui_->add_macro_line_edit->clear ();
    }
}

void Configuration::impl::delete_frequencies ()
{
  auto selection_model = ui_->frequencies_table_view->selectionModel ();
  selection_model->select (selection_model->selection (), QItemSelectionModel::SelectCurrent | QItemSelectionModel::Rows);
  next_frequencies_.removeDisjointRows (selection_model->selectedRows ());
  ui_->frequencies_table_view->resizeColumnToContents (FrequencyList_v2::mode_column);
}

void Configuration::impl::load_frequencies ()
{
  QDialogButtonBox::tr("OK");
  
  QFileDialog::tr("Directory:");
  QFileDialog::tr("File &name:");
  QFileDialog::tr("&Open");
  QFileDialog::tr("&Choose");
  QFileDialog::tr("&Save");
  QFileDialog::tr("Cancel");
  QFileDialog::tr("All files (*)");
  QFileDialog::tr("New Folder");
  QFileDialog::tr("Delete");
  QFileDialog::tr("&Delete");
  QFileDialog::tr("&Rename");
  QFileDialog::tr("Show &hidden files");
  QFileDialog::tr("&New Folder");
  QFileDialog::tr("Look in:");
  QFileDialog::tr("Files of type:");
  QFileDialog::tr("'%1' is write protected.\nDo you want to delete it anyway?");
  QFileDialog::tr("Are you sure you want to delete '%1'?");
  QFileDialog::tr("Could not delete directory.");
  QFileDialog::tr("%1\nDirectory not found.\nPlease verify the "
                                            "correct directory name was given.");
  QFileDialog::tr("Recent Places");
  QFileDialog::tr("Back");
  QFileDialog::tr("Go back");
  QFileDialog::tr("Alt+Left");
  QFileDialog::tr("Forward");
  QFileDialog::tr("Go forward");
  QFileDialog::tr("Alt+Right");
  QFileDialog::tr("Parent Directory");
  QFileDialog::tr("Go to the parent directory");
  QFileDialog::tr("Alt+Up");
  QFileDialog::tr("Create New Folder");
  QFileDialog::tr("Create a New Folder");
  QFileDialog::tr("List View");
  QFileDialog::tr("Change to list view mode");
  QFileDialog::tr("Detail View");
  QFileDialog::tr("Change to detail view mode");
  QFileDialog::tr("Sidebar");
  QFileDialog::tr("List of places and bookmarks");

  QFileSystemModel::tr("Name");
  QFileSystemModel::tr("Size");
  QFileSystemModel::tr("Type");
  QFileSystemModel::tr("Date Modified");

  QFileDialog* fileDlg=new QFileDialog(this);
    fileDlg->setWindowTitle(tr ("Load Working Frequencies"));
  fileDlg->setFileMode(QFileDialog::ExistingFile);
  fileDlg->setNameFilter(tr ("Frequency files (*.qrg);;All files (*.*)"));
  fileDlg->setDirectory(data_dir_.absolutePath ());
  fileDlg->setLabelText(QFileDialog::Reject,tr("Cancel"));
  QString file_name = "";
  if (fileDlg->exec()) {
      file_name = fileDlg->selectedFiles()[0];
  }   
//  auto file_name = fileDlg->getOpenFileName (this, tr ("Load Working Frequencies"), data_dir_.absolutePath (), tr ("Frequency files (*.qrg);;All files (*.*)"),NULL,QFileDialog::DontUseNativeDialog);
  delete fileDlg;
  if (!file_name.isEmpty ())
    {
      
      auto const list = read_frequencies_file (file_name);
      if (list.size ()
          && (!next_frequencies_.frequency_list ().size ()
              || JTDXMessageBox::Yes == JTDXMessageBox::query_message (this
                                                               , tr ("Replace Working Frequencies")
                                                               , tr ("Are you sure you want to discard your current "
                                                                     "working frequencies and replace them with the "
                                                                     "loaded ones?"))))
        {
          next_frequencies_.frequency_list (list); // update the model
        }
    }
}

void Configuration::impl::merge_frequencies ()
{
  QFileDialog* fileDlg=new QFileDialog(this);
    fileDlg->setWindowTitle(tr ("Merge Working Frequencies"));
  fileDlg->setFileMode(QFileDialog::ExistingFile);
  fileDlg->setNameFilter(tr ("Frequency files (*.qrg);;All files (*.*)"));
  fileDlg->setDirectory(data_dir_.absolutePath ());
  fileDlg->setLabelText(QFileDialog::Reject,tr("Cancel"));
  QString file_name = "";
  if (fileDlg->exec()) {
      file_name = fileDlg->selectedFiles()[0];
  }   
//  auto file_name = QFileDialog::getOpenFileName (this, tr ("Merge Working Frequencies"), data_dir_.absolutePath (), tr ("Frequency files (*.qrg);;All files (*.*)"));
delete fileDlg;
  if (!file_name.isEmpty ())
    {
      next_frequencies_.frequency_list_merge (read_frequencies_file (file_name)); // update the model
    }
}

FrequencyList_v2::FrequencyItems Configuration::impl::read_frequencies_file (QString const& file_name)
{
  QFile frequencies_file {file_name};
  frequencies_file.open (QFile::ReadOnly);
  QDataStream ids {&frequencies_file};
  FrequencyList_v2::FrequencyItems list;
  quint32 magic;
  ids >> magic;
  if (qrg_magic != magic)
    {
      JTDXMessageBox::warning_message (this, tr ("Not a valid frequencies file"), tr ("Incorrect file magic"));
      return list;
    }
  quint32 version;
  ids >> version;
  // handle version checks and QDataStream version here if
  // necessary
  if (version > qrg_version)
    {
      JTDXMessageBox::warning_message (this, tr ("Not a valid frequencies file"), tr ("Version is too new"));
      return list;
    }

  // de-serialize the data using version if necessary to
  // handle old schemata
  ids >> list;

  if (ids.status () != QDataStream::Ok || !ids.atEnd ())
    {
      JTDXMessageBox::warning_message (this, tr ("Not a valid frequencies file"), tr ("Contents corrupt"));
      list.clear ();
      return list;
    }

  return list;
}

void Configuration::impl::save_frequencies ()
{
  auto file_name = QFileDialog::getSaveFileName (this, tr ("Save Working Frequencies"), data_dir_.absolutePath (), tr ("Frequency files (*.qrg);;All files (*.*)"));
  if (!file_name.isNull ())
    {
      if (!file_name.endsWith(".qrg")) file_name += ".qrg";
      QFile frequencies_file {file_name};
      frequencies_file.open (QFile::WriteOnly);
      QDataStream ods {&frequencies_file};
      auto selection_model = ui_->frequencies_table_view->selectionModel ();
      if (selection_model->hasSelection ()) {
          JTDXMessageBox msgbox;
          msgbox.setWindowTitle(tr("Only Save Selected  Working Frequencies"));
          msgbox.setIcon(JTDXMessageBox::Question);
          msgbox.setText(tr("Are you sure you want to save only the "
                                                                 "working frequencies that are currently selected? "
                                                                 "Click No to save all."));
          msgbox.setStandardButtons(JTDXMessageBox::Yes | JTDXMessageBox::No);
          msgbox.setDefaultButton(JTDXMessageBox::No);
          msgbox.translate_buttons();
          
          if (JTDXMessageBox::Yes == msgbox.exec())
            {
              selection_model->select (selection_model->selection (), QItemSelectionModel::SelectCurrent | QItemSelectionModel::Rows);
              ods << qrg_magic << qrg_version << next_frequencies_.frequency_list (selection_model->selectedRows ());
            }
          else 
            {
              ods << qrg_magic << qrg_version << next_frequencies_.frequency_list ();
            }
        }
      else
        {
          ods << qrg_magic << qrg_version << next_frequencies_.frequency_list ();
        }
    }
}

void Configuration::impl::reset_frequencies ()
{
  JTDXMessageBox msgbox;
  msgbox.setWindowTitle(tr("Reset Working Frequencies"));
  msgbox.setIcon(JTDXMessageBox::Question);
  msgbox.setText(tr("Are you sure you want to discard your current "
                                                       "working frequencies and replace them with default "
                                                       "ones?"));
  msgbox.setStandardButtons(JTDXMessageBox::Yes | JTDXMessageBox::No);
  msgbox.setDefaultButton(JTDXMessageBox::No);
  msgbox.translate_buttons();

  if (JTDXMessageBox::Yes == msgbox.exec())
    {
      next_frequencies_.reset_to_defaults ();
    }
}

void Configuration::impl::on_content_reset_push_button_clicked ()
{
	ui_->content_line_edit->setText ("AVI,CMD,GIF,HTML,HYBRID,IMAGE,JOINT,JPG,MP4,PHOTO");
}

void Configuration::impl::on_countries_clear_push_button_clicked ()
{
    ui_->countries_line_edit->setText ("");
    ui_->enableCountryFilter_check_box->setChecked(false);
}

void Configuration::impl::on_callsigns_clear_push_button_clicked ()
{
    ui_->callsigns_line_edit->setText ("");
    ui_->enableCallsignFilter_check_box->setChecked(false);
}

void Configuration::impl::insert_frequency ()
{
  if (QDialog::Accepted == frequency_dialog_->exec ())
    {
      ui_->frequencies_table_view->setCurrentIndex (next_frequencies_.add (frequency_dialog_->item ()));
      ui_->frequencies_table_view->resizeColumnToContents (FrequencyList_v2::mode_column);
    }
}

void Configuration::impl::delete_stations ()
{
  auto selection_model = ui_->stations_table_view->selectionModel ();
  selection_model->select (selection_model->selection (), QItemSelectionModel::SelectCurrent | QItemSelectionModel::Rows);
  next_stations_.removeDisjointRows (selection_model->selectedRows ());
  ui_->stations_table_view->resizeColumnToContents (StationList::band_column);
  ui_->stations_table_view->resizeColumnToContents (StationList::offset_column);
}

void Configuration::impl::insert_station ()
{
  if (QDialog::Accepted == station_dialog_->exec ())
    {
      ui_->stations_table_view->setCurrentIndex (next_stations_.add (station_dialog_->station ()));
      ui_->stations_table_view->resizeColumnToContents (StationList::band_column);
      ui_->stations_table_view->resizeColumnToContents (StationList::offset_column);
    }
}

void Configuration::impl::on_save_path_select_push_button_clicked (bool /* checked */)
{
  QFileDialog fd {this, tr ("Save Directory"), ui_->save_path_display_label->text ()};
  fd.setFileMode (QFileDialog::Directory);
  fd.setOption (QFileDialog::ShowDirsOnly);
  if (fd.exec ())
    {
      if (fd.selectedFiles ().size ())
        {
          ui_->save_path_display_label->setText (fd.selectedFiles ().at (0));
        }
    }
}

bool Configuration::impl::have_rig ()
{
  if (!open_rig ())
    {
      JTDXMessageBox::critical_message (this, "JTDX", tr ("Failed to open connection to rig"));
    }
  return rig_active_;
}

bool Configuration::impl::open_rig (bool force)
{
  auto result = false;

  auto const rig_data = gather_rig_data ();
  if (force || !rig_active_ || rig_data != saved_rig_params_)
    {
      try
        {
//    printf("%s(%0.1f) Coniguration rig_open, active %d, force %d\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),jtdxtime_->GetOffset(),rig_active_,force);

          close_rig ();

          // create a new Transceiver object
          auto rig = transceiver_factory_.create (rig_data, transceiver_thread_);
          cached_rig_state_ = Transceiver::TransceiverState {};

          // hook up Configuration transceiver control signals to Transceiver slots
          //
          // these connections cross the thread boundary
          rig_connections_ << connect (this, &Configuration::impl::set_transceiver,
                                       rig.get (), &Transceiver::set);

          // hook up Transceiver signals to Configuration signals
          //
          // these connections cross the thread boundary
          rig_connections_ << connect (rig.get (), &Transceiver::resolution, this, [=] (int resolution) {
              rig_resolution_ = resolution;
            });
          rig_connections_ << connect (rig.get (), &Transceiver::update, this, &Configuration::impl::handle_transceiver_update);
          rig_connections_ << connect (rig.get (), &Transceiver::failure, this, &Configuration::impl::handle_transceiver_failure);

          // setup thread safe startup and close down semantics
          rig_connections_ << connect (this, &Configuration::impl::start_transceiver, rig.get (), &Transceiver::start);
          rig_connections_ << connect (this, &Configuration::impl::stop_transceiver, rig.get (), &Transceiver::stop);

          auto p = rig.release ();	// take ownership

          // schedule destruction on thread quit
          connect (transceiver_thread_, &QThread::finished, p, &QObject::deleteLater);

          // schedule eventual destruction for non-closing situations
          //
          // must   be   queued    connection   to   avoid   premature
          // self-immolation  since finished  signal  is  going to  be
          // emitted from  the object that  will get destroyed  in its
          // own  stop  slot  i.e.  a   same  thread  signal  to  slot
          // connection which by  default will be reduced  to a method
          // function call.
          connect (p, &Transceiver::finished, p, &Transceiver::deleteLater, Qt::QueuedConnection);

          ui_->test_CAT_push_button->setStyleSheet ({});
          rig_active_ = true;
          Q_EMIT start_transceiver (++transceiver_command_number_); // start rig on its thread
          rig_params_ = gather_rig_data ();
          result = true;
        }
      catch (std::exception const& e)
        {
          handle_transceiver_failure (e.what ());
        }

      saved_rig_params_ = rig_data;
      rig_changed_ = true;
    }
  else
    {
      result = true;
    }
  return result;
}

void Configuration::impl::set_cached_mode ()
{
  MODE mode {Transceiver::UNK};
  // override cache mode with what we want to enforce which includes
  // UNK (unknown) where we want to leave the rig mode untouched
  switch (data_mode_)
     {
     case data_mode_USB: mode = Transceiver::USB; break;
     case data_mode_data: mode = Transceiver::DIG_U; break;
     default: break;
     }

  cached_rig_state_.mode (mode);
}

void Configuration::impl::transceiver_frequency (Frequency f)
{
  cached_rig_state_.online (true); // we want the rig online
  set_cached_mode ();
  
  // apply any offset & calibration
  // we store the offset here for use in feedback from the rig, we
  // cannot absolutely determine if the offset should apply but by
  // simply picking an offset when the Rx frequency is set and
  // sticking to it we get sane behaviour
  if (current_offset_ != stations_.offset (f) || cached_rig_state_.frequency() != apply_calibration (f + current_offset_))
  {
    current_offset_ = stations_.offset (f);
    cached_rig_state_.frequency (apply_calibration (f + current_offset_));

//    printf("%s(%0.1f) Coniguration transceiver_frequency: %lld\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),jtdxtime_->GetOffset(),f);
    Q_EMIT set_transceiver (cached_rig_state_, ++transceiver_command_number_);
  }
}

void Configuration::impl::transceiver_tx_frequency (Frequency f)
{
  Q_ASSERT (!f || split_mode ());
  if (split_mode ())
    {
      cached_rig_state_.online (true); // we want the rig online
      set_cached_mode ();
      if (cached_rig_state_.split() != f ||  cached_rig_state_.tx_frequency() != f ||
         (f && (current_tx_offset_ != stations_.offset (f) || cached_rig_state_.tx_frequency() != apply_calibration (f + current_tx_offset_))))
      {
        cached_rig_state_.split (f);
        cached_rig_state_.tx_frequency (f);

        // lookup offset for tx and apply calibration
        if (f)
          {
            // apply and offset and calibration
            // we store the offset here for use in feedback from the
            // rig, we cannot absolutely determine if the offset should
            // apply but by simply picking an offset when the Rx
            // frequency is set and sticking to it we get sane behaviour
            current_tx_offset_ = stations_.offset (f);
            cached_rig_state_.tx_frequency (apply_calibration (f + current_tx_offset_));
          }

//        printf("%s(%0.1f) Coniguration transceiver_tx_frequency: %lld\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),jtdxtime_->GetOffset(),f);
        Q_EMIT set_transceiver (cached_rig_state_, ++transceiver_command_number_);
      }
    }
}

void Configuration::impl::transceiver_mode (MODE m)
{
  cached_rig_state_.online (true); // we want the rig online
  if (cached_rig_state_.mode() != m)
  {
    cached_rig_state_.mode (m);
//    printf("%s(%0.1f) Coniguration mode: %d\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),jtdxtime_->GetOffset(),m);
    Q_EMIT set_transceiver (cached_rig_state_, ++transceiver_command_number_);
  }
}

void Configuration::impl::transceiver_ptt (bool on)
{
  cached_rig_state_.online (true); // we want the rig online
  set_cached_mode ();
  cached_rig_state_.ptt (on);
//  printf("%s(%0.1f) Coniguration ptt: %d\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),jtdxtime_->GetOffset(),on);
  Q_EMIT set_transceiver (cached_rig_state_, ++transceiver_command_number_);
}

void Configuration::impl::transceiver_ft4_mode (bool on)
{
  cached_rig_state_.online (true); // we want the rig online
  set_cached_mode ();
  if (cached_rig_state_.ft4_mode() != on)
  {
    cached_rig_state_.ft4_mode (on);
//    printf("%s(%0.1f) Coniguration ft4_mode: %d\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),jtdxtime_->GetOffset(),on);
    Q_EMIT set_transceiver (cached_rig_state_, ++transceiver_command_number_);
  }
}

void Configuration::impl::sync_transceiver (bool /*force_signal*/)
{
//  printf("%s(%0.1f) Coniguration sync force: NULL\n",jtdxtime_->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),jtdxtime_->GetOffset());
  // pass this on as cache must be ignored
  // Q_EMIT sync (force_signal);
}

void Configuration::impl::handle_transceiver_update (TransceiverState const& state,
                                                     unsigned sequence_number)
{
#if WSJT_TRACE_CAT
  qDebug () << "Configuration::handle_transceiver_update: Transceiver State #:" << sequence_number << state;
#endif

  // only follow rig on some information, ignore other stuff
  cached_rig_state_.online (state.online ());
  cached_rig_state_.frequency (state.frequency ());
  cached_rig_state_.split (state.split ());

  if (state.online ())
    {
      ui_->test_PTT_push_button->setChecked (state.ptt ());

      if (isVisible ())
        {
          ui_->test_CAT_push_button->setStyleSheet ("QPushButton {background-color: green;}");

          auto const& rig = ui_->rig_combo_box->currentText ();
          auto ptt_method = static_cast<TransceiverFactory::PTTMethod> (ui_->PTT_method_button_group->checkedId ());
          auto CAT_PTT_enabled = transceiver_factory_.has_CAT_PTT (rig);
          ui_->test_PTT_push_button->setEnabled ((TransceiverFactory::PTT_method_CAT == ptt_method && CAT_PTT_enabled)
                                                 || TransceiverFactory::PTT_method_DTR == ptt_method
                                                 || TransceiverFactory::PTT_method_RTS == ptt_method);
        }
    }
  else
    {
      close_rig ();
    }

  // pass on to clients if current command is processed
  if (sequence_number == transceiver_command_number_)
    {
      TransceiverState reported_state {state};
      // take off calibration & offset
      reported_state.frequency (remove_calibration (reported_state.frequency ()) - current_offset_);

      if (reported_state.tx_frequency ())
        {
          // take off calibration & offset
          reported_state.tx_frequency (remove_calibration (reported_state.tx_frequency ()) - current_tx_offset_);
        }

      Q_EMIT self_->transceiver_update (reported_state);
    }
}

void Configuration::impl::handle_transceiver_failure (QString const& reason)
{
#if WSJT_TRACE_CAT
  qDebug () << "Configuration::handle_transceiver_failure: reason:" << reason;
#endif

  close_rig ();
  ui_->test_PTT_push_button->setChecked (false);

  if (isVisible ())
    {
      message_box_critical (tr ("Rig failure"), reason);
    }
  else
    {
      // pass on if our dialog isn't active
      Q_EMIT self_->transceiver_failure (reason);
    }
}

void Configuration::impl::close_rig ()
{
  ui_->test_PTT_push_button->setEnabled (false);

  // revert to no rig configured
  if (rig_active_)
    {
      ui_->test_CAT_push_button->setStyleSheet ("QPushButton {background-color: red;}");
      Q_EMIT stop_transceiver ();
      for (auto const& connection: rig_connections_)
        {
          disconnect (connection);
        }
      rig_connections_.clear ();
      rig_active_ = false;
    }
}

// load the available audio devices into the selection combo box and
// select the default device if the current device isn't set or isn't
// available
bool Configuration::impl::load_audio_devices (QAudio::Mode mode, QComboBox * combo_box, QAudioDeviceInfo * device)
{
  using std::copy;
  using std::back_inserter;

  bool result {false};

  combo_box->clear ();

  int current_index = -1;
  int default_index = -1;

  int extra_items {0};

  auto const& default_device = (mode == QAudio::AudioInput ? QAudioDeviceInfo::defaultInputDevice () : QAudioDeviceInfo::defaultOutputDevice ());

  // deal with special default audio devices on Windows
  if ("Default Input Device" == default_device.deviceName ()
      || "Default Output Device" == default_device.deviceName ())
    {
      default_index = 0;

      QList<QVariant> channel_counts;
      auto scc = default_device.supportedChannelCounts ();
      copy (scc.cbegin (), scc.cend (), back_inserter (channel_counts));

      combo_box->addItem (default_device.deviceName (), channel_counts);
      ++extra_items;
      if (default_device == *device)
        {
          current_index = 0;
          result = true;
        }
    }

  Q_FOREACH (auto const& p, QAudioDeviceInfo::availableDevices (mode))
    {
      // convert supported channel counts into something we can store in the item model
      QList<QVariant> channel_counts;
      auto scc = p.supportedChannelCounts ();
      copy (scc.cbegin (), scc.cend (), back_inserter (channel_counts));

      combo_box->addItem (p.deviceName (), channel_counts);
      if (p == *device)
        {
          current_index = combo_box->count () - 1;
        }
      else if (p == default_device)
        {
          default_index = combo_box->count () - 1;
        }
    }
  if (current_index < 0)	// not found - use default
    {
      *device = default_device;
      result = true;
      current_index = default_index;
    }
  combo_box->setCurrentIndex (current_index);

  return result;
}

// enable only the channels that are supported by the selected audio device
void Configuration::impl::update_audio_channels (QComboBox const * source_combo_box, int index, QComboBox * combo_box, bool allow_both)
{
  // disable all items
  for (int i (0); i < combo_box->count (); ++i)
    {
      combo_box->setItemData (i, combo_box_item_disabled, Qt::UserRole - 1);
    }

  Q_FOREACH (QVariant const& v, source_combo_box->itemData (index).toList ())
    {
      // enable valid options
      int n {v.toInt ()};
      if (2 == n)
        {
          combo_box->setItemData (AudioDevice::Left, combo_box_item_enabled, Qt::UserRole - 1);
          combo_box->setItemData (AudioDevice::Right, combo_box_item_enabled, Qt::UserRole - 1);
          if (allow_both)
            {
              combo_box->setItemData (AudioDevice::Both, combo_box_item_enabled, Qt::UserRole - 1);
            }
        }
      else if (1 == n)
        {
          combo_box->setItemData (AudioDevice::Mono, combo_box_item_enabled, Qt::UserRole - 1);
        }
    }
}

void Configuration::impl::set_application_font (QFont const& font)
{
//  qApp->setFont (font);
  QString ss;
    if (qApp->styleSheet ().size ())
    {
      auto sheet = qApp->styleSheet ();
      if (sheet.startsWith("file:///")) {
        sheet.remove ("file:///");
        QFile sf {sheet};
        if (sf.open (QFile::ReadOnly | QFile::Text)) {
            ss = sf.readAll () + ss;
            useDarkStyle_ = true;
            ui_->useDarkStyle_check_box->setChecked (useDarkStyle_);
        }
      }
      else if (useDarkStyle_){
        int lopp = sheet.indexOf("* { font-family:");
        if (lopp > 0) ss = sheet.mid(0,lopp);
        else {
          QFile sf {":/qdarkstyle/style.qss"};
          if (sf.open (QFile::ReadOnly | QFile::Text))
            ss = sf.readAll () + ss;
          else {
            useDarkStyle_ = false;
            ui_->useDarkStyle_check_box->setChecked (useDarkStyle_);
            ss = "";
          }
        }
      }
      else 
        ss = "";
    }
    else if (useDarkStyle_) {
      QFile sf {":/qdarkstyle/style.qss"};
      if (sf.open (QFile::ReadOnly | QFile::Text))
        ss = sf.readAll () + ss;
      else {
        useDarkStyle_ = false;
        ui_->useDarkStyle_check_box->setChecked (useDarkStyle_);
        ss = "";
      }
    }
  qApp->setStyleSheet (ss + "* {" + font_as_stylesheet (font) + '}');
  for (auto& widget : qApp->topLevelWidgets ())
    {
      widget->updateGeometry ();
    }
}

// load all the supported rig names into the selection combo box
void Configuration::impl::enumerate_rigs ()
{
  ui_->rig_combo_box->clear ();

  auto rigs = transceiver_factory_.supported_transceivers ();

  for (auto r = rigs.cbegin (); r != rigs.cend (); ++r)
    {
      if ("None" == r.key ())
        {
          // put None first
          ui_->rig_combo_box->insertItem (0, r.key (), r.value ().model_number_);
        }
      else
        {
          ui_->rig_combo_box->addItem (r.key (), r.value ().model_number_);
        }
    }

  ui_->rig_combo_box->setCurrentText (rig_params_.rig_name);
}

void Configuration::impl::fill_port_combo_box (QComboBox * cb)
{
  auto current_text = cb->currentText ();
  cb->clear ();
  Q_FOREACH (auto const& p, QSerialPortInfo::availablePorts ())
    {
      if (!p.portName ().contains ( "NULL" )) // virtual serial port pairs
        {
          // remove possibly confusing Windows device path (OK because
          // it gets added back by Hamlib)
          cb->addItem (p.systemLocation ().remove (QRegularExpression {R"(^\\\\\.\\)"}));
        }
    }
  cb->addItem("USB");
  cb->setEditText (current_text);
}

auto Configuration::impl::apply_calibration (Frequency f) const -> Frequency
{
  return std::llround (frequency_calibration_intercept_
                       + (1. + frequency_calibration_slope_ppm_ / 1.e6) * f);
}

auto Configuration::impl::remove_calibration (Frequency f) const -> Frequency
{
  return std::llround ((f - frequency_calibration_intercept_)
                       / (1. + frequency_calibration_slope_ppm_ / 1.e6));
}

#if !defined (QT_NO_DEBUG_STREAM)
ENUM_QDEBUG_OPS_IMPL (Configuration, DataMode);
ENUM_QDEBUG_OPS_IMPL (Configuration, Type2MsgGen);
#endif

ENUM_QDATASTREAM_OPS_IMPL (Configuration, DataMode);
ENUM_QDATASTREAM_OPS_IMPL (Configuration, Type2MsgGen);

ENUM_CONVERSION_OPS_IMPL (Configuration, DataMode);
ENUM_CONVERSION_OPS_IMPL (Configuration, Type2MsgGen);
