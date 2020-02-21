//
// MessageAggregator - an example application that utilizes the WSJT-X
//                     messaging facility
//
// This  application is  only  provided as  a  simple GUI  application
// example to demonstrate the WSJT-X messaging facility. It allows the
// user to set  the server details either as a  unicast UDP server or,
// if a  multicast group address  is provided, as a  multicast server.
// The benefit of the multicast server is that multiple servers can be
// active at  once each  receiving all  WSJT-X broadcast  messages and
// each able to  respond to individual WSJT_X clients.  To utilize the
// multicast  group features  each  WSJT-X client  must  set the  same
// multicast  group address  as  the UDP  server  address for  example
// 239.255.0.0 for a site local multicast group.
//
// The  UI is  a small  panel  to input  the service  port number  and
// optionally  the  multicast  group  address.   Below  that  a  table
// representing  the  log  entries   where  any  QSO  logged  messages
// broadcast  from WSJT-X  clients are  displayed. The  bottom of  the
// application main  window is a  dock area  where a dock  window will
// appear for each WSJT-X client, this  window contains a table of the
// current decode  messages broadcast  from that  WSJT-X client  and a
// status line showing  the status update messages  broadcast from the
// WSJT_X client. The dock windows may  be arranged in a tab bar, side
// by side,  below each  other or, completely  detached from  the dock
// area as floating windows. Double clicking the dock window title bar
// or  dragging and  dropping with  the mouse  allows these  different
// arrangements.
//
// The application  also provides a  simple menu bar including  a view
// menu that allows each dock window to be hidden or revealed.
//

#include <iostream>
#include <exception>

#include <QtWidgets>
#include <QFile>
#include <QStandardItemModel>
#include <QStandardItem>
#include <QSortFilterProxyModel>
#include <QFont>
#include <QDateTime>
#include <QTime>
#include <QHash>

#include "MessageServer.hpp"
#include "NetworkMessage.hpp"

#include "qt_helpers.hpp"

using port_type = MessageServer::port_type;
using Frequency = MessageServer::Frequency;

//QRegExp message_alphabet {"[- A-Za-z0-9+./?]*"};
QRegExp message_alphabet {"[- @A-Za-z0-9+./?#<>]*"};

//
// Decodes Model - simple data model for all decodes
//
// The model is a basic table with uniform row format. Rows consist of
// QStandardItem instances containing the string representation of the
// column data  and if the underlying  field is not a  string then the
// UserRole+1 role contains the underlying data item.
//
// Three slots  are provided to add  a new decode, remove  all decodes
// for a client  and, to build a  reply to CQ message for  a given row
// which is emitted as a signal respectively.
//
class DecodesModel
  : public QStandardItemModel
{
  Q_OBJECT;

public:
  DecodesModel (QObject * parent = nullptr)
    : QStandardItemModel {0, 7, parent}
    , text_font_ {"Courier", 10}
  {
    setHeaderData (0, Qt::Horizontal, tr ("Client"));
    setHeaderData (1, Qt::Horizontal, tr ("Time"));
    setHeaderData (2, Qt::Horizontal, tr ("Snr"));
    setHeaderData (3, Qt::Horizontal, tr ("DT"));
    setHeaderData (4, Qt::Horizontal, tr ("DF"));
    setHeaderData (5, Qt::Horizontal, tr ("Md"));
    setHeaderData (6, Qt::Horizontal, tr ("Message"));
  }

  Q_SLOT void add_decode (bool is_new, QString const& client_id, QTime time, qint32 snr, float delta_time
                          , quint32 delta_frequency, QString const& mode, QString const& message)
  {
    if (!is_new)
      {
        int target_row {-1};
        for (auto row = 0; row < rowCount (); ++row)
          {
            if (data (index (row, 0)).toString () == client_id)
              {
                auto row_time = item (row, 1)->data ().toTime ();
                if (row_time == time
                    && item (row, 2)->data ().toInt () == snr
                    && item (row, 3)->data ().toFloat () == delta_time
                    && item (row, 4)->data ().toUInt () == delta_frequency
                    && data (index (row, 5)).toString () == mode
                    && data (index (row, 6)).toString () == message)
                  {
                    return;
                  }
                if (time <= row_time)
                  {
                    target_row = row; // last row with same time
                  }
              }
          }
        if (target_row >= 0)
          {
            insertRow (target_row + 1, make_row (client_id, time, snr, delta_time, delta_frequency, mode, message));
            return;
          }
      }

    appendRow (make_row (client_id, time, snr, delta_time, delta_frequency, mode, message));
  }

  QList<QStandardItem *> make_row (QString const& client_id, QTime time, qint32 snr, float delta_time
                                   , quint32 delta_frequency, QString const& mode, QString const& message) const
  {
    auto time_item = new QStandardItem {time.toString ("hh:mm")};
    time_item->setData (time);
    time_item->setTextAlignment (Qt::AlignRight);

    auto snr_item = new QStandardItem {QString::number (snr)};
    snr_item->setData (snr);
    snr_item->setTextAlignment (Qt::AlignRight);

    auto dt = new QStandardItem {QString::number (delta_time)};
    dt->setData (delta_time);
    dt->setTextAlignment (Qt::AlignRight);

    auto df = new QStandardItem {QString::number (delta_frequency)};
    df->setData (delta_frequency);
    df->setTextAlignment (Qt::AlignRight);

    auto md = new QStandardItem {mode};
    md->setTextAlignment (Qt::AlignHCenter);

    QList<QStandardItem *> row {
      new QStandardItem {client_id}, time_item, snr_item, dt, df, md, new QStandardItem {message}};
    Q_FOREACH (auto& item, row)
      {
        item->setEditable (false);
        item->setFont (text_font_);
        item->setTextAlignment (item->textAlignment () | Qt::AlignVCenter);
      }
    return row;
  }

  Q_SLOT void clear_decodes (QString const& client_id)
  {
    for (auto row = rowCount () - 1; row >= 0; --row)
      {
        if (data (index (row, 0)).toString () == client_id)
          {
            removeRow (row);
          }
      }
  }

  Q_SLOT void do_reply (QModelIndex const& source)
  {
    auto row = source.row ();
    Q_EMIT reply (data (index (row, 0)).toString ()
                  , item (row, 1)->data ().toTime ()
                  , item (row, 2)->data ().toInt ()
                  , item (row, 3)->data ().toFloat ()
                  , item (row, 4)->data ().toInt ()
                  , data (index (row, 5)).toString ()
                  , data (index (row, 6)).toString ());
  }

  Q_SIGNAL void reply (QString const& id, QTime time, qint32 snr, float delta_time, quint32 delta_frequency
                       , QString const& mode, QString const& message);

private:
  QFont text_font_;
};

//
// Beacons Model - simple data model for all beacon spots
//
// The model is a basic table with uniform row format. Rows consist of
// QStandardItem instances containing the string representation of the
// column data  and if the underlying  field is not a  string then the
// UserRole+1 role contains the underlying data item.
//
// Two slots are provided to add a new decode and remove all spots for
// a client.
//
class BeaconsModel
  : public QStandardItemModel
{
  Q_OBJECT;

public:
  BeaconsModel (QObject * parent = nullptr)
    : QStandardItemModel {0, 9, parent}
    , text_font_ {"Courier", 10}
  {
    setHeaderData (0, Qt::Horizontal, tr ("Client"));
    setHeaderData (1, Qt::Horizontal, tr ("Time"));
    setHeaderData (2, Qt::Horizontal, tr ("Snr"));
    setHeaderData (3, Qt::Horizontal, tr ("DT"));
    setHeaderData (4, Qt::Horizontal, tr ("Frequency"));
    setHeaderData (5, Qt::Horizontal, tr ("Drift"));
    setHeaderData (6, Qt::Horizontal, tr ("Callsign"));
    setHeaderData (7, Qt::Horizontal, tr ("Grid"));
    setHeaderData (8, Qt::Horizontal, tr ("Power"));
  }

  Q_SLOT void add_beacon_spot (bool is_new, QString const& client_id, QTime time, qint32 snr, float delta_time
                          , Frequency frequency, qint32 drift, QString const& callsign, QString const& grid
                          , qint32 power)
  {
    if (!is_new)
      {
        int target_row {-1};
        for (auto row = 0; row < rowCount (); ++row)
          {
            if (data (index (row, 0)).toString () == client_id)
              {
                auto row_time = item (row, 1)->data ().toTime ();
                if (row_time == time
                    && item (row, 2)->data ().toInt () == snr
                    && item (row, 3)->data ().toFloat () == delta_time
                    && item (row, 4)->data ().value<Frequency> () == frequency
                    && data (index (row, 5)).toInt () == drift
                    && data (index (row, 6)).toString () == callsign
                    && data (index (row, 7)).toString () == grid
                    && data (index (row, 8)).toInt () == power)
                  {
                    return;
                  }
                if (time <= row_time)
                  {
                    target_row = row; // last row with same time
                  }
              }
          }
        if (target_row >= 0)
          {
            insertRow (target_row + 1, make_row (client_id, time, snr, delta_time, frequency, drift, callsign, grid, power));
            return;
          }
      }

    appendRow (make_row (client_id, time, snr, delta_time, frequency, drift, callsign, grid, power));
  }

  QList<QStandardItem *> make_row (QString const& client_id, QTime time, qint32 snr, float delta_time
                                   , Frequency frequency, qint32 drift, QString const& callsign
                                   , QString const& grid, qint32 power) const
  {
    auto time_item = new QStandardItem {time.toString ("hh:mm")};
    time_item->setData (time);
    time_item->setTextAlignment (Qt::AlignRight);

    auto snr_item = new QStandardItem {QString::number (snr)};
    snr_item->setData (snr);
    snr_item->setTextAlignment (Qt::AlignRight);

    auto dt = new QStandardItem {QString::number (delta_time)};
    dt->setData (delta_time);
    dt->setTextAlignment (Qt::AlignRight);

    auto freq = new QStandardItem {Radio::pretty_frequency_MHz_string (frequency)};
    freq->setData (frequency);
    freq->setTextAlignment (Qt::AlignRight);

    auto dri = new QStandardItem {QString::number (drift)};
    dri->setData (drift);
    dri->setTextAlignment (Qt::AlignRight);

    auto gd = new QStandardItem {grid};
    gd->setTextAlignment (Qt::AlignRight);

    auto pwr = new QStandardItem {QString::number (power)};
    pwr->setData (power);
    pwr->setTextAlignment (Qt::AlignRight);

    QList<QStandardItem *> row {
      new QStandardItem {client_id}, time_item, snr_item, dt, freq, dri, new QStandardItem {callsign}, gd, pwr};
    Q_FOREACH (auto& item, row)
      {
        item->setEditable (false);
        item->setFont (text_font_);
        item->setTextAlignment (item->textAlignment () | Qt::AlignVCenter);
      }
    return row;
  }

  Q_SLOT void clear_decodes (QString const& client_id)
  {
    for (auto row = rowCount () - 1; row >= 0; --row)
      {
        if (data (index (row, 0)).toString () == client_id)
          {
            removeRow (row);
          }
      }
  }

private:
  QFont text_font_;
};

class ClientWidget
  : public QDockWidget
{
  Q_OBJECT;

public:
  explicit ClientWidget (QAbstractItemModel * decodes_model, QAbstractItemModel * beacons_model
                         , QString const& id, QWidget * parent = 0)
    : QDockWidget {id, parent}
    , id_ {id}
    , decodes_table_view_ {new QTableView}
    , beacons_table_view_ {new QTableView}
    , message_line_edit_ {new QLineEdit}
    , decodes_stack_ {new QStackedLayout}
    , auto_off_button_ {new QPushButton {tr ("&Auto Off")}}
    , halt_tx_button_ {new QPushButton {tr ("&Halt Tx")}}
    , mode_label_ {new QLabel}
    , dx_call_label_ {new QLabel}
    , frequency_label_ {new QLabel}
    , report_label_ {new QLabel}
  {
    // set up widgets
    auto decodes_proxy_model = new IdFilterModel {id, this};
    decodes_proxy_model->setSourceModel (decodes_model);
    decodes_table_view_->setModel (decodes_proxy_model);
    decodes_table_view_->verticalHeader ()->hide ();
    decodes_table_view_->hideColumn (0);
    decodes_table_view_->horizontalHeader ()->setStretchLastSection (true);

    auto form_layout = new QFormLayout;
    form_layout->addRow (tr ("Free text:"), message_line_edit_);
    message_line_edit_->setValidator (new QRegExpValidator {message_alphabet, this});
    connect (message_line_edit_, &QLineEdit::textEdited, [this] (QString const& text) {
        Q_EMIT do_free_text (id_, text, false);
      });
    connect (message_line_edit_, &QLineEdit::editingFinished, [this] () {
        Q_EMIT do_free_text (id_, message_line_edit_->text (), true);
      });

    auto decodes_page = new QWidget;
    auto decodes_layout = new QVBoxLayout {decodes_page};
    decodes_layout->setContentsMargins (QMargins {2, 2, 2, 2});
    decodes_layout->addWidget (decodes_table_view_);
    decodes_layout->addLayout (form_layout);

    auto beacons_proxy_model = new IdFilterModel {id, this};
    beacons_proxy_model->setSourceModel (beacons_model);
    beacons_table_view_->setModel (beacons_proxy_model);
    beacons_table_view_->verticalHeader ()->hide ();
    beacons_table_view_->hideColumn (0);
    beacons_table_view_->horizontalHeader ()->setStretchLastSection (true);

    auto beacons_page = new QWidget;
    auto beacons_layout = new QVBoxLayout {beacons_page};
    beacons_layout->setContentsMargins (QMargins {2, 2, 2, 2});
    beacons_layout->addWidget (beacons_table_view_);

    decodes_stack_->addWidget (decodes_page);
    decodes_stack_->addWidget (beacons_page);

    // stack alternative views
    auto content_layout = new QVBoxLayout;
    content_layout->setContentsMargins (QMargins {2, 2, 2, 2});
    content_layout->addLayout (decodes_stack_);

    // set up controls
    auto control_button_box = new QDialogButtonBox;
    control_button_box->addButton (auto_off_button_, QDialogButtonBox::ActionRole);
    control_button_box->addButton (halt_tx_button_, QDialogButtonBox::ActionRole);
    connect (auto_off_button_, &QAbstractButton::clicked, [this] (bool /* checked */) {
        Q_EMIT do_halt_tx (id_, true);
      });
    connect (halt_tx_button_, &QAbstractButton::clicked, [this] (bool /* checked */) {
        Q_EMIT do_halt_tx (id_, false);
      });
    content_layout->addWidget (control_button_box);

    // set up status area
    auto status_bar = new QStatusBar;
    status_bar->addPermanentWidget (mode_label_);
    status_bar->addPermanentWidget (dx_call_label_);
    status_bar->addPermanentWidget (frequency_label_);
    status_bar->addPermanentWidget (report_label_);
    content_layout->addWidget (status_bar);
    connect (this, &ClientWidget::topLevelChanged, status_bar, &QStatusBar::setSizeGripEnabled);

    // set up central widget
    auto content_widget = new QFrame;
    content_widget->setFrameStyle (QFrame::StyledPanel | QFrame::Sunken);
    content_widget->setLayout (content_layout);
    setWidget (content_widget);
    // setMinimumSize (QSize {550, 0});
    setFeatures (DockWidgetMovable | DockWidgetFloatable);
    setAllowedAreas (Qt::BottomDockWidgetArea);

    // connect up table view signals
    connect (decodes_table_view_, &QTableView::doubleClicked, this, [this, decodes_proxy_model] (QModelIndex const& index) {
        Q_EMIT do_reply (decodes_proxy_model->mapToSource (index));
      });
  }

  Q_SLOT void update_status (QString const& id, Frequency f, QString const& mode, QString const& dx_call
                             , QString const& report, QString const& tx_mode, bool tx_enabled
                             , bool transmitting, bool decoding)
  {
    if (id == id_)
      {
        mode_label_->setText (QString {"Mode: %1%2"}
           .arg (mode)
           .arg (tx_mode.isEmpty () || tx_mode == mode ? "" : '(' + tx_mode + ')'));
        dx_call_label_->setText ("DX CALL: " + dx_call);
        frequency_label_->setText ("QRG: " + Radio::pretty_frequency_MHz_string (f));
        report_label_->setText ("SNR: " + report);
        update_dynamic_property (frequency_label_, "transmitting", transmitting);
        auto_off_button_->setEnabled (tx_enabled);
        halt_tx_button_->setEnabled (transmitting);
        update_dynamic_property (mode_label_, "decoding", decoding);
      }
  }

  Q_SLOT void decode_added (bool /*is_new*/, QString const& client_id, QTime /*time*/, qint32 /*snr*/
      , float /*delta_time*/, quint32 /*delta_frequency*/, QString const& /*mode*/
      , QString const& /*message*/)
  {
    if (client_id == id_)
      {
        decodes_stack_->setCurrentIndex (0);
        decodes_table_view_->resizeColumnsToContents ();
        decodes_table_view_->scrollToBottom ();
      }
  }

  Q_SLOT void beacon_spot_added (bool /*is_new*/, QString const& client_id, QTime /*time*/, qint32 /*snr*/
      , float /*delta_time*/, Frequency /*delta_frequency*/, qint32 /*drift*/, QString const& /*callsign*/
      , QString const& /*grid*/, qint32 /*power*/)
  {
    if (client_id == id_)
      {
        decodes_stack_->setCurrentIndex (1);
        beacons_table_view_->resizeColumnsToContents ();
        beacons_table_view_->scrollToBottom ();
      }
  }

  Q_SIGNAL void do_reply (QModelIndex const&);
  Q_SIGNAL void do_halt_tx (QString const& id, bool auto_only);
  Q_SIGNAL void do_free_text (QString const& id, QString const& text, bool);

private:
  class IdFilterModel final
    : public QSortFilterProxyModel
  {
  public:
    IdFilterModel (QString const& id, QObject * parent = nullptr)
      : QSortFilterProxyModel {parent}
      , id_ {id}
    {}

  protected:
    bool filterAcceptsRow (int source_row, QModelIndex const& source_parent) const override
    {
      auto source_index_col0 = sourceModel ()->index (source_row, 0, source_parent);
      return sourceModel ()->data (source_index_col0).toString () == id_;
    }

  private:
    QString id_;
  };

  QString id_;
  QTableView * decodes_table_view_;
  QTableView * beacons_table_view_;
  QLineEdit * message_line_edit_;
  QStackedLayout * decodes_stack_;
  QAbstractButton * auto_off_button_;
  QAbstractButton * halt_tx_button_;
  QLabel * mode_label_;
  QLabel * dx_call_label_;
  QLabel * frequency_label_;
  QLabel * report_label_;
};

class MainWindow
  : public QMainWindow
{
  Q_OBJECT;

public:
  MainWindow ()
    : log_ {new QStandardItemModel {0, 10, this}}
    , decodes_model_ {new DecodesModel {this}}
    , beacons_model_ {new BeaconsModel {this}}
    , server_ {new MessageServer {this}}
    , multicast_group_line_edit_ {new QLineEdit}
    , log_table_view_ {new QTableView}
  {
    // logbook
    log_->setHeaderData (0, Qt::Horizontal, tr ("Date/Time"));
    log_->setHeaderData (1, Qt::Horizontal, tr ("Callsign"));
    log_->setHeaderData (2, Qt::Horizontal, tr ("Grid"));
    log_->setHeaderData (3, Qt::Horizontal, tr ("Name"));
    log_->setHeaderData (4, Qt::Horizontal, tr ("Frequency"));
    log_->setHeaderData (5, Qt::Horizontal, tr ("Mode"));
    log_->setHeaderData (6, Qt::Horizontal, tr ("Sent"));
    log_->setHeaderData (7, Qt::Horizontal, tr ("Rec'd"));
    log_->setHeaderData (8, Qt::Horizontal, tr ("Power"));
    log_->setHeaderData (9, Qt::Horizontal, tr ("Comments"));
    connect (server_, &MessageServer::qso_logged, this, &MainWindow::log_qso);

    // menu bar
    auto file_menu = menuBar ()->addMenu (tr ("&File"));

    auto exit_action = new QAction {tr ("E&xit"), this};
    exit_action->setShortcuts (QKeySequence::Quit);
    exit_action->setToolTip (tr ("Exit the application"));
    file_menu->addAction (exit_action);
    connect (exit_action, &QAction::triggered, this, &MainWindow::close);

    view_menu_ = menuBar ()->addMenu (tr ("&View"));

    // central layout
    auto central_layout = new QVBoxLayout;

    // server details
    auto port_spin_box = new QSpinBox;
    port_spin_box->setMinimum (1);
    port_spin_box->setMaximum (std::numeric_limits<port_type>::max ());
    auto group_box_layout = new QFormLayout;
    group_box_layout->addRow (tr ("Port number:"), port_spin_box);
    group_box_layout->addRow (tr ("Multicast Group (blank for unicast server):"), multicast_group_line_edit_);
    auto group_box = new QGroupBox {tr ("Server Details")};
    group_box->setLayout (group_box_layout);
    central_layout->addWidget (group_box);

    log_table_view_->setModel (log_);
    log_table_view_->verticalHeader ()->hide ();
    central_layout->addWidget (log_table_view_);

    // central widget
    auto central_widget = new QWidget;
    central_widget->setLayout (central_layout);

    // main window setup
    setCentralWidget (central_widget);
    setDockOptions (AnimatedDocks | AllowNestedDocks | AllowTabbedDocks);
    setTabPosition (Qt::BottomDockWidgetArea, QTabWidget::North);

    // connect up server
    connect (server_, &MessageServer::error, [this] (QString const& message) {
        QMessageBox::warning (this, tr ("Network Error"), message);
      });
    connect (server_, &MessageServer::client_opened, this, &MainWindow::add_client);
    connect (server_, &MessageServer::client_closed, this, &MainWindow::remove_client);
    connect (server_, &MessageServer::client_closed, decodes_model_, &DecodesModel::clear_decodes);
    connect (server_, &MessageServer::client_closed, beacons_model_, &BeaconsModel::clear_decodes);
    connect (server_, &MessageServer::decode, decodes_model_, &DecodesModel::add_decode);
    connect (server_, &MessageServer::WSPR_decode, beacons_model_, &BeaconsModel::add_beacon_spot);
    connect (server_, &MessageServer::clear_decodes, decodes_model_, &DecodesModel::clear_decodes);
    connect (server_, &MessageServer::clear_decodes, beacons_model_, &BeaconsModel::clear_decodes);
    connect (decodes_model_, &DecodesModel::reply, server_, &MessageServer::reply);

    // UI behaviour
    connect (port_spin_box, static_cast<void (QSpinBox::*)(int)> (&QSpinBox::valueChanged)
             , [this] (port_type port) {server_->start (port);});
    connect (multicast_group_line_edit_, &QLineEdit::editingFinished, [this, port_spin_box] () {
        server_->start (port_spin_box->value (), QHostAddress {multicast_group_line_edit_->text ()});
      });

    port_spin_box->setValue (2237); // start up in unicast mode
    show ();
  }

  Q_SLOT void log_qso (QString const& /*id*/, QDateTime time, QString const& dx_call, QString const& dx_grid
                       , Frequency dial_frequency, QString const& mode, QString const& report_sent
                       , QString const& report_received, QString const& tx_power, QString const& comments
                       , QString const& name)
  {
    QList<QStandardItem *> row;
    row << new QStandardItem {time.toString ("dd-MMM-yyyy hh:mm")}
        << new QStandardItem {dx_call}
        << new QStandardItem {dx_grid}
        << new QStandardItem {name}
        << new QStandardItem {Radio::frequency_MHz_string (dial_frequency)}
        << new QStandardItem {mode}
        << new QStandardItem {report_sent}
        << new QStandardItem {report_received}
        << new QStandardItem {tx_power}
        << new QStandardItem {comments};
    log_->appendRow (row);
    log_table_view_->resizeColumnsToContents ();
    log_table_view_->horizontalHeader ()->setStretchLastSection (true);
    log_table_view_->scrollToBottom ();
  }

private:
  void add_client (QString const& id)
  {
    auto dock = new ClientWidget {decodes_model_, beacons_model_, id, this};
    dock->setAttribute (Qt::WA_DeleteOnClose);
    auto view_action = dock->toggleViewAction ();
    view_action->setEnabled (true);
    view_menu_->addAction (view_action);
    addDockWidget (Qt::BottomDockWidgetArea, dock);
    connect (server_, &MessageServer::status_update, dock, &ClientWidget::update_status);
    connect (server_, &MessageServer::decode, dock, &ClientWidget::decode_added);
    connect (server_, &MessageServer::WSPR_decode, dock, &ClientWidget::beacon_spot_added);
    connect (dock, &ClientWidget::do_reply, decodes_model_, &DecodesModel::do_reply);
    connect (dock, &ClientWidget::do_halt_tx, server_, &MessageServer::halt_tx);
    connect (dock, &ClientWidget::do_free_text, server_, &MessageServer::free_text);
    connect (view_action, &QAction::toggled, dock, &ClientWidget::setVisible);
    dock_widgets_[id] = dock;
    server_->replay (id);
  }

  void remove_client (QString const& id)
  {
    auto iter = dock_widgets_.find (id);
    if (iter != std::end (dock_widgets_))
      {
        (*iter)->close ();
        dock_widgets_.erase (iter);
      }
  }

  QStandardItemModel * log_;
  QMenu * view_menu_;
  DecodesModel * decodes_model_;
  BeaconsModel * beacons_model_;
  MessageServer * server_;
  QLineEdit * multicast_group_line_edit_;
  QTableView * log_table_view_;

  // maps client id to widgets
  QHash<QString, ClientWidget *> dock_widgets_;
};

#include "MessageAggregator.moc"

int main (int argc, char * argv[])
{
  QApplication app {argc, argv};
  try
    {
      QObject::connect (&app, SIGNAL (lastWindowClosed ()), &app, SLOT (quit ()));

      app.setApplicationName ("WSJT-X Reference UDP Message Aggregator Server");
      app.setApplicationVersion ("1.0");

      {
        QFile file {":/qss/default.qss"};
        if (!file.open (QFile::ReadOnly))
          {
            throw_qstring ("failed to open \"" + file.fileName () + "\": " + file.errorString ());
          }
        app.setStyleSheet (file.readAll());
      }

      MainWindow window;
      return app.exec ();
    }
  catch (std::exception const & e)
    {
      QMessageBox::critical (nullptr, app.applicationName (), e.what ());
      std:: cerr << "Error: " << e.what () << '\n';
    }
  catch (...)
    {
      QMessageBox::critical (nullptr, app.applicationName (), QObject::tr ("Unexpected error"));
      std:: cerr << "Unexpected error\n";
    }
  return -1;
}
