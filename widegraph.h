// -*- Mode: C++ -*-
#ifndef WIDEGRAPH_H
#define WIDEGRAPH_H
#include <QDialog>
#include <QScopedPointer>
#include <QDir>
#include "WFPalette.hpp"
#include "JTDXDateTime.h"
#define MAX_SCREENSIZE 2048

namespace Ui {
  class WideGraph;
}

class QSettings;
class Configuration;

class WideGraph : public QDialog
{
  Q_OBJECT

public:
  explicit WideGraph(QSettings *, JTDXDateTime * jtdxtime, QWidget *parent = 0);
  ~WideGraph ();

  void   dataSink2(float s[], float df3, int ihsym, int ndiskdata);
  void   setRxFreq(int n);
  int    rxFreq();
  int    nStartFreq();
  int    Fmin();
  int    Fmax();
  int    fSpan();
  void   saveSettings();
  void   setRxRange(int fMin);
  void   setFsample(int n);
  void   setPeriod(double trperiod, int nsps);
  void   setTxFreq(int n);
  void   setMode(QString mode);
  void   setTopJT65(int n);
  void   setModeTx(QString modeTx);
  void   setLockTxFreq(bool b);
  void   setFilter(bool b);
  void   setHoundFilter(bool b);
  void	 setDarkStyle(bool b);
  bool   scale();
  bool   flatten();
  void   setTol(int n);
  void   setRxBand(QString band);
  void   setWSPRtransmitted();

signals:
  void freezeDecode2(int n);
  void f11f12(int n);
  void setXIT2(int n);
  void setFreq3(int rxFreq, int txFreq);
  void setRxFreq3(int rxFreq);
  void filter_on3();
  void toggle_filter3();
  void esc_key();

public slots:
  void wideFreezeDecode(int n);
  void setFreq2(int rxFreq, int txFreq);
  void setRxFreq2(int rxFreq);
  void filter_on2();
  void toggle_filter2();
  void setDialFreq(double d);

protected:
  virtual void keyPressEvent( QKeyEvent *e );
  void closeEvent (QCloseEvent *);

private slots:
  void on_waterfallAvgSpinBox_valueChanged(int arg1);
  void on_bppSpinBox_valueChanged(int arg1);
  void on_spec2dComboBox_currentIndexChanged(int n);
  void on_fSplitSpinBox_valueChanged(int n);
  void on_fStartSpinBox_valueChanged(int n);
  void on_paletteComboBox_activated(const QString &palette);
  void on_timestampComboBox_currentIndexChanged(int n);
  void on_cbScale_toggled(bool b);
  void on_cbFlatten_toggled(bool b);
  void on_cbControls_toggled(bool b);
  void on_cbBars_toggled(bool b);
  void on_cbFreq_toggled(bool b);
  void on_adjust_palette_push_button_clicked (bool);
  void on_gainSlider_valueChanged(int value);
  void on_zeroSlider_valueChanged(int value);
  void on_gain2dSlider_valueChanged(int value);
  void on_zero2dSlider_valueChanged(int value);
  void on_sbPercent2dPlot_valueChanged(int n);

private:
  void   readPalette();

  QScopedPointer<Ui::WideGraph> ui;

  QSettings * m_settings;
  QDir m_palettes_path;
  WFPalette m_userPalette;

  double m_tr0;
  double m_TRperiod;

  qint32 m_waterfallAvg;
  qint32 m_nsps;
  qint32 m_fMin;
  qint32 m_fMax;
  qint32 m_nSubMode;
  qint32 m_Percent2DScreen;
  qint32 m_topJT65;
  qint32 m_timestamp;

  bool	 m_bars;
  bool	 m_freq;
  bool   m_lockTxFreq;
  bool   m_filter;
  bool   m_bScale;
  bool   m_bFlatten;
  bool   m_bHaveTransmitted;    //Set true at end of a WSPR transmission
  JTDXDateTime * m_jtdxtime;

  QString m_mode;
  QString m_modeTx;
  QString m_waterfallPalette;
};

#endif // WIDEGRAPH_H
