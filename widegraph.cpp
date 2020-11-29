#include "widegraph.h"
#include <QApplication>
#include <QSettings>
#include <qmath.h>
#include "ui_widegraph.h"
#include "commons.h"
#include "Configuration.hpp"
#include "moc_widegraph.cpp"
#include "JTDXMessageBox.hpp"
static float swide[MAX_SCREENSIZE];

namespace
{
  auto user_defined = QObject::tr ("User Defined");
}

WideGraph::WideGraph(QSettings * settings, JTDXDateTime * jtdxtime, QWidget *parent) :
  QDialog(parent),
  ui(new Ui::WideGraph),
  m_settings (settings),
  m_palettes_path {":/Palettes"},
  m_tr0 {0.0},
  m_lockTxFreq {false},
  m_filter {false},
  m_jtdxtime {jtdxtime}
{
  ui->setupUi(this);

  setWindowTitle (QApplication::applicationName () + " - " + tr ("Wide Graph"));
  setWindowFlags (Qt::WindowCloseButtonHint | Qt::WindowMinimizeButtonHint);
  setMaximumWidth (MAX_SCREENSIZE);
  setMaximumHeight (880);

  ui->widePlot->m_jtdxtime = m_jtdxtime;
  ui->widePlot->setCursor(Qt::CrossCursor);
  ui->widePlot->setMaximumHeight(800);
  ui->widePlot->setCurrent(false);
  ui->cbControls->setCursor(Qt::ArrowCursor);
  ui->cbBars->setCursor(Qt::ArrowCursor);

  connect(ui->widePlot, SIGNAL(freezeDecode1(int)),this,
          SLOT(wideFreezeDecode(int)));

  connect(ui->widePlot, SIGNAL(setFreq1(int,int)),this,
          SLOT(setFreq2(int,int)));

  connect(ui->widePlot, SIGNAL(setRxFreq1(int)),this,
          SLOT(setRxFreq2(int)));

  connect(ui->widePlot, SIGNAL(filter_on1()),this,
          SLOT(filter_on2()));

  connect(ui->widePlot, SIGNAL(toggle_filter1()),this,
          SLOT(toggle_filter2()));

  //Restore user's settings
  m_settings->beginGroup("WideGraph");
  restoreGeometry (m_settings->value ("geometry", saveGeometry ()).toByteArray ());

  if(m_settings->value("PlotZero").toInt()>=-50 && m_settings->value("PlotZero").toInt()<=50)
    ui->widePlot->setPlotZero(m_settings->value("PlotZero", 0).toInt());
  else ui->widePlot->setPlotZero(0);

  if(m_settings->value("PlotGain").toInt()>=-50 && m_settings->value("PlotGain").toInt()<=50)
    ui->widePlot->setPlotGain(m_settings->value("PlotGain", 0).toInt());
  else ui->widePlot->setPlotGain(0);

  if(m_settings->value("Plot2dGain").toInt()>=-50 && m_settings->value("Plot2dGain").toInt()<=50)
    ui->widePlot->setPlot2dGain(m_settings->value("Plot2dGain", 0).toInt());
  else ui->widePlot->setPlot2dGain(0);

  if(m_settings->value("Plot2dZero").toInt()>=-50 && m_settings->value("Plot2dZero").toInt()<=50)
    ui->widePlot->setPlot2dZero(m_settings->value("Plot2dZero", 0).toInt());
  else ui->widePlot->setPlot2dZero(0);

  ui->zeroSlider->setValue(ui->widePlot->plotZero());
  ui->gainSlider->setValue(ui->widePlot->plotGain());
  ui->gain2dSlider->setValue(ui->widePlot->plot2dGain());
  ui->zero2dSlider->setValue(ui->widePlot->plot2dZero());

  ui->gainWFLabel->setText("Gain " + QString::number(ui->gainSlider->value()));
  ui->zeroWFLabel->setText("Zero " + QString::number(ui->zeroSlider->value()));
  ui->gainSpecLabel->setText("Gain " + QString::number(ui->gain2dSlider->value()));
  ui->zeroSpecLabel->setText("Zero " + QString::number(ui->zero2dSlider->value()));

  int n = m_settings->value("BinsPerPixel",5).toInt(); if(!(n>=1 && n<=100)) n=5;

  m_timestamp = 1; int itstamp=m_settings->value("Timestamp",1).toInt();
  QString ststamp=m_settings->value("Timestamp","1").toString();
  if(ststamp == "0" || ststamp == "1" || ststamp == "2") m_timestamp = itstamp; 
  ui->timestampComboBox->setCurrentIndex(m_timestamp); ui->widePlot->setTimestamp(m_timestamp);

  m_bars=m_settings->value("Bars",true).toBool();
  ui->cbBars->setChecked(m_bars);
  ui->widePlot->setBars(m_bars);
  
  m_freq=m_settings->value("Freq",false).toBool();
  ui->cbFreq->setChecked(m_freq);
  ui->widePlot->showFreq(m_freq);

  m_bScale=m_settings->value("Scale",true).toBool();
  ui->cbScale->setChecked(m_bScale);
  ui->widePlot->setScale(m_bScale);

  m_bFlatten=m_settings->value("Flatten",false).toBool();
  ui->cbFlatten->setChecked(m_bFlatten);
  ui->widePlot->setFlatten(m_bFlatten);

  if(m_settings->value("PlotWidth").toInt()>=636 && m_settings->value("PlotWidth").toInt()<=4096)
    ui->widePlot->setBreadth(m_settings->value("PlotWidth",1000).toInt());
  else ui->widePlot->setBreadth(1000);

  ui->bppSpinBox->setValue(n);

  m_Percent2DScreen=m_settings->value("Percent2D",30).toInt();
  if(!(m_Percent2DScreen>=0 && m_Percent2DScreen<=100)) m_Percent2DScreen =30;
  ui->sbPercent2dPlot->setValue(m_Percent2DScreen);

  m_waterfallAvg = m_settings->value("WaterfallAvg",1).toInt();
  if(!(m_waterfallAvg>=1 && m_waterfallAvg<=50)) m_waterfallAvg=1;
  ui->waterfallAvgSpinBox->setValue(m_waterfallAvg); ui->widePlot->setWaterfallAvg(m_waterfallAvg);

  ui->widePlot->setCurrent(m_settings->value("Current",true).toBool());
  ui->widePlot->setCumulative(m_settings->value("Cumulative",false).toBool());
  if(ui->widePlot->current()) ui->spec2dComboBox->setCurrentIndex(0);
  if(ui->widePlot->cumulative()) ui->spec2dComboBox->setCurrentIndex(1);

  int nbpp=m_settings->value("BinsPerPixel",5).toInt(); if(!(nbpp>=1 && nbpp<=100)) nbpp=5;
  ui->widePlot->setBinsPerPixel(nbpp);

  if(m_settings->value("StartFreq").toInt()>=0 && m_settings->value("StartFreq").toInt()<=4900) {
    ui->widePlot->setStartFreq(m_settings->value("StartFreq",0).toInt());
    ui->fStartSpinBox->setValue(ui->widePlot->startFreq());
  }
  else {
    ui->widePlot->setStartFreq(0);
    ui->fStartSpinBox->setValue(0);
  }

  m_waterfallPalette=m_settings->value("WaterfallPalette","Default").toString();
  m_userPalette = WFPalette {m_settings->value("UserPalette").value<WFPalette::Colours> ()};

  int m_fMin = m_settings->value ("Fmin", 2400).toInt (); if(!(m_fMin>=0 && m_fMin<=5000)) m_fMin=2400;
  ui->fSplitSpinBox->setValue (m_fMin);
  setRxRange (m_fMin);
  ui->controls_widget->setVisible(!m_settings->value("HideControls",false).toBool());
  ui->cbControls->setChecked(!m_settings->value("HideControls",false).toBool());

  m_settings->endGroup();

  saveSettings ();		// update config with defaults

  QStringList allFiles = m_palettes_path.entryList(QDir::NoDotAndDotDot |
        QDir::System | QDir::Hidden | QDir::AllDirs | QDir::Files,
        QDir::DirsFirst);
  int index=0;
  foreach(QString file, allFiles) {
    QString t=file.left(file.length()-4);
    ui->paletteComboBox->addItem(t);
    if(t==m_waterfallPalette) ui->paletteComboBox->setCurrentIndex(index);
    index++;
  }
  ui->paletteComboBox->addItem (user_defined);
  if (user_defined == m_waterfallPalette) ui->paletteComboBox->setCurrentIndex(index);
  readPalette ();
  m_bHaveTransmitted=false;
}

WideGraph::~WideGraph ()
{
}

void WideGraph::closeEvent (QCloseEvent * e)
{
  saveSettings ();
  QDialog::closeEvent (e);
}

void WideGraph::saveSettings()                                           //saveSettings
{
  m_settings->beginGroup ("WideGraph");
  m_settings->setValue ("geometry", saveGeometry ());
  m_settings->setValue ("PlotZero", ui->widePlot->plotZero());
  m_settings->setValue ("PlotGain", ui->widePlot->plotGain());
  m_settings->setValue ("Plot2dGain", ui->widePlot->plot2dGain());
  m_settings->setValue ("Plot2dZero", ui->widePlot->plot2dZero());
  m_settings->setValue ("PlotWidth", ui->widePlot->plotWidth ());
  m_settings->setValue ("BinsPerPixel", ui->bppSpinBox->value ());
  m_settings->setValue ("Percent2D",m_Percent2DScreen);
  m_settings->setValue ("WaterfallAvg", ui->waterfallAvgSpinBox->value ());
  m_settings->setValue ("Current", ui->widePlot->current());
  m_settings->setValue ("Cumulative", ui->widePlot->cumulative());
  m_settings->setValue ("BinsPerPixel", ui->widePlot->binsPerPixel ());
  m_settings->setValue ("StartFreq", ui->widePlot->startFreq ());
  m_settings->setValue ("WaterfallPalette", m_waterfallPalette);
  m_settings->setValue ("UserPalette", QVariant::fromValue (m_userPalette.colours ()));
  m_settings->setValue ("Fmin", m_fMin);
  m_settings->setValue ("Timestamp",m_timestamp);
  m_settings->setValue ("Scale",m_bScale);
  m_settings->setValue ("Flatten",m_bFlatten);
  m_settings->setValue ("HideControls", ui->controls_widget->isHidden ());
  m_settings->setValue ("Bars", m_bars);
  m_settings->setValue ("Freq", m_freq);
  m_settings->endGroup ();
}

void WideGraph::dataSink2(float s[], float df3, int ihsym, int ndiskdata)  //dataSink2
{
  static float splot[NSMAX];
  int nbpp = ui->widePlot->binsPerPixel();
  static int n=0;

  //Average spectra over specified number, m_waterfallAvg
  if (n==0) {
    for (int i=0; i<NSMAX; i++)
      splot[i]=s[i];
  } else {
    for (int i=0; i<NSMAX; i++)
      splot[i] += s[i];
  }
  n++;

  if (n>=m_waterfallAvg) {
    for (int i=0; i<NSMAX; i++)
        splot[i] /= n;                       //Normalize the average
    n=0;
    int i=int(ui->widePlot->startFreq()/df3 + 0.5);
    int jz=5000.0/(nbpp*df3);
		if(jz>MAX_SCREENSIZE) jz=MAX_SCREENSIZE;
    for (int j=0; j<jz; j++) {
      float sum=0;
      for (int k=0; k<nbpp; k++) {
        sum += splot[i++];
      }
      swide[j]=sum;
    }

// Time according to this computer
    qint64 ms = m_jtdxtime->currentMSecsSinceEpoch2() % 86400000;
    double tr = fmod(0.001*ms,m_TRperiod);
    if((ndiskdata && ihsym <= m_waterfallAvg) || (!ndiskdata && tr<m_tr0)) {
      float flagValue=1.0e30;
      if(m_bHaveTransmitted) flagValue=2.0e30;
      for (int i=0; i<2048; i++) {
        swide[i] = flagValue;
      }
      m_bHaveTransmitted=false;
    }
    m_tr0=tr;
    ui->widePlot->draw(swide,true);
  }
}

void WideGraph::on_bppSpinBox_valueChanged(int n)                            //bpp
{
  ui->widePlot->setBinsPerPixel(n);
}

void WideGraph::on_waterfallAvgSpinBox_valueChanged(int n)                  //Navg
{
  m_waterfallAvg = n;
  ui->widePlot->setWaterfallAvg(n);
}

void WideGraph::keyPressEvent(QKeyEvent *e)                                 //F11, F12
{  
  switch(e->key())
  {
  int n;
  case Qt::Key_F11:
    n=11;
    if(e->modifiers() & Qt::ControlModifier) n+=100;
    emit f11f12(n);
    break;
  case Qt::Key_F12:
    n=12;
    if(e->modifiers() & Qt::ControlModifier) n+=100;
    emit f11f12(n);
    break;
  case Qt::Key_Z:
    if(e->modifiers() & Qt::AltModifier) {
      emit toggle_filter3 ();
      return;
    }
    break;
  case Qt::Key_Escape:
    emit esc_key ();
    break;
  default:
    QDialog::keyPressEvent (e);
  }
}

void WideGraph::setRxFreq(int n)                                           //setRxFreq
{
  ui->widePlot->setRxFreq(n);
  ui->widePlot->draw(swide,false);
  if(m_lockTxFreq) setTxFreq(n);
}

int WideGraph::rxFreq()                                                   //rxFreq
{
  return ui->widePlot->rxFreq();
}

int WideGraph::nStartFreq()                                             //nStartFreq
{
  return ui->widePlot->startFreq();
}

void WideGraph::wideFreezeDecode(int n)                              //wideFreezeDecode
{
  emit freezeDecode2(n);
}

void WideGraph::setRxRange(int fMin)                                //setRxRange
{
  ui->widePlot->setRxRange(fMin);
  ui->widePlot->DrawOverlay();
  ui->widePlot->update();
}

int WideGraph::Fmin()                                              //Fmin
{
  return m_fMin;
}

int WideGraph::Fmax()                                              //Fmax
{
  int n=ui->widePlot->Fmax();
  if(n>5000) n=5000;
  return n;
}

int WideGraph::fSpan()
{
  return ui->widePlot->fSpan ();
}

void WideGraph::setPeriod(double trperiod, int nsps)                  //SetPeriod
{
  m_TRperiod=trperiod;
  m_nsps=nsps;
  ui->widePlot->setNsps(trperiod, nsps);
}

void WideGraph::setTxFreq(int n)                                   //setTxFreq
{
  emit setXIT2(n);
  ui->widePlot->setTxFreq(n);
}

void WideGraph::setMode(QString mode)                              //setMode
{
  m_mode=mode;
  ui->fSplitSpinBox->setVisible(m_mode=="JT9+JT65");
  ui->labTime->setVisible(m_mode!="JT9+JT65"); ui->timestampComboBox->setVisible(m_mode!="JT9+JT65");
  ui->widePlot->setMode(mode);
  ui->widePlot->DrawOverlay();
  ui->widePlot->update();
}

void WideGraph::setTopJT65(int n)                              //set top JT65 RX freq
{
  m_topJT65=n;
  ui->widePlot->setTopJT65(n);
  ui->widePlot->DrawOverlay();
  ui->widePlot->update();
}

void WideGraph::setModeTx(QString modeTx)                          //setModeTx
{
  m_modeTx=modeTx;
  ui->widePlot->setModeTx(modeTx);
  ui->widePlot->DrawOverlay();
  ui->widePlot->update();
}
                                                        //Current-Cumulative-Yellow
void WideGraph::on_spec2dComboBox_currentIndexChanged(int n)
{
  ui->widePlot->setCurrent(false);
  ui->widePlot->setCumulative(false);
  if(n==0) ui->widePlot->setCurrent(true);
  if(n==1) ui->widePlot->setCumulative(true);
  if(ui->widePlot->m_bScaleOK) ui->widePlot->draw(swide,false);
}

void WideGraph::on_fSplitSpinBox_valueChanged(int n)              //fSplit
{
  m_fMin=n;
  setRxRange(m_fMin);
}

void WideGraph::setLockTxFreq(bool b)                             //LockTxFreq
{
  m_lockTxFreq=b;
  ui->widePlot->setLockTxFreq(b);
}

void WideGraph::setFilter(bool b)                             //Filter
{
  m_filter=b;
  ui->widePlot->setFilter(b);
}

void WideGraph::setHoundFilter(bool b)
{
  ui->widePlot->setHoundFilter(b);
}

void WideGraph::setDarkStyle(bool b)
{
  ui->widePlot->setDarkStyle(b);
}

void WideGraph::setFreq2(int rxFreq, int txFreq)                  //setFreq2
{
  emit setFreq3(rxFreq,txFreq);
}

void WideGraph::setRxFreq2(int rxFreq)                  //setRxFreq2 no lock Tx=RX
{
  emit setRxFreq3(rxFreq);
}

void WideGraph::filter_on2()                  //filter_on2
{
  emit filter_on3();
}

void WideGraph::toggle_filter2()              //toggle_filter2
{
  emit toggle_filter3();
}

void WideGraph::setDialFreq(double d)                             //setDialFreq
{
  ui->widePlot->setDialFreq(d);
}

void WideGraph::setRxBand(QString band)
{
  ui->widePlot->setRxBand(band);
}


void WideGraph::on_fStartSpinBox_valueChanged(int n)             //fStart
{
  ui->widePlot->setStartFreq(n);
}

void WideGraph::readPalette ()                                   //readPalette
{
  try
    {
      if (user_defined == m_waterfallPalette)
        {
          ui->widePlot->setColours (WFPalette {m_userPalette}.interpolate ());
        }
      else
        {
          ui->widePlot->setColours (WFPalette {m_palettes_path.absoluteFilePath (m_waterfallPalette + ".pal")}.interpolate());
        }
    }
  catch (std::exception const& e)
    {
      JTDXMessageBox::critical_message (this, "WideGraph", e.what());
    }
}

void WideGraph::on_paletteComboBox_activated (QString const& palette)    //palette selector
{
  m_waterfallPalette = palette;
  readPalette();
}

void WideGraph::on_timestampComboBox_currentIndexChanged(int n)
{
  m_timestamp = n;
  ui->widePlot->setTimestamp(n);
}

void WideGraph::on_cbScale_toggled(bool b)
{
  m_bScale=b;
  ui->widePlot->setScale(m_bScale);
}

void WideGraph::on_cbFlatten_toggled(bool b)
{
  m_bFlatten=b;
  ui->widePlot->setFlatten(m_bFlatten);
}

void WideGraph::on_cbControls_toggled(bool b)
{
  ui->controls_widget->setVisible(b);
}

void WideGraph::on_cbBars_toggled(bool b)
{
  m_bars = b;
  ui->widePlot->setBars(m_bars);
}

void WideGraph::on_cbFreq_toggled(bool b)
{
  m_freq = b;
  ui->widePlot->showFreq(m_freq);
}

void WideGraph::on_adjust_palette_push_button_clicked (bool)   //Adjust Palette
{
  try
    {
      if (user_defined != m_waterfallPalette) m_userPalette = WFPalette {m_palettes_path.absoluteFilePath (m_waterfallPalette + ".pal")} ;
      if (m_userPalette.design ())
        {
          m_waterfallPalette = user_defined;
          ui->paletteComboBox->setCurrentText (m_waterfallPalette);
          readPalette ();
        }
    }
  catch (std::exception const& e)
    {
      JTDXMessageBox::critical_message (this, "WideGraph",e.what());
    }
}

bool WideGraph::scale()                                              //Flatten
{
  return m_bScale;
}

bool WideGraph::flatten()                                              //Flatten
{
  return m_bFlatten;
}

void WideGraph::on_gainSlider_valueChanged(int value)                 //Gain
{
  ui->widePlot->setPlotGain(value);
  ui->gainWFLabel->setText("Gain " + QString::number(value));
}

void WideGraph::on_zeroSlider_valueChanged(int value)                 //Zero
{
  ui->widePlot->setPlotZero(value);
  ui->zeroWFLabel->setText("Zero " + QString::number(value));
}

void WideGraph::on_gain2dSlider_valueChanged(int value)               //Gain2
{
  ui->widePlot->setPlot2dGain(value);
  if(ui->widePlot->m_bScaleOK) ui->widePlot->draw(swide,false);
  ui->gainSpecLabel->setText("Gain " + QString::number(value));
}

void WideGraph::on_zero2dSlider_valueChanged(int value)               //Zero2
{
  ui->widePlot->setPlot2dZero(value);
  if(ui->widePlot->m_bScaleOK) ui->widePlot->draw(swide,false);
  ui->zeroSpecLabel->setText("Zero " + QString::number(value));
}

void WideGraph::setTol(int n)                                         //setTol
{
  ui->widePlot->setTol(n);
  ui->widePlot->DrawOverlay();
  ui->widePlot->update();
}

void WideGraph::setWSPRtransmitted()
{
  m_bHaveTransmitted=true;
}

void WideGraph::on_sbPercent2dPlot_valueChanged(int n)
{
  m_Percent2DScreen=n;
  ui->widePlot->SetPercent2DScreen(n);
}
