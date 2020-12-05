#include "plotter.h"
#include <qmath.h>
#include <QDebug>
#include <algorithm>
#include <QVector>
#include "commons.h"
#include "Radio.hpp"

#include "moc_plotter.cpp"

#define MAX_SCREENSIZE 2048

extern "C" {
  void flat4_(float swide[], int* iz, int* nflatten);
}
extern dec_data dec_data;

CPlotter::CPlotter(QWidget *parent) :                  //CPlotter Constructor
  QFrame {parent},
  m_bScaleOK {false},
  m_filter {false},
  m_fSpan {2000.0},
  m_plotZero {0},
  m_plotGain {0},
  m_plot2dGain {0},
  m_plot2dZero {0},
  m_nSubMode {0},
  m_Running {false},
  m_paintEventBusy {false},
  m_fftBinWidth {1500.0/2048.0},
  m_dialFreq {0.},
  m_sum {},
  m_dBStepSize {10},
  m_FreqUnits {1},
  m_hdivs {HORZ_DIVS},
  m_line {0},
  m_fSample {12000},
  m_nsps {6912},
  m_Percent2DScreen {30},      //percent of screen used for 2D display
  m_Percent2DScreen0 {0},
  m_rxFreq {1020},
  m_txFreq {0},
  m_startFreq {0},
  m_lastMouseX {-1},
  m_lastPaintedX {-1}
{
  setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
  setFocusPolicy(Qt::StrongFocus);
  setAttribute(Qt::WA_PaintOnScreen,false);
  setAutoFillBackground(false);
  setAttribute(Qt::WA_OpaquePaintEvent, false);
  setAttribute(Qt::WA_NoSystemBackground, true);
}

CPlotter::~CPlotter() { }                                      // Destructor

QSize CPlotter::minimumSizeHint() const
{
  return QSize(50, 50);
}

QSize CPlotter::sizeHint() const
{
  return QSize(180, 180);
}

void CPlotter::resizeEvent(QResizeEvent* )                    //resizeEvent()
{
  if(!size().isValid()) return;
  if( m_Size != size() or m_Percent2DScreen != m_Percent2DScreen0) {
    m_Size = size();
    m_w = m_Size.width();
    m_h = m_Size.height();
    m_h2 = m_Percent2DScreen*m_h/100.0;
    if(m_h2>m_h-30) m_h2=m_h-30;
    if(m_h2<1) m_h2=1;
    m_h1=m_h-m_h2;
    m_DialOverlayPixmap = QPixmap(m_Size.width(), m_h);
    m_DialOverlayPixmap.fill(Qt::transparent);
    m_HoverOverlayPixmap = QPixmap(m_Size.width(), m_h);
    m_HoverOverlayPixmap.fill(Qt::transparent);
    m_2DPixmap = QPixmap(m_Size.width(), m_h2);
    m_2DPixmap.fill(Qt::black);
    m_WaterfallPixmap = QPixmap(m_Size.width(), m_h1);
    m_OverlayPixmap = QPixmap(m_Size.width(), m_h2);
    m_OverlayPixmap.fill(Qt::black);
    m_WaterfallPixmap.fill(Qt::black);
    m_2DPixmap.fill(Qt::black);
    m_ScalePixmap = QPixmap(m_w,30);
    m_ScalePixmap.fill(Qt::white);
    m_Percent2DScreen0 = m_Percent2DScreen;
  }
  DrawOverlay();
}

void CPlotter::paintEvent(QPaintEvent *)                                // paintEvent()
{
  if(m_paintEventBusy) return;
  m_paintEventBusy=true;
  QPainter painter(this);
  painter.drawPixmap(0,0,m_ScalePixmap);
  painter.drawPixmap(0,30,m_WaterfallPixmap);
  painter.drawPixmap(0,m_h1,m_2DPixmap);
  int x = XfromFreq(m_rxFreq);
  if (m_bars) {
    painter.drawPixmap(0,30,m_DialOverlayPixmap);
    if(m_lastMouseX >= 0 && m_lastMouseX != x){
      painter.drawPixmap(m_lastMouseX, 0, m_HoverOverlayPixmap);
    }
  }
  if(m_freq && m_lastMouseX >= 0 && m_lastMouseX != m_lastPaintedX) {
    QFont font = CPlotter::font ();
    QString freq=QString::number(int(FreqfromX(m_lastMouseX)));
    int z = font.pointSize()*freq.length()+64/font.pointSize();
    QPoint pos = m_pos + QPoint(-z,-10);
    QToolTip::showText(pos,freq);
  }
  m_lastPaintedX = m_lastMouseX;
  m_paintEventBusy=false;
}

void scale_by_median(float swide[], int* iz)
{
  if(swide[0] > 1E29) return;
  std::vector<float> buf (swide, swide + *iz); std::sort(buf.begin(), buf.end());
  auto smedian=buf[*iz/2]+0.01; if(smedian <= 0.0) return;
  for (int i=0; i<*iz; ++i) swide[i] /= smedian;
}

void CPlotter::draw(float swide[], bool bScroll)                            //draw()
{
  int j,j0;
  float y,y2,ymin;

//  double fac = sqrt(m_binsPerPixel*m_waterfallAvg/15.0);
//  double gain = fac*pow(10.0,0.02*m_plotGain);
  double gain = pow(10.0,0.02*m_plotGain);
  double gain2d = pow(10.0,0.02*(m_plot2dGain));

//move current data down one line (must do this before attaching a QPainter object)
  if(bScroll) m_WaterfallPixmap.scroll(0,1,0,0,m_w,m_h1);
  QPainter painter1(&m_WaterfallPixmap);
  m_2DPixmap = m_OverlayPixmap.copy(0,0,m_w,m_h2);
  QPainter painter2D(&m_2DPixmap);
  if(!painter2D.isActive()) return;
  QFont Font("Arial");
  Font.setPointSize(11);
  QFontMetrics metrics(Font);
  Font.setWeight(QFont::Normal);
  painter2D.setFont(Font);
  painter2D.setPen(Qt::green);

  QPoint LineBuf[MAX_SCREENSIZE];
  j=0;
  j0=int(m_startFreq/m_fftBinWidth + 0.5);
  int iz=XfromFreq(5000.0);
  int jz=iz*m_binsPerPixel;
  m_fMax=FreqfromX(iz);

  if(bScroll) {
    if(m_Scale) scale_by_median(swide,&iz);
    flat4_(swide,&iz,&m_Flatten);
    if(m_Scale) scale_by_median(&dec_data.savg[j0],&jz);
    flat4_(&dec_data.savg[j0],&jz,&m_Flatten);
  }

  ymin=1.e30;
  if(swide[0]>1.e29 and swide[0]< 1.5e30) painter1.setPen(Qt::green);
  if(swide[0]>1.4e30) painter1.setPen(Qt::yellow);
  for(int i=0; i<iz; i++) {
    y=swide[i];
    if(y<ymin) ymin=y;
    int y1 = 10.0*gain*y + 10*m_plotZero +40;
    if (y1<0) y1=0;
    if (y1>254) y1=254;
    if (swide[i]<1.e29) painter1.setPen(g_ColorTbl[y1]);
    painter1.drawPoint(i,0);
  }

  float y2min=1.e30;
  float y2max=-1.e30;
  for(int i=0; i<iz; i++) {
    y=swide[i] - ymin;
    y2=0;
    if(m_bCurrent) y2 = gain2d*y + m_plot2dZero;            //Current

    if(bScroll) {
      float sum=0.0;
      int j=j0+m_binsPerPixel*i;
      for(int k=0; k<m_binsPerPixel; k++) {
        sum+=dec_data.savg[j++];
      }
      m_sum[i]=sum;
    }
    if(m_bCumulative) y2=gain2d*(m_sum[i]/m_binsPerPixel + m_plot2dZero);
    if(m_Flatten==0) y2 += 15;                      //### could do better! ###

    if(i==iz-1) painter2D.drawPolyline(LineBuf,j);
    LineBuf[j].setX(i);
    LineBuf[j].setY(int(0.9*m_h2-y2*m_h2/70.0));
    if(y2<y2min) y2min=y2;
    if(y2>y2max) y2max=y2;
    j++;
  }

  if(swide[0]>1.0e29) m_line=0;
  if(m_mode=="FT4" and m_line==34) m_line=0;
  m_line++;
  if(m_timestamp!=0 || m_mode=="JT9+JT65") {
    if(m_line == 16) {
      painter1.setPen(Qt::white);
      QString t;
      if(m_TRperiod < 60.0) {
        qint64 ms = m_jtdxtime->currentMSecsSinceEpoch2() % 86400000;
        int n = fmod(0.001*ms,m_TRperiod);
        QDateTime t1=m_jtdxtime->currentDateTimeUtc2().addSecs(-n);
        t=t1.toString("hh:mm:ss") + "    " + m_rxBand;
      } else {
        t=m_jtdxtime->currentDateTimeUtc2().toString("hh:mm") + "    " + m_rxBand;
      }
      QRect rect{5, -2, m_w-10, metrics.height()};
      QRect boundingRect;
      painter1.drawText(rect, m_timestamp==2?0x0082:0x0081,t, &boundingRect);
/*QPen pen = painter1.pen();
pen.setStyle(Qt::DotLine);
painter1.setPen(pen);
painter1.drawRect(boundingRect.adjusted(0, 0, -pen.width(), -pen.width()));
pen.setStyle(Qt::DashLine);
painter1.setPen(pen);
painter1.drawRect(rect.adjusted(0, 0, -pen.width(), -pen.width())); */
    }
  }
  update();                    //trigger a new paintEvent
  m_bScaleOK=true;
}

void CPlotter::DrawOverlay()                                 //DrawOverlay()
{
  if(m_OverlayPixmap.isNull()) return;
  if(m_WaterfallPixmap.isNull()) return;
  int w = m_WaterfallPixmap.width();
  int x,y,x1,x2;
  float pixperdiv;

  double df = m_binsPerPixel*m_fftBinWidth;
  QRect rect;
  {
    QPainter painter(&m_OverlayPixmap);
    if (!painter.isActive()) painter.begin(this);
//    painter.initFrom(this);
    QLinearGradient gradient(0, 0, 0 ,m_h2);         //fill background with gradient
    gradient.setColorAt(1, Qt::black);
    gradient.setColorAt(0, Qt::darkBlue);
    painter.setBrush(gradient);
    painter.drawRect(0, 0, m_w, m_h2);
    painter.setBrush(Qt::SolidPattern);
    m_fSpan = w*df;
//  int n=m_fSpan/10;
    m_freqPerDiv=10;
    if(m_fSpan>100) m_freqPerDiv=20;
    if(m_fSpan>250) m_freqPerDiv=50;
    if(m_fSpan>500) m_freqPerDiv=100;
    if(m_fSpan>1000) m_freqPerDiv=200;
    if(m_fSpan>2500) m_freqPerDiv=500;

    pixperdiv = m_freqPerDiv/df;
    m_hdivs = w*df/m_freqPerDiv + 1.9999;

    float xx0=float(m_startFreq)/float(m_freqPerDiv);
    xx0=xx0-int(xx0);
    int x0=xx0*pixperdiv+0.5;
    for( int i=1; i<m_hdivs; i++) {                  //draw vertical grids
      x = (int)((float)i*pixperdiv ) - x0;
      if(x >= 0 and x<=m_w) {
        painter.setPen(QPen(Qt::white, 1,Qt::DotLine));
        painter.drawLine(x, 0, x , m_h2);
      }
    }

    pixperdiv = (float)m_h2 / (float)VERT_DIVS;
    painter.setPen(QPen(Qt::white, 1,Qt::DotLine));
    for( int i=1; i<VERT_DIVS; i++) {                //draw horizontal grids
      y = (int)( (float)i*pixperdiv );
      painter.drawLine(0, y, w, y);
    }
  }

  QRect rect0;
  QPainter painter0(&m_ScalePixmap);
  if (!painter0.isActive()) painter0.begin(this);
//  painter0.initFrom(this);

  //create Font to use for scales
  QFont Font("Arial");
  Font.setPointSize(12);
  QFontMetrics metrics(Font);
  Font.setWeight(QFont::Normal);
  painter0.setFont(Font);
  painter0.setPen(Radio::convert_dark("#000000",m_useDarkStyle));

  if(m_binsPerPixel < 1) m_binsPerPixel=1;
  m_hdivs = w*df/m_freqPerDiv + 0.9999;

  m_ScalePixmap.fill(Radio::convert_dark("#ffffff",m_useDarkStyle));
  painter0.drawRect(0, 0, w, 30);
  MakeFrequencyStrs();

//draw tick marks on upper scale
  pixperdiv = m_freqPerDiv/df;
  for( int i=0; i<m_hdivs; i++) {                    //major ticks
    x = (int)((m_xOffset+i)*pixperdiv );
    painter0.drawLine(x,18,x,30);
  }
  int minor=5;
  if(m_freqPerDiv==200) minor=4;
  for( int i=1; i<minor*m_hdivs; i++) {             //minor ticks
    x = i*pixperdiv/minor;
    painter0.drawLine(x,24,x,30);
  }

  //draw frequency values
  for( int i=0; i<=m_hdivs; i++) {
    x = (int)((m_xOffset+i)*pixperdiv - pixperdiv/2);
    rect0.setRect(x,0, (int)pixperdiv, 20);
    painter0.drawText(rect0, Qt::AlignHCenter|Qt::AlignVCenter,m_HDivText[i]);
  }

  float bw=0.0;
  if(m_modeTx=="FT8") bw=7*12000.0/1920.0;
  else if(m_mode=="FT4") bw=3*12000.0/576.0;
  else if(m_modeTx=="JT65") bw=66.0*11025.0/4096.0;
  else if(m_modeTx=="JT9") bw=9.0*12000.0/m_nsps;
  else if(m_modeTx=="T10") bw=9.0*4.0*12000.0/6912.0;

  if(m_filter==true) { //Mark Filter Freq with blue
    QPen pen4(QColor(Radio::convert_dark("#0000ff",m_useDarkStyle)), 2);
    painter0.setPen(pen4);
    if(m_mode=="FT8") {
      if(!m_houndFilter) { x1=XfromFreq(m_rxFreq-60.0); x2=XfromFreq(m_rxFreq+110.0); }
      else { x1=XfromFreq(m_rxFreq-290.0); x2=XfromFreq(m_rxFreq+340.0); }
      painter0.drawLine(x1,23,x1,30); painter0.drawLine(x1,23,x2,23); painter0.drawLine(x2,23,x2,30);
    }
    else if(m_mode=="FT4") {
      x1=XfromFreq(m_rxFreq-95.0); x2=XfromFreq(m_rxFreq+179.0);
      painter0.drawLine(x1,23,x1,30); painter0.drawLine(x1,23,x2,23); painter0.drawLine(x2,23,x2,30);
    }
    else if((m_mode=="JT65" or m_mode=="JT9+JT65") and m_modeTx=="JT65") {
      x1=XfromFreq(m_rxFreq-200.0); x2=XfromFreq(m_rxFreq+375.0);
      painter0.drawLine(x1,23,x1,30); painter0.drawLine(x1,23,x2,23); painter0.drawLine(x2,23,x2,30);
      QPen pen5(QColor(255,170,127), 4); //Mark Filter show Freq with pink
      painter0.setPen(pen5);
      x1=XfromFreq(m_rxFreq-50.0); x2=XfromFreq(m_rxFreq+225.0);
      painter0.drawLine(x1,28,x2,28); painter0.drawLine(x1,26,x2,26);
    }
    else if(m_mode=="JT9") {
      x1=XfromFreq(m_rxFreq-50.0); x2=XfromFreq(m_rxFreq+65.6);
      painter0.drawLine(x1,23,x1,30); painter0.drawLine(x1,23,x2,23); painter0.drawLine(x2,23,x2,30);
    }
    else if(m_mode=="T10") {
      x1=XfromFreq(m_rxFreq-80.0); x2=XfromFreq(m_rxFreq+142.4);
      painter0.drawLine(x1,23,x1,30); painter0.drawLine(x1,23,x2,23); painter0.drawLine(x2,23,x2,30);
    }
  }

  QPen pen0(Qt::green, 3);                 //Mark Rx Freq with green
  painter0.setPen(pen0);
  if(m_mode=="WSPR-2") {                   //### WSPR-15 code needed here, too ###
    x1=XfromFreq(1400); x2=XfromFreq(1600);
    painter0.drawLine(x1,29,x2,29);
  }
  QPainter overPainter(&m_DialOverlayPixmap);
  if (m_bars) {
    if (!overPainter.isActive()) overPainter.begin(this);
    overPainter.setCompositionMode(QPainter::CompositionMode_Source);
    overPainter.fillRect(0, 0, m_Size.width(), m_h, Qt::transparent);
  }
  if(m_mode.startsWith("FT") or m_mode.startsWith("JT") or m_mode=="T10") {
    x1=XfromFreq(m_rxFreq); x2=XfromFreq(m_rxFreq+bw);
    painter0.drawLine(x1,24,x1,30); painter0.drawLine(x1,28,x2,28); painter0.drawLine(x2,24,x2,30);
    if (m_bars) {
      overPainter.setPen(Qt::green);
      overPainter.drawLine(x1,0,x1,m_h); overPainter.drawLine(x2,0,x2,m_h);
    }
  }

  if(m_mode.startsWith("FT") or m_mode.startsWith("JT") or m_mode=="T10" or m_mode.left(4)=="WSPR") {
    QPen pen1(Qt::red, 3);                   //Mark Tx freq with red
    painter0.setPen(pen1);
    x1=XfromFreq(m_txFreq); x2=XfromFreq(m_txFreq+bw);
    if(m_mode=="WSPR-2") {                  //### WSPR-15 code needed here, too
      bw=4*12000.0/8192.0;                  //WSPR
      x1=XfromFreq(m_txFreq-0.5*bw); x2=XfromFreq(m_txFreq+0.5*bw);
    }
    painter0.drawLine(x1,17,x1,21); painter0.drawLine(x1,17,x2,17); painter0.drawLine(x2,17,x2,21);
    if (m_bars) {
      overPainter.setPen(Qt::red);
      overPainter.drawLine(x1,0,x1,m_h); overPainter.drawLine(x2,0,x2,m_h);
    }
  }
  QPainter hoverPainter(&m_HoverOverlayPixmap);
  if (m_bars) {
    if (!hoverPainter.isActive()) hoverPainter.begin(this);
    int fwidth=XfromFreq(m_rxFreq+bw)-XfromFreq(m_rxFreq);
    hoverPainter.setCompositionMode(QPainter::CompositionMode_Source);
    hoverPainter.fillRect(0, 0, m_Size.width(), m_h, Qt::transparent);
    hoverPainter.setPen(QPen(Qt::white));
    hoverPainter.drawLine(0, 30, 0, m_h); // first slot, left line hover
    hoverPainter.drawLine(fwidth, 30, fwidth, m_h); // first slot, right line hover
  }

  if(m_mode=="JT9+JT65") {
    QPen pen2(Qt::blue, 3);                //Mark the JT65 | JT9 divider
    painter0.setPen(pen2);
    x1=XfromFreq(m_fMin);
    if(x1<2) x1=2;
    painter0.drawLine(x1,8,x1,28);
	if(m_topJT65 < m_fMin) {
      QPen pen6(Qt::gray, 3);                //Mark no RX area
      painter0.setPen(pen6);
      x1=XfromFreq(m_topJT65); x2=XfromFreq(m_fMin)-3;
      painter0.drawLine(x1,22,x1,28); painter0.drawLine(x1,26,x2,26); painter0.drawLine(x1,24,x2,24);
    }
  }

  if(m_mode=="JT9+JT65" || m_mode=="JT65") {
	QPen pen7(Qt::gray, 3);                //Mark top JT65 decoding frequency
    painter0.setPen(pen7);
    x1=XfromFreq(m_topJT65);
    if(x1<2) x1=2;
    if((m_mode=="JT9+JT65" && m_topJT65 != m_fMin) || m_mode=="JT65") painter0.drawLine(x1,8,x1,28);
	if(m_mode=="JT9+JT65" && m_topJT65 == m_fMin) painter0.drawLine(x1,20,x1,28);
  }
  float f_quard = 0;
  if(m_dialFreq>1.83 and m_dialFreq< 1.84) f_quard = 1.838;
  else if(m_dialFreq>3.56 and m_dialFreq< 3.57) f_quard = 3.57;
  else if(m_dialFreq>5.28 and m_dialFreq< 5.29) f_quard = 5.2886;
  else if(m_dialFreq>7.03 and m_dialFreq< 7.05) f_quard = 7.04;
  else if(m_dialFreq>10.13 and m_dialFreq< 10.15) f_quard = 10.1401;
  else if(m_dialFreq>14.09 and m_dialFreq< 14.1) f_quard = 14.097;
  else if(m_dialFreq>18.1 and m_dialFreq< 18.11) f_quard = 18.106;
  else if(m_dialFreq>21.09 and m_dialFreq< 21.1) f_quard = 21.096;
  else if(m_dialFreq>24.92 and m_dialFreq< 24.93) f_quard = 24.926;
  else if(m_dialFreq>28.12 and m_dialFreq< 28.13) f_quard = 28.126;
  else if(m_dialFreq>50.28 and m_dialFreq< 50.3) f_quard = 50.2944;
  else if(m_dialFreq>70.08 and m_dialFreq< 70.1) f_quard = 70.0924;
  if (f_quard >0) {
    float f1=1.0e6*(f_quard - m_dialFreq);
    float f2=f1+200.0;
    x1=XfromFreq(f1); x2=XfromFreq(f2);
    if(x1<=m_w and x2>=0) {
      QPen pen1(QColor(255,165,0),3);             //Mark WSPR sub-band orange
      painter0.setPen(pen1);
      painter0.drawLine(x1,9,x2,9);
    }
  }
}

void CPlotter::MakeFrequencyStrs()                       //MakeFrequencyStrs
{
  int f=(m_startFreq+m_freqPerDiv-1)/m_freqPerDiv;
  f*=m_freqPerDiv;
  m_xOffset=float(f-m_startFreq)/m_freqPerDiv;
  for(int i=0; i<=m_hdivs; i++) {
    m_HDivText[i].setNum(f);
    f+=m_freqPerDiv;
  }
}

int CPlotter::XfromFreq(float f)
{
  int x = int(m_w * (f - m_startFreq)/m_fSpan + 0.5);
  if(x<0 ) return 0;
  if(x>m_w) return m_w;
  return x;
}

float CPlotter::FreqfromX(int x)                               //FreqfromX()
{
  return float(m_startFreq + x*m_binsPerPixel*m_fftBinWidth);
}

void CPlotter::setFilter (bool b)        //set filter lines
{
  m_filter=b;
  DrawOverlay(); 
  update();                              //trigger a new paintEvent}	
}

void CPlotter::setHoundFilter (bool b)     //set wide filter lines for Hound mode
{
  m_houndFilter=b;
  if(m_filter) { DrawOverlay(); update(); }
}  

void CPlotter::setDarkStyle (bool b)     //set wide filter lines for Hound mode
{
  m_useDarkStyle=b;
  DrawOverlay();
  update();
}  

void CPlotter::SetRunningState(bool running) { m_Running = running; }
void CPlotter::setPlotZero(int plotZero) { m_plotZero=plotZero; }
int CPlotter::plotZero() { return m_plotZero; }
void CPlotter::setPlotGain(int plotGain) { m_plotGain=plotGain; }
int CPlotter::plotGain() { return m_plotGain; }
int CPlotter::plot2dGain() { return m_plot2dGain; }
void CPlotter::setPlot2dGain(int n) { m_plot2dGain=n; update(); }
int CPlotter::plot2dZero() { return m_plot2dZero; }
void CPlotter::setPlot2dZero(int plot2dZero) { m_plot2dZero=plot2dZero; }
void CPlotter::setStartFreq(int f) { m_startFreq=f; resizeEvent(NULL); update(); }
int CPlotter::startFreq() { return m_startFreq; }
int CPlotter::plotWidth() { return m_WaterfallPixmap.width(); }
void CPlotter::UpdateOverlay() { DrawOverlay(); }
void CPlotter::setDataFromDisk(bool b) { m_dataFromDisk=b; }
void CPlotter::setRxRange(int fMin) { m_fMin=fMin; }

void CPlotter::setBinsPerPixel(int n)
{
  m_binsPerPixel = n;
  DrawOverlay();                         //Redraw scales and ticks
  update();                              //trigger a new paintEvent}
}

int CPlotter::binsPerPixel() { return m_binsPerPixel; }
void CPlotter::setWaterfallAvg(int n) { m_waterfallAvg = n; }

void CPlotter::setRxFreq (int x)                               //setRxFreq
{
  m_rxFreq = x;         // x is freq in Hz
  DrawOverlay();
  update();
}

int CPlotter::rxFreq() { return m_rxFreq; }                      //rxFreq

void CPlotter::leaveEvent(QEvent *event)
{
    m_lastMouseX = -1;
    m_lastPaintedX = -1;
    event->ignore();
}

void CPlotter::mouseMoveEvent (QMouseEvent * event)
{
    int x = event->x();
    if(x < 0) x = 0;
    if(x>m_Size.width()) x = m_Size.width();
    if(m_freq) m_pos = event->globalPos();
    m_lastMouseX = x;
    update();

    event->ignore();
}

void CPlotter::mousePressEvent(QMouseEvent *event)             //mousePressEvent
{
  int x=event->x();
  int freq = m_rxFreq;
  if(x<0) x=0;
  if(x>m_Size.width()) x=m_Size.width();
  bool alt = (event->modifiers() & Qt::AltModifier);
  bool ctrl = (event->modifiers() & Qt::ControlModifier);
  bool rightbutton = (event->button() & Qt::RightButton);
  bool leftbutton = (event->button() & Qt::LeftButton);
  if(!rightbutton or (rightbutton and m_lockTxFreq)) freq = int(FreqfromX(x)+0.5);
  int tx_freq = m_txFreq;
  if (ctrl or m_lockTxFreq) tx_freq = freq;
  if (rightbutton and !m_lockTxFreq) tx_freq = int(FreqfromX(x)+0.5);
  emit setFreq1 (freq, tx_freq);
  if (leftbutton and !m_lockTxFreq) emit setRxFreq1 (freq);
  if (alt and leftbutton) emit filter_on1 ();
  int n=1;
  if(ctrl) n+=100;
  emit freezeDecode1(n);
}

void CPlotter::keyPressEvent( QKeyEvent *e ) //keyPressEvent
{
  switch(e->key())
    {
    case Qt::Key_Z:
      if(e->modifiers() & Qt::AltModifier) {
        emit toggle_filter1 ();
        return;
      }
      break;
    }
  QFrame::keyPressEvent (e);
}

void CPlotter::mouseDoubleClickEvent(QMouseEvent *event)          //mouse2click
{
  bool ctrl = (event->modifiers() & Qt::ControlModifier);
  int n=2;
  if(ctrl) n+=100;
  emit freezeDecode1(n);
}

void CPlotter::setNsps(double trperiod, int nsps)                    //setNsps
{
  m_TRperiod=trperiod;
  m_nsps=nsps;
  m_fftBinWidth=1500.0/2048.0;
  if(m_nsps==15360)  m_fftBinWidth=1500.0/2048.0;
  if(m_nsps==40960)  m_fftBinWidth=1500.0/6144.0;
  if(m_nsps==82944)  m_fftBinWidth=1500.0/12288.0;
  if(m_nsps==252000) m_fftBinWidth=1500.0/32768.0;
  DrawOverlay();                         //Redraw scales and ticks
  update();                              //trigger a new paintEvent}
}

void CPlotter::setBars(bool b)
{
  setMouseTracking(b || m_freq);
  m_bars=b;
  DrawOverlay();
  update();
}

void CPlotter::showFreq(bool b)
{
  setMouseTracking(b || m_bars);
  m_freq=b;
}

void CPlotter::setTxFreq(int n) { m_txFreq=n; DrawOverlay(); update(); }
void CPlotter::setMode(QString mode) { m_mode=mode; }
void CPlotter::setTopJT65(int n) { m_topJT65=n; }
void CPlotter::setModeTx(QString modeTx) { m_modeTx=modeTx; }
int CPlotter::Fmax() { return m_fMax; }
void CPlotter::setDialFreq(double d) { m_dialFreq=d; DrawOverlay(); update(); }
void CPlotter::setRxBand(QString band) { m_rxBand=band; }
void CPlotter::setTimestamp(int n) { m_timestamp=n; }
void CPlotter::setScale(bool b) { m_Scale=b; }
void CPlotter::setFlatten(bool b) { m_Flatten=0; if(b) m_Flatten=1; }
void CPlotter::setTol(int n) { m_tol=n; DrawOverlay(); }
void CPlotter::setColours(QVector<QColor> const& cl) { g_ColorTbl = cl; }
void CPlotter::SetPercent2DScreen(int percent) { m_Percent2DScreen=percent; resizeEvent(NULL); update(); }
