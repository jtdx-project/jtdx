// last time modified by Igor UA3DJY on 20200221

// Simple bargraph meter
// Implemented by Edson Pereira PY2SDR

#include "meterwidget.h"

#include <QPainter>
#include <QPolygon>

#include "moc_meterwidget.cpp"

MeterWidget::MeterWidget(QWidget * parent)
  : QWidget {parent}
  , m_signal {0}
  , m_sigPeak {0}
{
  for ( int i = 0; i < 10; i++ ) {
    signalQueue.enqueue(0);
  }
}

void MeterWidget::setValue(int value)
{
    m_signal = value;
    signalQueue.enqueue(value);
    signalQueue.dequeue();

    // Get signal peak
    int tmp = 0;
    for (int i = 0; i < signalQueue.size(); ++i) {
        if (signalQueue.at(i) > tmp)
            tmp = signalQueue.at(i);
    }
    m_sigPeak = tmp;

    update();
}

QSize MeterWidget::sizeHint () const
{
  return {10, 100};
}

void MeterWidget::paintEvent (QPaintEvent * event)
{
  QWidget::paintEvent (event);

  // Sanitize
  m_signal = m_signal < 0 ? 0 : m_signal;
  m_signal = m_signal > 90 ? 90 : m_signal;

  QPainter p {this};
  p.setPen (Qt::NoPen);

  auto const& target = contentsRect ();
  QRect r {QPoint {target.left (), static_cast<int> (target.top () + target.height () - m_signal / 90. * target.height ())}
    , QPoint {target.right (), target.bottom ()}};
  p.setBrush (QColor {255, 180, 0});
  if (m_signal > 89 || m_signal < 30) {p.setBrush(QColor {255, 150, 150});}

  p.drawRect (r);

  if (m_sigPeak)
    {
      // Draw peak hold indicator
      auto peak = static_cast<int> (target.top () + target.height () - m_sigPeak / 90. * target.height ());
      p.setBrush (Qt::black);
      p.translate (target.left (), peak);
      p.drawPolygon (QPolygon {{{0, -4}, {0, 4}, {target.width (), 0}}});
    }
}
