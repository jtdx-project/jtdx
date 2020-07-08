// Simple bargraph dB meter
// Implemented by Edson Pereira PY2SDR
//

#include "signalmeter.h"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QLabel>
#include <QPainter>
#include <QFontMetrics>
#include <QDebug>

#include <meterwidget.h>

#include "moc_signalmeter.cpp"

class Scale final
  : public QWidget
{
public:
  explicit Scale (QWidget * parent = 0)
    : QWidget {parent}
  {
    setSizePolicy (QSizePolicy::Minimum, QSizePolicy::MinimumExpanding);
  }

  QSize sizeHint () const override
  {
    return minimumSizeHint ();
  }

  QSize minimumSizeHint () const override
  {
    QFontMetrics font_metrics {font (), nullptr};
//    return {tick_length + text_indent + font_metrics.width ("00+"), (font_metrics.height () + line_spacing) * range};
    return {tick_length + text_indent + font_metrics.boundingRect ("000000").width (), (font_metrics.height () + line_spacing) * range};
  }

protected:
  void paintEvent (QPaintEvent * event) override
  {
    QWidget::paintEvent (event);

    QPainter p {this};
    auto const& target = contentsRect ();
    QFontMetrics font_metrics {p.font (), this};
    auto font_offset = font_metrics.ascent () / 2;
    p.drawLine (target.left (), target.top () + font_offset, target.left (), target.bottom () - font_offset - font_metrics.descent ());
    for (int i = 0; i <= range; ++i)
      {
        p.save ();
        p.translate (target.left ()
                     , target.top () + font_offset + i * (target.height () - font_metrics.ascent () - font_metrics.descent ()) / range);
        p.drawLine (0, 0, tick_length, 0);
        auto text = i ? QString::number ((range - i) * scale) : QString {"%1%2"}.arg ((range - i) * scale).arg ('+');
        p.drawText (tick_length + text_indent, font_offset, text);
        p.restore ();
      }
  }

private:
//  static int const tick_length {3};
  static int const tick_length {4};
//  static int const text_indent {2};
  static int const text_indent {1};
  static int const line_spacing {0};
  static int const range {9};
  static int const scale {10};
};

SignalMeter::SignalMeter (QWidget * parent)
  : QFrame {parent}
{
  auto outer_layout = new QVBoxLayout;
  outer_layout->setSpacing (0);

  auto inner_layout = new QHBoxLayout;
  inner_layout->setContentsMargins (9, 0, 9, 0);
  inner_layout->setSpacing (0);

  m_meter = new MeterWidget;
  m_meter->setSizePolicy (QSizePolicy::Minimum, QSizePolicy::Minimum);
  inner_layout->addWidget (m_meter);

  m_scale = new Scale;
  inner_layout->addWidget (m_scale);

  m_reading = new QLabel(this);

  outer_layout->addLayout (inner_layout);
  outer_layout->addWidget (m_reading);
  setLayout (outer_layout);
}

void SignalMeter::setValue(float value)
{
  if(value<0) value=0;
  QFontMetrics font_metrics {m_scale->font (), nullptr};
  m_meter->setContentsMargins (0, font_metrics.ascent () / 2, 0, font_metrics.ascent () / 2 + font_metrics.descent ());
  m_meter->setValue(int(value));
  QString t;
//  t = QString::asprintf("%4.1f dB",value);
  t = QString::asprintf("%2.0fdB",value);
  m_reading->setText(t);
}
