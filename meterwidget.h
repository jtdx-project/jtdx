// -*- Mode: C++ -*-
#ifndef METERWIDGET_H
#define METERWIDGET_H

#include <QWidget>
#include <QQueue>

class MeterWidget : public QWidget
{
  Q_OBJECT
  Q_PROPERTY (int value READ value WRITE setValue)

public:
  explicit MeterWidget (QWidget *parent = 0);

  // value property
  int value () const {return m_signal;}
  Q_SLOT void setValue (int value);

  // QWidget implementation
  QSize sizeHint () const override;
protected:
  void paintEvent( QPaintEvent * ) override;

private:
  QQueue<int> signalQueue;
  int m_signal;
  int m_sigPeak;
};

#endif // METERWIDGET_H
