// -*- Mode: C++ -*-
#ifndef SLEEP_H
#define SLEEP_H
#include <qthread.h>

class Sleep : public QThread
{
public:
  static void msleep(int ms) {
    QThread::msleep(ms);
  }
  static int idealThreadCount() {
    return QThread::idealThreadCount();
  }
};

#endif // SLEEP_H
