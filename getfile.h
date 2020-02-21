// last time modified by Igor UA3DJY on 20200124

// -*- Mode: C++ -*-
#ifndef GETFILE_H
#define GETFILE_H
#include <QString>
#include <QFile>
#include <QDebug>

void getfile(QString fname, int ntrperiod);
float gran();
//int ptt(int* nport, int* ntx, int* iptt);
int ptt(int nport, int ntx, int* iptt, int* nopen);

extern "C" {
int ptt_(int nport, int ntx, int* iptt, int* nopen);
void wav12_(int d2[], int d1[], int* nbytes, int* nbitsam2);
}


#endif // GETFILE_H
