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
void wav12_(short d2[], short d1[], int* nbytes, short* nbitsam2);
}


#endif // GETFILE_H
