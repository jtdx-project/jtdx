// -*- Mode: C++ -*-
#ifndef ABOUTDLG_H
#define ABOUTDLG_H

#include <QDialog>
#include <QScopedPointer>

namespace Ui {
  class CAboutDlg;
}

class CAboutDlg : public QDialog
{
  Q_OBJECT;

public:
	explicit CAboutDlg(QWidget *parent = nullptr);
  ~CAboutDlg ();
  
private:
	QScopedPointer<Ui::CAboutDlg> ui;
};

#endif // ABOUTDLG_H
