#ifndef NON_INHERITING_PROCESS_HPP__
#define NON_INHERITING_PROCESS_HPP__

#include <QProcess>
#include "pimpl_h.hpp"

class QObject;

//
// class NonInheritingProcess - Manage a process without it inheriting
//                              all inheritable handles
//
//   On MS  Windows QProcess  creates sub-processes which  inherit all
// inheritable  handles, and  handles  on Windows  are inheritable  by
// default. This can cause the  lifetime of objects to be unexpectedly
// extended, which in turn can cause unexpected errors. The motivation
// for this class  was implementing log file rotation  using the Boost
// log library.  The  current log file's handle gets  inherited by any
// long  running sub-process  started by  QProcess and  that causes  a
// sharing  violation  when  attempting  to rename  the  log  file  on
// rotation, even though  the log library closes the  current log file
// before trying to rename it.
//
class NonInheritingProcess
  : public QProcess
{
public:
  NonInheritingProcess (QObject * parent = nullptr);
  ~NonInheritingProcess ();

private:
  class impl;
  pimpl<impl> m_;
};
#endif
