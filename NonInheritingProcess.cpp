#include "NonInheritingProcess.hpp"

#ifdef Q_OS_WIN
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif
#ifndef _UNICODE
#define _UNICODE
#endif
#ifdef _WIN32_WINNT
#undef _WIN32_WINNT
#endif
#define _WIN32_WINNT 0x0601
#include <windows.h>
#endif

#include <memory>
#include <functional>

#include "pimpl_impl.hpp"

namespace
{
#ifdef Q_OS_WIN
  struct start_info_deleter
  {
    void operator () (STARTUPINFOEXW * si)
    {
      if (si->lpAttributeList)
        {
          ::DeleteProcThreadAttributeList (si->lpAttributeList);
        }
      delete si;
    }
  };
#endif
}

class NonInheritingProcess::impl
{
public:
#ifdef Q_OS_WIN
  void extend_CreateProcessArguments (QProcess::CreateProcessArguments * args)
  {
    // 
    //   Here we modify the  CreateProcessArguments structure to use a
    // STARTUPINFOEX extended  argument to  CreateProcess. In  that we
    // set up  a list of  handles for the  new process to  inherit. By
    // doing   this  we   stop  all   inherited  handles   from  being
    // inherited. Unfortunately UpdateProcThreadAttribute does not let
    // us set  up an empty handle  list, so we populate  the list with
    // the three standard stream  handles that QProcess::start has set
    // up  as Pipes  to do  IPC. Even  though these  Pipe handles  are
    // created with inheritance disabled, UpdateProcThreadAtribute and
    // CreateProcess don't seem to mind, which suits us fine.
    //
    // Note: that we cannot just clear the inheritHandles flag as that
    // stops the standard stream  handles being inherited which breaks
    // our   IPC    using   std(in|out|err).    Only   be    using   a
    // PROC_THREAD_ATTRIBUTE_HANDLE_LIST attribute  in a STARTUPINFOEX
    // structure  can  we  avoid  the  all  or  nothing  behaviour  of
    // CreateProcess /w respect to handle inheritance.
    // 
    BOOL fSuccess;
    SIZE_T size {0};
    LPPROC_THREAD_ATTRIBUTE_LIST lpAttributeList = nullptr;
    ::InitializeProcThreadAttributeList (nullptr, 1, 0, &size);
    lpAttributeList = reinterpret_cast<LPPROC_THREAD_ATTRIBUTE_LIST> (::HeapAlloc (::GetProcessHeap (), 0, size));
    fSuccess = !!lpAttributeList;
    if (fSuccess)
      {
        fSuccess = ::InitializeProcThreadAttributeList (lpAttributeList, 1, 0, &size);
      }
    if (fSuccess)
      {
        // empty list of handles
        fSuccess = ::UpdateProcThreadAttribute (lpAttributeList, 0,
                                                PROC_THREAD_ATTRIBUTE_HANDLE_LIST,
                                                &args->startupInfo->hStdInput, 3 * sizeof (HANDLE),
                                                nullptr, 0);
      }
    if (fSuccess)
      {
        start_info_.reset (new STARTUPINFOEXW);
        start_info_->StartupInfo = *args->startupInfo;
        start_info_->StartupInfo.cb = sizeof (STARTUPINFOEXW);
        start_info_->lpAttributeList = lpAttributeList;
        args->startupInfo = reinterpret_cast<Q_STARTUPINFO*> (start_info_.get ());
        args->flags |= EXTENDED_STARTUPINFO_PRESENT;
      }
  }

  using start_info_type = std::unique_ptr<STARTUPINFOEXW, start_info_deleter>;
  start_info_type start_info_;
#endif
};

NonInheritingProcess::NonInheritingProcess (QObject * parent)
  : QProcess {parent}
{
#ifdef Q_OS_WIN
  using namespace std::placeholders;

  // enable cleanup after process starts or fails to start
  connect (this, &QProcess::started, [this] {m_->start_info_.reset ();});
  connect (this, &QProcess::errorOccurred, [this] (QProcess::ProcessError) {m_->start_info_.reset ();});
  setCreateProcessArgumentsModifier (std::bind (&NonInheritingProcess::impl::extend_CreateProcessArguments, &*m_, _1));
#endif
}

NonInheritingProcess::~NonInheritingProcess ()
{
}
