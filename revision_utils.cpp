// This source code file was last time modified by Igor UA3DJY on May 5th, 2017
// All changes are shown in the patch file coming together with the full JTDX source code.

#include "revision_utils.hpp"

#include <cstring>

#include <QCoreApplication>
#include <QRegularExpression>

#include "svnversion.h"

namespace
{
  QString revision_extract_number (QString const& s)
  {
    QString revision;

    // try and match a number
    QRegularExpression re {R"(^[$:]\w+: (\d+[^$]*)\$$)"};
    auto match = re.match (s);
    if (match.hasMatch ())
      {
        revision = 'r' + match.captured (1);
      }
    return revision;
  }
}

QString revision (QString const& svn_rev_string)
{
  QString result;
  auto revision_from_svn = revision_extract_number (svn_rev_string);

#if defined (CMAKE_BUILD)
  QString svn_info {":Rev: " WSJTX_STRINGIZE (SVNVERSION) " $"};

  auto revision_from_svn_info = revision_extract_number (svn_info);
  if (!revision_from_svn_info.isEmpty ())
    {
      // we managed to get the revision number from svn info etc.
      result = revision_from_svn_info;
    }
  else if (!revision_from_svn.isEmpty ())
    {
      // fall back to revision passed in if any
      result = revision_from_svn;
    }
  else
    {
      // match anything
      QRegularExpression re {R"(^[$:]\w+: ([^$]*)\$$)"};
      auto match = re.match (svn_info);
      if (match.hasMatch ())
        {
          result = match.captured (1);
        }
    }
#else
  if (!revision_from_svn.isEmpty ())
    {
      // not CMake build so all we have is revision passed
      result = revision_from_svn;
    }
#endif
  return result.trimmed ();
}

QString version (bool include_patch)
{
#if defined (CMAKE_BUILD)
  QString v {WSJTX_STRINGIZE (WSJTX_VERSION_MAJOR) "." WSJTX_STRINGIZE (WSJTX_VERSION_MINOR)};
  if (include_patch)
    {
      v += "." WSJTX_STRINGIZE (WSJTX_VERSION_PATCH)
# if defined (WSJTX_RC)
        + "-rc" WSJTX_STRINGIZE (WSJTX_RC)
# endif
        ;
    }
#else
  QString v {"Not for Release"};
#endif
  return v;
}

QString program_title (QString const& revision)
{
  QString id {QCoreApplication::applicationName () + "   v" + QCoreApplication::applicationVersion ()};
//  return /*id + " " + revision + */" JTDX v15.6 HF evaluation version by UA3DJY";
  return id + /*" " + revision + */"   JTDX v17.9    WSJT-X v1.7 based HF version by UA3DJY";
}
