# - Extract information from a git-svn working copy
# The module defines the following variables:
#
# If the command line client executable is found two macros are defined:
# GitSubversion_WC_INFO(<dir> <var-prefix>)
# GitSubversion_WC_INFO extracts information of a subversion working copy at
# a given location. This macro defines the following variables:
#  <var-prefix>_WC_URL - url of the repository (at <dir>)
#  <var-prefix>_WC_ROOT - root url of the repository
#  <var-prefix>_WC_REVISION - current revision
#  <var-prefix>_WC_LAST_CHANGED_AUTHOR - author of last commit
#  <var-prefix>_WC_LAST_CHANGED_DATE - date of last commit
#  <var-prefix>_WC_LAST_CHANGED_REV - revision of last commit
#  <var-prefix>_WC_INFO - output of command `svn info <dir>'
# Example usage:
#  find_package(Subversion)
#  if(SUBVERSION_FOUND)
#    GitSubversion_WC_INFO(${PROJECT_SOURCE_DIR} Project)
#    message("Current revision is ${Project_WC_REVISION}")
#  endif()

find_package (Git)

if(GIT_FOUND)
  # the git-svn commands should be executed with the C locale, otherwise
  # the message (which are parsed) may be translated, Alex
  set(_Subversion_SAVED_LC_ALL "$ENV{LC_ALL}")
  set(ENV{LC_ALL} C)

  # execute_process(COMMAND ${Subversion_SVN_EXECUTABLE} --version
  #   OUTPUT_VARIABLE Subversion_VERSION_SVN
  #   OUTPUT_STRIP_TRAILING_WHITESPACE)

  # restore the previous LC_ALL
  set(ENV{LC_ALL} ${_Subversion_SAVED_LC_ALL})

  # string(REGEX REPLACE "^(.*\n)?svn, version ([.0-9]+).*"
  #   "\\2" Subversion_VERSION_SVN "${Subversion_VERSION_SVN}")

  macro(GitSubversion_WC_INFO dir prefix)
    # the subversion commands should be executed with the C locale, otherwise
    # the message (which are parsed) may be translated, Alex
    set(_Subversion_SAVED_LC_ALL "$ENV{LC_ALL}")
    set(ENV{LC_ALL} C)

    execute_process(COMMAND ${GIT_EXECUTABLE} --git-dir=${dir}/.git svn info
      OUTPUT_VARIABLE ${prefix}_WC_INFO
      ERROR_VARIABLE Git_git_svn_info_error
      RESULT_VARIABLE Git_git_svn_info_result
      OUTPUT_STRIP_TRAILING_WHITESPACE)

    if(NOT ${Git_git_svn_info_result} EQUAL 0)
      message(SEND_ERROR "Command \"${GIT_EXECUTABLE} --git-dir=${dir}/.git svn info\" failed with output:\n${Git_git_svn_info_error}")
    else()

      string(REGEX REPLACE "^(.*\n)?URL: ([^\n]+).*"
        "\\2" ${prefix}_WC_URL "${${prefix}_WC_INFO}")
      string(REGEX REPLACE "^(.*\n)?Repository Root: ([^\n]+).*"
        "\\2" ${prefix}_WC_ROOT "${${prefix}_WC_INFO}")
      string(REGEX REPLACE "^(.*\n)?Revision: ([^\n]+).*"
        "\\2" ${prefix}_WC_REVISION "${${prefix}_WC_INFO}")
      string(REGEX REPLACE "^(.*\n)?Last Changed Author: ([^\n]+).*"
        "\\2" ${prefix}_WC_LAST_CHANGED_AUTHOR "${${prefix}_WC_INFO}")
      string(REGEX REPLACE "^(.*\n)?Last Changed Rev: ([^\n]+).*"
        "\\2" ${prefix}_WC_LAST_CHANGED_REV "${${prefix}_WC_INFO}")
      string(REGEX REPLACE "^(.*\n)?Last Changed Date: ([^\n]+).*"
        "\\2" ${prefix}_WC_LAST_CHANGED_DATE "${${prefix}_WC_INFO}")
    endif()

    # restore the previous LC_ALL
    set(ENV{LC_ALL} ${_Subversion_SAVED_LC_ALL})

  endmacro()

endif()
