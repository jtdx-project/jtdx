find_package (Subversion)
if (Subversion_FOUND AND EXISTS "${SOURCE_DIR}/.svn")
  # the FindSubversion.cmake module is part of the standard distribution
  include (FindSubversion)
  # extract working copy information for SOURCE_DIR into MY_XXX variables
  Subversion_WC_INFO (${SOURCE_DIR} MY)
  message ("${MY_WC_INFO}")
  # determine if the working copy has outstanding changes
  execute_process (COMMAND ${Subversion_SVN_EXECUTABLE} status ${SOURCE_DIR}
    OUTPUT_FILE "${OUTPUT_DIR}/svn_status.txt"
    OUTPUT_STRIP_TRAILING_WHITESPACE)
  file (STRINGS "${OUTPUT_DIR}/svn_status.txt" __svn_changes
    REGEX "^[^?].*$"
    )
  if (__svn_changes)
    message (WARNING "Source tree based on revision ${MY_WC_LAST_CHANGED_REV} appears to have local changes")
    set (MY_WC_LAST_CHANGED_REV "${MY_WC_LAST_CHANGED_REV}-dirty")
    foreach (__svn_change ${__svn_changes})
      message (STATUS "${__svn_change}")
    endforeach (__svn_change ${__svn_changes})
  endif (__svn_changes)
  message (STATUS "${SOURCE_DIR} contains a .svn and is revision ${MY_WC_LAST_CHANGED_REV}")
  # write a file with the SVNVERSION define
  file (WRITE "${OUTPUT_DIR}/svnversion.h.txt" "#define SVNVERSION ${MY_WC_LAST_CHANGED_REV}\n")
else (Subversion_FOUND AND EXISTS "${SOURCE_DIR}/.svn")
  # try git-svn
  if (Subversion_FOUND AND EXISTS "${SOURCE_DIR}/.git")
    include (${SOURCE_DIR}/CMake/Modules/FindGitSubversion.cmake)
    # extract working copy information for SOURCE_DIR into MY_XXX variables
    GitSubversion_WC_INFO (${SOURCE_DIR} MY)
    message ("${MY_WC_INFO}")
    # try and determine if the working copy has outstanding changes
    execute_process (COMMAND ${GIT_EXECUTABLE} --git-dir=${SOURCE_DIR}/.git --work-tree=${SOURCE_DIR} svn dcommit --dry-run
      RESULT_VARIABLE __git_svn_status
      OUTPUT_FILE "${OUTPUT_DIR}/svn_status.txt"
      ERROR_QUIET
      OUTPUT_STRIP_TRAILING_WHITESPACE)
    file (STRINGS "${OUTPUT_DIR}/svn_status.txt" __svn_changes
      REGEX "^diff-tree"
      )
    if ((NOT ${__git_svn_status} EQUAL 0) OR __svn_changes)
      message (WARNING "Source tree based on revision ${MY_WC_LAST_CHANGED_REV} appears to have local changes")
      set (MY_WC_LAST_CHANGED_REV "${MY_WC_LAST_CHANGED_REV}-dirty")
    endif ()
    # write a file with the SVNVERSION define
    file (WRITE "${OUTPUT_DIR}/svnversion.h.txt" "#define SVNVERSION r${MY_WC_LAST_CHANGED_REV}\n")
  else (Subversion_FOUND AND EXISTS "${SOURCE_DIR}/.svn")
    file (WRITE "${OUTPUT_DIR}/svnversion.h.txt" "#define SVNVERSION\n")
  endif (Subversion_FOUND AND EXISTS "${SOURCE_DIR}/.git")
endif (Subversion_FOUND AND EXISTS "${SOURCE_DIR}/.svn")

# copy the file to the final header only if the version changes
# reduces needless rebuilds
execute_process (COMMAND ${CMAKE_COMMAND} -E copy_if_different "${OUTPUT_DIR}/svnversion.h.txt" "${OUTPUT_DIR}/svnversion.h")
