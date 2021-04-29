message (STATUS "Checking for revision information")
file (WRITE "${OUTPUT_DIR}/scs_version.h.txt" "/* SCS version information */\n\n")
if (EXISTS "${SOURCE_DIR}/.svn")
  message (STATUS "Checking for Subversion revision information")
  find_package (Subversion QUIET REQUIRED)
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
    message (STATUS "Source tree based on revision ${MY_WC_LAST_CHANGED_REV} appears to have local changes")
    set (MY_WC_LAST_CHANGED_REV "${MY_WC_LAST_CHANGED_REV}-dirty")
    file (APPEND "${OUTPUT_DIR}/scs_version.h.txt" "#define SCS_VERSION_IS_DIRTY 1\n")
    foreach (__svn_change ${__svn_changes})
      message (STATUS "${__svn_change}")
    endforeach (__svn_change ${__svn_changes})
  endif (__svn_changes)
  message (STATUS "${SOURCE_DIR} contains a .svn and is revision ${MY_WC_LAST_CHANGED_REV}")
  # write a file with the SCS_VERSION define
  file (APPEND "${OUTPUT_DIR}/scs_version.h.txt" "#define SCS_VERSION r${MY_WC_LAST_CHANGED_REV}\n#define SCS_VERSION_STR \"r${MY_WC_LAST_CHANGED_REV}\"\n")
elseif (EXISTS "${SOURCE_DIR}/.git")
  if (EXISTS "${SOURCE_DIR}/.git/svn/.metadata")  # try git-svn
    message (STATUS "Checking for Subversion revision information using git-svn")
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
      message (STATUS "Source tree based on revision ${MY_WC_LAST_CHANGED_REV} appears to have local changes")
      set (MY_WC_LAST_CHANGED_REV "${MY_WC_LAST_CHANGED_REV}-dirty")
      file (APPEND "${OUTPUT_DIR}/scs_version.h.txt" "#define SCS_VERSION_IS_DIRTY 1\n")
    endif ()
    # write a file with the SVNVERSION define
    file (APPEND "${OUTPUT_DIR}/scs_version.h.txt" "#define SCS_VERSION r${MY_WC_LAST_CHANGED_REV}\n#define SCS_VERSION_STR \"r${MY_WC_LAST_CHANGED_REV}\"\n")
  else ()
    #
    # try git
    #
    message (STATUS "Checking for gitrevision information")
    include (${SOURCE_DIR}/CMake/Modules/GetGitRevisionDescription.cmake)
    get_git_head_revision (${SOURCE_DIR} GIT_REFSPEC GIT_SHA1)
    git_local_changes (${SOURCE_DIR} GIT_DIRTY)
    string (SUBSTRING "${GIT_SHA1}" 0 6 GIT_SHA1)
    if ("${GIT_DIRTY}" STREQUAL "DIRTY")
      message (STATUS "Source tree based on revision ${GIT_REFSPEC} ${GIT_SHA1} appears to have local changes")
      set (GIT_SHA1 "${GIT_SHA1}-dirty")
      file (APPEND "${OUTPUT_DIR}/scs_version.h.txt" "#define SCS_VERSION_IS_DIRTY 1\n")
      execute_process (COMMAND ${GIT_EXECUTABLE} --git-dir=${SOURCE_DIR}/.git --work-tree=${SOURCE_DIR} status
	ERROR_QUIET
	OUTPUT_STRIP_TRAILING_WHITESPACE)
    endif ()
    message (STATUS "refspec: ${GIT_REFSPEC} - SHA1: ${GIT_SHA1}")
    file (APPEND "${OUTPUT_DIR}/scs_version.h.txt" "#define SCS_VERSION ${GIT_SHA1}\n#define SCS_VERSION_STR \"${GIT_SHA1}\"\n")
  endif ()
else()
  message (STATUS "No SCS found")
  file (APPEND "${OUTPUT_DIR}/scs_version.h.txt" "#define SCS_VERSION 000000\n#define SCS_VERSION_STR \"000000\"\n")
endif ()

# copy the file to the final header only if the version changes
# reduces needless rebuilds
execute_process (COMMAND ${CMAKE_COMMAND} -E copy_if_different "${OUTPUT_DIR}/scs_version.h.txt" "${OUTPUT_DIR}/scs_version.h")
