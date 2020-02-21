set (ASCIIDOC_MANS
  man1/wsjtx.1.txt
  man1/wsprd.1.txt
  man1/jt65code.1.txt
  man1/rigctl-wsjtx.1.txt
  man1/rigctld-wsjtx.1.txt
  man1/message_aggregator.1.txt
  man1/udp_daemon.1.txt
  )

find_program (A2X_EXECUTABLE NAMES a2x a2x.py)
if (NOT A2X_EXECUTABLE)
  if (NOT CMAKE_BUILD_TYPE STREQUAL "Debug")
    message (SEND_ERROR "Failed to find a2x which is an optional requirement for non-debug
builds on *nix platforms.

You can choose to skip manpage generation and this error by setting
the CMake option WSJT_SKIP_MANPAGES to ON. This option is designed for
those that are building for their own use, package builders should not
opt to skip manpage generation since package building requires the
manpages.")

  else (NOT CMAKE_BUILD_TYPE STREQUAL "Debug")
    message (WARNING "Failed to find a2x skipping manpage generation.")
  endif (NOT CMAKE_BUILD_TYPE STREQUAL "Debug")
endif ()
find_program (GZIP_EXECUTABLE NAMES gzip)
find_program (SED_EXECUTABLE NAMES sed)

set (XSLTPROC_OPTS 
  "-param man.endnotes.list.enabled 0 -param man.endnotes.are.numbered 0"
  )
set (A2X_OPTS
  --format=manpage
  --xsltproc-opts=${XSLTPROC_OPTS}
  --doctype=manpage
  --no-xmllint
  -a VERSION=${wsjtx_VERSION}
  )

set (MANPAGES)
if (A2X_EXECUTABLE AND GZIP_EXECUTABLE AND SED_EXECUTABLE)
  foreach (f IN LISTS ASCIIDOC_MANS)
    get_filename_component (d "${f}" PATH)
    string (SUBSTRING "${d}" 3 -1 section)
    if (NOT section MATCHES "[1-9]")
      message (SEND_ERROR "Invalid man section ${section} in ${f}")
    endif (NOT section MATCHES "[1-9]")
    get_filename_component (filename "${f}" NAME)
    get_filename_component (filename_we "${f}" NAME_WE)
    set (f "${CMAKE_CURRENT_SOURCE_DIR}/${f}")
    set (d "man/${d}")
    set (o "${d}/${filename_we}.${section}.gz")
    add_custom_command (OUTPUT "${o}"
      COMMAND ${CMAKE_COMMAND} ARGS -E make_directory "${d}"
      COMMAND ${CMAKE_COMMAND} ARGS -E copy "${f}" "${d}"
      COMMAND ${A2X_EXECUTABLE} ARGS ${A2X_OPTS} "${d}/${filename}"
      COMMAND ${CMAKE_COMMAND} ARGS -E remove "${d}/${filename}"
#
# the following edit command is used to make the alias manpages
# compatible with Debian packaging rules when a2x generates alias
# pages baseed in the section directory instead of the man parent
# directory
#
      COMMAND ${SED_EXECUTABLE} ARGS -e '/^.so [^\\/]*$$/s@.so @&man${section}\\/@' -i.orig "${d}/*.${section}"
      COMMAND ${GZIP_EXECUTABLE} ARGS -f9 "${d}/*.${section}"
      DEPENDS "${f}" "${previous_output}"
      COMMENT "Generating ${o}"
      )
    list (APPEND MANPAGES "${o}")

    # use this as a dependency in the next iteration to serialize so
    # that gzips don't overlap in parallel builds
    set (previous_output "${o}")
  endforeach ()

  install (
    DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/man
    DESTINATION ${WSJT_MANPAGE_DESTINATION}
    PATTERN "*.orig" EXCLUDE
    #COMPONENT Runtime
    )
endif (A2X_EXECUTABLE AND GZIP_EXECUTABLE AND SED_EXECUTABLE)

add_custom_target (manpages DEPENDS ${MANPAGES})
