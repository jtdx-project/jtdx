include (CMakeParseArguments)

set (_THIS_MODULE_BASE_DIR "${CMAKE_CURRENT_LIST_DIR}")

# generate_product_version() function
#
# This function uses VersionInfo.in template file and VersionResource.rc file
# to generate WIN32 resource with version information and general resource strings.
#
# Usage:
#   generate_product_version(
#     SomeOutputResourceVariable
#     NAME MyVersinedTarget
#     ICON ${PATH_TO_ICON}
#     VERSION_MAJOR 2
#     VERSION_MINOR 3
#     VERSION_PATCH 1
#     VERSION_REVISION sha1
#   )
#
# You can use generated resource for your executable targets:
#   add_executable(target-name ${target-files} ${SomeOutputResourceVariable})
#   add_library (target-name SHARED ${target-files} ${SomeOutputResourceVariable})
#
# You can specify resource strings in arguments:
#   NAME               - name of executable (no defaults, ex: Microsoft Word)
#   BUNDLE             - bundle (${PROJECT_NAME} or ${NAME} is default, ex: Microsoft Office)
#   ICON               - path to application icon, default: ${CMAKE_SOURCE_DIR}/icons/windows-icons/${NAME}.ico
#   VERSION_MAJOR      - default: 1
#   VERSION_MINOR      - deafult: 0
#   VERSION_PATCH      - deafult: 0
#   VERSION_REVISION   - deafult: 0
#   VENDOR_NAME        - your vendor name, default: ${PROJECT_VENDOR}
#   LEGAL_COPYRIGHT    - default: ${PROJECT_COPYRIGHT}
#   COMMENTS           - default: ${PROJECT_DESCRIPTION}
#   ORIGINAL_FILENAME  - default: ${NAME}
#   INTERNAL_NAME      - default: ${NAME}
#   FILE_DESCRIPTION   - default: ${COMMENTS}
function(generate_version_info outfiles)
  set (options)
  set (oneValueArgs
    NAME
    BUNDLE
    ICON
    VERSION_MAJOR
    VERSION_MINOR
    VERSION_PATCH
    VERSION_REVISION
    VENDOR_NAME
    LEGAL_COPYRIGHT
    COMMENTS
    ORIGINAL_FILENAME
    INTERNAL_NAME
    FILE_DESCRIPTION)
  set (multiValueArgs)
  cmake_parse_arguments(PRODUCT "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  if (NOT PRODUCT_BUNDLE)
    if (PROJECT_NAME)
      set (PRODUCT_BUNDLE "${PROJECT_NAME}")
    else ()
      set (PRODUCT_BUNDLE "${PRODUCT_NAME}")
    endif ()
  endif()

  if (NOT PRODUCT_ICON)
    set (PRODUCT_ICON "${CMAKE_SOURCE_DIR}/icons/windows-icons/${PRODUCT_NAME}.ico")
  endif ()

  if (NOT PRODUCT_VERSION_MAJOR)
    if (WSJTX_VERSION_MAJOR)
      set (PRODUCT_VERSION_MAJOR ${WSJTX_VERSION_MAJOR})
    else ()
      set (PRODUCT_VERSION_MAJOR 1)
    endif ()
  else ()
    if (NOT ${PRODUCT_VERSION_MAJOR} MATCHES "^[0-9]+$")
      message (FATAL_ERROR "Numeric major version number required")
    endif ()
  endif ()

  if (NOT PRODUCT_VERSION_MINOR)
    if (WSJTX_VERSION_MINOR)
      set (PRODUCT_VERSION_MINOR ${WSJTX_VERSION_MINOR})
    else ()
      set (PRODUCT_VERSION_MINOR 0)
    endif ()
  else ()
    if (NOT ${PRODUCT_VERSION_MINOR} MATCHES "^[0-9]+$")
      message (FATAL_ERROR "Numeric minor version number required")
    endif ()
  endif()

  if (NOT PRODUCT_VERSION_PATCH)
    if (WSJTX_VERSION_SUB)
      set (PRODUCT_VERSION_PATCH ${WSJTX_VERSION_SUB})
    else ()
      set (PRODUCT_VERSION_PATCH 0)
    endif ()
  else ()
    if (NOT ${PRODUCT_VERSION_PATCH} MATCHES "^[0-9]+$")
      message (FATAL_ERROR "Numeric patch version number required")
    endif ()
  endif()

  if (NOT PRODUCT_VERSION_TWEAK)
    if (WSJTX_VERSION_TWEAK)
      set (PRODUCT_VERSION_TWEAK ${WSJTX_VERSION_TWEAK})
    else ()
      set (PRODUCT_VERSION_TWEAK 0)
    endif ()
  else()
    if (NOT ${PRODUCT_VERSION_TWEAK} MATCHES "^[0-9]+$")
      message (FATAL_ERROR "Numeric tweak version number required")
    endif()
  endif()

  if (NOT PROJECT_VERSION_REVISION AND BUILD_TYPE_REVISION)
    set (PRODUCT_VERSION_REVISION ${BUILD_TYPE_REVISION})
  endif ()

  if (NOT PRODUCT_VENDOR_NAME AND PROJECT_VENDOR)
    set (PRODUCT_VENDOR_NAME ${PROJECT_VENDOR})
  endif ()

  if (NOT PRODUCT_LEGAL_COPYRIGHT)
    if (PROJECT_COPYRIGHT)
      set (PRODUCT_LEGAL_COPYRIGHT ${PROJECT_COPYRIGHT})
    else ()
      string(TIMESTAMP PRODUCT_CURRENT_YEAR "%Y")
      set(PRODUCT_LEGAL_COPYRIGHT "${PRODUCT_VENDOR_NAME} (C) Copyright ${PRODUCT_CURRENT_YEAR}")
    endif ()
  endif()

  if (NOT PRODUCT_COMMENTS)
    if (PROJECT_DESCRIPTION)
      set(PRODUCT_COMMENTS ${PROJECT_DESCRIPTION})
    else ()
      set(PRODUCT_COMMENTS "${PRODUCT_NAME} v${PRODUCT_VERSION_MAJOR}.${PRODUCT_VERSION_MINOR}.${PRODUCT_VERSION_PATCH}")
    endif ()
  endif()

  if (NOT PRODUCT_ORIGINAL_FILENAME)
    set(PRODUCT_ORIGINAL_FILENAME "${PRODUCT_NAME}")
  endif()

  if (NOT PRODUCT_INTERNAL_NAME)
    set(PRODUCT_INTERNAL_NAME "${PRODUCT_NAME}")
  endif()

  if (NOT PRODUCT_FILE_DESCRIPTION)
    set(PRODUCT_FILE_DESCRIPTION "${PRODUCT_COMMENTS}")
  endif()

  set (_VersionInfoFile VersionInfo_${PRODUCT_NAME}.h)
  set (_VersionResourceFile VersionResource_${PRODUCT_NAME}.rc)
  configure_file(
    ${_THIS_MODULE_BASE_DIR}/VersionInfo.h.in
    ${_VersionInfoFile}
    @ONLY)
  configure_file(
    ${_THIS_MODULE_BASE_DIR}/VersionResource.rc.in
    ${_VersionResourceFile}
    @ONLY)
  list(APPEND ${outfiles} ${CMAKE_CURRENT_BINARY_DIR}/${_VersionInfoFile} ${CMAKE_CURRENT_BINARY_DIR}/${_VersionResourceFile})
  set (${outfiles} ${${outfiles}} PARENT_SCOPE)
endfunction()
