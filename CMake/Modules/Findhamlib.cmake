# - Try to find hamlib
#
# Once done, this will define:
#
#  hamlib_FOUND - system has Hamlib
#  hamlib_INCLUDE_DIRS - the Hamlib include directories
#  hamlib_LIBRARIES - link these to use Hamlib
#  hamlib_LIBRARY_DIRS - required shared/dynamic libraries are here
#
# If hamlib_STATIC is TRUE then static linking will be assumed
#

include (LibFindMacros)

set (hamlib_LIBRARY_DIRS)

# pkg-config?
find_path (__hamlib_pc_path NAMES hamlib.pc
  PATH_SUFFIXES lib/pkgconfig lib64/pkgconfig
  )
if (__hamlib_pc_path)
  set (__pc_path $ENV{PKG_CONFIG_PATH})
  list (APPEND __pc_path "${__hamlib_pc_path}")
  set (ENV{PKG_CONFIG_PATH} "${__pc_path}")
  unset (__pc_path CACHE)
endif ()
unset (__hamlib_pc_path CACHE)

# Use pkg-config to get hints about paths, libs and, flags
unset (__pkg_config_checked_hamlib CACHE)
# pkg_config will fail on Windows if the Hamlib USB backends are
# configured since libusb-1.0 does not ship with a pkg_config file on
# Windows, that's OK because we fix it up below
libfind_pkg_check_modules (PC_HAMLIB hamlib)

if (NOT PC_HAMLIB_FOUND)
  # The headers
  find_path (hamlib_INCLUDEDIR hamlib/rig.h)
  # The libraries
  if (hamlib_STATIC)
    libfind_library (hamlib libhamlib.a)
  else ()
    libfind_library (hamlib hamlib)
  endif ()
  if (WIN32)
    set (hamlib_EXTRA_LIBRARIES ws2_32)
  else ()
    set (hamlib_EXTRA_LIBRARIES m dl)
  endif ()

  # libusb-1.0 has no pkg-config file on Windows so we have to find it
  # ourselves
  if (CMAKE_SIZEOF_VOID_P MATCHES "8")
    find_library (LIBUSB NAMES usb-1.0 PATH_SUFFIXES MinGW64/dll)
  else ()
    find_library (LIBUSB NAMES usb-1.0 PATH_SUFFIXES MinGW32/dll)
  endif ()
  if (LIBUSB)
    set (hamlib_EXTRA_LIBRARIES ${LIBUSB} ${hamlib_EXTRA_LIBRARIES})
    get_filename_component (hamlib_libusb_path ${LIBUSB} PATH)
    set (hamlib_LIBRARY_DIRS ${hamlib_LIBRARY_DIRS} ${hamlib_libusb_path})
  endif (LIBUSB)
  set (hamlib_PROCESS_INCLUDES hamlib_INCLUDEDIR)
  set (hamlib_PROCESS_LIBS hamlib_LIBRARY hamlib_EXTRA_LIBRARIES)
else ()
  if (hamlib_STATIC)
    set (hamlib_PROCESS_INCLUDES PC_HAMLIB_STATIC_INCLUDE_DIRS)
    set (hamlib_PROCESS_LIBS PC_HAMLIB_STATIC_LDFLAGS)
    set (hamlib_LIBRARY_DIRS ${PC_HAMLIB_STATIC_LIBRARY_DIRS})
  else ()
    set (hamlib_PROCESS_INCLUDES PC_HAMLIB_INCLUDE_DIRS)
    set (hamlib_PROCESS_LIBS PC_HAMLIB_LDFLAGS)
    set (hamlib_LIBRARY_DIRS ${PC_HAMLIB_LIBRARY_DIRS})
  endif ()
endif ()
libfind_process (hamlib)

if (WIN32)
  find_path (hamlib_dll_path libhamlib-2.dll)
  if (hamlib_dll_path)
    set (hamlib_LIBRARY_DIRS ${hamlib_LIBRARY_DIRS} ${hamlib_dll_path})
  endif ()
endif ()

# Handle the  QUIETLY and REQUIRED  arguments and set  HAMLIB_FOUND to
# TRUE if all listed variables are TRUE
include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (hamlib DEFAULT_MSG hamlib_INCLUDE_DIRS hamlib_LIBRARIES hamlib_LIBRARY_DIRS)
