#This source code file was last time modified by Igor Chernikov UA3DJY on October 30th, 2016.
#All changes are shown in the patch file coming together with the full JTDX source code.
# Load version number components.
include (${CMAKE_SOURCE_DIR}/Versions.cmake)

# Compute the full version string.
if (WSJTX_RC AND NOT WSJTX_VERSION_IS_RELEASE)
  set (WSJTX_VERSION_PATCH ${WSJTX_VERSION_PATCH}-rc${WSJTX_RC})
elseif (NOT WSJTX_VERSION_IS_RELEASE)
#  set (WSJTX_VERSION_PATCH ${WSJTX_VERSION_PATCH}-devel)
  set (WSJTX_VERSION_PATCH ${WSJTX_VERSION_PATCH})
endif (WSJTX_RC AND NOT WSJTX_VERSION_IS_RELEASE)

set (wsjtx_VERSION ${WSJTX_VERSION_MAJOR}.${WSJTX_VERSION_MINOR}.${WSJTX_VERSION_PATCH})
