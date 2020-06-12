cmake_minimum_required (VERSION 2.8.10 FATAL_ERROR)

if (APPLE)
  #
  # The following variables define the portability and compatability attributes of the Mac OS X build
  # they are choosen with care and should not be changed without good cause.
  #
  # Among other things these options are chosen to match the portability and compatability options of the
  # Qt framework dylibs which can be checked as follows:
  #
  # otool -l <binary> | grep -A3 LC_VERSION_MIN_MACOSX
  #
  set (CMAKE_OSX_DEPLOYMENT_TARGET 10.11
    CACHE STRING "Earliest version of OS X supported

Earliest version we can support with C++11 & libc++ is 10.9.
Do not override this if you intend to build an official deployable installer.")
  set (CMAKE_OSX_SYSROOT /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk
    CACHE STRING "Mac OS X SDK to build with

Change this to the newest SDK available that you can install on your system (10.13 preferred).
Do not override this if you intend to build an official deployable installer.")
endif (APPLE)

project (jtdx C CXX Fortran)

#
# CMake policies
#
if (POLICY CMP0020)
  cmake_policy (SET CMP0020 NEW) # link to Qt winmain on Windows
endif (POLICY CMP0020)

if (POLICY CMP0043)
  cmake_policy (SET CMP0043 NEW) # ignore COMPILE_DEFINITIONS_<CONFIG>
endif (POLICY CMP0043)

if (POLICY CMP0063)
  cmake_policy (SET CMP0063 NEW) # honour visibility properties for all library types
endif (POLICY CMP0063)

if (POLICY CMP0071)
  cmake_policy (SET CMP0071 NEW) # run automoc and autouic on generated sources
endif (POLICY CMP0071)

include (${PROJECT_SOURCE_DIR}/CMake/VersionCompute.cmake)
message (STATUS "Building ${CMAKE_PROJECT_NAME}-${wsjtx_VERSION}")

#
# project information
#
set (PROJECT_NAME "JTDX")
set (PROJECT_VENDOR "Igor Chernikov, UA3DJY")
set (PROJECT_CONTACT "Igor Chernikov <c_igor@inbox.ru>")
set (PROJECT_COPYRIGHT "Copyright (C) 2016-2020 by Igor Chernikov, UA3DJY and Arvo Järve, ES1JA.")
set (PROJECT_HOMEPAGE http://jtdx.tech/)
set (PROJECT_MANUAL index.php/videos-guides)
set (PROJECT_MANUAL_DIRECTORY_URL http://jtdx.tech/)
set (PROJECT_SAMPLES_URL http://jtdx.tech/public_html/downloads/)
set (PROJECT_SAMPLES_UPLOAD_DEST frs.sourceforge.net:/home/frs/project/wsjt/)
set (PROJECT_SUMMARY_DESCRIPTION "${PROJECT_NAME} - JT9 and JT65 Modes for LF, MF and HF Amateur Radio.")
set (PROJECT_DESCRIPTION "${PROJECT_SUMMARY_DESCRIPTION}
 ${PROJECT_NAME} implements JT9, a new mode designed especially for the LF, MF,
 and HF bands, as well as the popular mode JT65. Both modes were
 designed for making reliable, confirmed QSOs under extreme
 weak-signal conditions. They use nearly identical message structure
 and source encoding. JT65 was designed for EME (“moonbounce”) on the
 VHF/UHF bands and has also proved very effective for worldwide QRP
 communication at HF; in contrast, JT9 is optimized for HF and lower
 frequencies. JT9 is about 2 dB more sensitive than JT65A while using
 less than 10% of the bandwidth. World-wide QSOs are possible with
 power levels of a few watts and compromise antennas. A 2 kHz slice
 of spectrum is essentially full when occupied by ten JT65 signals.
 As many as 100 JT9 signals can fit into the same space, without
 overlap.
 .
 ${PROJECT_NAME} offers a “bi-lingual“ operating mode in which you can transmit
 and receive JT65 and JT9 signals, switching between modes
 automatically as needed. Displayed bandwidth can be as large as 5
 kHz. If your receiver has as upper-sideband filter at least 4 kHz
 wide, you can have all the typical JT65 and JT9 activity on screen at
 once, available for making QSOs with a click of the mouse. Even with
 standard SSB-width IF filters, switching between JT65 and JT9 modes
 is quick and convenient. Be sure to read the online ${PROJECT_NAME} User's
 Guide.")

set (CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/CMake/Modules ${CMAKE_MODULE_PATH})

# make sure that the default configuration is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
    "Choose the type of build, options are: None Debug Release."
    FORCE)
endif (NOT CMAKE_BUILD_TYPE)
if (CMAKE_BUILD_TYPE MATCHES "[Dd][Ee][Bb][Uu][Gg]")
  set (is_debug_build 1)
endif ()


#
# Options & features
#
#	Some of these directly effect compilation by being defined in
#	wsjtx_config.h.in which makes them available to the C/C++
#	pre-processor.
#
include (CMakeDependentOption)

# Allow the developer to select if Dynamic or Static libraries are built
OPTION (BUILD_SHARED_LIBS "Build Shared Libraries" OFF)
# Set the LIB_TYPE variable to STATIC
SET (LIB_TYPE STATIC)
if (BUILD_SHARED_LIBS)
  # User wants to build Dynamic Libraries, so change the LIB_TYPE variable to CMake keyword 'SHARED'
  set (LIB_TYPE SHARED)
endif (BUILD_SHARED_LIBS)

option (UPDATE_TRANSLATIONS "Update source translation translations/*.ts
files (WARNING: make clean will delete the source .ts files! Danger!)")
option (WSJT_SHARED_RUNTIME "Debugging option that allows running from a shared Cloud directory.")
option (WSJT_QDEBUG_TO_FILE "Redirect Qt debuging messages to a trace file.")
option (WSJT_TRACE_CAT "Debugging option that turns on CAT diagnostics.")
option (WSJT_TRACE_CAT_POLLS "Debugging option that turns on CAT diagnostics during polling.")
option (WSJT_HAMLIB_TRACE "Debugging option that turns on minimal Hamlib internal diagnostics.")
option (WSJT_SOFT_KEYING "Apply a ramp to CW keying envelope to reduce transients." ON)
option (WSJT_SKIP_MANPAGES "Skip *nix manpage generation.")
option (WSJT_GENERATE_DOCS "Generate documentation files." ON)
option (WSJT_RIG_NONE_CAN_SPLIT "Allow split operation with \"None\" as rig.")
option (WSJT_TRACE_UDP "Debugging option that turns on UDP message protocol diagnostics.")
option (WSJT_BUILD_UTILS "Build simulators and code demonstrators." ON)
option (JTDX_DEBUG_TO_FILE "Write debu to jtdx_debug.txt.")

CMAKE_DEPENDENT_OPTION (WSJT_HAMLIB_VERBOSE_TRACE "Debugging option that turns on full Hamlib internal diagnostics." OFF WSJT_HAMLIB_TRACE OFF)
CMAKE_DEPENDENT_OPTION (WSJT_QDEBUG_IN_RELEASE "Leave Qt debugging statements in Release configuration." OFF
  "NOT is_debug_build" OFF)
CMAKE_DEPENDENT_OPTION (WSJT_ENABLE_EXPERIMENTAL_FEATURES "Enable features not fully ready for public releases." ON
  is_debug_build OFF)
CMAKE_DEPENDENT_OPTION (WSJT_CREATE_WINMAIN
  "The wsjtx target is normally built as GUI executable with a WinMain entry point on Windows,
if you want a console application instead then set this option to OFF.

If you just want to see the debug output from the application then the easiest way is to
attach a debugger which will then receive the console output inside its console." ON
  "WIN32" OFF)

set (PROJECT_ARCHITECTURE "${CMAKE_SYSTEM_PROCESSOR}")
if (NOT PROJECT_ARCHITECTURE)
  # This is supposed to happen already on Windows
  if (CMAKE_SIZEOF_VOID_P MATCHES 8)
    set (PROJECT_ARCHITECTURE "x64")
  else ()
    set (PROJECT_ARCHITECTURE "$ENV{PROCESSOR_ARCHITECTURE}")
  endif ()
endif (NOT PROJECT_ARCHITECTURE)
message (STATUS "******************************************************")
message (STATUS "Building for for: ${CMAKE_SYSTEM_NAME}-${PROJECT_ARCHITECTURE}")
message (STATUS "******************************************************")


#
# install locations
#

if (APPLE)
  set (CMAKE_INSTALL_BINDIR ${CMAKE_PROJECT_NAME}.app/Contents/MacOS)
  set (CMAKE_INSTALL_DATAROOTDIR ${CMAKE_PROJECT_NAME}.app/Contents/Resources)
endif ()

include (GNUInstallDirs)

set (PLUGIN_DESTINATION ${CMAKE_INSTALL_LIBDIR}/plugins)
set (QT_CONF_DESTINATION ${CMAKE_INSTALL_BINDIR})
if (WIN32)
  set (PLUGIN_DESTINATION plugins)
elseif (APPLE)
  set (PLUGIN_DESTINATION ${CMAKE_INSTALL_BINDIR}/../PlugIns)
  set (QT_CONF_DESTINATION ${CMAKE_INSTALL_DATAROOTDIR})
endif ()

set (WSJT_PLUGIN_DESTINATION ${PLUGIN_DESTINATION} CACHE PATH "Path for plugins")
set (WSJT_QT_CONF_DESTINATION ${QT_CONF_DESTINATION} CACHE PATH "Path for the qt.conf file")


#
# Project sources
#
set (wsjt_qt_CXXSRCS
  qt_helpers.cpp
  MetaDataRegistry.cpp
  NetworkServerLookup.cpp
  revision_utils.cpp
  WFPalette.cpp
  Radio.cpp
  IARURegions.cpp
  Bands.cpp
  Modes.cpp
  FrequencyList.cpp
  StationList.cpp
  FrequencyLineEdit.cpp
  FrequencyDeltaLineEdit.cpp
  CandidateKeyFilter.cpp
  ForeignKeyDelegate.cpp
  LiveFrequencyValidator.cpp
  GetUserId.cpp
  TraceFile.cpp
  AudioDevice.cpp
  Transceiver.cpp
  TransceiverBase.cpp
  EmulateSplitTransceiver.cpp
  TransceiverFactory.cpp
  PollingTransceiver.cpp
  HamlibTransceiver.cpp
  HRDTransceiver.cpp
  DXLabSuiteCommanderTransceiver.cpp
  NetworkMessage.cpp
  MessageClient.cpp
  LettersSpinBox.cpp
  HelpTextWindow.cpp
  SampleDownloader.cpp
  SampleDownloader/DirectoryDelegate.cpp
  SampleDownloader/Directory.cpp
  SampleDownloader/FileNode.cpp
  SampleDownloader/RemoteFile.cpp
  DisplayManual.cpp
  FrequencyDelegate.cpp
  FrequencyDeltaDelegate.cpp
  )

set (wsjt_qtmm_CXXSRCS
  Audio/BWFFile.cpp
  )

set (jt9_CXXSRCS
  lib/ipcomm.cpp
  )

set (wsjtx_CXXSRCS
  logbook/adif.cpp
  logbook/countrydat.cpp
  logbook/logbook.cpp
  qsohistory.cpp
  psk_reporter.cpp
  Modulator.cpp
  Detector.cpp
  logqso.cpp
  displaytext.cpp
  decodedtext.cpp
  getfile.cpp
  soundout.cpp
  soundin.cpp
  meterwidget.cpp
  signalmeter.cpp
  plotter.cpp
  widegraph.cpp
  about.cpp
  WsprTxScheduler.cpp
  mainwindow.cpp
  Configuration.cpp
  main.cpp
  eqsl.cpp
  wsprnet.cpp
  WSPRBandHopping.cpp
  JTDXMessageBox.cpp
  JTDXDateTime.cpp
  )

set (wsjt_CXXSRCS
  lib/crc10.cpp
  lib/crc12.cpp
  lib/crc14.cpp
  )
# deal with a GCC v6 UB error message
set_source_files_properties (
  lib/crc10.cpp
  lib/crc12.cpp
  lib/crc14.cpp
  PROPERTIES COMPILE_FLAGS -fpermissive)

if (WIN32)
  set (wsjt_CXXSRCS
    ${wsjt_CXXSRCS}
    killbyname.cpp
    )

  set (wsjt_qt_CXXSRCS
    ${wsjt_qt_CXXSRCS}
    OmniRigTransceiver.cpp
    )
endif (WIN32)

set (wsjt_FSRCS
  lib/afc10.f90
  lib/afc65b.f90
  lib/afc9.f90
  lib/agcc.f90
  lib/ft4/agccft4.f90
  lib/agccft8.f90
  lib/azdist.f90
  lib/baddata.f90
  lib/baseline.f90
  lib/bpdecode144.f90
  lib/bpdecode174.f90
  lib/ft8v2/bpdecode174_91.f90
  lib/ccf2.f90
  lib/ccf65.f90
  lib/chkcall.f90
  lib/chkcrc12a.f90
  lib/ft8v2/chkcrc14a.f90
  lib/chkfalse.f90
  lib/chkfalse8.f90
  lib/chkflscall.f90
  lib/chkftrsd.f90
  lib/chkgrid.f90
  lib/chkhist.f90
  lib/chklong8.f90
  lib/chkmsg.f90
  lib/chkspecial8.f90
  lib/chkss10.f90
  lib/chkss2.f90
  lib/cqcall3.f90
  lib/crc.f90
  lib/cwfilter.f90
  lib/datacor.f90
  lib/db.f90
  lib/decode9.f90
  lib/decode10.f90
  lib/decode65a.f90
  lib/decode65b.f90
  lib/decoder.f90
  lib/deg2grid.f90
  lib/delbraces.f90
  lib/demod64a.f90
  lib/determ.f90
  lib/downsam10.f90
  lib/downsam9.f90
  lib/downsam9s.f90
  lib/encode174.f90
  lib/ft8v2/encode174_91.f90
  lib/encode232.f90
  lib/entail.f90
  lib/extract.f90
  lib/extract_call.f90
  lib/fano232.f90
  lib/fchisq.f90
  lib/fchisq10.f90
  lib/fchisq65.f90
  lib/fftw3mod.f90
  lib/fil3.f90
  lib/fil3c.f90
  lib/fil4.f90
  lib/filbig.f90
  lib/filt8.f90
  lib/filterscq.f90
  lib/filterscq2.f90
  lib/filtersde.f90
  lib/filtersfree.f90
  lib/filtersstd.f90
  lib/fix_contest_msg.f90
  lib/flat2.f90
  lib/flat4.f90
  lib/fmtmsg.f90
  lib/four2a.f90
  lib/foxgen.f90
  lib/ft4/ft4_baseline.f90
  lib/ft4_decode.f90
  lib/ft4_mod1.f90
  lib/ft4/ft4_downsample.f90
  lib/ft4/ft4sim.f90
  lib/ft8_decode.f90
  lib/ft8_downsample.f90
  lib/ft8_mod1.f90
  lib/ft8apset.f90
  lib/ft8b.f90
  lib/ft8mf1.f90
  lib/ft8mfcq.f90
  lib/ft8s.f90
  lib/ft8sd.f90
  lib/ft8sd1.f90
  lib/ft8sim.f90
  lib/ft8sim_gfsk.f90
  lib/fqso_first.f90
  lib/ft4/gen_ft4wave.f90
  lib/gen_ft8wave.f90
  lib/gen_ft8wavesub.f90
  lib/gen10.f90
  lib/gen65.f90
  lib/gen9.f90
  lib/ft4/genft4.f90
  lib/genft8.f90
  lib/genft8sd.f90
  lib/genft8refsig.f90
  lib/genwspr.f90
  lib/geodist.f90
  lib/ft4/get_ft4_bitmetrics.f90
  lib/ft4/getcandidates4.f90
  lib/gfsk_pulse.f90
  lib/graycode.f90
  lib/graycode65.f90
  lib/grayline.f90
  lib/grid2deg.f90
  lib/hash.f90
  lib/hashing.f90
  lib/hintdx73.f90
  lib/hintdx739.f90
  lib/hintdx7310.f90
  lib/hintdxcq.f90
  lib/hintdxgrid.f90
  lib/hintdxgrid9.f90
  lib/hintdxgrid10.f90
  lib/hintdxr.f90
  lib/hintdxr9.f90
  lib/hintdxr10.f90
  lib/hintdxrr.f90
  lib/hintdxrr9.f90
  lib/hintdxrr10.f90
  lib/hintdyn.f90
  lib/hintdyncq.f90
  lib/hintrxcq.f90
  lib/hintrxgrid.f90
  lib/hintrxgrid9.f90
  lib/hintrxgrid10.f90
  lib/hintwidecq.f90
  lib/hintwidedx.f90
  lib/image.f90
  lib/indexx.f90
  lib/init_random_seed.f90
  lib/interleave63.f90
  lib/interleave9.f90
  lib/inter_wspr.f90
  lib/iso_c_utilities.f90
  lib/jplsubs.f
  lib/jt65_decode.f90
  lib/jt65_mod2.f90
  lib/jt65_mod4.f90
  lib/jt65_mod5.f90
  lib/jt65_mod6.f90
  lib/jt65_mod8.f90
  lib/jt65_mod9.f90
  lib/jt65_mod10.f90
  lib/jt65_mod11.f90
  lib/jt10_decode.f90  
  lib/jt9_decode.f90
  lib/jt9s_decode.f90
  lib/jt9_mod1.f90
  lib/jt9fano.f90
  lib/mixlpf.f90
  lib/morse.f90
  lib/move.f90
  lib/msgparser.f90
  lib/ft8v2/my_hash.f90
  lib/options.f90
  lib/osd174.f90
  lib/ft8v2/osd174_91.f90
  lib/ft4/osd4_174_91.f90
  lib/wsprd/osdwspr.f90
  lib/packjt.f90
  lib/ft8v2/packjt77.f90
  lib/ft8v2/packjt77sd.f90
  lib/partint.f90
  lib/ft4/partintft4.f90
  lib/partintft8.f90
  lib/pctile.f90
  lib/peakdt10.f90
  lib/peakdt9.f90
  lib/peakup.f90
  lib/polyfit.f90
  lib/process_dd.f90
  lib/prog_args.f90
  lib/readwav.f90
  lib/rms_augap.f90
  lib/savec2.f90
  lib/sec_midn.f90
  lib/searchcalls.f90
  lib/shell.f90
  lib/sleep_msec.f90
  lib/slope.f90
  lib/smo.f90
  lib/softsym.f90
  lib/softsym9s.f90
  lib/softsym10.f90
  lib/splitmsg.f90
  lib/splitmsgdupe.f90
  lib/splitdupecq.f90
  lib/splitmsgcq.f90
  lib/stdmsg.f90
  lib/subtract9.f90
  lib/subtract9s.f90
  lib/subtract10.f90
  lib/subtract65.f90
  lib/ft4/subtractft4.f90
  lib/subtractft81.f90
  lib/subtractft82.f90
  lib/subtractft83.f90
  lib/subtractft84.f90
  lib/subtractft85.f90
  lib/subtractft86.f90
  lib/subtractft87.f90
  lib/subtractft88.f90
  lib/subtractft89.f90
  lib/subtractft810.f90
  lib/subtractft811.f90
  lib/subtractft812.f90
  lib/sun.f90
  lib/symspec.f90
  lib/symspec10.f90
  lib/symspec2.f90
  lib/symspec65.f90
  lib/ft4/sync4d.f90
  lib/sync10.f90
  lib/sync65.f90
  lib/sync8.f90
  lib/sync8d.f90
  lib/sync9.f90
  lib/sync9s.f90
  lib/timer_C_wrapper.f90
  lib/timer_impl.f90
  lib/timer_module.f90
#  lib/timf2.f90
  lib/to_contest_msg.f90
  lib/tone8.f90
  lib/tone8myc.f90
  lib/tonesd.f90
  lib/twkfreq.f90
  lib/twkfreq1.f90
  lib/wav12.f90
  lib/wavhdr.f90
  lib/xcor.f90
  lib/watterson.f90
  lib/wavhdr.f90
  lib/wqencode.f90
  lib/wspr_downsample.f90
  lib/zplot9.f90
  )

set (ka9q_CSRCS
  lib/ftrsd/decode_rs.c
  lib/ftrsd/encode_rs.c
  lib/ftrsd/init_rs.c
  )
set_source_files_properties (${ka9q_CSRCS} PROPERTIES COMPILE_FLAGS -Wno-sign-compare)

set (wsjt_CSRCS
  ${ka9q_CSRCS}
  lib/ftrsd/ftrsd2.c
  lib/sgran.c
  lib/gran.c
  lib/igray.c
  lib/init_random_seed.c
  lib/wsprd/nhash.c
  lib/tab.c
  lib/usleep.c
  lib/wisdom.c
  lib/wrapkarn.c
  )

set (wsjt_qt_UISRCS
  wf_palette_design_dialog.ui
  )

set (wsprsim_CSRCS
  lib/wsprd/wsprsim.c
  lib/wsprd/wsprsim_utils.c
  lib/wsprd/wsprd_utils.c
  lib/wsprd/fano.c
  lib/wsprd/tab.c
  lib/wsprd/nhash.c
  )

set (wsprd_CSRCS
  lib/wsprd/wsprd.c
  lib/wsprd/wsprsim_utils.c
  lib/wsprd/wsprd_utils.c
  lib/wsprd/fano.c
  lib/wsprd/jelinek.c
  lib/wsprd/tab.c
  lib/wsprd/nhash.c
  lib/init_random_seed.c
  )

set (wsjtx_UISRCS
  mainwindow.ui
  about.ui
  widegraph.ui
  logqso.ui
  Configuration.ui
  )

set (message_aggregator_CXXSRCS
  MessageServer.cpp
  MessageAggregator.cpp
  )

set (UDPDaemon_CXXSRCS
  NetworkMessage.cpp
  MessageServer.cpp
  Radio.cpp
  UDPDaemon.cpp
  )

set (all_CXXSRCS
  ${wsjt_CXXSRCS}
  ${wsjt_qt_CXXSRCS}
  ${wsjt_qtmm_CXXSRCS}
  ${jt9_CXXSRCS}
  ${wsjtx_CXXSRCS}
  ${message_aggregator_CXXSRCS}
  ${UDPDaemon_CXXSRCS}
  )

set (all_C_and_CXXSRCS
  ${wsjt_CSRCS}
  ${wsprsim_CSRCS}
  ${wsprd_CSRCS}
  ${all_CXXSRCS}
  )

set (message_aggregator_STYLESHEETS
  qss/default.qss
  )

set (TOP_LEVEL_RESOURCES
  shortcuts.txt
  mouse_commands.txt
  prefixes.txt
  cty.dat
  lotw-user-activity.csv
  contrib/decpasses.png
  contrib/dtrange.png
  )

set (PALETTE_FILES
  Palettes/Banana.pal
  Palettes/Blue1.pal
  Palettes/Blue2.pal
  Palettes/Blue3.pal
  Palettes/Brown.pal
  Palettes/Cyan1.pal
  Palettes/Cyan2.pal
  Palettes/Cyan3.pal
  Palettes/Default.pal
  Palettes/Default16.pal
  Palettes/Digipan.pal
  Palettes/Fldigi.pal
  Palettes/Gray1.pal
  Palettes/Gray2.pal
  Palettes/Green1.pal
  Palettes/Green2.pal
  Palettes/Jungle.pal
  Palettes/Linrad.pal
  Palettes/Negative.pal
  Palettes/Orange.pal
  Palettes/Pink.pal
  Palettes/Rainbow.pal
  Palettes/Scope.pal
  Palettes/Sunburst.pal
  Palettes/VK4BDJ.pal
  Palettes/YL2KF.pal
  Palettes/Yellow1.pal
  Palettes/Yellow2.pal
  Palettes/ZL1FZ.pal
)

if (APPLE)
  set (WSJTX_ICON_FILE ${CMAKE_PROJECT_NAME}.icns)
  set (ICONSRCS
    icons/Darwin/${CMAKE_PROJECT_NAME}.iconset/icon_16x16.png
    icons/Darwin/${CMAKE_PROJECT_NAME}.iconset/icon_16x16@2x.png
    icons/Darwin/${CMAKE_PROJECT_NAME}.iconset/icon_32x32.png
    icons/Darwin/${CMAKE_PROJECT_NAME}.iconset/icon_32x32@2x.png
    icons/Darwin/${CMAKE_PROJECT_NAME}.iconset/icon_128x128.png
    icons/Darwin/${CMAKE_PROJECT_NAME}.iconset/icon_128x128@2x.png
    icons/Darwin/${CMAKE_PROJECT_NAME}.iconset/icon_256x256.png
    icons/Darwin/${CMAKE_PROJECT_NAME}.iconset/icon_256x256@2x.png
    icons/Darwin/${CMAKE_PROJECT_NAME}.iconset/icon_512x512.png
    icons/Darwin/${CMAKE_PROJECT_NAME}.iconset/icon_512x512@2x.png
    )
  add_custom_command (
    OUTPUT ${WSJTX_ICON_FILE}
    COMMAND iconutil -c icns --output "${CMAKE_BINARY_DIR}/${WSJTX_ICON_FILE}" "${CMAKE_SOURCE_DIR}/icons/Darwin/${CMAKE_PROJECT_NAME}.iconset"
    DEPENDS ${ICONSRCS}
    COMMENT "Building Icons"
    )
endif (APPLE)

set_source_files_properties (${WSJTX_ICON_FILE} PROPERTIES MACOSX_PACKAGE_LOCATION Resources)

if (WSJT_QDEBUG_IN_RELEASE)
  # context info in Qt message handler in release configuration
  set_property (DIRECTORY APPEND PROPERTY
    COMPILE_DEFINITIONS $<$<NOT:$<CONFIG:Debug>>:QT_MESSAGELOGCONTEXT>
    )
else (WSJT_QDEBUG_IN_RELEASE)
  # disable Qt trace and warning messages from release configurations
  set_property (DIRECTORY APPEND PROPERTY
    COMPILE_DEFINITIONS $<$<NOT:$<CONFIG:Debug>>:QT_NO_DEBUG_OUTPUT;QT_NO_WARNING_OUTPUT>
    )
endif (WSJT_QDEBUG_IN_RELEASE)

set_property (SOURCE ${all_C_and_CXXSRCS} APPEND_STRING PROPERTY COMPILE_FLAGS " -include wsjtx_config.h")
set_property (SOURCE ${all_C_and_CXXSRCS} APPEND PROPERTY OBJECT_DEPENDS wsjtx_config.h)

if (WIN32)
  # generate the OmniRig COM interface source
  find_program (DUMPCPP dumpcpp)
  if (DUMPCPP-NOTFOUND)
    message (FATAL_ERROR "dumpcpp tool not found")
  endif (DUMPCPP-NOTFOUND)
  execute_process (
    COMMAND ${DUMPCPP} -getfile {4FE359C5-A58F-459D-BE95-CA559FB4F270}
    OUTPUT_VARIABLE AXSERVER
    OUTPUT_STRIP_TRAILING_WHITESPACE
    )
  string (STRIP "${AXSERVER}" AXSERVER)
  if (NOT AXSERVER)
    message (FATAL_ERROR "You need to install OmniRig on this computer")
  endif (NOT AXSERVER)
  string (REPLACE "\"" "" AXSERVER ${AXSERVER})
  file (TO_CMAKE_PATH ${AXSERVER} AXSERVERSRCS)
endif (WIN32)


#
# decide on platform specifc packing and fixing up
#
if (APPLE)
  set (WSJTX_BUNDLE_VERSION ${wsjtx_VERSION})

  # make sure CMAKE_INSTALL_PREFIX ends in /
  string (LENGTH "${CMAKE_INSTALL_PREFIX}" LEN)
  math (EXPR LEN "${LEN} -1" )
  string (SUBSTRING "${CMAKE_INSTALL_PREFIX}" ${LEN} 1 ENDCH)
  if (NOT "${ENDCH}" STREQUAL "/")
    set (CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}/")
  endif ()
endif (APPLE)


#
# find some useful tools
#
find_program(CTAGS ctags)
find_program(ETAGS etags)

#
# Boost
#
set (Boost_NO_SYSTEM_PATHS TRUE)
if (Boost_NO_SYSTEM_PATHS)
  set (BOOST_ROOT ${PROJECT_SOURCE_DIR}/boost)
endif ()
find_package (Boost 1.63 REQUIRED)
if (Boost_FOUND)
  include_directories (${Boost_INCLUDE_DIRS})
endif ()

#
# OpenMP
#
find_package (OpenMP)

#
# fftw3 single precision library
#
find_package (FFTW3 COMPONENTS double single threads REQUIRED)

#
# libhamlib setup
#
set (hamlib_STATIC 1)
find_package (hamlib 3 REQUIRED)
find_program (RIGCTL_EXE rigctl)
find_program (RIGCTLD_EXE rigctld)

message (STATUS "hamlib_INCLUDE_DIRS: ${hamlib_INCLUDE_DIRS}")
message (STATUS "hamlib_LIBRARIES: ${hamlib_LIBRARIES}")
message (STATUS "hamlib_LIBRARY_DIRS: ${hamlib_LIBRARY_DIRS}")

#
# Qt5 setup
#

# Widgets finds its own dependencies.
find_package (Qt5Widgets 5 REQUIRED)
find_package (Qt5Multimedia 5 REQUIRED)
find_package (Qt5LinguistTools 5 REQUIRED)

if (WIN32)
  add_definitions (-DQT_NEEDS_QTMAIN)
  find_package (Qt5AxContainer REQUIRED)
endif (WIN32)

#
# sub-directories
#
add_subdirectory (samples)
if (WSJT_GENERATE_DOCS)
  add_subdirectory (doc)
endif (WSJT_GENERATE_DOCS)

#
# C & C++ setup
#
set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra")

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -fexceptions -frtti")

if (NOT APPLE)
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-pragmas")
  if (${OPENMP_FOUND})
    if (OpenMP_C_FLAGS)
      set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_C_FLAGS}")
      set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
    endif ()
  endif ()
  set (CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -fdata-sections -ffunction-sections")
  set (CMAKE_C_FLAGS_MINSIZEREL "${CMAKE_C_FLAGS_MINSIZEREL} -fdata-sections -ffunction-sections")
  set (CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -fdata-sections -ffunction-sections")
  set (CMAKE_CXX_FLAGS_MINSIZEREL "${CMAKE_CXX_FLAGS_MINSIZEREL} -fdata-sections -ffunction-sections")
endif (NOT APPLE)

if (WIN32)
  set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-keep-inline-dllexport")
endif (WIN32)
if (APPLE AND ${CMAKE_CXX_COMPILER_ID} STREQUAL "Clang")
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -stdlib=libc++")
else ()
  set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pthread")
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} --std=gnu++11 -pthread")
endif ()


#
# Fortran setup
#
set (General_FFLAGS "-Wall -Wno-conversion -fno-second-underscore")

# FFLAGS depend on the compiler
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  # gfortran

  # CMake compiler test is supposed to do this but doesn't yet
  if (CMAKE_OSX_DEPLOYMENT_TARGET)
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -mmacosx-version-min=${CMAKE_OSX_DEPLOYMENT_TARGET}")
  endif (CMAKE_OSX_DEPLOYMENT_TARGET)
  if (CMAKE_OSX_SYSROOT)
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -isysroot ${CMAKE_OSX_SYSROOT}")
  endif (CMAKE_OSX_SYSROOT)

  set (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -fbounds-check -funroll-all-loops -fno-f2c ${General_FFLAGS}")
  set (CMAKE_Fortran_FLAGS_DEBUG   "${CMAKE_Fortran_FLAGS_DEBUG} -fbounds-check -fno-f2c ${General_FFLAGS}")
elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
  # ifort (untested)
  set (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -f77rtl ${General_FFLAGS}")
  set (CMAKE_Fortran_FLAGS_DEBUG   "${CMAKE_Fortran_FLAGS_DEBUG} -f77rtl ${General_FFLAGS}")
elseif (Fortran_COMPILER_NAME MATCHES "g77")
  # g77
  set (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -funroll-all-loops -fno-f2c -m32 ${General_FFLAGS}")
  set (CMAKE_Fortran_FLAGS_DEBUG   "${CMAKE_Fortran_FLAGS_DEBUG} -fbounds-check -fno-f2c -m32 ${General_FFLAGS}")
else (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  message ("No optimized Fortran compiler flags are known, we just try -O3...")
  set (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3 ${General_FFLAGS}")
  set (CMAKE_Fortran_FLAGS_DEBUG   "${CMAKE_Fortran_FLAGS_DEBUG} -fbounds-check ${General_FFLAGS}")
endif (Fortran_COMPILER_NAME MATCHES "gfortran.*")


#
# Linker setup
#
if (NOT APPLE)
  set (CMAKE_EXE_LINKER_FLAGS_RELEASE "${CMAKE_EXE_LINKER_FLAGS_RELEASE} -Wl,--gc-sections")
  set (CMAKE_EXE_LINKER_FLAGS_MINSIZEREL "${CMAKE_EXE_LINKER_FLAGS_MINSIZEREL} -Wl,--gc-sections")
endif (NOT APPLE)


#
# setup and test Fortran C/C++ interaction
#

include (FortranCInterface)
FortranCInterface_VERIFY (CXX)
FortranCInterface_HEADER (FC.h MACRO_NAMESPACE "FC_" SYMBOL_NAMESPACE "FC_"
  SYMBOLS
  grayline
  )


#
# sort out pre-requisites
#

#
# Setup RPATH so that built executable targets will run in both the
# build tree and the install location without having to set a
# (DYLD|LD)_LIBRARY_PATH override.
#

# use the full RPATH of the build tree
set (CMAKE_SKIP_BUILD_RPATH FALSE)

# when building, don't use the install RPATH, it will still be used
# later on in the install phase
set (CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)

# set (CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}")

# add the automaticaly determined parts of the RPATH which point to
# directories outside of the build tree to the install RPATH
set (CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

# the RPATH to be used when installing, but only if it's not a system
# directory
# list (FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}" isSystemDir)
# if ("${isSystemDir}" STREQUAL "-1")
#   set (CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}")
# endif ("${isSystemDir}" STREQUAL "-1")

set (QT_NEED_RPATH FALSE)
if (NOT "${QT_LIBRARY_DIR}" STREQUAL "/lib" AND NOT "${QT_LIBRARY_DIR}" STREQUAL "/usr/lib" AND NOT "${QT_LIBRARY_DIR}" STREQUAL "/lib64" AND NOT "${QT_LIBRARY_DIR}" STREQUAL "/usr/lib64")
  set (QT_NEED_RPATH TRUE)
endif ()

#
# stuff only qmake can tell us
#
get_target_property (QMAKE_EXECUTABLE Qt5::qmake LOCATION)
function (QUERY_QMAKE VAR RESULT)
  exec_program (${QMAKE_EXECUTABLE} ARGS "-query ${VAR}" RETURN_VALUE return_code OUTPUT_VARIABLE output)
  if (NOT return_code)
    file (TO_CMAKE_PATH "${output}" output)
    set (${RESULT} ${output} PARENT_SCOPE)
  endif (NOT return_code)
  message (STATUS "Asking qmake for ${RESULT} and got ${output}")
endfunction (QUERY_QMAKE)

query_qmake (QT_INSTALL_PLUGINS QT_PLUGINS_DIR)
query_qmake (QT_INSTALL_IMPORTS QT_IMPORTS_DIR)
query_qmake (QT_HOST_DATA QT_DATA_DIR)
set (QT_MKSPECS_DIR ${QT_DATA_DIR}/mkspecs)

# Tell CMake to run moc when necessary
set (CMAKE_AUTOMOC ON)

# don't use Qt "keywords" signal, slot, emit in generated files to
# avoid compatability issue with other libraries
# ADD_DEFINITIONS (-DQT_NO_KEYWORDS)
# ADD_DEFINITIONS (-DUNICODE)	#as per qmake

# As moc files are generated in the binary dir, tell CMake to always
# look for includes there:
set (CMAKE_INCLUDE_CURRENT_DIR ON)

# project definitions
add_definitions (-DQT5 -DCMAKE_BUILD -DBIGSYM=1)
if (CMAKE_HOST_UNIX)
  add_definitions (-DUNIX)
elseif (CMAKE_HOST_WIN32)
  add_definitions (-DWIN32)
endif ()

#
# source navigation
#
set (sources
  ${CMAKE_SOURCE_DIR}/*
  ${CMAKE_SOURCE_DIR}/logbook/*
  ${CMAKE_SOURCE_DIR}/lib/*
  )
add_custom_target (ctags COMMAND ${CTAGS} -o ${CMAKE_SOURCE_DIR}/tags -R ${sources})
add_custom_target (etags COMMAND ${ETAGS} -o ${CMAKE_SOURCE_DIR}/TAGS -R ${sources})


# Qt i18n
set (LANGUAGES
  en_US
  ru_RU
  et_EE
  es_ES
  hr_HR
  fr_FR
  it_IT
  lv_LV
  pl_PL
  pt_PT
  pt_BR
  zh_HK
  zh_CN
  ja_JP
  ca_ES
  da_DK
  )
foreach (lang_ ${LANGUAGES})
  file (TO_NATIVE_PATH translations/jtdx_${lang_}.ts ts_)
  list (APPEND TS_FILES ${ts_})
endforeach ()  
if (UPDATE_TRANSLATIONS)
  message (STATUS "UPDATE_TRANSLATIONS option is set.")
  qt5_create_translation (
    QM_FILES ${wsjt_qt_UISRCS} ${wsjtx_UISRCS} ${wsjt_qt_CXXSRCS} ${wsjtx_CXXSRCS}
    ${TS_FILES}
    OPTIONS -source-language en_US
    )
else ()
  qt5_add_translation (QM_FILES ${TS_FILES})
endif ()
add_custom_target (translations DEPENDS ${QM_FILES})
set_property (DIRECTORY PROPERTY CLEAN_NO_CUSTOM TRUE)
# do this after i18n to stop lupdate walking the boost tree which it
# chokes on
if (Boost_FOUND)
  include_directories (${Boost_INCLUDE_DIRS})
endif ()

# embedded resources
function (add_resources resources path)
  foreach (resource_file_ ${ARGN})
    get_filename_component (name_ ${resource_file_} NAME)
    if (IS_ABSOLUTE "${resource_file_}")
      file (TO_NATIVE_PATH ${resource_file_} source_)
    else ()
      file (TO_NATIVE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/${resource_file_} source_)
    endif ()
    file (TO_NATIVE_PATH ${path}/${name_} dest_)
    set (resources_ "${resources_}\n    <file alias=\"${dest_}\">${source_}</file>")
    set (${resources} ${${resources}}${resources_} PARENT_SCOPE)
  endforeach (resource_file_ ${ARGN})
endfunction (add_resources resources path)

add_resources (wsjtx_RESOURCES "" ${TOP_LEVEL_RESOURCES})
add_resources (wsjtx_RESOURCES /Palettes ${PALETTE_FILES})
add_resources (wsjtx_RESOURCES /Translations ${QM_FILES})

configure_file (wsjtx.qrc.in wsjtx.qrc @ONLY)


# UI generation
qt5_wrap_ui (wsjt_qt_GENUISRCS ${wsjt_qt_UISRCS})
qt5_wrap_ui (wsjtx_GENUISRCS ${wsjtx_UISRCS})

# Resource generation
qt5_add_resources (wsjtx_RESOURCES_RCC 
  ${CMAKE_BINARY_DIR}/wsjtx.qrc
  contrib/QDarkStyleSheet/qdarkstyle/style.qrc
)

# AX COM servers
if (WIN32)
  include (QtAxMacros)
  wrap_ax_server (GENAXSRCS ${AXSERVERSRCS})
endif (WIN32)


#
# targets
#

# build a library of package functionality (without and optionally with OpenMP support)
add_library (wsjt_cxx STATIC ${wsjt_CSRCS} ${wsjt_CXXSRCS})

# build an OpenMP variant of the Fortran library routines
add_library (wsjt_fort STATIC ${wsjt_FSRCS})
target_link_libraries (wsjt_fort  ${FFTW3_LIBRARIES})
if (${OPENMP_FOUND} OR APPLE)
  add_library (wsjt_fort_omp STATIC ${wsjt_FSRCS})
  target_link_libraries (wsjt_fort_omp  ${FFTW3_LIBRARIES})
  if (OpenMP_C_FLAGS)
    set_target_properties (wsjt_fort_omp
      PROPERTIES
      COMPILE_FLAGS "${OpenMP_C_FLAGS}"
      )
  endif ()
  set_target_properties (wsjt_fort_omp
    PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/fortran_modules_omp
    )
  file (MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/fortran_modules_omp)
  if (APPLE)
    # On  Mac  we don't  have  working  OpenMP  support in  the  C/C++
    # compilers so  we have to manually  set the correct flags  to get
    # OpenMP support in jt9.
    target_compile_options (wsjt_fort_omp
      PRIVATE
      $<$<COMPILE_LANGUAGE:Fortran>:-fopenmp>   # assumes GNU style Fortran compiler
      )
  endif (APPLE)
endif (${OPENMP_FOUND} OR APPLE)

# build a library of package Qt functionality
add_library (wsjt_qt STATIC ${wsjt_qt_CXXSRCS} ${wsjt_qt_GENUISRCS} ${GENAXSRCS})
target_link_libraries (wsjt_qt Qt5::Widgets Qt5::Network)
target_include_directories (wsjt_qt BEFORE PRIVATE ${hamlib_INCLUDE_DIRS})
if (WIN32)
  target_link_libraries (wsjt_qt Qt5::AxContainer Qt5::AxBase)
endif (WIN32)

add_library (wsjt_qtmm STATIC ${wsjt_qtmm_CXXSRCS} ${wsjt_qtmm_GENUISRCS})
target_link_libraries (wsjt_qtmm Qt5::Multimedia)

add_executable (ft4sim lib/ft4/ft4sim.f90 wsjtx.rc)
target_link_libraries (ft4sim wsjt_fort wsjt_cxx)

add_executable (ft8sim lib/ft8sim.f90 wsjtx.rc)
target_link_libraries (ft8sim wsjt_fort wsjt_cxx)

add_executable (ft8sim_gfsk lib/ft8sim_gfsk.f90 wsjtx.rc)
target_link_libraries (ft8sim_gfsk wsjt_fort wsjt_cxx)

add_executable (jt65sim lib/jt65sim.f90 wsjtx.rc)
target_link_libraries (jt65sim wsjt_fort wsjt_cxx)

add_executable (jt9sim lib/jt9sim.f90 wsjtx.rc)
target_link_libraries (jt9sim wsjt_fort wsjt_cxx)

add_executable (jt10sim lib/jt10sim.f90 wsjtx.rc)
target_link_libraries (jt10sim wsjt_fort wsjt_cxx)

add_executable (jt65code lib/jt65code.f90 wsjtx.rc)
target_link_libraries (jt65code wsjt_fort wsjt_cxx)

add_executable (jt9code lib/jt9code.f90 wsjtx.rc)
target_link_libraries (jt9code wsjt_fort wsjt_cxx)

add_executable (wsprd ${wsprd_CSRCS} lib/indexx.f90 lib/wsprd/osdwspr.f90)
target_include_directories (wsprd PRIVATE ${FFTW3_INCLUDE_DIRS})
target_link_libraries (wsprd ${FFTW3_LIBRARIES})

add_executable (wsprsim ${wsprsim_CSRCS})

add_executable (jtdxjt9 lib/jt9.f90 lib/jt9a.f90 ${jt9_CXXSRCS} wsjtx.rc)
if (${OPENMP_FOUND} OR APPLE)
  if (APPLE)
    # On  Mac  we don't  have  working  OpenMP  support in  the  C/C++
    # compilers so we  have to manually set the  correct linking flags
    # and libraries to get OpenMP support in jt9.
    set_target_properties (jtdxjt9
      PROPERTIES
      Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/fortran_modules_omp
      LINK_LIBRARIES "gomp;gcc_s.1" # assume GNU libgcc OpenMP
      )
      target_compile_options (jtdxjt9
	PRIVATE
     	$<$<COMPILE_LANGUAGE:Fortran>:-fopenmp>   # assumes GNU style Fortran compiler
	)
  else (APPLE)
     if (OpenMP_C_FLAGS)
       set_target_properties (jtdxjt9
         PROPERTIES
         COMPILE_FLAGS "${OpenMP_C_FLAGS}"
         LINK_FLAGS "${OpenMP_C_FLAGS}"
         )
     endif (OpenMP_C_FLAGS)
     set_target_properties (jtdxjt9
	PROPERTIES
        Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/fortran_modules_omp
        )
  endif (APPLE)
  if (WIN32)
     set_target_properties (jtdxjt9
       PROPERTIES
##       COMPILE_FLAGS "${OpenMP_C_FLAGS}"
##       LINK_FLAGS "${OpenMP_C_FLAGS}"
#      LINK_FLAGS -Wl,--stack,8388608
       LINK_FLAGS -Wl,--stack,4194304
##       Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/fortran_modules_omp
      )
  endif ()
  target_link_libraries (jtdxjt9 wsjt_fort_omp wsjt_cxx Qt5::Core)
else (${OPENMP_FOUND} OR APPLE)
  target_link_libraries (jtdxjt9 wsjt_fort wsjt_cxx Qt5::Core)
endif (${OPENMP_FOUND} OR APPLE)

# build the main application
add_executable (jtdx MACOSX_BUNDLE
  ${wsjtx_CXXSRCS}
  ${wsjtx_GENUISRCS}
  wsjtx.rc
  ${WSJTX_ICON_FILE}
  ${wsjtx_RESOURCES_RCC}
  )

if (WSJT_CREATE_WINMAIN)
  set_target_properties (jtdx PROPERTIES WIN32_EXECUTABLE ON)
endif (WSJT_CREATE_WINMAIN)

set_target_properties (jtdx PROPERTIES
  MACOSX_BUNDLE_INFO_PLIST "${CMAKE_CURRENT_SOURCE_DIR}/Darwin/Info.plist.in"
  MACOSX_BUNDLE_INFO_STRING "${WSJTX_DESCRIPTION_SUMMARY}"
  MACOSX_BUNDLE_ICON_FILE "${WSJTX_ICON_FILE}"
  MACOSX_BUNDLE_BUNDLE_VERSION ${wsjtx_VERSION}
  MACOSX_BUNDLE_SHORT_VERSION_STRING "v${wsjtx_VERSION}"
  MACOSX_BUNDLE_LONG_VERSION_STRING "Version ${wsjtx_VERSION}"
  MACOSX_BUNDLE_BUNDLE_NAME "${PROJECT_NAME}"
  MACOSX_BUNDLE_BUNDLE_EXECUTABLE_NAME "${PROJECT_NAME}"
  MACOSX_BUNDLE_COPYRIGHT "${PROJECT_COPYRIGHT}"
  MACOSX_BUNDLE_GUI_IDENTIFIER "tech.jtdx"
  )

target_include_directories (jtdx PRIVATE ${FFTW3_INCLUDE_DIRS})
if (APPLE)
  target_link_libraries (jtdx wsjt_fort wsjt_cxx wsjt_qt wsjt_qtmm ${hamlib_LIBRARIES} ${FFTW3_LIBRARIES})
else (APPLE)
  target_link_libraries (jtdx wsjt_fort_omp wsjt_cxx wsjt_qt wsjt_qtmm ${hamlib_LIBRARIES} ${FFTW3_LIBRARIES})
  if (OpenMP_C_FLAGS)
    set_target_properties (jtdx PROPERTIES
      COMPILE_FLAGS "${OpenMP_C_FLAGS}"
      LINK_FLAGS "${OpenMP_C_FLAGS}"
      )
  endif (OpenMP_C_FLAGS)
  set_target_properties (jtdx PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/fortran_modules_omp
    )
  if (WIN32)
    set_target_properties (jtdx PROPERTIES
      LINK_FLAGS -Wl,--stack,16777216
      )
  endif ()
endif ()
qt5_use_modules (jtdx SerialPort) # not sure why the interface link library syntax above doesn't work

add_resources (message_aggregator_RESOURCES /qss ${message_aggregator_STYLESHEETS})
configure_file (message_aggregator.qrc.in message_aggregator.qrc @ONLY)
qt5_add_resources (message_aggregator_RESOURCES_RCC 
  ${CMAKE_BINARY_DIR}/message_aggregator.qrc
  contrib/QDarkStyleSheet/qdarkstyle/style.qrc
)
add_executable (message_aggregator
  ${message_aggregator_CXXSRCS}
  wsjtx.rc
  ${message_aggregator_RESOURCES_RCC}
  )
target_link_libraries (message_aggregator wsjt_qt Qt5::Widgets)

add_executable (udp_daemon
  ${UDPDaemon_CXXSRCS}
  wsjtx.rc
  )
target_link_libraries (udp_daemon Qt5::Core Qt5::Network)

if (WSJT_CREATE_WINMAIN)
  set_target_properties (message_aggregator PROPERTIES WIN32_EXECUTABLE ON)
endif (WSJT_CREATE_WINMAIN)

if (UNIX)
  if (NOT WSJT_SKIP_MANPAGES)
    add_subdirectory (manpages)
    add_dependencies (jtdx manpages)
  endif (NOT WSJT_SKIP_MANPAGES)
  if (NOT APPLE)
    add_subdirectory (debian)
    add_dependencies (jtdx debian)
  endif (NOT APPLE)
endif (UNIX)

#
# installation
#
install (TARGETS jtdx
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT runtime
  BUNDLE DESTINATION . COMPONENT runtime
  )

install (TARGETS jtdxjt9 jt65code jt9code wsprd message_aggregator udp_daemon
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT runtime
  BUNDLE DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT runtime
  )

install (PROGRAMS
  ${RIGCTL_EXE}
  DESTINATION ${CMAKE_INSTALL_BINDIR}
  #COMPONENT runtime
  RENAME rigctl-jtdx${CMAKE_EXECUTABLE_SUFFIX}
  )

install (PROGRAMS
  ${RIGCTLD_EXE}
  DESTINATION ${CMAKE_INSTALL_BINDIR}
  #COMPONENT runtime
  RENAME rigctld-jtdx${CMAKE_EXECUTABLE_SUFFIX}
  )

install (FILES
  README
  COPYING
  AUTHORS
  THANKS
  NEWS
  INSTALL
  BUGS
  DESTINATION ${CMAKE_INSTALL_DOCDIR}
  #COMPONENT runtime
  )

install (FILES
  contrib/Ephemeris/JPLEPH
  contrib/CallDB/ALLCALL7.TXT
  contrib/CallDB/CALL3.TXT
  DESTINATION ${CMAKE_INSTALL_DATADIR}/${CMAKE_PROJECT_NAME}
  #COMPONENT runtime
  )

#
# Mac installer files
#
if (APPLE)
    install (FILES
        Darwin/ReadMe.txt
        Darwin/sysctl.conf
        DESTINATION .
        #COMPONENT runtime
    )
endif (APPLE)


#
# uninstall support
#
configure_file (
  "${CMAKE_CURRENT_SOURCE_DIR}/CMake/cmake_uninstall.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
  @ONLY)
add_custom_target (uninstall
  "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake")


# creates svnversion.h using cmake script
add_custom_target (revisiontag
  COMMAND ${CMAKE_COMMAND} -D SOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR} -D OUTPUT_DIR=${PROJECT_BINARY_DIR} -P ${CMAKE_CURRENT_SOURCE_DIR}/CMake/getsvn.cmake
  COMMENT "Generating Subversion revision information"
  VERBATIM
  )
# explicitly say that the wsjt_qt depends on custom target, this is
# done indirectly so that the revisiontag target gets built exactly
# once per build
add_dependencies(wsjt_qt revisiontag)


#
# versioning and configuration
#
configure_file (
  "${PROJECT_SOURCE_DIR}/wsjtx_config.h.in"
  "${PROJECT_BINARY_DIR}/wsjtx_config.h"
  )
include_directories (BEFORE "${PROJECT_BINARY_DIR}")


if (NOT WIN32 AND NOT APPLE)
  # install a desktop file so wsjtx appears in the application start
  # menu with an icon
  install (
    FILES jtdx.desktop
    DESTINATION share/applications
    #COMPONENT runtime
    )
  install (
    FILES icons/Unix/jtdx_icon.png
    DESTINATION share/pixmaps
    #COMPONENT runtime
    )
endif (NOT WIN32 AND NOT APPLE)


#
# bundle fixup only done in Release or MinSizeRel configurations
#
if (NOT is_debug_build)

  get_target_property (QtCore_location Qt5::Core LOCATION)
  get_filename_component (QtCore_location ${QtCore_location} PATH)
  list (APPEND fixup_library_dirs ${QtCore_location})

  if (APPLE)
    set (CMAKE_POSTFLIGHT_SCRIPT
      "${jtdx_BINARY_DIR}/postflight.sh")
    set (CMAKE_POSTUPGRADE_SCRIPT
      "${jtdx_BINARY_DIR}/postupgrade.sh")
    configure_file ("${jtdx_SOURCE_DIR}/Darwin/postflight.sh.in"
      "${CMAKE_POSTFLIGHT_SCRIPT}")
    configure_file ("${jtdx_SOURCE_DIR}/Darwin/postupgrade.sh.in"
      "${CMAKE_POSTUPGRADE_SCRIPT}")
  endif ()

  if (APPLE OR WIN32)
    # install rules for including 3rd party libs such as Qt

    # install a qt.conf file
    install (CODE "
        get_filename_component (the_qt_conf \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/${WSJT_QT_CONF_DESTINATION}/qt.conf\" REALPATH)
        file (WRITE \"\${the_qt_conf}\"
\"[Paths]
\")
"
      #COMPONENT runtime
      )

    # if a system Qt is used (e.g. installed in /usr/lib/), it will not be included in the installation
    set (fixup_exe "\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_BINDIR}/${CMAKE_PROJECT_NAME}${CMAKE_EXECUTABLE_SUFFIX}")

    #get_filename_component (hamlib_lib_dir ${hamlib_LIBRARIES} PATH)

    if (APPLE)
      # install required Qt plugins
      install (
	DIRECTORY
	${QT_PLUGINS_DIR}/platforms
	${QT_PLUGINS_DIR}/audio
	${QT_PLUGINS_DIR}/accessible
	${QT_PLUGINS_DIR}/imageformats
	DESTINATION ${WSJT_PLUGIN_DESTINATION}
	CONFIGURATIONS Release MinSizeRel
	#COMPONENT runtime
	FILES_MATCHING PATTERN "*${CMAKE_SHARED_LIBRARY_SUFFIX}"
	PATTERN "*minimal*${CMAKE_SHARED_LIBRARY_SUFFIX}" EXCLUDE
	PATTERN "*offscreen*${CMAKE_SHARED_LIBRARY_SUFFIX}" EXCLUDE
	PATTERN "*quick*${CMAKE_SHARED_LIBRARY_SUFFIX}" EXCLUDE
	PATTERN "*_debug${CMAKE_SHARED_LIBRARY_SUFFIX}" EXCLUDE
	)
      # install (
      # 	DIRECTORY
      # 	${QT_PLUGINS_DIR}/platforms
      # 	${QT_PLUGINS_DIR}/audio
      #         ${QT_PLUGINS_DIR}/accessible
      # 	DESTINATION ${WSJT_PLUGIN_DESTINATION}
      # 	CONFIGURATIONS Debug
      # 	#COMPONENT runtime
      # 	FILES_MATCHING PATTERN "*_debug${CMAKE_SHARED_LIBRARY_SUFFIX}"
      # 	PATTERN "*minimal*${CMAKE_SHARED_LIBRARY_SUFFIX}" EXCLUDE
      # 	PATTERN "*offscreen*${CMAKE_SHARED_LIBRARY_SUFFIX}" EXCLUDE
      # 	PATTERN "*quick*${CMAKE_SHARED_LIBRARY_SUFFIX}" EXCLUDE
      # 	)

      # add plugins path for Mac Bundle
      install (CODE "
          get_filename_component (the_qt_conf \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/${WSJT_QT_CONF_DESTINATION}/qt.conf\" REALPATH)
          file (APPEND \"\${the_qt_conf}\"
\"Plugins = PlugIns
\")
"
	#COMPONENT runtime
	)
    endif (APPLE)

    if (WIN32)
      # DLL directory
      #set (hamlib_lib_dir ${hamlib_lib_dir}/../bin)

      get_filename_component (fftw_lib_dir ${FFTW3F_LIBRARY} PATH)
      list (APPEND fixup_library_dirs ${fftw_lib_dir})

      # install required Qt plugins
      install (
	DIRECTORY
	${QT_PLUGINS_DIR}/platforms
	${QT_PLUGINS_DIR}/accessible
	${QT_PLUGINS_DIR}/audio
	${QT_PLUGINS_DIR}/imageformats
	DESTINATION ${WSJT_PLUGIN_DESTINATION}
	CONFIGURATIONS Release MinSizeRel
	#COMPONENT runtime
	FILES_MATCHING PATTERN "*${CMAKE_SHARED_LIBRARY_SUFFIX}"
	PATTERN "*minimal*${CMAKE_SHARED_LIBRARY_SUFFIX}" EXCLUDE
	PATTERN "*offscreen*${CMAKE_SHARED_LIBRARY_SUFFIX}" EXCLUDE
	PATTERN "*quick*${CMAKE_SHARED_LIBRARY_SUFFIX}" EXCLUDE
	PATTERN "*d${CMAKE_SHARED_LIBRARY_SUFFIX}" EXCLUDE
	)
      # install (
      # 	DIRECTORY
      # 	${QT_PLUGINS_DIR}/platforms
      # 	${QT_PLUGINS_DIR}/accessible
      # 	${QT_PLUGINS_DIR}/audio
      # 	DESTINATION ${WSJT_PLUGIN_DESTINATION}
      # 	CONFIGURATIONS Debug
      # 	#COMPONENT runtime
      # 	FILES_MATCHING PATTERN "*d${CMAKE_SHARED_LIBRARY_SUFFIX}"
      # 	PATTERN "*minimal*${CMAKE_SHARED_LIBRARY_SUFFIX}" EXCLUDE
      # 	PATTERN "*offscreen*${CMAKE_SHARED_LIBRARY_SUFFIX}" EXCLUDE
      # 	PATTERN "*quick*${CMAKE_SHARED_LIBRARY_SUFFIX}" EXCLUDE
      # 	)

      # add plugins path for WIN32
      file (RELATIVE_PATH _plugins_path "${CMAKE_INSTALL_PREFIX}/${WSJT_QT_CONF_DESTINATION}" "${CMAKE_INSTALL_PREFIX}/${WSJT_PLUGIN_DESTINATION}")
      install (CODE "
        get_filename_component (the_qt_conf \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/${WSJT_QT_CONF_DESTINATION}/qt.conf\" REALPATH)
        file (APPEND \"\${the_qt_conf}\"
\"Plugins = ${_plugins_path}
\")
"
	#COMPONENT runtime
	)

      set (gp_tool "objdump")	# we want MinGW tool - not MSVC (See GetPrerequisites.cmake)
    endif (WIN32)

    #list (APPEND fixup_library_dirs ${hamlib_lib_dir})
    list (APPEND fixup_library_dirs ${hamlib_LIBRARY_DIRS})

    install (CODE "
        get_filename_component (the_path \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/${WSJT_PLUGIN_DESTINATION}\" REALPATH)
        file (GLOB_RECURSE QTPLUGINS \"\${the_path}/*${CMAKE_SHARED_LIBRARY_SUFFIX}\")
        set (CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/CMake/Modules ${CMAKE_MODULE_PATH})
        include (BundleUtilities)
        set (BU_CHMOD_BUNDLE_ITEMS ON)
        set (gp_tool ${gp_tool})
        # canonicalize path in install context
        get_filename_component (the_exe ${fixup_exe} REALPATH)
        fixup_bundle (\"\${the_exe}\" \"\${QTPLUGINS}\" \"${fixup_library_dirs}\")"
      #COMPONENT runtime
      )
  endif (APPLE OR WIN32)

endif (NOT is_debug_build)


#
# packaging
#
set (CPACK_MONOLITHIC_INSTALL 1)
#set (CPACK_PACKAGE_NAME "${CMAKE_PROJECT_NAME}")
set (CPACK_PACKAGE_NAME JTDX)
set (CPACK_PACKAGE_VERSION_MAJOR ${WSJTX_VERSION_MAJOR})
set (CPACK_PACKAGE_VERSION_MINOR ${WSJTX_VERSION_MINOR})
set (CPACK_PACKAGE_VERSION_PATCH ${WSJTX_VERSION_PATCH})

if (WIN32)
  set (CPACK_GENERATOR "NSIS")
elseif (APPLE)
  set (CPACK_GENERATOR "DragNDrop")
else ()
  find_program (DPKG_BUILDER dpkg-buildpackage DOC "Debian package builder")
  if (DPKG_BUILDER)
    #
    # Derive the correct filename for a Debian package because the DEB
    # generator doesn't do this correctly at present.
    #
    find_program (DPKG_PROGRAM dpkg DOC "dpkg program of Debian-based systems")
    if (DPKG_PROGRAM)
      execute_process (
	COMMAND ${DPKG_PROGRAM} --print-architecture
	OUTPUT_VARIABLE CPACK_DEBIAN_PACKAGE_ARCHITECTURE
	OUTPUT_STRIP_TRAILING_WHITESPACE
	)
    else (DPKG_PROGRAM)
      set (CPACK_DEBIAN_PACKAGE_ARCHITECTURE noarch)
    endif (DPKG_PROGRAM)

    list (APPEND CPACK_GENERATOR "DEB")
  endif (DPKG_BUILDER)

  find_program (RPMBUILDER rpmbuild DOC "RPM package builder")
  if (RPMBUILDER)
    list (APPEND CPACK_GENERATOR "RPM")
  endif (RPMBUILDER)
endif ()

set (CPACK_DEBIAN_PACKAGE_DESCRIPTION "${PROJECT_DESCRIPTION}")
set (CPACK_DEBIAN_PACKAGE_HOMEPAGE "${PROJECT_HOMEPAGE}")
set (CPACK_DEBIAN_PACKAGE_DEPENDS "libgfortran3 (>=4.8.2), libqt5serialport5 (>=5.2), libqt5multimedia5-plugins (>=5.2), libqt5widgets5 (>=5.2), libusb-1.0, libudev, libc6 (>=2.19)")
set (CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)

set (CPACK_RPM_PACKAGE_ARCHITECTURE ${CMAKE_SYSTEM_PROCESSOR})
set (CPACK_RPM_PACKAGE_REQUIRES "qt5-qtserialport >= 5.2, qt5-qtmultimedia >= 5.2, libusb-1.0, libudev, glibc >= 2, libgfortran >= 4.8.2")
set (CPACK_RPM_EXCLUDE_FROM_AUTO_FILELIST_ADDITION /usr/share/pixmaps /usr/share/applications /usr/share/man /usr/share/man1)

configure_file ("${PROJECT_SOURCE_DIR}/CMakeCPackOptions.cmake.in"
  "${PROJECT_BINARY_DIR}/CMakeCPackOptions.cmake" @ONLY)
set (CPACK_PROJECT_CONFIG_FILE "${PROJECT_BINARY_DIR}/CMakeCPackOptions.cmake")

include (CPack)
