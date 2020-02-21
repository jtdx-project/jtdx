# This source code file was last time modified by Rimas Kudelis on September 03rd, 2016
# All changes are shown in the patch file coming together with the full JTDX source code.

#-------------------------------------------------
#
# Project created by QtCreator 2011-07-07T08:39:24
#
#-------------------------------------------------

QT       += network multimedia
greaterThan(QT_MAJOR_VERSION, 4): QT += widgets
CONFIG   += thread
#CONFIG   += console

TARGET = wsjtx
VERSION = "Not for Release"
TEMPLATE = app
DEFINES = QT5
QMAKE_CXXFLAGS += -std=c++11
DEFINES += PROJECT_MANUAL="'\"http://www.physics.princeton.edu/pulsar/K1JT/wsjtx-doc/wsjtx-main.html\"'"

isEmpty (DESTDIR) {
DESTDIR = ../wsjtx_exp_install
}

isEmpty (HAMLIB_DIR) {
HAMLIB_DIR = ../../hamlib3/mingw32
}

isEmpty (FFTW3_DIR) {
FFTW3_DIR = .
}

F90 = gfortran
gfortran.output = ${QMAKE_FILE_BASE}.o
gfortran.commands = $$F90 -c -O2 -o ${QMAKE_FILE_OUT} ${QMAKE_FILE_NAME}
gfortran.input = F90_SOURCES
QMAKE_EXTRA_COMPILERS += gfortran

win32 {
DEFINES += WIN32
QT += axcontainer
TYPELIBS = $$system(dumpcpp -getfile {4FE359C5-A58F-459D-BE95-CA559FB4F270})
}

unix {
DEFINES += UNIX
}

#
# Order matters here as the link is in this order so referrers need to be after referred
#
SOURCES += \
  logbook/adif.cpp \
  logbook/countrydat.cpp \
  logbook/logbook.cpp \
  Radio.cpp NetworkServerLookup.cpp revision_utils.cpp \
  Transceiver.cpp TransceiverBase.cpp TransceiverFactory.cpp \
  PollingTransceiver.cpp EmulateSplitTransceiver.cpp LettersSpinBox.cpp \
  HRDTransceiver.cpp DXLabSuiteCommanderTransceiver.cpp \
  HamlibTransceiver.cpp FrequencyLineEdit.cpp Bands.cpp \
  FrequencyList.cpp StationList.cpp ForeignKeyDelegate.cpp \
  FrequencyItemDelegate.cpp LiveFrequencyValidator.cpp \
  Configuration.cpp	psk_reporter.cpp AudioDevice.cpp \
  Modulator.cpp Detector.cpp logqso.cpp displaytext.cpp \
  getfile.cpp soundout.cpp soundin.cpp meterwidget.cpp signalmeter.cpp \
  WFPalette.cpp plotter.cpp widegraph.cpp about.cpp WsprTxScheduler.cpp mainwindow.cpp \
  main.cpp decodedtext.cpp wsprnet.cpp \
  Modes.cpp \
  WSPRBandHopping.cpp MessageAggregator.cpp

HEADERS  += qt_helpers.hpp \
  pimpl_h.hpp pimpl_impl.hpp \
  Radio.hpp NetworkServerLookup.hpp revision_utils.hpp \
  mainwindow.h plotter.h soundin.h soundout.h \
  about.h WFPalette.hpp widegraph.h getfile.h decodedtext.h \
  commons.h sleep.h displaytext.h logqso.h LettersSpinBox.hpp \
  Bands.hpp FrequencyList.hpp StationList.hpp ForeignKeyDelegate.hpp FrequencyItemDelegate.hpp LiveFrequencyValidator.hpp \
  FrequencyLineEdit.hpp AudioDevice.hpp Detector.hpp Modulator.hpp psk_reporter.h \
  Transceiver.hpp TransceiverBase.hpp TransceiverFactory.hpp PollingTransceiver.hpp \
  EmulateSplitTransceiver.hpp DXLabSuiteCommanderTransceiver.hpp HamlibTransceiver.hpp \
  Configuration.hpp wsprnet.h signalmeter.h meterwidget.h \
  logbook/logbook.h logbook/countrydat.h logbook/adif.h \
  Modes.hpp WSPRBandHopping.hpp \
  WsprTxScheduler.h

INCLUDEPATH += qmake_only

win32 {
SOURCES += killbyname.cpp OmniRigTransceiver.cpp
HEADERS += OmniRigTransceiver.hpp
}

FORMS    += mainwindow.ui about.ui Configuration.ui widegraph.ui \
    logqso.ui wf_palette_design_dialog.ui \

RC_FILE = wsjtx.rc
RESOURCES = wsjtx.qrc

unix {
LIBS += -L lib -ljt9
LIBS += -lhamlib
LIBS += -lfftw3f $$system($$F90 -print-file-name=libgfortran.so)
}

win32 {
INCLUDEPATH += $${HAMLIB_DIR}/include
INCLUDEPATH += C:\JTSDK\wsjtx_exp\build\Release
INCLUDEPATH += C:\JTSDK\hamlib3\include
INCLUDEPATH += C:\JTSDK\qt5\5.2.1\mingw48_32\include\QtSerialPort

LIBS += -L$${HAMLIB_DIR}/lib -lhamlib
LIBS += -L./lib -ljt9
LIBS += -L$${FFTW3_DIR} -lfftw3f-3
LIBS += -lws2_32
LIBS += $$system($$F90 -print-file-name=libgfortran.a)
}
