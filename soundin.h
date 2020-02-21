// -*- Mode: C++ -*-
#ifndef SOUNDIN_H__
#define SOUNDIN_H__

#include <QObject>
#include <QString>
#include <QDateTime>
#include <QScopedPointer>
#include <QPointer>
#include <QAudioInput>

#include "AudioDevice.hpp"

class QAudioDeviceInfo;
class QAudioInput;

// Gets audio data from sound sample source and passes it to a sink device
class SoundInput
  : public QObject
{
  Q_OBJECT;

public:
  SoundInput (QObject * parent = nullptr)
    : QObject {parent}
    , m_sink {nullptr}
  {
  }

  ~SoundInput ();

  // sink must exist from the start call until the next start call or
  // stop call
  Q_SLOT void start(QAudioDeviceInfo const&, int framesPerBuffer, AudioDevice * sink, unsigned downSampleFactor, AudioDevice::Channel = AudioDevice::Mono);
  Q_SLOT void suspend ();
  Q_SLOT void resume ();
  Q_SLOT void stop ();

  Q_SIGNAL void error (QString message) const;
  Q_SIGNAL void status (QString message) const;

private:
  // used internally
  Q_SLOT void handleStateChanged (QAudio::State) const;

  bool audioError () const;

  QScopedPointer<QAudioInput> m_stream;
  QPointer<AudioDevice> m_sink;
};

#endif
