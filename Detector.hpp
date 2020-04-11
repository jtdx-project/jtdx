// This source code file was last time modified by Igor UA3DJY on 20190927
// All changes are shown in the patch file coming together with the full JTDX source code.

#ifndef DETECTOR_HPP__
#define DETECTOR_HPP__

#include "AudioDevice.hpp"
#include "JTDXDateTime.h"

#include <QScopedArrayPointer>

//
// output device that distributes data in predefined chunks via a signal
//
// the underlying device for this abstraction is just the buffer that
// stores samples throughout a receiving period
//
class Detector : public AudioDevice
{
  Q_OBJECT;

public:
  //
  // if the data buffer were not global storage and fixed size then we
  // might want maximum size passed as constructor arguments
  //
  // we down sample by a factor of 4
  //
  // the samplesPerFFT argument is the number after down sampling
  //
  Detector (unsigned frameRate, double periodLengthInSeconds, JTDXDateTime * jtdxtime, unsigned downSampleFactor = 4, QObject * parent = 0);

  void setPeriod(double p) {m_period=p;}
  bool reset () override;

  Q_SIGNAL void framesWritten (qint64) const;
  Q_SLOT void setBlockSize (unsigned);

protected:
  qint64 readData (char * /* data */, qint64 /* maxSize */) override
  {
    return -1;			// we don't produce data
  }

  qint64 writeData (char const * data, qint64 maxSize) override;

private:
  void clear ();		// discard buffer contents

  unsigned m_frameRate;
  double m_period;
  unsigned m_downSampleFactor;
  qint32 m_samplesPerFFT;	// after any down sampling
  qint32 m_ns;
  static size_t const max_buffer_size {7 * 512};
  QScopedArrayPointer<short> m_buffer; // de-interleaved sample buffer
  // big enough for all the
  // samples for one increment of
  // data (a signals worth) at
  // the input sample rate
  unsigned m_bufferPos;
  JTDXDateTime * m_jtdxtime;
};

#endif
