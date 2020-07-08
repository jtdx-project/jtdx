#include "Detector.hpp"
#include <QtAlgorithms>
#include <QDebug>
#include <math.h>
#include "commons.h"
#include "moc_Detector.cpp"

extern "C" {
  void   fil4_(qint16*, qint32*, qint16*, qint32*, float*);
}
extern dec_data dec_data;

Detector::Detector (unsigned frameRate, double periodLengthInSeconds,
                    JTDXDateTime * jtdxtime, unsigned downSampleFactor, QObject * parent)
  : AudioDevice (parent)
  , m_frameRate (frameRate)
  , m_period (periodLengthInSeconds)
  , m_downSampleFactor (downSampleFactor)
  , m_samplesPerFFT {max_buffer_size}
  , m_ns (999)
  , m_buffer ((downSampleFactor > 1) ?
              new short [max_buffer_size * downSampleFactor] : nullptr)
  , m_bufferPos (0)
  , m_jtdxtime {jtdxtime}
{
  (void)m_frameRate;            // quell compiler warning
  clear ();
}

void Detector::setBlockSize (unsigned n)
{
  m_samplesPerFFT = n;
}

bool Detector::reset ()
{
  clear ();
  // don't call base call reset because it calls seek(0) which causes
  // a warning
  return isOpen ();
}

void Detector::clear ()
{
  // set index to roughly where we are in time (1ms resolution)
  // qint64 now (QDateTime::currentMSecsSinceEpoch ());
  // unsigned msInPeriod ((now % 86400000LL) % (m_period * 1000));
  // dec_data.params.kin = qMin ((msInPeriod * m_frameRate) / 1000, static_cast<unsigned> (sizeof (dec_data.d2) / sizeof (dec_data.d2[0])));
  dec_data.params.kin = 0;
  m_bufferPos = 0;

  // fill buffer with zeros (G4WJS commented out because it might cause decoder hangs)
  // qFill (dec_data.d2, dec_data.d2 + sizeof (dec_data.d2) / sizeof (dec_data.d2[0]), 0);
}

qint64 Detector::writeData (char const * data, qint64 maxSize)
{
  static unsigned mstr0=999999;
  qint64 ms0 = m_jtdxtime -> currentMSecsSinceEpoch2() % 86400000;
  unsigned mstr = ms0 % int(1000.0*m_period); // ms into the nominal Tx start time
  if(mstr < mstr0) {              //When mstr has wrapped around to 0, restart the buffer
    dec_data.params.kin = 0;
    m_bufferPos = 0;
//    printf("%s(%0.1f) reset buffer mstr:%d mstr0:%d maxSize:%lld\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),mstr,mstr0,maxSize);
  }
  mstr0=mstr;

  // no torn frames
  Q_ASSERT (!(maxSize % static_cast<qint64> (bytesPerFrame ())));
  // these are in terms of input frames (not down sampled)
  size_t framesAcceptable ((sizeof (dec_data.d2) /
                            sizeof (dec_data.d2[0]) - dec_data.params.kin) * m_downSampleFactor);
  size_t framesAccepted (qMin (static_cast<size_t> (maxSize /
                                                    bytesPerFrame ()), framesAcceptable));

  if (framesAccepted < static_cast<size_t> (maxSize / bytesPerFrame ())) {
    qDebug () << "dropped " << maxSize / bytesPerFrame () - framesAccepted
                << " frames of data on the floor!"
                << dec_data.params.kin << mstr;
    }

    for (unsigned remaining = framesAccepted; remaining; ) {
      size_t numFramesProcessed (qMin (m_samplesPerFFT *
                                       m_downSampleFactor - m_bufferPos, remaining));

      if(m_downSampleFactor > 1) {
        store (&data[(framesAccepted - remaining) * bytesPerFrame ()],
               numFramesProcessed, &m_buffer[m_bufferPos]);
        m_bufferPos += numFramesProcessed;

        if(m_bufferPos==m_samplesPerFFT*m_downSampleFactor) {
          qint32 framesToProcess (m_samplesPerFFT * m_downSampleFactor);
          qint32 framesAfterDownSample (m_samplesPerFFT);
          if(m_downSampleFactor > 1 && dec_data.params.kin>=0 &&
             dec_data.params.kin < (NTMAX*12000 - framesAfterDownSample)) {
            fil4_(&m_buffer[0], &framesToProcess, &dec_data.d2[dec_data.params.kin],
                  &framesAfterDownSample, &dec_data.dd2[dec_data.params.kin]);
            dec_data.params.kin += framesAfterDownSample;
          } else {
            // qDebug() << "framesToProcess     = " << framesToProcess;
            // qDebug() << "dec_data.params.kin = " << dec_data.params.kin;
            // qDebug() << "secondInPeriod      = " << secondInPeriod();
            // qDebug() << "framesAfterDownSample" << framesAfterDownSample;
          }
//    printf("%s(%0.1f) frameswritten %d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),dec_data.params.kin);
          Q_EMIT framesWritten (dec_data.params.kin);
//    printf("%s(%0.1f) frameswritten done\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset());
          m_bufferPos = 0;
        }

      } else {
        store (&data[(framesAccepted - remaining) * bytesPerFrame ()],
               numFramesProcessed, &dec_data.d2[dec_data.params.kin]);
        m_bufferPos += numFramesProcessed;
        dec_data.params.kin += numFramesProcessed;
        if (m_bufferPos == static_cast<unsigned> (m_samplesPerFFT)) {
          Q_EMIT framesWritten (dec_data.params.kin);
          m_bufferPos = 0;
        }
      }
      remaining -= numFramesProcessed;
    }



  return maxSize;    // we drop any data past the end of the buffer on
  // the floor until the next period starts
}
