// last time modified by Igor UA3DJY on 20200128

#include "Modulator.hpp"
#include <limits>
#include <qmath.h>
#if QT_VERSION >= QT_VERSION_CHECK(5, 15, 0)
#include <QRandomGenerator>
#endif
#include <QDebug>
#include "mainwindow.h"
#include "soundout.h"
#include "commons.h"

#include "moc_Modulator.cpp"

extern float gran();		// Noise generator (for tests only)

#define RAMP_INCREMENT 64  // MUST be an integral factor of 2^16

#if defined (WSJT_SOFT_KEYING)
# define SOFT_KEYING WSJT_SOFT_KEYING
#else
# define SOFT_KEYING 1
#endif

double constexpr Modulator::m_twoPi;

Modulator::Modulator (unsigned frameRate, double periodLengthInSeconds, JTDXDateTime * jtdxtime, QObject * parent)
  : AudioDevice {parent}
  , m_quickClose {false}
  , m_phi {0.0}
  , m_toneSpacing {0.0}
  , m_fSpread {0.0}
  , m_period {periodLengthInSeconds}
  , m_frameRate {frameRate}
  , m_state {Idle}
  , m_tuning {false}
  , m_cwLevel {false}
  , m_j0 {-1}
  , m_toneFrequency0 {1500.0}
  , m_jtdxtime {jtdxtime}
  , debug_file_ {QDir(QStandardPaths::writableLocation (QStandardPaths::DataLocation)).absoluteFilePath ("jtdx_debug.txt").toStdString()}
{
}

void Modulator::start (unsigned symbolsLength, double framesPerSymbol,
                       double frequency, double toneSpacing,
                       SoundOutput * stream, Channel channel,
                       bool synchronize, double dBSNR, double TRperiod)
{
  QThread::currentThread()->setPriority(QThread::HighPriority);
  Q_ASSERT (stream);
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");
  fprintf (pFile,"\n");
  fclose (pFile);
#endif

// Time according to this computer which becomes our base time
  qint64 ms0 = m_jtdxtime->currentMSecsSinceEpoch2() % 86400000;
//  qDebug() << "ModStart" << m_jtdxtime->currentDateTimeUtc().toString("hh:mm:ss.sss");
  unsigned mstr = ms0 % int(1000.0*m_period); // ms into the nominal Tx start time
  if (m_state != Idle) stop ();
  m_quickClose = false;
  m_symbolsLength = symbolsLength;
  m_isym0 = std::numeric_limits<unsigned>::max (); // big number
  m_frequency0 = 0.;
  m_phi = 0.;
  m_addNoise = dBSNR < 0.;
  m_nsps = framesPerSymbol;
  m_frequency = frequency;
  m_amp = std::numeric_limits<qint16>::max ();
  m_toneSpacing = toneSpacing;
  m_TRperiod=TRperiod;
  unsigned delay_ms=1000;
  if(m_nsps==1920) delay_ms=500;   //FT8
  else if(m_nsps==576) {
    delay_ms=500;   //FT4
  }
  // noise generator parameters
  if (m_addNoise) {
    m_snr = qPow (10.0, 0.05 * (dBSNR - 6.0));
    m_fac = 3000.0;
    if (m_snr > 1.0) m_fac = 3000.0 / m_snr;
  }

  // round up to an exact portion of a second that allows for startup delays
  //m_ic = (mstr / delay_ms) * m_frameRate * delay_ms / 1000;
  auto mstr2 = mstr - delay_ms;
  if (mstr <= delay_ms) {
    m_ic = 0;
  } else {
    m_ic = mstr2 * (m_frameRate / 1000);
  }
  m_silentFrames = 0;
  // calculate number of silent frames to send
  if (m_ic == 0 && synchronize && !m_tuning)	{
    m_silentFrames = m_frameRate / (1000 / delay_ms) - (mstr * (m_frameRate / 1000));
  }
//  printf ("%s(%0.1f) Modulator startdelay_ms=%d m_frameRate=%d mstr=%d mstr2 = %d m_ic=%d m_silentFrames=%lld \n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),delay_ms,m_frameRate,mstr,mstr2,m_ic,m_silentFrames);
#if JTDX_DEBUG_TO_FILE
  pFile = fopen (debug_file_.c_str(),"a");  
  fprintf (pFile,"%s(%0.1f) Modulator startdelay_ms=%d m_frameRate=%d mstr=%d mstr2 = %d m_ic=%d m_silentFrames=%lld \n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),delay_ms,m_frameRate,mstr,mstr2,m_ic,m_silentFrames);
  fclose (pFile);
#endif
  initialize (QIODevice::ReadOnly, channel);
  Q_EMIT stateChanged ((m_state = (synchronize && m_silentFrames) ?
                        Synchronizing : Active));
  m_stream = stream;
  if (m_stream) m_stream->restart (this);
}

void Modulator::tune (bool newState)
{
//  printf("%s(%0.1f) Modulator tune %d ->%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),m_tuning,newState);
#if JTDX_DEBUG_TO_FILE
  FILE * pFile = fopen (debug_file_.c_str(),"a");  
  fprintf (pFile,"%s(%0.1f) Modulator tune %d ->%d\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),m_tuning,newState);
  fclose (pFile);
#endif
  m_tuning = newState;
  if (!m_tuning) stop (true);
}

void Modulator::stop (bool quick)
{
  m_quickClose = quick;
  close ();
}

void Modulator::close ()
{
  if(m_stream) {
    if(m_quickClose) { m_stream->reset (); }
    else { m_stream->stop (); }
  }
  if(m_state != Idle) Q_EMIT stateChanged ((m_state = Idle));
  AudioDevice::close ();
}

qint64 Modulator::readData (char * data, qint64 maxSize)
{
  double toneFrequency=1500.0;
  if(m_nsps==6) {
    toneFrequency=1000.0;
    m_frequency=1000.0;
    m_frequency0=1000.0;
  }
  if(maxSize==0) return 0;
  Q_ASSERT (!(maxSize % qint64 (bytesPerFrame ()))); // no torn frames
  Q_ASSERT (isOpen ());

  qint64 numFrames (maxSize / bytesPerFrame ());
  qint16 * samples (reinterpret_cast<qint16 *> (data));
  qint16 * end (samples + numFrames * (bytesPerFrame () / sizeof (qint16)));
  qint64 framesGenerated (0);

  switch (m_state)
    {
    case Synchronizing:
      {
        if (m_silentFrames)	{  // send silence up to first second
          framesGenerated = qMin (m_silentFrames, numFrames);
          for ( ; samples != end; samples = load (0, samples)) { // silence
          }
          m_silentFrames -= framesGenerated;
          return framesGenerated * bytesPerFrame ();
        }

        Q_EMIT stateChanged ((m_state = Active));
        m_cwLevel = false;
        m_ramp = 0;		// prepare for CW wave shaping
      }
      // fall through

    case Active:
      {
        unsigned int isym=0;
        qint16 sample=0;
        if(!m_tuning) isym=m_ic/(4.0*m_nsps);            // Actual fsample=48000
		bool slowCwId=((isym >= m_symbolsLength) && (icw[0] > 0));
        m_nspd=2560;                 // 22.5 WPM

        if(m_TRperiod > 16.0 && slowCwId) {     // Transmit CW ID?
          m_dphi = m_twoPi*m_frequency/m_frameRate;
          unsigned ic0 = m_symbolsLength * 4 * m_nsps;
          unsigned j(0);

          while (samples != end) {
            j = (m_ic - ic0)/m_nspd + 1; // symbol of this sample
            bool level {bool (icw[j])};
            m_phi += m_dphi;
            if (m_phi > m_twoPi) m_phi -= m_twoPi;
            sample=0;
            float amp=32767.0;
            float x=0.0;
            if(m_ramp!=0) {
              x=qSin(float(m_phi));
              if(SOFT_KEYING) {
                amp=qAbs(qint32(m_ramp));
                if(amp>32767.0) amp=32767.0;
              }
              sample=round(amp*x);
            }
            if (int (j) <= icw[0] && j < NUM_CW_SYMBOLS) { // stop condition
              samples = load (postProcessSample (sample), samples);
              ++framesGenerated;
              ++m_ic;
            } else {
//              printf("%s(%0.1f) Modulator Idle1 %lld frames generated\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),framesGenerated);
#if JTDX_DEBUG_TO_FILE
              FILE * pFile = fopen (debug_file_.c_str(),"a");  
              fprintf (pFile,"%s(%0.1f) Modulator Idle1 %lld frames generated\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),framesGenerated);
              fclose (pFile);
#endif
              Q_EMIT stateChanged ((m_state = Idle));
              return framesGenerated * bytesPerFrame ();
            }

            // adjust ramp
            if ((m_ramp != 0 && m_ramp != std::numeric_limits<qint16>::min ()) || level != m_cwLevel) {
              // either ramp has terminated at max/min or direction has changed
              m_ramp += RAMP_INCREMENT; // ramp
            }
            m_cwLevel = level;
          }
          return framesGenerated * bytesPerFrame ();
        } //End of code for CW ID

        double const baud (12000.0 / m_nsps);
        // fade out parameters (no fade out for tuning)
        unsigned int i0,i1;
        if(m_tuning) {
          i1 = i0 = 9999 * m_nsps;
        } else {
          i0=(m_symbolsLength - 0.017) * 4.0 * m_nsps;
          i1= m_symbolsLength * 4.0 * m_nsps;
        }

        sample=0;
        for (unsigned i = 0; i < numFrames && m_ic <= i1; ++i) {
          if(m_TRperiod > 16.0 || m_tuning) {
            isym=0;
            if(!m_tuning) isym=m_ic / (4.0 * m_nsps);         //Actual fsample=48000
            if (isym != m_isym0 || m_frequency != m_frequency0) {
              if(itone[0]>=100) {
                m_toneFrequency0=itone[0];
              } else {
                if(m_toneSpacing==0.0) {
                  m_toneFrequency0=m_frequency + itone[isym]*baud;
                } else {
                  m_toneFrequency0=m_frequency + itone[isym]*m_toneSpacing;
                }
              }
//            qDebug() << "B" << m_ic << numFrames << isym << itone[isym] << toneFrequency0 << m_nsps;
              m_dphi = m_twoPi * m_toneFrequency0 / m_frameRate;
              m_isym0 = isym;
              m_frequency0 = m_frequency;         //???
            }

            int j=m_ic/480;
            if(m_fSpread>0.0 and j!=m_j0) {
#if QT_VERSION >= QT_VERSION_CHECK(5, 15, 0)
            float x1=QRandomGenerator::global ()->generateDouble ();
            float x2=QRandomGenerator::global ()->generateDouble ();
#else
            float x1=(float)qrand()/RAND_MAX;
            float x2=(float)qrand()/RAND_MAX;
#endif
              toneFrequency = m_toneFrequency0 + 0.5*m_fSpread*(x1+x2-1.0);
              m_dphi = m_twoPi * toneFrequency / m_frameRate;
              m_j0=j;
            }

            m_phi += m_dphi;
            if (m_phi > m_twoPi) m_phi -= m_twoPi;
            //ramp for first tone
            if (m_ic==0) m_amp = m_amp * 0.008144735;
            if (m_ic > 0 and  m_ic < 191) m_amp = m_amp / 0.975;
            //ramp for last tone
            if (m_ic > i0) m_amp = 0.99 * m_amp;
            if (m_ic > i1) m_amp = 0.0;
            sample=qRound(m_amp*qSin(m_phi));
          }
          //transmit from a precomputed FT8 wave[] array:
          if(!m_tuning and (m_toneSpacing < 0.0)) { m_amp=32767.0; sample=qRound(m_amp*foxcom_.wave[m_ic]); }
          samples = load (postProcessSample (sample), samples);
          ++framesGenerated; ++m_ic;
        }

        if (m_amp == 0.0) { // TODO G4WJS: compare double with zero might not be wise
          if (icw[0] == 0) {
            // no CW ID to send
//            printf("%s(%0.1f) Modulator Idle2 %lld frames generated\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),framesGenerated);
#if JTDX_DEBUG_TO_FILE
            FILE * pFile = fopen (debug_file_.c_str(),"a");  
            fprintf (pFile,"%s(%0.1f) Modulator Idle2 %lld frames generated\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),framesGenerated);
            fclose (pFile);
#endif
            Q_EMIT stateChanged ((m_state = Idle));
            return framesGenerated * bytesPerFrame ();
          }
          m_phi = 0.0;
        }

        m_frequency0 = m_frequency;
        // done for this chunk - continue on next call
        if (samples != end && framesGenerated) {
//          printf("%s(%0.1f) Modulator Idle3 %lld frames generated\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),framesGenerated);
#if JTDX_DEBUG_TO_FILE
          FILE * pFile = fopen (debug_file_.c_str(),"a");  
          fprintf (pFile,"%s(%0.1f) Modulator Idle3 %lld frames generated\n",m_jtdxtime->currentDateTimeUtc2().toString("hh:mm:ss.zzz").toStdString().c_str(),m_jtdxtime->GetOffset(),framesGenerated);
          fclose (pFile);
#endif
        }
        while (samples != end) { // pad block with silence
          samples = load (0, samples);
          ++framesGenerated;
        }
        return framesGenerated * bytesPerFrame ();
      }
      // fall through

    case Idle:
      break;
    }

  Q_ASSERT (Idle == m_state);
  return 0;
}

qint16 Modulator::postProcessSample (qint16 sample) const
{
  if (m_addNoise) {  // Test frame, we'll add noise
    qint32 s = m_fac * (gran () + sample * m_snr / 32768.0);
    if (s > std::numeric_limits<qint16>::max ()) {
      s = std::numeric_limits<qint16>::max ();
    }
    if (s < std::numeric_limits<qint16>::min ()) {
      s = std::numeric_limits<qint16>::min ();
    }
    sample = s;
  }
  return sample;
}
