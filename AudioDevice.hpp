// last time modified by Igor UA3DJY on 20200125

#ifndef AUDIODEVICE_HPP__
#define AUDIODEVICE_HPP__

#include <QIODevice>

class QDataStream;

//
// abstract base class for audio devices
//
class AudioDevice : public QIODevice
{
public:
  enum Channel {Mono, Left, Right, Both}; // these are mapped to combobox index so don't change

  static char const * toString (Channel c)
  {
    return Mono == c ? "Mono" : Left == c ? "Left" : Right == c ? "Right" : "Both";
  }

  static Channel fromString (QString const& str)
  {
    QString s (str.toCaseFolded ().trimmed ().toLatin1 ());
    return "both" == s ? Both : "right" == s ? Right : "left" == s ? Left : Mono;
  }

  bool initialize (OpenMode mode, Channel channel);

  bool isSequential () const override {return true;}

  size_t bytesPerFrame () const {return sizeof (qint32) * (Mono == m_channel ? 1 : 2);}

  Channel channel () const {return m_channel;}

protected:
  AudioDevice (QObject * parent = 0)
    : QIODevice (parent)
  {
  }

  void store (char const * source, size_t numFrames, qint32 * dest)
  {
    qint32 const * begin (reinterpret_cast<qint32 const *> (source));
    for ( qint32 const * i = begin; i != begin + numFrames * (bytesPerFrame () / sizeof (qint32)); i += bytesPerFrame () / sizeof (qint32))
      {
	switch (m_channel)
	  {
	  case Mono:
	    *dest++ = *i;
	    break;

	  case Right:
	    *dest++ = *(i + 1);
	    break;

	  case Both:		// should be able to happen but if it
				// does we'll take left
	    Q_ASSERT (Both == m_channel);
	  case Left:
	    *dest++ = *i;
	    break;
	  }
      }
  }

  qint32 * load (qint32 const sample, qint32 * dest)
  {
    switch (m_channel)
      {
      case Mono:
	*dest++ = sample;
	break;

      case Left:
	*dest++ = sample;
	*dest++ = 0;
	break;

      case Right:
	*dest++ = 0;
	*dest++ = sample;
	break;

      case Both:
	*dest++ = sample;
	*dest++ = sample;
	break;
      }
    return dest;
  }

private:
  Channel m_channel;
};

Q_DECLARE_METATYPE (AudioDevice::Channel);

#endif
