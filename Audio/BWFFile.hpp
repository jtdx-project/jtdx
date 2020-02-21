#ifndef BWF_FILE_HPP__
#define BWF_FILE_HPP__

#include <array>

#include <QFile>
#include <QMap>
#include <QByteArray>

#include "pimpl_h.hpp"

class QObject;
class QString;
class QAudioFormat;

//
// BWFFile - Broadcast Wave Format File (a.k.a. WAV file)
//
// The  BWF file  format is  a  backward compatible  variation of  the
// Microsoft  WAV file  format. It  contains  an extra  chunk with  id
// 'bext' that contains metadata defined by the EBU in:
//
//  https://tech.ebu.ch/docs/tech/tech3285.pdf
//
// Also relevant is the recommendation document:
//
//  https://tech.ebu.ch/docs/r/r098.pdf
//
// which suggests a format to the free text coding history field.
//
// This class also supports the LIST-INFO chunk type which also allows
// metadata to be  added to a WAV  file, the defined INFO  tag ids are
// documented here:
//
//  http://bwfmetaedit.sourceforge.net/listinfo.html
//
// These  ids  are not  enforced  but  they  are recommended  as  most
// operating systems and audio applications  recognize some or more of
// them. Notably Microsoft Windows is not one of the operating systems
// that  does :(  In fact  there seems  to be  no documented  metadata
// tagging format that Windows Explorer recognizes.
//
// Changes to  the 'bext' fields  and the LIST-INFO dictionary  may be
// made right up  until the file is closed as  the relevant chunks are
// saved to the end of the file after the end of the sample data.
//
// This class emulates the QFile class, in fact it uses a QFile object
// instance internally and forwards many of its operations directly to
// it.
//
// BWFFile  is a  QIODevice subclass  and the  implementation provides
// access to  the audio sample  data contained in  the BWF file  as if
// only that data were  in the file. I.e. the first  sample is at file
// offset zero  and the  size of the  file is the  size of  the sample
// data.  The headers,  trailers and  metadata are  hidden but  can be
// accessed by the operations below.
//
class BWFFile
  : public QIODevice
{
  Q_OBJECT
public:
  using FileHandleFlags = QFile::FileHandleFlags;
  using Permissions = QFile::Permissions;
  using FileError = QFile::FileError;
  using MemoryMapFlags = QFile::MemoryMapFlags;
  using InfoDictionary = QMap<std::array<char, 4>, QByteArray>;
  using UMID = std::array<quint8, 64>;

  explicit BWFFile (QAudioFormat const&, QObject * parent = nullptr);
  explicit BWFFile (QAudioFormat const&, QString const& name,
                    QObject * parent = nullptr);

  // The  InfoDictionary should  contain  valid  WAV format  LIST-INFO
  // identifiers as keys, a list of them can be found here:
  //
  // http://bwfmetaedit.sourceforge.net/listinfo.html
  //
  // For  files  opened for  ReadOnly  access  the dictionary  is  not
  // written to  the file.  For  files opened ReadWrite,  any existing
  // LIST-INFO tags will  be merged into the dictionary  when the file
  // is opened and if the file  is modified the merged dictionary will
  // be written back to the file.
  //
  // Note that the sample  data may no be in the  native endian, it is
  // the   callers   responsibility   to  do   any   required   endian
  // conversions. The  internal data is  always in native  endian with
  // conversions  being handled  automatically. Use  the BWF::format()
  // operation     to    access     the    format     including    the
  // QAudioFormat::byteOrder()  operation to  determine the  data byte
  // ordering.
  //
  explicit BWFFile (QAudioFormat const&, QString const& name,
                    InfoDictionary const&, QObject * parent = nullptr);

  ~BWFFile ();
  QAudioFormat const& format () const;
  InfoDictionary& list_info ();

  //
  // Broadcast Audio Extension fields
  //
  // If any of these modifiers are  called then a "bext" chunk will be
  // written to the file if the  file is writeable and the sample data
  // is modified.
  //
  enum class BextVersion : quint16 {v_0, v_1, v_2};
  BextVersion bext_version () const;
  void bext_version (BextVersion = BextVersion::v_2);

  QByteArray bext_description () const;
  void bext_description (QByteArray const&); // max 256 bytes

  QByteArray bext_originator () const;
  void bext_originator (QByteArray const&);        // max 32 bytes

  QByteArray bext_originator_reference () const;
  void bext_originator_reference (QByteArray const&); // max 32 bytes

  QDateTime bext_origination_date_time () const;
  void bext_origination_date_time (QDateTime const&); // 1s resolution

  quint64 bext_time_reference () const;
  void bext_time_reference (quint64); // samples since midnight at start

  UMID bext_umid () const; // bext version >= 1 only
  void bext_umid (UMID const&);

  quint16 bext_loudness_value () const;
  void bext_loudness_value (quint16); // bext version >= 2 only

  quint16 bext_loudness_range () const;
  void bext_loudness_range (quint16); // bext version >= 2 only

  quint16 bext_max_true_peak_level () const;
  void bext_max_true_peak_level (quint16); // bext version >= 2 only

  quint16 bext_max_momentary_loudness () const;
  void bext_max_momentary_loudness (quint16); // bext version >= 2 only

  quint16 bext_max_short_term_loudness () const;
  void bext_max_short_term_loudness (quint16); // bext version >= 2 only

  QByteArray bext_coding_history () const;
  void bext_coding_history (QByteArray const&); // See EBU R 98


  // Emulate QFile interface
  bool open (OpenMode) override;
  bool open (FILE *, OpenMode, FileHandleFlags = QFile::DontCloseHandle);
  bool open (int fd, OpenMode, FileHandleFlags = QFile::DontCloseHandle);
  bool copy (QString const& new_name);
  bool exists () const;
  bool link (QString const& link_name);
  bool remove ();
  bool rename (QString const& new_name);
  void setFileName (QString const& name);
  QString symLinkTarget () const;
  QString fileName () const;
  Permissions permissions () const;

  // Resize is of the sample data portion, header and trailer chunks
  // are excess to the given size
  bool resize (qint64 new_size);

  bool setPermissions (Permissions permissions);
  FileError error () const;
  bool flush ();
  int handle () const;

  // The mapping offset is relative to the start of the sample data
  uchar * map (qint64 offset, qint64 size,
               MemoryMapFlags = QFile::NoOptions);
  bool unmap (uchar * address);

  void unsetError ();


  //
  // QIODevice implementation
  //

  // The size returned is of the sample data only, header and trailer
  // chunks are hidden and handled internally
  qint64 size () const override;

  bool isSequential () const override;

  // The reset  operation clears the  'bext' and LIST-INFO as  if they
  // were  never supplied.  If the  file  is writable  the 'bext'  and
  // LIST-INFO chunks will not be  written making the resulting file a
  // lowest common denominator WAV file.
  bool reset () override;

  // Seek offsets are relative to the start of the sample data
  bool seek (qint64) override;

  void close () override;

protected:
  qint64 readData (char * data, qint64 max_size) override;
  qint64 writeData (char const* data, qint64 max_size) override;

private:
  class impl;
  pimpl<impl> m_;
};

#endif
