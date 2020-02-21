#ifndef W_F_PALETTE_HPP__
#define W_F_PALETTE_HPP__

#include <QMetaType>
#include <QList>
#include <QVector>
#include <QColor>

class QString;

//
// Class WFPalette
//
//	Encapulates  a waterfall palette  description.  A  colour gradient
//	over 256 intervals is described  by a list of RGB colour triplets.
//	The list of  colours are use to interpolate  the full 256 interval
//	waterfall colour gradient.
//
// Responsibilities
//
//	Construction from  a string which is  a path to  a file containing
//	colour  descriptions  in  the   form  rrr;ggg;bbb  on  up  to  256
//	consecutive lines, where rrr, ggg and, bbb are integral numbers in
//	the range 0<=n<256.
//
//	Construction from a list of QColor instances.  Up to the first 256
//	list elements are used.
//
//	Includes a design GUI to create or adjust a WFPalette.
//
class WFPalette
{
public:
  using Colours = QList<QColor>;

  WFPalette () = default;
  explicit WFPalette (Colours const&);
  explicit WFPalette (QString const& file_path);
  WFPalette (WFPalette const&) = default;
  WFPalette& operator = (WFPalette const&) = default;

  Colours colours () const {return colours_;}

  // interpolate a gradient over 256 steps
  QVector<QColor> interpolate () const;

  // returns true if colours have been modified
  bool design ();

private:
  Colours colours_;
};

Q_DECLARE_METATYPE (WFPalette::Colours);

#endif
