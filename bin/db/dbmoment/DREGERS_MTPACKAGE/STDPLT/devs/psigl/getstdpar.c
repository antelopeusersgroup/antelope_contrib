/* Standard run-time options:
 *
 *	Option:		Default: 	Comment:
 *	-------		--------	--------
 *	scale=flt	1.0		scales lines & poly, but not text or symbols
 *	xscale=flt	1.0		scale x direction (affected by scale)
 *	yscale=flt	1.0		scale y direction (affected by scale)
 *	xorig=flt	0.0 inch	x-origin (not affected by scale)
 *	yorig=flt	0.0 inch	y-origin (not affected by scale)
 *	pagerot=flt	0.0 deg		page rotation (rounds to 90 degree incr.)
 *	textscale=flt 	1.0		Scale text
 *	symscale=flt 	1.0		Scale symbols
 *	xpagelen=flt	11.0 inches	Page width for page rotation purposes
 *	ypagelen=flt	8.5 inches	Page height for page rotation purposes
 *	fat=int		0		Adds to all line width values
 *	grid=flt			No effect, compatibility only
 *	label=str			No effect, compatibility only
 *	pause=int			No effect, compatibility only
 *	erase=bool			No effect, compatibility only
 *	window=bool			No effect, compatibility only
 *	dolabel=bool			No effect, compatibility only
 */

#include "const.h"
#include "types.h"

getstdpar()
{
  IMPORT float	PixInch;
  IMPORT float	XPageLen, YPageLen;	/* Virtual pixels */
  IMPORT int	PageRot;		/* 0, 1, 2, 3 = 0, 90, 180, 270 */
  IMPORT float	TextScale;		/* Runtime text scale option */
  IMPORT float	TextSize;		/* Plot time text size (inches) */
  IMPORT float	SymScale;		/* Runtime symbol scale option */
  IMPORT float	XOrigin, YOrigin;	/* Virtual pixels */
  IMPORT float	Scale;
  IMPORT float	XScale, YScale;
  IMPORT int	PenFat;
  IMPORT int	FatBase;

  float ftemp;
  int   dtemp;

  if (getpar("xpagelen", "f", &ftemp))
    XPageLen = ftemp * PixInch;
  if (getpar("ypagelen", "f", &ftemp))
    YPageLen = ftemp * PixInch;

  if (getpar("pagerot", "f", &ftemp)) {
    while(ftemp < 0.0)    ftemp += 360.0;
    while(ftemp >= 360.0) ftemp -= 360.0;
    PageRot = (int)((ftemp + 45.0) / 90.0);
  }

  getpar("textscale", "f", &TextScale);
  TextSize *= TextScale;

  getpar("symscale", "f", &SymScale);

  if (getpar("xorig", "f", &ftemp))
    XOrigin = ftemp * PixInch;
  if (getpar("yorig", "f", &ftemp))
    YOrigin = ftemp * PixInch;

  getpar("scale", "f", &Scale);	   /* Scale and XScale are kept separate, */
  getpar("xscale", "f", &XScale);  /* must be multiplied at plot time. */
  getpar("yscale", "f", &YScale);

  getpar("fat", "d", &FatBase);
  PenFat += FatBase;

  /* Rest of these are just for compatibility. */
  getpar("grid",    "f", &ftemp);
  getpar("label",   "d", &dtemp);
  getpar("pause",   "d", &dtemp);
  getpar("erase",   "b", &dtemp);
  getpar("window",  "b", &dtemp);
  getpar("dolabel", "b", &dtemp);
}

