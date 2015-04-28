/* Other run-time parameters:
 *
 *	Option			Default		Comment
 *	------			-------		-------
 *	devxoff = flt		0.0 inches	Translate image along long edge
 *	devyoff = flt		0.0 inches	Translate image along short edge
 *	devrot = flt		0.0 deg		Rotate image (no effect on DevXOff)
 *	devscale = flt		1.0		Scale image
 *
 *  Translation is done before rotation or scaling. These differ from the normal
 * ima options in that they affect everything uniformly.
 *
 *  	opt = bool		1		Turn optimization on/off
 */

#include "const.h"
#include "types.h"

getspcpar()
{
  IMPORT float 	DevXOff, DevYOff, DevRot, DevScale;
  IMPORT int   	Optimize;

  IMPORT int   	PenMode;
  IMPORT int	PenFat;
  IMPORT int	PenDash;

  IMPORT int	BrushMode;
  IMPORT int	BrushPat;

  IMPORT int	TextFont;
  IMPORT int	TextAngle;
  IMPORT int	TextCenter;

  getpar("devxoff",  "f", &DevXOff);
  getpar("devyoff",  "f", &DevYOff);
  getpar("devrot",   "f", &DevRot);
  getpar("devscale", "f", &DevScale);

  getpar("opt", "b", &Optimize);

  /* These change default values. Useful for testing program, should be removed
   * in final version.
   */
  getpar("penmode",    "d", &PenMode);
  getpar("penfat",     "d", &PenFat);
  getpar("pendash",    "d", &PenDash);

  getpar("brushmode",  "d", &BrushMode);
  getpar("brushpat",   "d", &BrushPat);

  getpar("textfont",   "d", &TextFont);
  getpar("textangle",  "f", &TextAngle);
  getpar("textcenter", "d", &TextCenter);
}

