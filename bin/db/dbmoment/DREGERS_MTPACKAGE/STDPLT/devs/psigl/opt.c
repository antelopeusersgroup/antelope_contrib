/*   This is the peephole optimizer. All PS code emitted by the code generator
 * passes through here. The optimizer removes redundant operations by keeping
 * track of the current LaserWriter state as PostScript would maintain it.
 *   Entry points:
 *
 *	OpenOptimizer()		Call this first
 *	GenX()			Generate statement X
 *	CloseOptimizer()	Clean up
 */

#include "const.h"
#include "types.h"

double fabs();

/* For comparing two floats */
#define SMALL 0.0001
#define eq(f1, f2) (fabs( ((double)(f1)) - ((double)(f2)) ) < SMALL)

IMPORT int Optimize;		/* Flag turns optimization off */

struct GS {			/* Current PostScript graphics state */
  float CurX, CurY;		/* Current pen position */
  float	GrayLevel;		/* 0.0 (black) .. 1.0 (white) */
  float	LineWidth;		/* Device pixels */
  float	StrokeOn1;		/* Dash pattern: Each on/off */
  float	StrokeOff1;		/* is an end value, not a length */
  float	StrokeOn2;		/* value. Special case: all 0's = solid */
  float StrokeOff2;
  float FontID;			/* Current font (PATFONT, 0, 1, 2, 3) */
};
#define PATFONT	 -1.0		/* Special value for pattern font */


struct OS {			/* Other PostScript state info */
  float GsaveLevel;		/* # of nested gsaves */
};

#define UNCOMMITTED 1e37	/* Set state variable to this for unknown */
#define UNCLEVEL    0.9e37	/* Value considered UNCOMMITTED if > UNCLEVEL */



/* -------- Saved state info --------- */

PRIVATE struct GS GS;		/* Current graphics state */
PRIVATE struct OS OS;		/* Other current state info */

PRIVATE struct GS SaveGS;	/* One level stack for gsaves */


/* -------- Utility function: set all state info to UNCOMMITTED --------- */
PRIVATE SetUnknownState()
{
  GS.CurX =
  GS.CurY =
  GS.GrayLevel =
  GS.LineWidth =
  GS.StrokeOn1 = 
  GS.StrokeOff1 =
  GS.StrokeOn2 = 
  GS.StrokeOff2 =
  GS.FontID =
    UNCOMMITTED;

  OS.GsaveLevel =
    UNCOMMITTED;
}


OpenOptimizer()
{
  OpenOutput();
  SetUnknownState();
}


CloseOptimizer()
{
  CloseOutput();
}



/* Output a general PostScript statement. All state info is set to UNCOMMITTED.
 */
Gen(format, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10)
char *format;
{
  OutputF(format, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10);
  SetUnknownState();
}

/*****************************************************************************/
/* Output an arc statement.
 */
GenArc(x, y, radius, ang1, ang2)
double x, y;
double radius;
double ang1, ang2;   /* Degrees */
{
  OutputF("%g %g %g ", x, y, radius);
  OutputF("%g %g arc\n", ang1, ang2);
  GS.CurX = GS.CurY = UNCOMMITTED;
}


/*****************************************************************************/
/* Change fonts. Font ID = 0: Courier
 *		  	   1: Times-Roman
 *			   2: Times-Italic
 *			   3: Times-Bold
 * Use GenPatFont() to get the pattern font.
 */
GenChangeFont(font)
int font;
{
  if (!Optimize || !eq(GS.FontID, (double)font)) {
    OutputF("Font%d setfont\n", font);
  }
  GS.FontID = (double)font;
}

/*****************************************************************************/
/* Output a closepath statement.
 */
GenClosePath()
{
  OutputF("closepath\n");
  GS.CurX = GS.CurY = UNCOMMITTED;
}

/*****************************************************************************/
/* Output an eoclip statement.
 */
GenEoClip()
{
  OutputF("eoclip\n");
}

/*****************************************************************************/
/* Output a clip statement.
 */
GenClip()
{
  OutputF("clip\n");
}

/*****************************************************************************/
/* Output an eofill statement.
 */
GenEoFill()
{
  OutputF("eofill\n");
  GS.CurX = GS.CurY = UNCOMMITTED;
}

/*****************************************************************************/
/* Output a gsave statement.
 */
GenGSave()
{
  OutputF("gsave\n");
  if (!(OS.GsaveLevel >= UNCLEVEL || eq(OS.GsaveLevel, 0.0)))
    OS.GsaveLevel += 1.0;
  else {
    OS.GsaveLevel = 1.0;
    SaveGS = GS;
  }
}

/*****************************************************************************/
/* Output a grestore statement.
 */
GenGRestore()
{
  OutputF("grestore\n");
  if (OS.GsaveLevel > UNCLEVEL || !eq(OS.GsaveLevel, 1.0)) {
    if (!(OS.GsaveLevel > UNCLEVEL))
      OS.GsaveLevel -= 1.0;
    GS.CurX =
    GS.CurY =
    GS.GrayLevel =
    GS.LineWidth =
    GS.StrokeOn1 = 
    GS.StrokeOff1 =
    GS.StrokeOn2 = 
    GS.StrokeOff2 =
    GS.FontID =
      UNCOMMITTED;
  }
  else {
    OS.GsaveLevel = 0.0;
    GS = SaveGS;
  }
}

/*****************************************************************************/
/* Output a genimage statement, using the PatFill procedure and matrix
 * [1 0 0 -1 0 1] (unit map to virtual device coordinates with inversion.)
 */
GenImage(width, height)
int width, height;
{
  OutputF("%d %d 1 [1 0 0 -1 0 1] /PatFill load image\n", width, height);
  GS.CurX = GS.CurY = UNCOMMITTED;
}

/*****************************************************************************/
/* Output a genimagemask statement, using the PatFill procedure and matrix
 * [1 0 0 -1 0 1] (unit map to virtual device coordinates with inversion.)
 */
GenImageMask(width, height)
int width, height;
{
  OutputF("%d %d false [1 0 0 -1 0 1] /PatFill load imagemask\n", width, height);
  GS.CurX = GS.CurY = UNCOMMITTED;
}

/*****************************************************************************/
/* Output a lineto statement. Coordinates are in virtual device pixels.
 */
GenLineTo(newx, newy)
double newx, newy;
{
  if (!Optimize || !eq(newx, GS.CurX) || !eq(newy, GS.CurY))
    OutputF("%g %g lineto\n", newx, newy);
  GS.CurX = newx;
  GS.CurY = newy;
}

/*****************************************************************************/
/* Output a moveto statement. Coordinates are in PostScript units.
 */
GenMoveTo(newx, newy)
double newx, newy;
{
  if (!Optimize || !eq(newx, GS.CurX) || !eq(newy, GS.CurY))
    OutputF("%g %g moveto\n", newx, newy);
  GS.CurX = newx;
  GS.CurY = newy;
}

/*****************************************************************************/
/* Output a newpath statement.
 */
GenNewPath()
{
  OutputF("newpath\n");
  GS.CurX = GS.CurY = UNCOMMITTED;
}

/*****************************************************************************/
/* Change to pattern font.
 */
GenPatFont()
{
  if (!Optimize || !eq(GS.FontID, PATFONT))
    OutputF("PatFont setfont\n");
  GS.FontID = PATFONT;
}

/*****************************************************************************/
/* Output a put statement.
 */
GenPut(dict, key, value)
char *dict;
char *key;	/* Don't prepend a slash */
char *value;
{
  OutputF("%s /%s %s put\n", dict, key, value);
}


/*****************************************************************************/
/* Output a rotate statement. Angle is in degrees.
 */
GenRotate(angle)
double angle;
{
  OutputF("%g rotate\n", angle);
  GS.CurX = GS.CurY = UNCOMMITTED;   /* Saved values are no longer valid */
}

/*****************************************************************************/
/* Output a scale statement.
 */
GenScale(scalex, scaley)
double scalex, scaley;
{
  OutputF("%g %g scale\n", scalex, scaley);
  GS.CurX = GS.CurY = UNCOMMITTED;   /* Saved values are no longer valid */
}

/*****************************************************************************/
/* Output a setdash statement. On and off's are end values in PostScript
 * units. Special case: all 0's means solid.
 */
GenSetDash(on1, off1, on2, off2)
double on1, off1, on2, off2;
{
  if (!Optimize || !eq(on1,  GS.StrokeOn1) ||
  		   !eq(off1, GS.StrokeOff1) ||
		   !eq(on2,  GS.StrokeOn2) ||
		   !eq(off2, GS.StrokeOff2)) {
    if (eq(on1, 0.0) && eq(off1, 0.0) && eq(on2, 0.0) && eq(off2, 0.0))
      OutputF("[] 0 setdash\n");
    else {
      OutputF("[%g %g %g %g] 0 setdash\n", on1, off1 - on1,
      					   on2 - off1, off2 - on2);
    }
  }
  GS.StrokeOn1  = on1;
  GS.StrokeOff1 = off1;
  GS.StrokeOn2  = on2;
  GS.StrokeOff2 = off2;
}

/*****************************************************************************/
/* Output a setgray statement.
 */
GenSetGray(newgray)
double newgray;
{
  if (!Optimize || !eq(newgray, GS.GrayLevel))
    OutputF("%g setgray\n", newgray);
  GS.GrayLevel = newgray;
}

/*****************************************************************************/
/* Output a setlinewidth statement.
 */
GenSetLineWidth(newwidth)
double newwidth;
{
  if (!Optimize || !eq(newwidth, GS.LineWidth))
    OutputF("%g setlinewidth\n", newwidth);
  GS.LineWidth = newwidth;
}

/*****************************************************************************/
/* Output a show statement. No \n, \b's or other non-printing characters
 * allowed. GenShow() takes care of quoting parenthesis and other troublesome
 * characters.
 */
GenShow(s)
char *s;
{
  OutputF("(");
  while (*s) {
    if (*s == '(' || *s == ')' || *s == '\\')
      OutputF("\\");
    OutputF("%c", *s);
    s++;
  }
  OutputF(") show\n");
  GS.CurX = GS.CurY = UNCOMMITTED;
}

/*****************************************************************************/
/* Output a showpage.
 */
GenShowPage()
{
  OutputF("showpage\n");
  SetUnknownState();	/* showpage is destructive. */
}

/*****************************************************************************/
/* Output a store. Both key and value are strings. Don't prepend a / to the key.
 */
GenStore(key, value)
char *key;
char *value;
{
  OutputF("/%s %s store\n", key, value);
}

/*****************************************************************************/
/* Output a stroke.
 */
GenStroke()
{
  OutputF("stroke\n");
  GS.CurX = GS.CurY = UNCOMMITTED;
}

/*****************************************************************************/
/* Do a tile fill. Tiles will be aligned to 32 pixel boundaries, regardless
 * of the input coordinates.
 */
GenTileFill(xmin, ymin, xmax, ymax, tile)
double xmin, ymin;
double xmax, ymax;
int    tile;		/* Font character code */
{
  double temp;
  long   ixmin, iymin;

  if (xmin > xmax) {
    temp = xmin; xmin = xmax; xmax = temp;
  }
  if (ymin > ymax) {
    temp = ymin; ymin = ymax; ymax = temp;
  }

  ixmin = (long)xmin;
  iymin = (long)ymin;
  ixmin -= ixmin % PATSIZE;
  iymin -= iymin % PATSIZE;

  /*:: debug */
  OutputF("save\n");
  OutputF("TStr 0 %d put\n", tile);
  OutputF("%ld %d %g {\n", iymin, PATSIZE, ymax);
  OutputF("  %ld exch moveto\n", ixmin);
  OutputF("  %ld %d %g {\n", ixmin, PATSIZE, xmax);
  OutputF("    pop TStr show\n", tile);
  OutputF("  } bind for\n");
  OutputF("} bind for\n");
  GS.CurX = GS.CurY = UNCOMMITTED;
  /*:: debug */
  OutputF("restore\n");
#ifdef VM
  OutputF("@vmstatus\n");
#endif	/* VM */
}

/*****************************************************************************/
/* Output a translate.
 */
GenTranslate(addx, addy)
double addx, addy;
{
  OutputF("%g %g translate\n", addx, addy);
  GS.CurX = GS.CurY = UNCOMMITTED;
}

/*****************************************************************************/
/* Output a comment.
 */
GenComment(str)
char *str;
{
  OutputF("%%%s\n", str);
}

