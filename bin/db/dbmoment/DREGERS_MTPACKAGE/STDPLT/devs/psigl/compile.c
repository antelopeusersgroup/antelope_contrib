#include <stdio.h>
#include "const.h"
#include "../../h/igl.h"
#include "types.h"

IMPORT	int intbytes;

/* CompileCmd(code, name, format). The IGL code has already been checked
 * for validity. Format is an integer from 0 to 15 indicating one of the
 * IGL parse formats. The name might be useful in an error message. CompileCmd()
 * leaves the input pointer at the start of the next command.
 */
CompileCmd(code, name, format)
int	code;
char	*name;
int	format;
{
  switch(code) {
    /* --- Vector commands --- */
    case IGL_MOVE: {
        IMPORT float XCur, YCur;
	GetXY(&XCur, &YCur);
      }
      break;
    case IGL_DRAW: {
	IMPORT float XCur, YCur;
	float  x, y;
	GetXY(&x, &y);
	DashLine(XCur, YCur, x, y); 
	XCur = x;
	YCur = y;
      }
      break;
    case IGL_LINE: {
	IMPORT float XCur, YCur;
	float  x, y;
	GetXY(&x, &y);
	GetXY(&XCur, &YCur);
	DashLine(x, y, XCur, YCur);
      }
      break;
    case IGL_POINT: {
        IMPORT float XCur, YCur;
	IMPORT int   PenFat;
	IMPORT int   PenMode;
	float  radius;
	GetXY(&XCur, &YCur);

	radius = (float)PenFat;
	if (PenFat <= 0)
	  radius = 1.0;
	if (PenFat > 6)
	  radius = 6.0;

	DrawPoint(XCur, YCur, radius, PenMode);
      }
      break;
    case IGL_BOX: {
        IMPORT float XCur, YCur;
	float x, y;
	GetXY(&XCur, &YCur);
	GetXY(&x, &y);
	DashLine(XCur, YCur, x, YCur);
	DashLine(x, YCur, x, y);
	DashLine(x, y, XCur, y);
	DashLine(XCur, y, XCur, YCur);
      }
      break;

    /* --- Window commands (no effect, LaserWriter does its own clipping) --- */
    case IGL_WINDOW: {
/*      float ignore;
        GetXY(&ignore, &ignore);
	GetXY(&ignore, &ignore);
*/
	float x1, y1, x2, y2;
	GetXY (&x1, &y1);
	GetXY (&x2, &y2);
	Window (x1, y1, x2, y2);
      }
      break;
    case IGL_UNWINDOW:
      UnWindow();
      break;

    /* --- Pen state commands --- */
    case IGL_SETPEN: {
        IMPORT int PenFat;
	IMPORT int PenMode;
	IMPORT int PenDash;
	IMPORT struct PenType Pen[];
        int ipen;
	ipen = GetC() & 0xFF;
	PenFat = Pen[ipen].fat;
	PenMode = Pen[ipen].mode;
	PenDash = Pen[ipen].dash;
      }
      break;
    case IGL_DEFBWPEN: {
        IMPORT struct PenType Pen[];
	int ipen;
	ipen = GetC() & 0xFF;
	Pen[ipen].fat  = GetC() & 0xFF;
	Pen[ipen].dash = GetC() & 0xFF;
	Pen[ipen].mode = GetC() & 0xFF;
      }
      break;
    case IGL_DEFCPEN: { /* No color, just make do with BW */
        IMPORT struct PenType Pen[];
	int ipen;
	ipen = GetC() & 0xFF;
	Pen[ipen].fat  = GetC() & 0xFF;
	Pen[ipen].dash = GetC() & 0xFF;
	GetC();   			/* Skip color */
	Pen[ipen].mode = PENBLACK;
      }
      break;
    case IGL_SETPENMODE: {
        IMPORT int PenMode;
	PenMode = GetC() & 0xFF;
      }
      break;
    case IGL_SETFAT: {
        IMPORT int PenFat, FatBase;
	PenFat = FatBase + GetC() & 0xFF;
	if (PenFat < 0) PenFat = 0;
      }
      break;
    case IGL_SETDASH: {
        IMPORT int PenDash;
	PenDash = GetC() & 0xFF;
      }
      break;
    case IGL_SETPENCOLOR: {  /* No color, just skip */
        GetC();
      }
      break;
    case IGL_DEFCOLOR:	{ /* No color, just skip */
        GetC();
	GetC();
	GetC();
	GetC();
      }
      break;
    case IGL_DEFDASH: {
        int   idash;
	float len1, len2, len3, len4;   /* inches */
	IMPORT float Dash[][4];
	IMPORT float PixInch;
	idash = GetC() & 0xFF;
	len1 = (float)(GetI() / FLOATNORM);
	len2 = (float)(GetI() / FLOATNORM);
	len3 = (float)(GetI() / FLOATNORM);
	len4 = (float)(GetI() / FLOATNORM);
	Dash[idash][0] = PixInch * len1;
	Dash[idash][1] = PixInch * (len1 + len2);
	Dash[idash][2] = PixInch * (len1 + len2 + len3);
	Dash[idash][3] = PixInch * (len1 + len2 + len3 + len4);
      }
      break;

    /* Brush state commands */
    case IGL_DEFPATTERN: {
        int	ipat;
	int	row;
	IMPORT PatRow Pattern[][PATSIZE];
	PatRow  GetPatRow();
	ipat = GetC() & 0xFF;
	for (row = 0; row < PATSIZE; row++)
	  Pattern[ipat][row] = GetPatRow();
      }
      break;
    case IGL_DEFBWBRUSH: {
        int	ibrush;
	IMPORT	struct BrushType Brush[];
	ibrush = GetC() & 0xFF;
	Brush[ibrush].pat  = GetC() & 0xFF;
	Brush[ibrush].mode = GetC() & 0xFF;
      }
      break;
    case IGL_DEFCBRUSH:	{ /* No color, just use black. */
        int	ibrush;
	IMPORT	struct BrushType Brush[];
	ibrush = GetC() & 0xFF;
	Brush[ibrush].pat  = 0;
	Brush[ibrush].mode = BRUSHOR;
      }
      break;
    case IGL_SETBRUSH: {
        int	ibrush;
	IMPORT	struct BrushType Brush[];
	IMPORT	int BrushPat;
	IMPORT	int BrushMode;
	ibrush = GetC() & 0xFF;
	BrushPat  = Brush[ibrush].pat;
	BrushMode = Brush[ibrush].mode;
      }
      break;
    case IGL_SETBRUSHCOLOR: {  /* No color, just skip */
        GetC();
      }
      break;
    case IGL_SETPATTERN: {
        IMPORT int BrushPat;
	BrushPat = GetC() & 0xFF;
      }
      break;
    case IGL_SETBRUSHMODE: {
        IMPORT int BrushMode;
	BrushMode = GetC() & 0xFF;
      }
      break;

    /* AreaFill commands */
    case IGL_POLYFILL: {
        IMPORT	int BrushPat;
	IMPORT	int BrushMode;

	int		i;
	int		nvert;
	struct vert	*vert;		/* Malloced vertex array for one polygon. */
	char		*malloc();

	nvert = GetI();
	if (!(vert = (struct vert *)malloc(nvert * sizeof(struct vert))))
	  Fatal("Out of heap space.");
	for (i = 0; i < nvert; i++)
	  GetXY(&(vert[i].x), &(vert[i].y));
	FillPolys(1, &nvert, &vert, BrushMode, BrushPat);
	free(vert);
      }
      break;
    case IGL_POLYFILLN: {
        IMPORT	int BrushPat;
	IMPORT	int BrushMode;

	int		npoly;
	int		i;
	int		*nvert;		/* Malloced nvert array */
	struct vert	**poly;		/* Malloced polygon array */
	char		*malloc();

	npoly = GetI();
	if (!(nvert = (int *)malloc(npoly * sizeof(int))))
	  Fatal("Out of heap space.");
	if (!(poly = (struct vert **)malloc(npoly * sizeof(struct vert *))))
	  Fatal("Out of heap space.");
	for (i = 0; i < npoly; i++) {
	  int j;
	  nvert[i] = GetI();
	  if (!(poly[i] = (struct vert *)malloc(nvert[i] * sizeof(struct vert))))
	    Fatal("Out of heap space.");
	  for (j = 0; j < nvert[i]; j++)
	    GetXY(&(poly[i][j].x), &(poly[i][j].y));
	}
	FillPolys(npoly, nvert, poly, BrushMode, BrushPat);

	for (i = 0; i < npoly; i++)
	  free(poly[i]);
	free(poly);
	free(nvert);
      }
      break;
    case IGL_BOXFILL: {
        IMPORT int BrushMode;
	IMPORT int BrushPat;

	float  		x1, y1, x2, y2;
	int    		nvert;
	struct vert	vert[4];
	struct vert	*poly[1];

	GetXY(&x1, &y1);
	GetXY(&x2, &y2);
	nvert = 4;
	vert[0].x = x1;	vert[0].y = y1;
	vert[1].x = x2;	vert[1].y = y1;
	vert[2].x = x2;	vert[2].y = y2;
	vert[3].x = x1;	vert[3].y = y2;
	poly[0] = vert;
	FillPolys(1, &nvert, poly, BrushMode, BrushPat);
      }
      break;

    /* Text commands */
    case IGL_TEXT:
      Do_Text();
      break;
    case IGL_SETTEXTANGLE: {
        IMPORT float TextAngle;
        TextAngle = (float)(GetI() / ANGLENORM);
        TextAngle *= 360.0;
      }
      break;
    case IGL_SETTEXTSIZE: {
        IMPORT float TextSize, TextScale;
	TextSize = (float)(GetI() / FLOATNORM);

	/* Fix:
		change from point size to inches  GHM 12/22/88
	*/
	if ( TextSize > 4.0 ) TextSize /= 72.;
	TextSize *= TextScale;
      }
      break;
    case IGL_TEXTCENTER: {
    	IMPORT int TextCenter;
	TextCenter = GetC() & 0xFF;
      }
      break;
    case IGL_SETTEXTFONT: {
        IMPORT int TextFont;
	TextFont = GetC() & 0xFF;
      }
      break;

    /* Symbol commands */
    case IGL_SYMBOL: {
        int isym;
	float x, y;
	float size;	/* Inches */
	float angle;	/* Degrees */
	isym = GetC() & 0xFF;
	GetXY(&x, &y);
	size = (float)(GetI())/FLOATNORM;
	angle = 360.0 * (float)(GetI()) / ANGLENORM;
	Do_Symbol(x, y, isym, size, angle);
      }
      break;
    case IGL_BDOT: {
        float x, y;
	IMPORT float PixInch;
	GetXY(&x, &y);
	DrawPoint(x, y, PixInch * (float)(GetI()) / FLOATNORM, PENBLACK);
      }
      break;
    case IGL_WDOT: {
        float x, y;
	IMPORT float PixInch;
	GetXY(&x, &y);
	DrawPoint(x, y, PixInch * (float)(GetI()) / FLOATNORM, PENWHITE);
      }
      break;

    /* Misc. commands */
    case IGL_PAUSE:
      break;
    case IGL_ERASE:
    case IGL_ENDPLOT:
      EndPage();
      StartPage();
      break;
    case IGL_PLOTLABEL: {   /* Accept it but don't bother storing the label */
        int c;
	while ((c = GetC()) != EOF && c != '\0');
      }
      break;

    case IGL_SETINTBYTES: {	/* set the # of bytes for x and y in input */
	intbytes = GetC();
	break;
    }
    default:
      NotYetImplemented(code, name, format);
      break;
  }
}


/* GetXY(&x, &y)  float x, y;
 *
 *   Read integer x, y coordinates from input (1000 pix = 1 inch.) Convert to
 * final virtual coordinates, using run-time scale and translation
 * parameters. Pagerot is not accounted for, that is done at the end
 * by rotating PostScript's transformation matrix.
 *   This routine supplies all x, y coordinates to the compiler.
 */
GetXY(x, y)
float *x, *y;
{
  IMPORT float	Scale;
  IMPORT float	XScale, YScale;
  IMPORT float	XOrigin, YOrigin;
  IMPORT float	PixInch;

  float xinp, yinp;	/* 1000 pixels/inch. */

  switch (intbytes) {
  case 2: 
	xinp = (float)GetH();
	yinp = (float)GetH();
	break;
  case 4:
	xinp = (float)GetW();
	yinp = (float)GetW();
	break;
  default:
	Fatal ("Unimplemented value of XBYTES = %d", intbytes);
  }
  *x = XOrigin + xinp * Scale * XScale * PixInch / FLOATNORM;
  *y = YOrigin + yinp * Scale * YScale * PixInch / FLOATNORM;
}


GetI ()
{
  int n;
  switch (intbytes) {
  case 2: 
	n = GetH();
	break;
  case 4:
	n = GetW();
	break;
  default:
	Fatal ("Unimplemented value of INTBYTES = %d", intbytes);
  }
  return (n);
}

/* Until we're finished...
 */
NotYetImplemented(code, name, format)
int	code;
char	*name;
int	format;
{
  Warning("Warning: %s not yet implemented. Ignoring.", name);
  switch(format) {
    case 0:
    case 1:
      break;
    case 2: {
	float x, y;
	GetXY(&x, &y);
	break;
    }
    case 3: {
        int c;
        while ((c = GetC()) != EOF && c != '\0');
        if (c != '\0')
          Fatal("Unexpected end of file, 1, compile");
      }
      break;
    case 4:
      GetC();
      break;
    case 5: {
        int i;
        GetC();
        for (i = 0; i < PATSIZE; i++)
          GetPatRow();
      }
      break;
    case 6: {
        int i, n;
        n = GetI();
        for (i = 0; i < n; i++)
          GetC();
      }
      break;
    case 7:
      GetC(); GetC(); GetC(); GetC();
      break;
    case 8:
      GetI();
      break;
    case 9:{
        int i, n;
        n = GetI();
        for (i = 0; i < n; i++) {
          GetI(); GetI();
        }
      }
      break;
    case 10:
      GetI(); GetI(); GetI(); GetI();
      break;
    case 11:
      GetC(); GetC(); GetC();
      break;
    case 12:
      GetC(); GetI(); GetI(); GetI(); GetI();
      break;
    case 13: {
        int p, i;
	p = GetI();
	for (i = 0; i < p; i++) {
	  int n, j;
	  n = GetI();
	  for (j = 0; j < n; j++) {
	    GetI(); GetI();
	  }
	}
      }
      break;
    case 14: {
        int c;
	GetI(); GetI();
	while ((c = GetC()) != EOF && c != '\0');
	if (c != '\0')
	  Fatal("Unexpected end of file, 2, compile");
      }
      break;
    case 15:
      GetC(); GetC();
      break;
    default:
      Fatal("Bug! Funny format %d passed to NotYetImplemented()", format);
      break;
  }
}
