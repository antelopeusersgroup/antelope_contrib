#include <stdio.h>
/* Code generator entry points:
 *
 *	StartCode()		 Call this first. 
 *	EndCode()		 Call this last. Shuts off the code generator
 *	StartPage()		 Call this to start a new page
 *	EndPage()		 Call this at the end of a page
 *
 *	DrawVector(x1, y1, x2, y2, penfat, penmode, on1, off1, on2, off2)
 *	float x1, y1, x2, y2;
 *	int penfat, penmode;
 *	float on1, off1, on2, off2;
 *
 *
 *	DrawPoint(x, y, penfat, penmode)
 *	float x, y;
 *	int penfat;
 *	int penmode;
 *
 *
 *	FillPolys(npoly, nvert, vert, patmode, pat)
 *	int 		npoly;
 *	int 		nvert[npoly];
 *	struct vert	*vert[npoly];
 *	int 		patmode;
 *	PatRow  	pat[PATSIZE];
 *
 *
 *	DrawText(x, y, str, textfont, textsize, textangle, penmode, penfat)
 *	float 	x, y;		 Virtual pixels, left-bottom corner 
 *	char	*str;		 No newlines or backspaces 
 *	int	textfont; 	 0, 1, 2 or 3 
 *	float	textsize;	 inches 
 *	float	textangle;	 degrees 
 *	int	penmode;
 *	int	penfat;
 *
 *	Window(x1, y1, x2, y2)
 *	float	x1, y1, x2, y2;	 corners of rectangular clipping region.
 *
 *	UnWindow()
 */

#include "const.h"
#include "types.h"


/* For comparing floats */
double fabs();
#define SMALL 0.0001
#define eq(f1, f2) (fabs( ((double)(f1)) - ((double)(f2)) ) < SMALL)

#define PS_DEF_PIXINCH	 72.0	/* Default PostScript unit size */

PRIVATE int CGenOn = 0;		/* EndCode() must work no matter what */


PRIVATE int Mode;		/* Legal values: */
#define EMPTYPATH 0		/*   Current path is empty. */
#define BLDNGSTROKEPATH 1	/*   Processing a series of DrawVector()s. */


/* If Mode == BLDNGSTROKEPATH, the following state variables are maintained:
 * ------------------------------------------------------------------------- */
PRIVATE int SP_PenMode;		/* Pen mode for this stroke path */
PRIVATE int SP_PenFat;		/* Pen fat for this stroke path */
PRIVATE float SP_On1, SP_Off1,	/* Pen dash (end values in virtual pixels) */
	      SP_On2, SP_Off2;	/*   Special case: all 0's = solid */
PRIVATE int SP_PathLength;	/* Path counter */

#define MAXPATHLENGTH 100	/* A safe value */



/* Area fill info:
 * ------------------------------------------------------------------------- 
 *   Pattern fills are done with pattern fonts and clipping paths. Whenever
 * FillPolys() is called with a new brush pattern, it is entered as a new 
 * character in the font. Because of PostScript's font cache, we can't
 * reuse a character later, that means after 256 different patterns have been
 * used, no more patterns can be defined. Any further poly fills with unavailable
 * patterns use solid black as a default.
 */

#define BYTELIMIT 256
PRIVATE PatRow FontChar[BYTELIMIT][PATSIZE]; /* Patterns stored in font. */
					     /* Slots can't be reused. */
PRIVATE int NextFreeChar;		 /* Index to FontChar[] */

PRIVATE int CharCode[BYTELIMIT];	 /* For fast lookup, the character code
					  * associated with a given pattern id.
					  */
#define UNASSIGNED -1			 /* For CharCode[] */



/* This code begins our PostScript program.
 */
PRIVATE char *Prologue[] = {
  "%!",
  "save",				/* Matched by a restore at the end */
  "20 dict begin",			/* Create a dictionary for locals */
  "/Font0 /Courier      findfont def",	/* Load the text fonts and assign them */
  "/Font1 /Times-Roman  findfont def",	/*  names for easy reference. */
  "/Font2 /Times-Italic findfont def",
  "/Font3 /Times-Bold   findfont def",
  "/PatDict 256 dict def",		/* Dictionary to hold brush patterns */
  "/PatFontDict 20 dict def",		/* New font to print patterns */
  "PatFontDict begin",			/* Build pattern font: */
  "  /FontMatrix [0.03125 0 0 0.03125 0 0] def",  /* Char grid is 32 by 32 units */
  "  /FontType 3 def",			/* Type = 3 required for user fonts */
  "  /FontBBox [0 0 32 32] def",	/* Bounding box */
  "  /Encoding 256 array def",		/* Encoding vector: Character codes */
  "  0 1 255 {",			/*   map to names /pat0, /pat1... /pat255.*/
  "    Encoding exch",			/*   Patx is the name of a data */
  "    dup /sn exch 3 string cvs def",	/*   acquisition procedure defined in */
  "    /s 3 sn length add string def",	/*   PatDict which is passed to imagemask */
  "    (pat) s copy pop",		/*   to draw the tile. */
  "    s 3 sn putinterval",
  "    s cvn",
  "    put",
  "  } for",				/* Finish building Encoding. */
  "  /BuildChar {",		/* Stack contains font charcode */
  "    32 0",			/* Send width & bounds to font cache. */
  "    0 0 32 32",
  "    setcachedevice",
  "    exch begin",		/* Put font on dict stack */
  "      Encoding exch get",	/* Get name of acquisition procedure */
  "      PatDict  exch get",	/* Get acquisition procedure */
  "      32 exch 32 exch true exch [1.025 0 0 1.025 0 0] exch imagemask",
/* Logically, the above transformation matrix should be [1 0 0 1 0 0].
 * But because of some accumulated roundoffs during all the matrix multiplying,
 * that matrix causes ugly seams between pattern tiles. To make the tiles lock
 * correctly into device pixels, we had to adjust the value experimentally
 * until the patterns looked right.
 */
  "    end",
  "  } bind def",
  "end",
  "/PatFontName PatFontDict definefont pop",  /* Name our new font */
  "/PatFont /PatFontName findfont 32 scalefont def",
  "/TStr 1 string def",	/* Holds tile code during pattern fills */
#ifdef VM
  "/vmstr 20 string def",
  "/@vmstatus",
  "{",
  "	(VM Max: ) print vmstatus vmstr cvs print",
  "	(VM used: ) print vmstr cvs  print",
  "	(VM level: ) print vmstr cvs print",
  "	(\n) print flush",
  "} def",
#endif	/* VM */
  NULL			/* Terminator */
};

PRIVATE char *Epilogue[] = {
  "end",		/* Pop local dictionary off stack */
  "restore",		/* Undo all our manipulations */
  NULL
};




PRIVATE MakePolyPath(npoly, nverts, verts)
int npoly;
int nverts[];
struct vert *verts[];
{
  int i, j;
  for (i = 0; i < npoly; i++) {
    for (j = 0; j < nverts[i]; j++) {
      if (j)
        GenLineTo(verts[i][j].x, verts[i][j].y);
      else
        GenMoveTo(verts[i][j].x, verts[i][j].y);
    }
    GenClosePath();
  }
}

StartCode()
{
  char **pro;	/* Walk down prologue array */
  int  i;

  CGenOn = 1;
  Mode = EMPTYPATH;
  NextFreeChar = 0;
  for (i = 0; i < BYTELIMIT; i++)
    CharCode[i] = UNASSIGNED;

  OpenOptimizer();
  pro = &Prologue[0];
  while (*pro)
    Gen("%s\n", *(pro++));
}


EndCode()
{
  if (CGenOn) {
    char **epil;
    
    CGenOn = 0;
    epil = &Epilogue[0];
    while (*epil)
      Gen("%s\n", *(epil++));
    CloseOptimizer();
  }
}


/* StartPage() assumes that initgraphics has been executed (either by the
 * last showpage or by the fact that this is the first executing statement).
 */
StartPage()
{
  IMPORT float	DevXOff, DevYOff;	/* inches */
  IMPORT float	DevRot;			/* degrees */
  IMPORT float	DevScale;

  IMPORT int	PageRot;		/* 0, 1, 2, 3 (= 0, 90, 180, 270) */
  IMPORT float	PixInch;

  IMPORT float	XPageLen;
  IMPORT float	YPageLen;

  float xoff, yoff;

  /* Scale so that one PS unit = one LaserWriter pixel */
  GenScale(PS_DEF_PIXINCH/PixInch, PS_DEF_PIXINCH/PixInch);

  /* Shift origin for pagerot. Because the LaserWriter's default grid is rotated
   * 90 degrees with respect to imagen's grid, we add in a 90 degree rotation
   * here to compensate.
   */
  switch ((PageRot + 1) % 4) {	 /* Swap XPageLen and YPageLen because of 90 deg */
    case 0:			 /* rot.					 */
      xoff = yoff = 0.0;
      break;
    case 1:
      xoff = YPageLen;
      yoff = 0.0;
      break;
    case 2:
      xoff = YPageLen;
      yoff = XPageLen;
      break;
    case 3:
      xoff = 0.0;
      yoff = XPageLen;
      break;
  }
  xoff -= DevYOff * PixInch;  /* Switched because of 90 deg. rot. DefXOff */
  yoff += DevXOff * PixInch;  /* always translates along the long edge. */
  GenTranslate(xoff, yoff);

  /* After fixing the origin, scale according to DevScale */
  GenScale(DevScale, DevScale);

  /* Again, sneak in a 90 degree rot. because imagen's grid is sideways. */
  GenRotate(DevRot + PageRot * 90.0 + 90.0);
}

/*-------------- Utility function: NewPath() -------------------- 
 * Sets Mode = EMPTYPATH and outputs whatever cleanup code is needed
 * so that the current path is empty (and the current point undefined.)
 * Always call this function rather than manipulating Mode directly.
 */
PRIVATE NewPath()
{
  switch(Mode) {
    case EMPTYPATH:
      break;
    case BLDNGSTROKEPATH:
      GenStroke();
      break;
    default:
      Fatal("Bug! Funny code generator mode %d.", Mode);
      break;
  }
  Mode = EMPTYPATH;	/* Current path is now guaranteed to be empty */
}



EndPage()
{
  NewPath();
  GenShowPage();
}



DrawVector(x1, y1, x2, y2, penfat, penmode, on1, off1, on2, off2)
float x1, y1, x2, y2;
int   penfat, penmode;
float on1, off1, on2, off2;   /* End values, special case 0 0 0 0 for solid. */
{
  if (Mode != BLDNGSTROKEPATH ||
      penfat != SP_PenFat ||
      penmode != SP_PenMode ||
      !eq(on1,  SP_On1) ||
      !eq(off1, SP_Off1) ||
      !eq(on2,  SP_On2) ||
      !eq(off2, SP_Off2) ||
      SP_PathLength >= MAXPATHLENGTH) {

    /* Start new stroke path */
    NewPath();

    Mode = BLDNGSTROKEPATH;

    SP_PenFat = penfat;
    SP_PenMode = penmode;
    SP_On1  = on1;
    SP_Off1 = off1;
    SP_On2  = on2;
    SP_Off2 = off2;
    SP_PathLength = 0;

    GenSetGray(penmode == PENWHITE? 1.0: 0.0);
    GenSetLineWidth(penfat <= 0? 1.0: (double)penfat);
    GenSetDash(on1, off1, on2, off2);
  }

  /* Add new vector to stroking path */
  SP_PathLength++;
  GenMoveTo(x1, y1);
  GenLineTo(x2, y2);
}


/* Draw a filled circle.
 */
DrawPoint(x, y, radius, penmode)
float x, y;
float radius;	/* Virtual pixels */
int   penmode;
{
  NewPath();
  GenSetGray(penmode == PENWHITE? 1.0: 0.0);
  GenArc(x, y, radius, 0.0, 360.0);
  GenEoFill();
}


FillPolys(npoly, nvert, vert, brushmode, brushpat)
int 		npoly;
int 		nvert[];
struct vert 	*vert[];
int 		brushmode;
int		brushpat;
{
  int i;
  int blackfill;	/* Boolean, true if no more patterns available in font */
  IMPORT PatRow Pattern[][PATSIZE];

  NewPath();

  /* See if a new font char has to be created. */
  blackfill = 0;
  if (brushmode != BRUSHWHITE) {
    int neednewpattern;		/* Boolean */
    if (CharCode[brushpat] == UNASSIGNED)
      neednewpattern = 1;
    else {	/* Has the pattern changed? */
      int row;
      neednewpattern = 0;
      for (row = 0; row < PATSIZE; row++)
        if (Pattern[brushpat][row] != FontChar[CharCode[brushpat]][row]) {
	  neednewpattern = 1;
	  break;
	}
    }

    if (neednewpattern) {
    if (NextFreeChar >= BYTELIMIT)
      blackfill = 1;		/* No more room in font, use black fill */
    else {
      int row;
      char charname[10];  /* "patxxx" */
      char hexpat[PATSIZE * (PATSIZE / 4) + 4 + 1];
      char *hexptr;

      hexptr = hexpat;
      *(hexptr++) = '{';
      *(hexptr++) = '<';
      for (row = 0; row < PATSIZE; row++) {
        int     dig;
	PatRow  bits;
        FontChar[NextFreeChar][row] = Pattern[brushpat][row];
	bits = Pattern[brushpat][PATSIZE - 1 - row];
	for (dig = PATSIZE/4 - 1; dig >= 0; dig--) {
	  int nibble;
	  nibble = (bits >> (dig * 4)) & 0xF;
	  if (nibble < 10)
	    *(hexptr++) = '0' + nibble;
	  else
	    *(hexptr++) = 'A' + nibble - 10;
	}
      }
      *(hexptr++) = '>';
      *(hexptr++) = '}';
      *hexptr = '\0';
      CharCode[brushpat] = NextFreeChar;
      sprintf(charname, "pat%d", NextFreeChar);
      GenPut("PatDict", charname, hexpat);
      NextFreeChar++;
    }
    }
  }

  /* Set the color now, before the gsave. */
  if (!blackfill && (brushmode == BRUSHEQU || brushmode == BRUSHWHITE))
    GenSetGray(1.0);
  else
    GenSetGray(0.0);

  /* If necessary, erase the area first. */
  if (blackfill || (brushmode == BRUSHEQU || brushmode == BRUSHWHITE)) {
    MakePolyPath(npoly, nvert, vert);
    GenEoFill();
  }

  /* Now, do the pattern fill */

  if (!blackfill && brushmode != BRUSHWHITE) {
    float xmin, xmax;
    float ymin, ymax;
    int   i;

    GenSetGray(0.0);
    GenPatFont();
    GenGSave();

    xmin = xmax = vert[0][0].x;
    ymin = ymax = vert[0][0].y;
    for (i = 0; i < npoly; i++) {
      int j;
      for (j = 0; j < nvert[i]; j++) {
        double x, y;
	x = vert[i][j].x;
	y = vert[i][j].y;
	if (x < xmin) xmin = x;
	if (y < ymin) ymin = y;
	if (x > xmax) xmax = x;
	if (y > ymax) ymax = y;
      }
    }

    MakePolyPath(npoly, nvert, vert);
    GenEoClip();
    GenTileFill(xmin, ymin, xmax, ymax, CharCode[brushpat]);
    GenGRestore();
  }
}


DrawText(x, y, str, textfont, textsize, textangle, penmode, penfat)
float 	x, y;		 /* Virtual pixels, left-bottom corner */
char	*str;		 /* No newlines or backspaces */
int	textfont; 	 /* 0, 1, 2 or 3 */
float	textsize;	 /* inches */
float	textangle;	 /* degrees */
int	penmode;
int	penfat;
{
  IMPORT float PixInch;

  NewPath();
  GenChangeFont(textfont);
  GenSetGray(penmode == PENWHITE? 1.0: 0.0);
  GenSetLineWidth(penfat <= 0? 1.0: (double)penfat);
  GenGSave();
    GenMoveTo(x, y);
    GenScale(textsize * PixInch, textsize * PixInch);
    GenRotate(textangle);
    GenShow(str);
  GenGRestore();
}

Window(x1, y1, x2, y2)
  float	x1, y1, x2, y2;		/* corners of rectangular clipping region.*/
{
  char str[80];
  IMPORT int Clipping_Window;
  if (Clipping_Window) UnWindow();
  NewPath();
  sprintf (str, "Window: %lf, %lf, %lf, %lf", x1, y1, x2, y2);
  GenComment(str);
  GenGSave();
  /* set up a new clipping path. */
  NewPath();
  GenMoveTo(x1, y1);
  GenLineTo(x1, y2);
  GenLineTo(x2, y2);
  GenLineTo(x2, y1);
  GenClosePath();
  GenClip();
  GenNewPath();
  ++Clipping_Window;
}

UnWindow()
{
  IMPORT int Clipping_Window;
  NewPath();
  GenGRestore();
  Clipping_Window = 0;
  GenComment ("UnWindow");
}
