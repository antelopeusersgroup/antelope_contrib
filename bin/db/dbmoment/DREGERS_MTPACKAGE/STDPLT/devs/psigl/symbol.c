/* Draw symbols.
 */

#include "const.h"
#include "types.h"

#include "../com/symboldata.h"

#define OUTLINE_MASK (1 << 6)	/* Parameter bits in isym */
#define FILL_MASK    (1 << 7)

Do_Symbol(x, y, isym, size, angle)
float x, y;	/* Center */
int   isym;	/* Symbol ID plus Outline/Fill bits */
float size;	/* Inches */
float angle;	/* Degrees */
{
  double cos(), sin();
  double c, s;
  char   *malloc();
  int    symnum;
  struct vert *vert;	/* Malloced array for holding vertices */
  struct vert *poly[1]; /* Array for polygon */
  int	 nvert;
  int	 index;
  int	 i;

  IMPORT int BrushMode;
  IMPORT int BrushPat;
  IMPORT int PenMode;
  IMPORT int PenFat;

  IMPORT float PixInch;
  IMPORT float SymScale;

  if (!(isym & (OUTLINE_MASK | FILL_MASK)))
    isym |= OUTLINE_MASK;	/* Special case: 00 = 01 */
  symnum = isym & ~(OUTLINE_MASK | FILL_MASK);
  if (symnum < 0 || symnum >= NSYMBOL)
    symnum = 0;		/* Gremlin */

  c = cos(angle * PI / 180.0) * size * PixInch * SymScale;
  s = sin(angle * PI / 180.0) * size * PixInch * SymScale;

  nvert = symheads[symnum].npoints;
  if (!(vert = (struct vert *)malloc(nvert * sizeof(struct vert))))
    Fatal("Out of heap space.");
  index = symheads[symnum].index;
  for (i = index; i < index + nvert; i++) {
    vert[i - index].x = x + c * symdata[i].xp - s * symdata[i].yp;
    vert[i - index].y = y + s * symdata[i].xp + c * symdata[i].yp;
  }

  poly[0] = vert;

  if (isym & FILL_MASK)
    FillPolys(1, &nvert, poly, BrushMode, BrushPat);
  if (isym & OUTLINE_MASK) {
    int i;
    for (i = 0; i < nvert - 1; i++) {
      DrawVector(vert[i].x, vert[i].y, vert[i+1].x, vert[i+1].y,
      		 PenFat, PenMode, 0.0, 0.0, 0.0, 0.0);
    }
    DrawVector(vert[nvert-1].x, vert[nvert-1].y,
    	       vert[0].x, 	vert[0].y,
	       PenFat, PenMode, 0.0, 0.0, 0.0, 0.0);
  }
  free(vert);
}
