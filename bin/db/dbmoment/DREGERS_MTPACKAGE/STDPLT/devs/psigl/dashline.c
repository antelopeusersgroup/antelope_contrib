#include "const.h"
#include "types.h"

/* DashLine(x1, y1, x2, y2) makes a call to DrawVector() using the current
 * pen parameters. Since this is done so often, this function saves a lot
 * of typing.
 */
DashLine(x1, y1, x2, y2)
float x1, y1, x2, y2;	/* Virtual device pixels */
{
  IMPORT int   PenMode;
  IMPORT int   PenDash;
  IMPORT float Dash[][4];
  IMPORT int   PenFat;

  if (PenDash == 0) /* Solid */
    DrawVector(x1, y1, x2, y2, PenFat, PenMode, 0.0, 0.0, 0.0, 0.0);
  else
    DrawVector(x1, y1, x2, y2, PenFat, PenMode, Dash[PenDash][0],
    						Dash[PenDash][1],
    						Dash[PenDash][2],
    						Dash[PenDash][3]);
}
