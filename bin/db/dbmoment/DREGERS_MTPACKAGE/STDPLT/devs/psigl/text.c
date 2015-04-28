/* Handler for IGL_TEXT command.
 */

#include <stdio.h>
#include "const.h"
#include "types.h"

#define MAXLEN 100



PRIVATE float Width(c)
char c;
{
  IMPORT int TextFont;

  /* Import font data */
  IMPORT float CourierWidth;	  	/* Font 0 */
  IMPORT float TimesRomanWidth[], 	/* Font 1 */
  	       TimesItalicWidth[],	/* Font 2 */
	       TimesBoldWidth[];	/* Font 3 */

  switch (TextFont) {
    case 0: return CourierWidth;
    case 1: return TimesRomanWidth[c];
    case 2: return TimesItalicWidth[c];
    case 3: return TimesBoldWidth[c];
  }
}
Do_Text()
{
  IMPORT float PixInch;

  IMPORT float TextSize;
  IMPORT float TextAngle;
  IMPORT int   TextCenter, TextFont;
  IMPORT float XCur, YCur;
  IMPORT int   PenMode, PenFat;


  IMPORT float BaseLineDepth[4];
  float	 Width();
  float  totalwidth;	/* of string */

  char   str[MAXLEN + 1];
  char	 *ptr;
  int	 len;
  int	 c;

  int    hshift, vshift;	/* 0, 1 or 2 */
  float  xorig, yorig;		/* Bottom-left of current line */
  float  x, y;			/* To just probe along: */

  float  textsinang, textcosang;
  double sin(), cos();

  char   sbuild[MAXLEN + 1];	/* To aid in sub-string building */
  int	 pos;			/* index to sbuild */

  GetXY(&XCur, &YCur);

  len = 0;
  while ((c = GetC()) != '\0' && c != EOF)
    if (len < MAXLEN)
      str[len++] = c;
  if (c == EOF)
    Warning("Unexpected end of file, 1, text");
  str[len] = '\0';
  if (len >= MAXLEN)
    str[len - 1] = '*';		/* To signal overflow */

  hshift = TextCenter & 0x3;

	/* Fix?
		I think that the fields are 4 bits each
	*/

/*  vshift = (TextCenter >> 2) & 0x3; */
  vshift = (TextCenter >> 4) & 0x3; 

  textsinang = sin(TextAngle * PI / 180.0);
  textcosang = cos(TextAngle * PI / 180.0);

  xorig = XCur;
  yorig = YCur;

  /* Adjust xorig, yorig for vshift */
  yorig -= textcosang * BaseLineDepth[TextFont] * 0.5 * vshift * 
             PixInch * TextSize;
  xorig += textsinang * BaseLineDepth[TextFont] * 0.5 * vshift *
    	     PixInch * TextSize;

  /* Adjust xorig, yorig for hshift */
  ptr = str;
  totalwidth = 0.0;
  while (*ptr)
    totalwidth += Width(*(ptr++));
  xorig -= textcosang * totalwidth * 0.5 * hshift * PixInch * TextSize;
  yorig -= textsinang * totalwidth * 0.5 * hshift * PixInch * TextSize;

  ptr = str;

  while (*ptr != '\0') {
    x = xorig;	/* Start a line */
    y = yorig;
    pos = 0;
    while (*ptr != '\0' && *ptr != '\n' && *ptr != '\r') {
      if (*ptr != '\b') {		/* Regular text */
        float xstart, ystart;
        int   startpos;
        startpos = pos;
        xstart = x;
        ystart = y;
        while (*ptr != '\0' && *ptr != '\b' && *ptr != '\n' && *ptr != '\r') {
          sbuild[pos++] = *ptr;
          x += textcosang * Width(*ptr) * PixInch * TextSize;
          y += textsinang * Width(*ptr) * PixInch * TextSize;
  	  ptr++;
        }
        sbuild[pos] = '\0';
        DrawText(xstart, ystart, sbuild + startpos, TextFont, 
      	         TextSize, TextAngle, PenMode, PenFat);
      }
      else {			/* Backspace */
        while (*ptr == '\b') {
          if (pos >= 0) {
  	    pos--;
  	    x -= textcosang * Width(*ptr) * PixInch * TextSize;
	    y -= textsinang * Width(*ptr) * PixInch * TextSize;
	  }
	  ptr++;
        }
      }
    }
    if (*ptr != '\0') {   /* Must be a newline */
      yorig -= textcosang * PixInch * TextSize;	/* "Draw" a newline */
      xorig += textsinang * PixInch * TextSize;
      ptr++;
    }
  }
}

