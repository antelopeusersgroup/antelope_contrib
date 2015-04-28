/* Build the command lookup table.
 */

#include "const.h"
#include "../../h/igl.h"
#include "types.h"


/* Code-indexed IGL command table. Unused entries have a NULL name and 0 format.
 */
EXPORT struct Cmd CmdTable[CMDTABLESIZE];


/* This list is used to build the command table on startup.
 */
PRIVATE struct CmdEntry CmdList[] = {
/* Code			Name		format
   ----			----		------ */
  {IGL_MOVE,		"move",		 2},
  {IGL_DRAW,		"draw",		 2},
  {IGL_LINE,		"line",		10},
  {IGL_POINT,		"point",	 2},
  {IGL_BOX,		"box",		10},
  {IGL_SETFAT,		"setfat",	 4},
  {IGL_SETPENCOLOR,	"setpencolor",	 4},
  {IGL_SETPENMODE,	"setpenmode",	 4},
  {IGL_SETPEN,		"setpen",	 4},
  {IGL_SETDASH,		"setdash",	 4},

  {IGL_POLYFILL,	"polyfill",	 9},
  {IGL_POLYFILLN,	"polyfilln",	13},
  {IGL_BOXFILL,		"boxfill",	10},
  {IGL_SETPATTERN,	"setpattern",	 4},
  {IGL_SETBRUSHMODE,	"setbrushmode",	 4},
  {IGL_SETBRUSHCOLOR,	"setbrushcolor", 4},
  {IGL_SETBRUSH,	"setbrush",	 4},

  {IGL_DEFBWPEN,	"defbwpen",	 7},
  {IGL_DEFCPEN,		"defcpen",	 7},
  {IGL_DEFBWBRUSH,	"defbwbrush",	11},
  {IGL_DEFCBRUSH,	"defcbrush",	15},
  {IGL_DEFCOLOR,	"defcolor",	 7},
  {IGL_DEFPATTERN,	"defpattern",	 5},
  {IGL_DEFDASH,		"defdash",	12},

  {IGL_TEXT,		"text",		14},
  {IGL_SETTEXTANGLE,	"settextangle",	 8},
  {IGL_SETTEXTSIZE,	"settextsize",	 8},
  {IGL_SETTEXTFONT,	"settextfont",	 4},
  {IGL_TEXTCENTER,	"settextcenter", 4},    /* Special case */

  {IGL_SYMBOL,		"symbol",	12},
  {IGL_LOADSYMS,	"loadsyms",	 1},
  {IGL_SETSYMANGLE,	"setsymangle",	 8},
  {IGL_SETSYMSIZE,	"setsymsize",	 8},
  {IGL_DEFSYMCOLOR,	"defsymcolor",	 7},
  {IGL_DEFSYMPATTERN,	"defsympattern",16},
  {IGL_BDOT,		"bdot",		 2},
  {IGL_WDOT,		"wdot",		 2},

  {IGL_WINDOW,		"window",	10},
  {IGL_UNWINDOW,	"unwindow",	 1},

  {IGL_INCLUDE,		"include",	 3},
  {IGL_ERASE,		"erase",	 1},
  {IGL_PAUSE,		"pause",	 1},
  {IGL_ENDPLOT,		"endplot",	 1},
  {IGL_PLOTLABEL,	"plotlabel",	 3},
  {IGL_SETINTBYTES,	"setintbytes",	 4},

  {0,			"",		 0}	/* Terminator */
};


BuildCmdTable()
{
  struct CmdEntry *lookup;
  int		  i;

  for (i = 0; i < CMDTABLESIZE; i++) {
    CmdTable[i].name = NULL;
    CmdTable[i].format = 0;
  }

  lookup = &CmdList[0];
  while (lookup->code) {
    if (lookup->code < CMDTABLEBASE || lookup->code >= CMDTABLEBASE + CMDTABLESIZE)
      Fatal("Bug! CmdList entry code %d out of range.", lookup->code);
    CmdTable[lookup->code - CMDTABLEBASE].name   = lookup->name;
    CmdTable[lookup->code - CMDTABLEBASE].format = lookup->format;
    lookup++;
  }
}
