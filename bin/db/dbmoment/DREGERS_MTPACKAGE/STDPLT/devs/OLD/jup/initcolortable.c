#include <stdio.h>
#include "../com/global.h"
#include "../../h/igl.h" 

extern char colors[][4];
extern int pencolor;

/* inits the first 8 colors to be those in globalvar.c */
initcolortable()
{
	int icol,rcol,gcol,bcol;

	for (icol=0; icol<8; icol++) {
		rcol = colors[icol][1];
		gcol = colors[icol][2];
		bcol = colors[icol][3];
		rcol = rcol & 0xff;
		gcol = gcol & 0xff;
		bcol = bcol & 0xff;
		defcolor(icol,rcol,gcol,bcol);
	}
	setcolor(pencolor);
}
