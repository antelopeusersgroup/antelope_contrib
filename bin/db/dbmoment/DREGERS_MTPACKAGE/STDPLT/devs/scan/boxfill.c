#include	<stdio.h>
#include 	"../com/global.h"
#include 	"../../h/igl.h"

extern int ixwmin, ixwmax, iywmin, iywmax;


/* boxfill should check that box is within plotting window */
boxfill(x1,y1,x2,y2)
int x1,y1,x2,y2;
  {
	register int ix,t;
	if (x2<x1) { t=x1; x1=x2; x2=t; }
	if (y2<y1) { t=y1; y1=y2; y2=t; }

	/* totally out of bounds */
	if ((x1<ixwmin && x2<ixwmin) || (x1>ixwmax && x2>ixwmax)) return;
	if ((y1<iywmin && y2<iywmin) || (y1>iywmax && y2>iywmax)) return;

	/* clip box if necessary */
	if (x1<ixwmin)  x1=ixwmin;
	if (x2>ixwmax)  x2=ixwmax;
	if (y1<iywmin)  y1=iywmin;
	if (y2>iywmax)  y2=iywmax;
	if (x2<x1) return;
	for (ix=x1; ix<x2; ix++)
		linefill(ix,y1,y2);
  }
