#include <stdio.h>
extern int plotout;
extern int ixwmin,ixwmax,iywmin,iywmax;

/* faster boxfill for rastertech */
/* note prmfil must be turned on in open_close.c or elsewhere to fill */
/* note boxfill should check that box is within plotting window */
boxfill(x1,y1,x2,y2)
int x1, y1, x2, y2;
   {
	char c[5];
	register int dx, dy, t;
	int x, y;
	extern int xlast, ylast;

	if(x1 > x2) { t=x1; x1=x2; x2=t; }
	if(y1 > y2) { t=y1; y1=y2; y2=t; }

	/* totally out of bounds */
	if (( x1<ixwmin && x2<ixwmin) || (x1>ixwmax && x2>ixwmax)) return;
	if (( y1<iywmin && y2<iywmin) || (y1>iywmax && y2>iywmax)) return;

	/* clip box if necessary */
	if (x1<ixwmin) x1=ixwmin;
	if (x2>ixwmax) x2=ixwmax;
	if (y1<iywmin) y1=iywmin;
	if (y2>iywmax) y2=iywmax;

	xlast= x1;
	ylast= y1;
	move(x1,y1);
	c[0]= '\216'; 	/* rectangle */
	c[1]= (x2 >> 8) & 0xff;
	c[2]= x2 & 0xff;
	c[3]= (y2 >> 8) & 0xff;
	c[4]= y2 & 0xff;
	write(plotout,c,5);
	return;
   }
