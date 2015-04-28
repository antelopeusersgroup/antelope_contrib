#include <stdio.h>
extern int plotout;
extern int ixwmin, ixwmax, iywmin, iywmax;

move(x,y)
register int x, y;
   {
	char c[5];
	c[0]= '\001';
	c[1] = (x >> 8) & 0xff;
	c[2] = x & 0xff;
	c[3] = (y >> 8) & 0xff;
	c[4] = y & 0xff;
	write(plotout,c,5);
   }

draw(x,y)
register int x, y;
   {
	char c[5];

	c[0] = '\201';
	c[1] = (x >> 8) & 0xff;
	c[2] = x & 0xff;
	c[3] = (y >> 8) & 0xff;
	c[4] = y & 0xff;
	write(plotout,c,5);
   }

vector(x1,y1,x2,y2)
register int x1,y1,x2,y2;
   {
	extern int xlast, ylast;
	if(x1 != xlast || y1 != ylast) move(x1,y1);
	draw(x2,y2);
	xlast= x2;
	ylast= y2;
   }

linefill(x,y1,y2)
register int x,y1,y2;
   {
	extern int xlast, ylast;
	int t;

	if (y2<y1) {t=y1; y1=y2; y2=t;}

	if (y1<iywmin) y1=iywmin;
	if (y2>iywmax) y2=iywmax;

	move(x,y1);
	draw(x,y2);
	xlast= x;
	ylast= y2;
   }
