#include	<stdio.h>
extern int plotout;  

extern int ixwmin,ixwmax,iywmin,iywmax;

/* boxfill should check that box is within the window */
boxfill(x1,y1,x2,y2)
int x1, y1, x2, y2;
   {
	char c[4];
	register int dx, dy, t;
	int x, y;
	extern int xlast, ylast;

	if(x1 > x2) { t=x1; x1=x2; x2=t; }
	if(y1 > y2) { t=y1; y1=y2; y2=t; }

	/* box totally out of bounds */
	if ((x1<ixwmin && x2<ixwmin) || (x1>ixwmax && x2>ixwmax)) return;
	if ((y1<iywmin && y2<iywmin) || (y1>iywmax && y2>iywmax)) return;

	/* clip box if necessary */
	if (x1<ixwmin) x1=ixwmin;
	if (x2>ixwmax) x2=ixwmax;
	if (y1<iywmin) y1=iywmin;
	if (y2>iywmax) y2=iywmax;

	dx= x2-x1;
	dy= y2-y1;
	if(dx == 0) dx=1;
	if(dy == 0) dy=1;
	xlast= x1;
	ylast= y1;

	/* small box - less than 127 by 127 */
	if(dx < 127 && dy < 127)
	   {
		move(x1,y1);
		c[0]= ',';
		c[1]= dx & 0xff;
		c[2]= dy & 0xff;
		write(plotout,c,3);  
		return;
	   }
	
	/* large box - greater than 127 by 127 pixels */
	move(x1,y1);
	c[0]= 'o';
	c[1]= ((x2 & 0x300)>>4) | ((y2>>8) & 0x3);
	c[2]= x2 & 0xff;
	c[3]= y2 & 0xff;
	write(plotout,c,4);  

	/******************8
	for(x= x1; x <= x2; x += 255)
	for(y= y1; y <= y2; y += 255)
	   {
		dx= (x1+255 > x2 ? x1+255 -x2 : 255);
		dy= (y1+255 > y2 ? y1+255 -y2 : 255);
		move(x,y);
		c[0]= ',';
		c[1]= dx & 0xff;
		c[2]= dy & 0xff;
		write(plotout,c,3);  
	   }
	   **************************/
}
