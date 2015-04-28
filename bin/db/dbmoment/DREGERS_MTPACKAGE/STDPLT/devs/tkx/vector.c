#include	<stdio.h>
#include	"devpar.h"
#define hiXY(xy)	(xy>>5)|040
#define loY(y)		(y&037)|0140
#define loX(x)		(x&037)|0100
#define LO		0x1f
#define HI		0x3e0

vector(x1,y1,x2,y2)
register int x1,y1,x2,y2;
   {
	char cbuf[16];
	register char *pc;
	register int n;
	extern int xcur, ycur, gsreqd;
	extern int plotout;
	static int xhold = -10000;
	static int yhold = -10000;

	/*
	fprintf(stderr,"x1=%3d y1=%3d , x2=%3d y2=%3d\n",x1,y1,x2,y2);
	fprintf(stderr,"xcur=%3d ycur=%3d\n",xcur,ycur);
	*/
	pc= cbuf;
	n= 0;
	if(gsreqd || x1 != xhold || y1 != yhold) /* do a move */
	   {
		*pc++ = GS;
		*pc++ = hiXY(y1); 	/* y/32+32 hi y byte */
		*pc++ =  loY(y1); 	/* y%32+96 low y byte */
		*pc++ = hiXY(x1); 	/* x/32+32 hi x byte */
		*pc++ =  loX(x1); 	/* x%32+64 low x byte */
		n += 5;
		/*
		fprintf(stderr,"M x1=%3d y1=%3d ",x1,y1);
		*/
	   }
	/* do draw */
	if((y1&HI) == (y2&HI) && (x1&HI) == (x2&HI)) /* use short vector */
	   {
		if((y1&LO) == (y2&LO))
		   {
			*pc++ = loX(x2);
			n++;
		   }
		else
		   {
			*pc++ = loY(y2);
			*pc++ = loX(x2);
			n += 2;
		   }
	   }
	 else
	   {
		*pc++ = hiXY(y2); 	/* y/32+32 hi y byte */
		*pc++ =  loY(y2); 	/* y%32+96 low y byte */
		*pc++ = hiXY(x2); 	/* x/32+32 hi x byte */
		*pc++ =  loX(x2); 	/* x%32+64 low x byte */
		n += 4;
	   }
	/*
	fprintf(stderr,"D x2=%3d y2=%3d\n",x2,y2);
	*/
	write(plotout,cbuf,n);
	gsreqd= 0;
	xcur= x2;
	ycur= y2;
	xhold= x2;
	yhold= y2;
   }
