#include	<stdio.h>
#include	"devpar.h"

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
		*pc++ = (y1>>5)|040; 	/* y/32+32 hi y byte */
		*pc++ = (y1&037)|0140; 	/* y%32+96 low y byte */
		*pc++ = (x1>>5)|040; 	/* x/32+32 hi x byte */
		*pc++ = (x1&037)|0100; 	/* x%32+64 low x byte */
		n += 5;
		/*
		fprintf(stderr,"M x1=%3d y1=%3d ",x1,y1);
		*/
	   }
	/* do draw */
	*pc++ = (y2>>5)|040; 	/* y/32+32 hi y byte */
	*pc++ = (y2&037)|0140; 	/* y%32+96 low y byte */
	*pc++ = (x2>>5)|040; 	/* x/32+32 hi x byte */
	*pc++ = (x2&037)|0100; 	/* x%32+64 low x byte */
	n += 4;
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
