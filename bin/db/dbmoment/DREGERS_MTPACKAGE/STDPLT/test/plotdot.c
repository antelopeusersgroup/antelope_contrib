#include	<stdio.h>

main()
   {
	int ix, iy, imode;
	float size, x, y;
	for(iy=0; iy<6; iy++)
	for(ix=0; ix<6; ix++)
	   {
		x= 0.5 + (float)(ix);
		y= 0.5 + (float)(iy);
		size= (float)(ix+iy)/36.0;
		imode= (ix+iy)%2;
		plotdot(x,y,size,imode);
	   }
   }
