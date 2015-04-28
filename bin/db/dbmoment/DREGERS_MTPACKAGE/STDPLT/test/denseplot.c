#include	<stdio.h>
float	xmin	=   0.05;
float	xmax	=   8.95;
float	ymin	=   0.05;
float	ymax	=   8.45;
float	step	=   0.05;
main()
   {
	float x, y;

	for(x=xmin; x < xmax + 0.5*step; x += step)
	   {
		plot(x,ymin,0);
		plot(x,ymax,1);
	   }
	for(y=ymin; y < ymax + 0.5*step; y += step)
	   {
		plot(xmin,y,0);
		plot(xmax,y,1);
	   }
   }
