#include	<stdio.h>

int rasxmin, rasymin, rasxmax, rasymax;
short rasbuf[256];
#define ENDRAS	-1

raster(file)
FILE *file;
   {
	extern int xcur, ycur;
	int len, header, off, y1, y2;

	rasxmin= rasxmax= xcur;
	rasymin=  200000;
	rasymax= -200000;

	while( (header= geth(file)) != ENDRAS )
	   {
		len= header & 0xff;
		off= (header>>8) & 0xff;
		fread(rasbuf,2,len,file);
		y1= ycur + 16*off;
		y2= y1   + 16*len;
		if(y1 < rasymin) rasymin= y1;
		if(y2 > rasymax) rasymax= y2;
		rasxmax++;
	   }
   }
