#include	<stdio.h>
#include	"../../h/chars.h"

extern int textfont, textcenter;
extern float pixinch, textsize, textcosang, textsinang, xscale, yscale;

extern int penmode; /**TAKE THS OUT */

#define DROPBIT 02000
short *symadd;
char *symbase;

text(xcur,ycur,str)
int xcur,ycur;
char str[];
/*
 * interpret characters into vectors
 */
   {
	int add,drop,xp,yp;
	char c,xyw,*symptr;
	float size, hstepx, hstepy, vstepx, vstepy, xbase, ybase;
	float wid, hgt;
	int n, hshift, vshift;
	int h, v, txorig, tyorig;
	symadd= ascii.saddr;
	symbase= ascii.svec;
	if (textsize > 8.0) size = textsize * pixinch / 72.27;
	else size= textsize * pixinch;
	hstepx=   0.6*size*textcosang;
	hstepy=   0.6*size*textsinang;
	vstepx=       size*textsinang;
	vstepy=      -size*textcosang;
	if(textcenter)
	   {
		n= strlen(str);
		wid= n * size * 0.6;
		hgt= size;
		hshift = textcenter & 0x00000003;
		/*hshift= textcenter % 3;*/
		xcur -= 0.5*hshift*wid*textcosang;
		ycur -= 0.5*hshift*wid*textsinang;
		vshift= (textcenter >>2) % 3;
		xcur += 0.5*vshift*hgt*textsinang; 
		ycur -= 0.5*vshift*hgt*textcosang;
	   }


	size = size/10.0;       /* do this as a constant for symbol file */
	txorig= xcur;
	tyorig= ycur;
	xbase= (float) xcur;
	ybase= (float) ycur;

	while( (c= *str++) != '\0')
	   {
		if(c < 040)
		  {
			switch (c)	/* standard carriage controls */
			   {
				case '\b': /* back space */
					xcur -= hstepx;
					ycur -= hstepy;
					xbase -= hstepx;
					ybase -= hstepy;
					break;
				case '\n': /* newline */
					txorig += vstepx;
					tyorig += vstepy;
					xcur= txorig;
					ycur= tyorig;
					xbase = (float) xcur;
					ybase = (float) ycur;
					break;
				default: /* map to blank */
					c= ' ';
					break;
			   }
			continue;
		   }
		if(c == ' ') goto moveah;
		add= symadd[(int)(c-040)];
		symptr= symbase +(add&01777);
		drop= (add&DROPBIT ? 2 : 0);
		do
		   {
			xyw= *symptr++;
			h= (((xyw&0160)>>4)*size); 
			v= (((xyw&07) - drop)*size);
			xp= (int) (xbase + h*textcosang - v*textsinang);
			yp= (int) (ybase + h*textsinang + v*textcosang);
			if( !(xyw&0200) ) {
				do_line(xcur,ycur,xp,yp);
			}
			xcur=xp; ycur=yp; 
		   } while( !(xyw&010) );
	moveah:
		xbase += hstepx; /* move to starting point of next char */
		ybase += hstepy; 
		xcur= (int) xbase; 
		ycur= (int) ybase;
	   }
   }
