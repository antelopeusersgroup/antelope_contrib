#include	<stdio.h>
#include	"chars.h"

extern int textfont, textcenter;
extern float pixinch, textsize, textcosang, textsinang, xscale, yscale;

/* to be implemented
	Control Sequences:     '\c', where 'c' is one of:
		b:	back space over the last character, a nop first
			the first character in a string.

		t:	tab to next 0.5 inch stop.

		|:	zero width, maximum height character. Prints
			nothing, but is used to determine text centering.

		_:	space

		R:	shift to roman font.

		I:	shift to italic font.

		B:	shift to bold font.

		S:	shift to special font.

		P:	shift to previous font.
 */

text(xcur,ycur,str)
int xcur,ycur;
char str[];
/*
 * interpret characters into vectors
 */
   {
	struct charglyph *cg;
	short *cd;
	int add,drop,xp,yp;
	char c,xyw,*symptr;
	float size, hstepx, hstepy, vstepx, vstepy, xbase, ybase;
	float wid, hgt;
	int n, hshift, vshift;
	int h, v, txorig, tyorig;

	/* start as Roman */
	cg= Rcg;
	cd= Rdata;
	size= textsize * pixinch / vpixels;
	hstepx=   0.6*size*textcosang;
	hstepy=   0.6*size*textsinang;
	vstepx=       size*textsinang;
	vstepy=      -size*textcosang;
	if(textcenter)
	   {
		getwidhgt(str,&wid,&hgt);
		n= strlen(str);
		wid= n * size * 0.6;
		hgt= size;
		hshift = textcenter & 0x03;
		xcur -= 0.5*hshift*wid*textcosang;
		/* ycur -= 0.5*hshift*wid*textsinang; */
		ycur -= 0.38*hshift*wid*textsinang; 
		vshift= (textcenter >>2) & 0x03;
		xcur += 0.5*vshift*hgt*textsinang; 
		ycur -= 0.38*vshift*hgt*textcosang; 
		/*     ycur -= 0.5*vshift*hgt*textcosang;  */
	   }

	txorig= xcur;
	tyorig= ycur;
	xbase= (float) xcur;
	ybase= (float) ycur;
	hstepx= hstepy= 0; /* make an initial backspace a no-op */

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
		isym= (int)(c-040);
		pcg= &cg[isym];
		d= &cd[pcg->index];
		for(i=0; i< pcg->npoints; i++)
		   {
			xyw= *d++;
			h= xyw & 0x3f;
			v= (xyw >> 8);
			xp= (int) (xbase + h*textcosang - v*textsinang);
			yp= (int) (ybase + h*textsinang + v*textcosang);
			if( xyw & 0x080 ) do_line(xcur,ycur,xp,yp);
			xcur=xp; ycur=yp; 
		   }
	moveah:
		hstepx=   (float)(pcg->width)/charpixels *textcosang;
		hstepy=   (float)(pcg->width)/charpixels *textsinang;
		xbase += hstepx; /* move to starting point of next char */
		ybase += hstepy; 
		xcur= (int) xbase; 
		ycur= (int) ybase;
	   }
   }
getwidhgt(str,wid,hgt)
char *str;
int *wid, *hgt;
   {
	/* returns width and height of a string in pixels */
	int c;
	*wid = *hgt = 0;
	while( (c= *str++) != '\0')
	   {
		if(c < 040) /* control character */
		   {
			/* temporary */
			c= ' ';
		   }
		*hgt += Rcg[c].vmove;
		*wid += Rcg[c].width;
	   }
   }
