#include <stdio.h>
#define SYMBUFSIZE	64
#include "symboldata.h"
#include "global.h"
#define OUTLINE	1
#define	FILL	2
#define	DTOR	0.0174532925
extern int	page;
extern float	pixinch;
extern float	symsize,symsinang,symcosang,symangle;

do_symbol(xpos,ypos,isym,size,angle)
int xpos,ypos,isym;
float size, angle;
   {
	double cos(), sin(), S, C;
	struct intpolygon *pout, *p;
	struct symboldata *pin, *q;
	struct intpolygon symbuf[SYMBUFSIZE];
	int i, release, np, mode;

	if(angle != symangle)
	   {
		symangle= angle;
		switch(page)
		   {
			case 0: 		break;
			case 1: angle +=  90.0; break;
			case 2: angle += 180.0; break;
			case 3: angle += 270.0; break;
		   }
		symsinang= sin(DTOR*angle);
		symcosang= cos(DTOR*angle);
	   }
	C= size * pixinch * symcosang;
	S= size * pixinch * symsinang;
	mode= (isym >> 6) &0x3;
	if(mode == 0) mode= OUTLINE;
	isym &= 0x3f;
	if(isym < 0 || isym >= NSYMBOL) isym= 0;
	np= symheads[isym].npoints;
	i= symheads[isym].index;
	pin= &symdata[i];
	release= 0;
	if(np < SYMBUFSIZE) pout= symbuf;
	 else
	   {
		pout= (struct intpolygon *)(malloc(np* sizeof(struct intpolygon)));
		if(pout == NULL)
			err(WARN,"out of memory in symbol");
		release= 1;
	   }
	for(i=0, q=pin, p=pout; i<np; i++, p++, q++)
	   {
		p->ixv= xpos + (int)( C * q->xp - S * q->yp);
		p->iyv= ypos + (int)( S * q->xp + C * q->yp);
	   }

	if(mode & OUTLINE)
	   {
		p= pout;
		for(i=1; i<np; i++)
			do_line(p[i-1].ixv,p[i-1].iyv,p[i].ixv,p[i].iyv);
		do_line(p[np-1].ixv,p[np-1].iyv,p[0].ixv,p[0].iyv);
	   }
	if(mode & FILL) polyfill(np,pout);
	if(release) free(pout);
   }
