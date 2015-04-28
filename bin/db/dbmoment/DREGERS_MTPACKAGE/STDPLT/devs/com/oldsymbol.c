#include <stdio.h>
#include "../../symboldata/sym.h"
#include "global.h"
#define zero(a) (a<.001 && a>-.001) 
extern struct symlist *currentsym;
extern struct symlist *symlist;
extern float pixinch,symsize,symsinang,symcosang,symangle;
extern double sqrt();
extern double cos();
extern double sin();

/* puts symbol that is centered,scaled to right size,and shifted to
   (xpos,ypos) into currentsym */
setcurrentsym(xpos,ypos,index)
int xpos,ypos,index;
{
	int j;
	float cx,cy,fvertx,fverty,a,b,c,b1,b2,den,temp1,temp2;
	float xp,yp,dx,x,y,mnormal,m2,mvert,costwo,junk;
	float tempang,tempcos,tempsin;


	tempang = symangle;
	tempcos = symcosang;
	tempsin = symsinang;

	/* put vertices values into currentsym */
	for (j=0; j<symlist[index].length; j++) {
		fvertx = (float) symlist[index].vertices[j].ixv;
		fvertx = fvertx*symsize;
		fverty = (float) symlist[index].vertices[j].iyv;
		fverty = fverty*symsize;
		currentsym->vertices[j].ixv = (short) fvertx;
		currentsym->vertices[j].iyv = (short) fverty;
	}

	currentsym->length = symlist[index].length;
	cx = (float) symlist[index].centerx;
	cx = symsize*cx;
	currentsym->centerx = (int) cx; 
	cy = (float) symlist[index].centery;
	cy = symsize*cy;
	currentsym->centery = (int) cy;

	/* flip 180 degrees if necessary */
	if (symangle > 3.141592) {
	   for (j=0; j<symlist[index].length; j++) {
	     fvertx=(short)(2*currentsym->centerx)-currentsym->vertices[j].ixv;
	     fverty=(short)(2*currentsym->centery)-currentsym->vertices[j].iyv;
	     currentsym->vertices[j].ixv = (short) fvertx;
	     currentsym->vertices[j].iyv = (short) fverty;
	   }
	   symangle -= 3.141592;
	   symcosang = cos(symangle);
	   symsinang = sin(symangle);
	}

	/* round to zero if symcosang near zero */
	if (symcosang<.001 && symcosang>-.001){ 
		symcosang = 0.0;
		symsinang = 1.0;
	}

	if (symcosang < 1.001 && symcosang > .991) symcosang = 1.0;

	for (j=0; j<symlist[index].length; j++) {
		fvertx = (int) currentsym->vertices[j].ixv;
		fverty = (int) currentsym->vertices[j].iyv;

		/* rotation is zero degrees */
		if (symcosang == 1.0) {
		  currentsym->vertices[j].ixv -= (short)currentsym->centerx;
		  currentsym->vertices[j].ixv += (short)xpos;
		  currentsym->vertices[j].iyv -= (short)currentsym->centery;
		  currentsym->vertices[j].iyv += (short)ypos;
		  continue;
		}

		a = (float) currentsym->centerx - fvertx;
		b = (float) currentsym->centery - fverty;

		/* this point is center point */
		if (a==0 && b==0) {
			currentsym->vertices[j].ixv = (short)xpos;
			currentsym->vertices[j].iyv = (short)ypos;
			continue;
		}

		c = sqrt(a*a+b*b);

		/* point is on vertical line with center */
		if (a==0) { /*centerx = fvertx*/
		    if (symcosang==0) {    /* rotate 90 degrees */
			if (b<0) {
				x =  currentsym->centerx - c;
				y =  currentsym->centery;
			} else {
				x =  currentsym->centerx + c;
				y =  currentsym->centery;
			}
	            } else 
		    if (b < 0) {
			x = fvertx - c*symsinang;
			y = fverty - c + c*symcosang;
		    } else {
			x = fvertx + c*symsinang;
			y = fverty + c - c*symcosang;
		    }
		};
		/* point is on horizontal line with center */
		if (b==0) { /*centery = fverty*/
		    if (symcosang==0) {  /*rotate 90 degrees*/
			if (a<0) {
				x = currentsym->centerx;
				y = currentsym->centery + c;
			} else {
				x = currentsym->centerx;
				y = currentsym->centery - c;
			}
	            } else
		    if (a >0 ) {
			x = fvertx +c - c*symcosang;
			y = fverty - c*symsinang;
		    } else {
			x = fvertx - c +c*symcosang;
			y = fverty + c*symsinang;
		    }
		};

		/* point is not vertical or horizontal to center*/
		if (a!=0 && b!=0) {
			if (symcosang==0) {   /*rotate 90 degrees*/
				x = currentsym->centerx+b;
				y = currentsym->centery-a;
			}
			if (symcosang!=0) {
				mvert = b/a;
				mnormal = -(a/b);
				xp = fvertx + a - a*symcosang;
				yp = fverty + b - b*symcosang;
				m2 = mvert + (symsinang/symcosang);
				den = 1 - mvert*(symsinang/symcosang);
				if zero(den) den=0.0;
				if(den==0) { /* new vert is vertical to center*/
					if (a<0) {
						x = currentsym->centerx;
						y = currentsym->centery + c;
					} else {
						x = currentsym->centerx;
						y = currentsym->centery - c;
					}
				} /*den==0*/
				if (den!=0) {
					m2 /=  den;
					/* m2 is angle of line between center
					   and new rotated vert */
					if(m2==0){/*new vert is hor. to center*/
						if (b<0) {
						x = currentsym->centerx - c;
						y = currentsym->centery;
						} else {
						x = currentsym->centerx + c;
						y = currentsym->centery;
						}
					} /*m2==0*/
					if (m2!=0) {
						b1 = yp - mnormal*xp;
						b2 = cy - m2*cx;
						x = (b2 - b1)/(mnormal - m2);
						y = mnormal*x + b1;
						y = m2*x + b2;
					} /*m2!=0*/
				} /*den!=0*/
			} /*cos!=0*/
		} /* a!=0 || b!=0 */
		currentsym->vertices[j].ixv = (short) x;
		currentsym->vertices[j].iyv = (short) y;
		currentsym->vertices[j].ixv -= (short)currentsym->centerx;
		currentsym->vertices[j].ixv += (short)xpos;
		currentsym->vertices[j].iyv -= (short)currentsym->centery;
		currentsym->vertices[j].iyv += (short)ypos;
	} /*for*/
	symangle = tempang;
	symcosang = tempcos;
	symsinang = tempsin;
}/*setcurrentsym*/

outlinesym()
{
	int i,dx1,dy1,dx2,dy2;
	for (i=0; i < currentsym->length-1; i++) {
		dx1 = (int) currentsym->vertices[i].ixv;
		dy1 = (int) currentsym->vertices[i].iyv;
		dx2 = (int) currentsym->vertices[i+1].ixv;
		dy2 = (int) currentsym->vertices[i+1].iyv;
		do_line(dx1,dy1,dx2,dy2);
	}
	dx1 = (int) currentsym->vertices[0].ixv;
	dy1 = (int) currentsym->vertices[0].iyv;
	do_line(dx2,dy2,dx1,dy1); /*line from last point to first point*/
}
