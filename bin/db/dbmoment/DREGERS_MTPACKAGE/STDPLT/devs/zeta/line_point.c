#include <stdio.h> /***********************/
do_line(x1,y1,x2,y2)
int x1,y1,x2,y2;
/*
 * Vector rasterizes the line defined by the endpoints (x1,y1) and (x2,y2).
 * If 'penfat' is nonzero then draw parallel lines to fatten the line.
 */
   {
	extern int penfat;
	extern int draw_fat;
	register int test, i;
	int tx1,ty1,tx2,ty2, i1, i2;

	if(draw_fat && (penfat > 0))
	   {
		i1= penfat/2;
		i2= (penfat+1)/2;
		test= ( abs(x2-x1) >= abs(y2-y1) );
		for(i= -i1; i <= i2; i++)
		   {
			if(test)
			   {
				tx1=x1; ty1=y1+i;
				tx2=x2; ty2=y2+i;
			   }
			 else
			   {
				tx1=x1+i; ty1=y1;
				tx2=x2+i; ty2=y2;
			   }
			if( clip(&tx1,&ty1,&tx2,&ty2) ) {
				continue;
			}
			vector(tx1,ty1,tx2,ty2);
		   }
	   }
	 else
	   {
		if( clip(&x1,&y1,&x2,&y2) ) {
			return;
		}

		vector(x1,y1,x2,y2);
	   }
   }
do_point(x1,y1)
register int x1,y1;
/*
 * Point turns on the point (x1,y1).
 * If penfat <= 0, then only the point x1, y1 is turned on.
 * If penfat == 1, then the neighbours to the N,E,S, and W are also turned on.
 * If penfat == 2, then in addition to penfat=1, NE, SE, SW, and NW are used.
 * If penfat >= 3, points 2N,2E,2S, and 2W are also turned on.
 * The extra points are done by vecrtor draws.
 *
 *
 *		   666
 *		  65556
 *		 6543456
 *		654212456
 *		653101356
 *		654212456
 *		 6543456
 *		  65556
 *		   666
 *
 * This routine could be made more effeicient by doing ther clipping here,
 * thus avoiding the intermediate call to do_line.
 */
   {

	extern int penfat;
	extern int draw_fat;
	if(penfat < 0) return;
	switch(((draw_fat) ? penfat : 0))
	   {
		case 0:	do_line(x1,y1,x1,y1);
			break;

		case 1:	do_line(x1-1,y1  ,x1+1,y1  );
			do_line(x1  ,y1-1,x1  ,y1+1);
			break;

		case 2:	do_line(x1-1,y1-1,x1-1,y1+1);
			do_line(x1  ,y1-1,x1  ,y1+1);
			do_line(x1+1,y1-1,x1+1,y1+1);
			break;

		case 3:	do_line(x1-1,y1-1,x1-1,y1+1);
			do_line(x1  ,y1-2,x1  ,y1+2);
			do_line(x1+1,y1-1,x1+1,y1+1);
			do_line(x1-2,y1  ,x1+2,y1  );
			break;

		case 4:	do_line(x1-2,y1-1,x1-2,y1+1);
			do_line(x1-1,y1-2,x1-1,y1+2);
			do_line(x1  ,y1-2,x1  ,y1+2);
			do_line(x1+1,y1-2,x1+1,y1+2);
			do_line(x1+2,y1-1,x1+2,y1+1);
			break;

		case 5:	do_line(x1-3,y1-1,x1-3,y1+1);
			do_line(x1-2,y1-2,x1-2,y1+2);
			do_line(x1-1,y1-3,x1-1,y1+3);
			do_line(x1  ,y1-3,x1  ,y1+3);
			do_line(x1+1,y1-3,x1+1,y1+3);
			do_line(x1+2,y1-2,x1+2,y1+2);
			do_line(x1+3,y1-1,x1+3,y1+1);
			break;

		default:
		case 6:	do_line(x1-4,y1-1,x1-4,y1+1);
			do_line(x1-3,y1-2,x1-3,y1+2);
			do_line(x1-2,y1-3,x1-2,y1+3);
			do_line(x1-1,y1-4,x1-1,y1+4);
			do_line(x1  ,y1-4,x1  ,y1+4);
			do_line(x1+1,y1-4,x1+1,y1+4);
			do_line(x1+2,y1-3,x1+2,y1+3);
			do_line(x1+3,y1-2,x1+3,y1+2);
			do_line(x1+4,y1-1,x1+4,y1+1);
			break;
	   }
   }

do_dash(x1,y1,x2,y2)
int x1,y1,x2,y2;
   {
	extern float pixinch,inchpix;
	extern float *dashptr;
	extern float dashval;
 	int i, ixold, iyold;
	int xstart,ystart;
	float sx, sy, alen, gap, d,tempsx,tempsy,oldalen,totgap;
	double fmod(), sqrt();

/* BUG, d needs to be set from dashval, else dash sequence restarts with every
       draw */
	float xp, yp;
	sx= (float)(x2 - x1);
	sy= (float)(y2 - y1);
	alen= sqrt(sx*sx +sy*sy);
	d= dashval;
	dashval= fmod(d+alen,dashptr[3]);
	if(alen < inchpix) return;
	sx= sx/alen;
	sy= sy/alen;
	xp= (float)(x1);
	yp= (float)(y1);
	for(i=0; i<4 && d > dashptr[i]; i++);
	while( alen > 0.0 )
	   {
		int ixp, iyp;
		gap= dashptr[i] -d;
		if(gap > alen) gap= alen;
		xp += gap*sx;
		yp += gap*sy;
		ixp= (int)(xp);
		iyp= (int)(yp);
		if(!(i%2)) do_line(x1,y1,ixp,iyp);
		alen -=  gap;
		d= dashptr[i];
		if( ++i >= 4 ) { i=0; d= 0.0; }
		x1= ixp;
		y1= iyp;
	   }
   }
/* old code .....
	ixold= x1;
	iyold= y1;
	xstart = x1;
	ystart = y1;
	sx= (float)(x2 - x1);
	sy= (float)(y2 - y1);
	alen= sqrt(sx*sx +sy*sy);
	oldalen=alen;
	if(alen < inchpix) return;
	sx= sx/alen;
	sy= sy/alen;
	d = 0.0;
	i = 0;
	totgap = 0.0;
	while( alen > 0.0) 
	   {
		gap= (dashptr[i] -d)*pixinch; 
		totgap += gap;
		if (gap>=alen) {
			gap=alen;
			totgap=oldalen;
		}	
		x1 = (int) (totgap*sx) + xstart;
		y1 = (int) (totgap*sy) + ystart;
		if(!(i%2)) do_line(ixold,iyold,x1,y1);
		alen -=  gap;
		if (alen<=0.0 && i%2) do_line(ixold,iyold,x2,y2);
		d= dashptr[i];
		if( ++i >= 4 ) { i=0; d= 0.0; }
		ixold= x1;
		iyold= y1;
	   }
   }
.... end of old code */

/* code for clip */
#define code(x,y) (x<ixwmin?1:(x>ixwmax?2:0))|(y<iywmin?4:(y>iywmax?8:0))

clip(x1,y1,x2,y2)	/* window the plot */
int *x1,*y1,*x2,*y2;
   {
	register int c1,c2,temp;
	register int swap;
	extern int ixwmin, iywmin, ixwmax, iywmax;

	c1=code(*x1,*y1);
	c2=code(*x2,*y2);
	swap=0;
	if(!(c1||c2)) return(0); /* line completely in bounds */
	while(c1|c2)
	   {
		if( c1&c2 )
			return(1);  /* line completely out of bounds */
		if(!c1)	/* interchange endpoints */
		   {
			temp= *x1;*x1= *x2;*x2=temp;
			temp= *y1;*y1= *y2;*y2=temp;
			temp=c1;c1=c2;c2=temp;
			swap= ~swap;
		   }
		if(c1<4)	/* move endpoint in x */
		   {
			temp=(c1&2?ixwmax:ixwmin);
			*y1= solve(temp,*x1,*y1,*x2,*y2);
			*x1=temp;
		   }
		  else		/* move endpoint in y */
		   {
			temp=(c1&8?iywmax:iywmin);
			*x1= solve(temp,*y1,*x1,*y2,*x2);
			*y1=temp;
		   }
		c1=code(*x1,*y1);
	   }
	if( swap )	/* put endpoints in order */
	   {
		temp= *x1; *x1= *x2; *x2=temp;
		temp= *y1; *y1= *y2; *y2=temp;
	   }
	return(0);
   }

solve(pnot,p1,q1,p2,q2)
register int pnot,p1,q1,p2,q2;
   {
	/* floating point version */
	double invslope;
	register int qnot;
	if(pnot==p1) return(q1);
	if(pnot==p2) return(q2);
	if(q1==q2) return(q1);
	invslope= (q1-q2)/( (double) (p1-p2));
	qnot= (pnot-p1)*invslope + (double) q1 + 0.5;
	return(qnot);
   }
double fmod(x,y)
float x, y;
   {
	double fm, floor();
	if(y== 0.0) return(1.0);
	fm= x - floor(x/y) *y;
	return(fm);
  }
