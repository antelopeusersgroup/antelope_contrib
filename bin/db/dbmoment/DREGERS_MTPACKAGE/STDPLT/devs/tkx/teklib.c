#include	<stdio.h>
#include	<sys/ttold.h>

extern FILE *f;

/* tektronix device specs */
#define XMAX 	1023	/* maximum x value */
#define YMAX 	780	/* maximum y value (manual says 779, but 780 fits) */
#define PIXINCH		100.0
#define INCHPIX		0.01

#define XSCL(x)		ti.xorig + ti.xscl * ( (x) - ti.xmin)
#define YSCL(y)		ti.yorig + ti.yscl * ( (y) - ti.ymin)
#define UNXSCL(x)	ti.xmin + ( (x) - ti.xorig) / ti.xscl
#define UNYSCL(y)	ti.ymin + ( (y) - ti.yorig) / ti.yscl

struct tekinfo
   {
	float	xorig, yorig;
	float	xscl, yscl;
	float	xmin, ymin;
	float	xcur, ycur;
	int	ixcur, iycur;
	int	ixwmin, iywmin;
	int	ixwmax, iywmax;
	int	gsreqd;
	int	plotout;
   }	ti	= {
	0.0,	0.0,
	1.0,	1.0,
	0.0,	0.0,
	0.0,	0.0,
	0,	0,
	0,	0,
	XMAX,	YMAX,
	1,
	1 };

erase()
   {
	write(ti.plotout,"\033\014",2);
	ti.gsreqd= 1;
	sleep(1);
   }
setorig(xorig,yorig)
float xorig, yorig;
   {
	ti.xorig= xorig;
	ti.yorig= yorig;
   }

setscl(xscl,yscl)
float xscl, yscl;
   {
	ti.xscl= xscl;
	ti.yscl= yscl;
   }

setmin(xmin,ymin)
float xmin, ymin;
   {
	ti.xmin= xmin;
	ti.ymin= ymin;
   }

plot(x,y,ipen)
float x, y;
int ipen;
   {
	int ix, iy;
	ix= (int)(x * PIXINCH);
	iy= (int)(y * PIXINCH);
	if(ipen) vector(ti.ixcur,ti.iycur,ix,iy);
	ti.ixcur= ix;
	ti.iycur= iy;
	ti.xcur= x;
	ti.ycur= y;
   }

uplot(xuser,yuser,ipen)
float xuser, yuser;
int ipen;
   {
	float x, y;
	int ix, iy;
	x= XSCL(xuser);
	y= YSCL(yuser);
	ix= (int)(x * PIXINCH);
	iy= (int)(y * PIXINCH);
	if(ipen) vector(ti.ixcur,ti.iycur,ix,iy);
	ti.ixcur= ix;
	ti.iycur= iy;
	ti.xcur= x;
	ti.ycur= y;
   }

box(x1,y1,x2,y2)
float x1, y1, x2, y2;
   {
	float t;
	if(x1 > x2) { t= x1; x1= x2; x2= t; }
	if(y1 > y2) { t= y1; y1= y2; y2= t; }
	plot(x1,y1,0);
	plot(x2,y1,1);
	plot(x2,y2,1);
	plot(x1,y2,1);
	plot(x1,y1,1);
   }

ubox(ux1,uy1,ux2,uy2)
float ux1, uy1, ux2, uy2;
   {
	float x1, y1, x2, y2;
	x1= XSCL(ux1);
	y1= YSCL(uy1);
	x2= XSCL(ux2);
	y2= YSCL(uy2);
	box(x1,y1,x2,y2);
   }

char getbox(x1,y1,x2,y2)
float *x1,*y1,*x2,*y2;
   {
	float tx1, ty1, tx2, ty2, t;
	char c, pick();
	c= pick(&tx1, &ty1);
	switch(c)
	   {
		case 'b':	/* little marker */
			plot(tx1-0.25,ty1,0); plot(tx1+0.25,ty1,1);
			plot(tx1,ty1-0.25,0); plot(tx1,ty1+0.25,1);
			break;
		case 'B':	/* big marker */
			plot(0.0,ty1,0); plot(8.0,ty1,1);
			plot(tx1,0.0,0); plot(tx1,8.0,1);
			break;
		default:
			return('\0');
	   }
	c= pick(&tx2, &ty2);
	if(c == '\0') return(0);
	if(tx1 > tx2) { t= tx1; tx1= tx2; tx2= t; }
	if(ty1 > ty2) { t= ty1; ty1= ty2; ty2= t; }
	*x1= tx1;
	*y1= ty1;
	*x2= tx2;
	*y2= ty2;
	return(c);
   }
char getubox(ux1,uy1,ux2,uy2)
float *ux1,*uy1,*ux2,*uy2;
   {
	float x1,y1,x2,y2;
	char c, getbox();

	if( (c=getbox(&x1, &y1, &x2, &y2)) != '\0')
	   {
		*ux1= UNXSCL(x1);
		*uy1= UNYSCL(y1);
		*ux2= UNXSCL(x2);
		*uy2= UNYSCL(y2);
	   }
	return(c);
   }

where(xp, yp)
float *xp, *yp;
   {
	*xp = ti.xcur;
	*yp = ti.ycur;
   }

uwhere(xp, yp)
float *xp, *yp;
   {
	*xp = XSCL(ti.xcur);
	*yp = YSCL(ti.ycur);
   }

window(x1,y1,x2,y2)
float x1, y1, x2, y2;
   {
	float t;
	int ix, iy;
	if(x1 > x2) { t= x1; x1= x2; x2= t; }
	if(y1 > y2) { t= y1; y1= y2; y2= t; }
	ix= (int)(x1 * PIXINCH);
	iy= (int)(y1 * PIXINCH);
	if(ix < 0) ix= 0;
	if(iy < 0) iy= 0;
	if(ix > XMAX) ix= XMAX;
	if(iy > YMAX) iy= YMAX;
	ti.ixwmin= ix;
	ti.iywmin= iy;
	ix= (int)(x2 * PIXINCH);
	iy= (int)(y2 * PIXINCH);
	if(ix < 0) ix= 0;
	if(iy < 0) iy= 0;
	if(ix > XMAX) ix= XMAX;
	if(iy > YMAX) iy= YMAX;
	ti.ixwmax= ix;
	ti.iywmax= iy;
   }

uwindow(ux1, uy1, ux2, uy2)
float ux1, uy1, ux2, uy2;
   {
	float x1, y1, x2, y2;
	x1= XSCL(ux1);
	y1= YSCL(uy1);
	x2= XSCL(ux2);
	y2= YSCL(uy2);
	window(x1,y1,x2,y2);
   }

unwindow()
   {
	ti.ixwmin= 0;
	ti.iywmin= 0;
	ti.ixwmax= XMAX;
	ti.iywmax= YMAX;
   }

char pick(x,y)
float *x, *y;
   {
	int n, ix, iy;
	char c, cbuf[32];
	write(1,"\035\033\032",3);
	ti.gsreqd= 1;
	settty();
	n= read(0,cbuf,16);
	unsettty();
	c= '\0';
	if(n != 6) return(c);
	ix= ((cbuf[1] &0x1f) << 5) | (cbuf[2] &0x1f);
	iy= ((cbuf[3] &0x1f) << 5) | (cbuf[4] &0x1f);
	*x= (float)(ix) * INCHPIX;
	*y= (float)(iy) * INCHPIX;
	c = cbuf[0];
	return(c);
   }

char upick(xuser,yuser)
float *xuser, *yuser;
   {
	float x, y;
	char c, pick();
	if( (c = pick(&x,&y)) != '\0')
	   {
		*xuser = UNXSCL(x);
		*yuser = UNYSCL(y);
	   }
	return(c);
   }

/* device codes */
#define GS	035
#define US	037
#define CSZ	032
#define ESC	033
#define ERASE	021

#define hiXY(xy)	(xy>>5)|040
#define loY(y)		(y&037)|0140
#define loX(x)		(x&037)|0100
#define LO		0x1f
#define HI		0x3e0


vector(x1,y1,x2,y2)
int x1,y1,x2,y2;
   {
	char cbuf[16];
	register char *pc;
	register int n;
	static int xhold = -10000;
	static int yhold = -10000;

	if(clip(&x1,&y1,&x2,&y2)) return;
	pc= cbuf;
	n= 0;
	if(ti.gsreqd || x1 != xhold || y1 != yhold) /* do a move */
	   {
		/*
		fprintf(f,"\tmove gs=%d x1=%4d y1=%4d\n",
			ti.gsreqd,x1,y1);
		*/
		*pc++ = GS;
		*pc++ = hiXY(y1); 	/* y/32+32 hi y byte */
		*pc++ =  loY(y1); 	/* y%32+96 low y byte */
		*pc++ = hiXY(x1); 	/* x/32+32 hi x byte */
		*pc++ =  loX(x1); 	/* x%32+64 low x byte */
		n += 5;
	   }
	/* do draw */
	/* use short vector */
	/*
	if((y1&HI) == (y2&HI) && (x1&HI) == (x2&HI))
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
	*/
		*pc++ = hiXY(y2); 	/* y/32+32 hi y byte */
		*pc++ =  loY(y2); 	/* y%32+96 low y byte */
		*pc++ = hiXY(x2); 	/* x/32+32 hi x byte */
		*pc++ =  loX(x2); 	/* x%32+64 low x byte */
		n += 4;
	/*
	   }
	*/
	write(ti.plotout,cbuf,n);
	ti.gsreqd= 0;
	xhold= x2;
	yhold= y2;
   }

setbuttons()
   {
	write(1,"\033P1;1;0|251/z\033\\",15);
	/*
	write(1,"\033P1;1;0%|26/a\033\\",15);
	*/
   }
settty()
   {
	struct sgttyb buf;
	ioctl(0,TIOCGETP,&buf);
	buf.sg_flags &= ~(O_ECHO);
	/*
	buf.sg_flags |= (O_RAW | O_CBREAK);
	*/
	ioctl(0,TIOCSETP,&buf);
   }
unsettty()
   {
	struct sgttyb buf;
	ioctl(0,TIOCGETP,&buf);
	buf.sg_flags |= (O_ECHO);
	/*
	buf.sg_flags &= ~(O_RAW | O_CBREAK);
	*/
	ioctl(0,TIOCSETP,&buf);
   }
/* code for clip */
#define code(x,y) (x<ti.ixwmin?1:(x>ti.ixwmax?2:0))|(y<ti.iywmin?4:(y>ti.iywmax?8:0))

clip(x1,y1,x2,y2)	/* window the plot */
int *x1,*y1,*x2,*y2;
   {
	register int c1,c2,temp;
	register int swap;

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
			temp=(c1&2?ti.ixwmax:ti.ixwmin);
			*y1= solve(temp,*x1,*y1,*x2,*y2);
			*x1=temp;
		   }
		  else		/* move endpoint in y */
		   {
			temp=(c1&8?ti.iywmax:ti.iywmin);
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
