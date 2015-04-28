/* 8-bit interface version */

#include	<stdio.h>
#include	<signal.h>
#include	<sgtty.h>
#define ESC	033
#define PIXINCH	64.0
#define XMAX	1024
#define YMAX	1024

erase()
   {
	char c[2];
	c[0]= '~';
	write(pc._pltout,c,1);
	sleep(1);
   }

struct vertex
   {
	int xv;
	int yv;
   };

ipolygon(x0,y0,p,np)
struct { int xv, yv; } *p;
int x0, y0;
int np;
   {
	int i, iy, ix, x1, x2, n, ymin, ymax;
	struct offon { int xstr, xend; } *s;
	char c[16];

	if( (s= (struct offon *)malloc(8*np)) == NULL)
	   {
		fprintf(stderr,"ipolygon: cannot allocate memory\n");
		return(-1);
	   }
	p[0].xv += x0;
	p[0].yv += y0;
	ymin= ymax= p[0].yv;
	p[0].yv= 2*p[0].yv +1;
	for(i=1; i<np; i++)
	   {
		p[i].xv += x0;
		p[i].yv += y0;
		if(p[i].yv < ymin) ymin= p[i].yv;
		if(p[i].yv > ymax) ymax= p[i].yv;
		p[i].yv= 2*p[i].yv +1;
	   }

	if(ymin < pc._iywmin) ymin= pc._iywmin;
	if(ymax > pc._iywmax) ymax= pc._iywmax;
	for(iy=ymin; iy<=ymax; iy++)
	   {
		n= intersect(2*iy,p,np,s);
		if(n == 0) continue;
		for(i=0; i<n; i++)
		   {
			x1= s[i].xstr;
			if(x1 < pc._ixwmin) x1= pc._ixwmin;
			x2= s[i].xend;
			if(x2 > pc._ixwmax) x2= pc._ixwmax;
			if(x2 >= x1)
			   {
				c[0]= 'Q';
				c[1]= (( (x1>>8) &03)<<4) | ( (iy>>8) &03 );
				c[2]= x1&0xff;
				c[3]= iy&0xff;
				c[4]= 'o';
				c[5]= (( (x2>>8) &03)<<4) | ( ((iy+1)>>8) &03 );
				c[6]= x2&0xff;
				c[7]= (iy+1)&0xff;
				write(pc._pltout,c,8);
			   }
		   }
	   }
	for(i=0; i<np; i++)
	   {
		p[i].xv= (p[i].xv -x0);
		p[i].yv= (p[i].yv -1)/2 -y0;
	   }
	free(s);
   }

reset()
   {
	char c[2];
	c[0]= ESC;
	c[1]= '\0';
	write(pc._pltout,c,2);
	sleep(3);
   }
char initstr[]	="\033G1888N";
init()
   {
	extern int ttyreset();
	ttyset();
	signal(SIGINT ,ttyreset);
	signal(SIGQUIT,ttyreset);
	write(pc._pltout,initstr,7);
	sleep(1);
   }
finish()
   {
	char c[2];
	c[0]= 1;
	write(pc._pltout,c,1);
	ttyreset();
   }

setcolor(icol)
int icol;
   {
	char c[2];
	c[0]= 'C';
	c[1]= icol& 0xff;
	write(pc._pltout,c,2);
   }
vector(x1,y1,x2,y2,nfat)
int x1,y1,x2,y2,nfat;
/*
 * Vector plots the line defined by the endpoints (x1,y1) and (x2,y2).
 * If 'nfat' is nonzero then draw parallel lines to fatten the line, by
 * recursive calls to vector.
 */
   {
	register int test, i;
	static int xstor, ystor;
	char c[8];

	if(nfat)
	   {
		test= ( abs(x2-x1) >= abs(y2-y1) );
		for(i= -(nfat/2);i<=(nfat+1)/2;i++)
		   {
			if(test)  vector(x1,y1+i,x2,y2+i,0);
			 else	  vector(x1+i,y1,x2+i,y2,0);
		   }
		return;
	   }
	if( clip(&x1,&y1,&x2,&y2) ) return;
	if(x1 == x2 && y1 == y2 ) return;

	if(x1 != xstor || y1 != ystor)
	   {
		c[0]= 'Q';
		c[1]= ( ( (x1>>8) &03)<<4) | ( (y1>>8) &03 );
		c[2]= x1&0xff;
		c[3]= y1&0xff;
		c[4]= 'A';
		c[5]= ( ( (x2>>8) &03)<<4) | ( (y2>>8) &03 );
		c[6]= x2&0xff;
		c[7]= y2&0xff;
		write(pc._pltout,c,8);
	   }
	 else
	   {
		c[0]= 'A';
		c[1]= ( ( (x2>>8) &03)<<4) | ( (y2>>8) &03 );
		c[2]= x2&0xff;
		c[3]= y2&0xff;
		write(pc._pltout,c,4);
	   }
	xstor= x2; ystor= y2;
   }

#define PIXFILLBUF	512
pixelfill(vals,nx,ny,fdx,fdy)
char *vals;
int nx, ny;
float fdx, fdy;
   {
	int len, x0, y0, val, oldval, ix, iy, xp, yp;
	float shiftx, shifty;
	int dxabs, dyabs, nbuf,dx,dy;
	char *pvals;
	char c[PIXFILLBUF], *cptr;
	dx= (int)fdx;
	if(fdx > 0.0)	if( fdx - (float)(dx) > 0.0 ) dx++;
	 else		if( fdx - (float)(dx) < 0.0 ) dx--;
	dy= (int)fdy;
	if(fdy > 0.0)	if( fdy - (float)(dy) > 0.0 ) dy++;
	 else		if( fdy - (float)(dy) < 0.0 ) dy--;
	len= nx*ny;
	iwhere(&x0,&y0);
	dxabs= abs(dx);
	dyabs= abs(dy);
	shiftx= (fdx < 0 ? -fdx : 0 );
	shifty= (fdy < 0 ? -fdy : 0 );
	pvals= vals;
	cptr= c;
	nbuf=0;

	oldval= -1;
	for(iy=0; iy<ny; iy++)
	   {
		yp= y0 + iy*fdy + shifty;
		for(ix=0; ix<nx; ix++)
		   {
			val= (unsigned int) *pvals++;
			if(val != oldval)
			   {
				*cptr++ = 'C';
				*cptr++ = val & 0xff;
				nbuf += 2;
			   }
			xp= x0 + ix*fdx + shiftx;
			*cptr++ = 'Q';
			*cptr++ = ( ( (xp>>8) &03)<<4) | ( (yp>>8) &03 );
			*cptr++ = xp&0xff;
			*cptr++ = yp&0xff;
			*cptr++ = ',';
			*cptr++ = dxabs & 0xff;
			*cptr++ = dyabs & 0xff;
			nbuf += 7;
			if(nbuf >= PIXFILLBUF-10)
			   {
				write(pc._pltout,c,nbuf);
				cptr= c;
				nbuf= 0;
			   }
			oldval= val;
		   }
	   }
	if(nbuf) write(pc._pltout,c,nbuf);
   }

loadctab(rgb,icolst,ncol)
struct { char r, g, b; } *rgb;
int icolst, ncol;
   {
	int i;
	char c[4];
	c[0]= 'K';
	c[1]= icolst & 0xff;
	c[2]= ncol   & 0xff;
	write(pc._pltout,c,3);
	write(pc._pltout,rgb,3*ncol);
   }

defcolor(icol,rval,gval,bval)
int icol;
float rval, gval, bval;
   {
	char c[6];
	int k;

	c[0]= 'K';
	c[1]= icol & 0xff;
	c[2]= 1;

	k= (int)(255.0*rval);
	if(k > 255) k=255;  if(k < 0) k=0;
	c[3]= k & 0xff;

	k= (int)(255.0*gval);
	if(k > 255) k=255;  if(k < 0) k=0;
	c[4]= k & 0xff;

	k= (int)(255.0*bval);
	if(k > 255) k=255;  if(k < 0) k=0;
	c[5]= k & 0xff;

	write(pc._pltout,c,6);
   }

struct sgttyb oldtty, newtty;
ttyset()
   {
	gtty(pc._pltout,&oldtty);
	newtty.sg_ispeed = oldtty.sg_ispeed;
	newtty.sg_ospeed = oldtty.sg_ospeed;
	newtty.sg_erase  = oldtty.sg_erase;
	newtty.sg_kill   = oldtty.sg_kill;
	newtty.sg_flags  = oldtty.sg_flags;
	/*newtty.sg_flags |= CBREAK| TANDEM | RAW;*/
	newtty.sg_flags |= CBREAK| RAW;
	stty(pc._pltout,&newtty);
   }
ttyreset()
   {
	stty(pc._pltout,&oldtty);
	exit(-1);
   }

setwmask(mask)
int mask;
   {
	char c[2];
	c[0]= 'L';
	c[1]= mask;
	write(1,c,2);
   }

setrmask(mask)
int mask;
   {
	char c[5];
	c[0]= 'M';
	c[1]= c[2]= c[3]= c[4]= mask;
	write(1,c,5);
   }

alpha(ix,iy)
int ix, iy;
   {
	iplot(ix,iy,0);
	putchar(01);
   }
aoi(x1,y1,x2,y2)
int x1, y1, x2, y2;
   {
	char c[4];
	c[0]= 'Q';
	c[1]= ( ( (x1>>8) &03)<<4) | ( (y1>>8) &03 );
	c[2]= x1&0xff;
	c[3]= y1&0xff;
	write(1,c,4);
	c[0]= 'r';
	c[1]= ( ( (x2>>8) &03)<<4) | ( (y2>>8) &03 );
	c[2]= x2&0xff;
	c[3]= y2&0xff;
	write(1,c,4);
   }
wrtaoi(buf,n)
char *buf;
int n;
   {
	char c[4];
	c[0]= 'X';
	write(1,c,1);
	write(1,buf,n);
   }

delay(n)
int n;
   {
	double a, b, c, d;
	int i;
	a=1.2345;
	b=7.6543;
	c=4.287;
	while(n--)
	for(i=0;i<200; i++) d= a*b+c*b+a*c;
   }

zoom(ixfac,iyfac)
int ixfac, iyfac;
   {
	char c[4];
	c[0]= 'E';
	c[1]= ixfac;
	c[2]= iyfac;
	write(1,c,3);
   }

setrasorig(ix,iy)
int ix, iy;
   {
	char c[5];
	iy = iy+779/7;
	c[0]= 'g';
	c[1]= ix/256;
	c[2]= ix %256;
	c[3]= iy/256;
	c[4]= iy %256;
	write(1,c,5);
   }

genctab(rgb,nlvl,fmt,grylvl)
struct guns { char r, g, b; } *rgb;
int nlvl;
char *fmt;
float grylvl;
   {
	struct guns *pos, *neg;
	int i;
	float wt1, wt2;

	wt1= (1.0 - grylvl)/(float)nlvl;
	wt2= grylvl/(float)nlvl;
	pos= rgb + nlvl-1;
	neg= pos;
	for(i=0; i<nlvl; i++)
	   {
		pos->r= (int)(255.0*(grylvl + wt1*i));
		pos->g= (int)(255.0*(grylvl - wt2*i));
		pos->b= (int)(255.0*(grylvl - wt2*i));
		pos++;

		neg->r= (int)(255.0*(grylvl - wt2*i));
		neg->g= (int)(255.0*(grylvl - wt2*i));
		neg->b= (int)(255.0*(grylvl + wt1*i));
		neg--;
	   }
   }

ipixel(ix,iy,delta,icol)
int ix, iy;
int delta, icol;
   {
	char c[10];

	c[0] = 'C';
	c[1] = icol & 0xff;
	c[2]= 'Q';
	c[3]= ( ( (ix>>8) &03)<<4) | ( (iy>>8) &03 );
	c[4]= ix&0xff;
	c[5]= iy&0xff;
	c[6] = ',';
	c[7] = delta & 0xff;
	c[8] = delta & 0xff;
	write(pc._pltout,c,9);
   }
upixel(xuser,yuser,delta,icol)
float xuser, yuser;
int delta, icol;
   {
	char c[10];
	int ix, iy;

	ix= (int)( (pc._xorig + (xuser-pc._xmin)*pc._xscl)*PIXINCH );
	iy= (int)( (pc._yorig + (yuser-pc._ymin)*pc._yscl)*PIXINCH );
	c[0] = 'C';
	c[1] = icol & 0xff;
	c[2]= 'Q';
	c[3]= ( ( (ix>>8) &03)<<4) | ( (iy>>8) &03 );
	c[4]= ix&0xff;
	c[5]= iy&0xff;
	c[6] = ',';
	c[7] = delta & 0xff;
	c[8] = delta & 0xff;
	write(pc._pltout,c,9);
   }
ubox(x1user,y1user,x2user,y2user,icol)
float x1user, y1user;
float x2user, y2user;
int icol;
   {
	char c[10];
	int ix1, iy1, ix2, iy2, dx, dy, itemp;

	ix1= (int)( (pc._xorig + (x1user-pc._xmin)*pc._xscl)*PIXINCH );
	iy1= (int)( (pc._yorig + (y1user-pc._ymin)*pc._yscl)*PIXINCH );
	ix2= (int)( (pc._xorig + (x2user-pc._xmin)*pc._xscl)*PIXINCH );
	iy2= (int)( (pc._yorig + (y2user-pc._ymin)*pc._yscl)*PIXINCH );
	if(ix2<ix1) { itemp= ix1; ix1=ix2; ix2= itemp; }
	if(iy2<iy1) { itemp= iy1; iy1=iy2; iy2= itemp; }
	dx= ix2-ix1;
	dy= iy2-iy1;
	c[0] = 'C';
	c[1] = icol & 0xff;
	c[2]= 'Q';
	c[3]= ( ( (ix1>>8) &03)<<4) | ( (iy1>>8) &03 );
	c[4]= ix1&0xff;
	c[5]= iy1&0xff;
	c[6] = ',';
	c[7] = dx & 0xff;
	c[8] = dy & 0xff;
	write(pc._pltout,c,9);
   }
