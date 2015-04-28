#include	<stdio.h>
#include	"global.h"
#include	"../../h/igl.h"

extern int ixwmin, ixwmax, iywmin, iywmax;
struct on_off { int on, off; };


struct on_off onoffbuf[POLYBUFSIZE];

polyfilln(npoly,nverts,verts)
int npoly, *nverts;
struct intpolygon **verts;
   {
	int release, i, n, ix, ixmin, ixmax, t, y1, y2;
	int np, nv, ns;
	struct on_off *s;
	struct intpolygon *p;

	np= 0;
	for(n=0; n<npoly; n++) np += nverts[n];
	release=0;
	if(np < POLYBUFSIZE) s= onoffbuf;
	 else
	   {
		s= (struct on_off *)malloc(np*sizeof(struct on_off));
		if(s == NULL)
		   {
			err(WARN,"cannot allocate memory in poly size = %d",np);
			return(-1);
		   }
		release= 1;
	   }
	ixmin= ixmax= verts[0][0].ixv;
	for(n=0; n<npoly; n++)
	   {
		if( (nv= nverts[n]) < 3) continue;
		p= verts[n];
		for(i=0; i<nv; i++)
		   {
			t= p[i].ixv;
			if(ixmin > t) ixmin= t;
			if(ixmax < t) ixmax= t;
			p[i].ixv= 2*t +1;
		   }
	   }
	if(ixmin < ixwmin) ixmin= ixwmin;
	if(ixmax > ixwmax) ixmax= ixwmax;
	if(ixmax < ixmin) return(0);

	for(ix= ixmin; ix <= ixmax; ix++)
	   {
		ns= 0;  /*  ns is the number of pairs of on_of points  */
			/*  2*ns is the total number of integers in s  */
		for(n=0; n<npoly; n++)
		   {
			if( (nv= nverts[n]) < 3) continue;
			p= verts[n];
			ns += intersect(2*ix,p,nv,&s[ns]);
			if(nv > 8 && ((ix%50) == 0))
				nverts[n] =polyreduce(2*ix,p,nv);
		   }
		polysort(s,2*ns);
		for(i=0; i<ns; i++)
		   {
			y1= s[i].on;
			y2= s[i].off;
			if(y1 < iywmin) y1= iywmin;
			if(y2 > iywmax) y2= iywmax;
			if(y2 < y1) continue;
			linefill(ix,y1,y2);
		   }
	   }
	if(release) free(s);
	return(0);
   }

polyfill(nvert,verts)
int nvert;
struct intpolygon *verts;
   {
	int release, i, n, ix, ixmin, ixmax, t, y1, y2;
	int np, ns;
	struct on_off *s;

	release=0;
	if(nvert < POLYBUFSIZE) s= onoffbuf;
	 else
	   {
		s= (struct on_off *)malloc(nvert*sizeof(struct on_off));
		if(s == NULL)
		   {
			err(WARN,"cannot allocate memory in poly size = %d",nvert);
			return(-1);
		   }
		release= 1;
	   }
	ixmin= ixmax= verts[0].ixv;
	for(i=0; i<nvert; i++)
	   {
		t= verts[i].ixv;
		if(ixmin > t) ixmin= t;
		if(ixmax < t) ixmax= t;
		verts[i].ixv= 2*t +1;
	   }
	if(ixmin < ixwmin) ixmin= ixwmin;
	if(ixmax > ixwmax) ixmax= ixwmax;
	if(ixmax < ixmin) return(0);
	for(ix= ixmin; ix <= ixmax; ix++)
	   {
		ns = intersect(2*ix,verts,nvert,s);
		if(nvert > 8 && ((ix%10) == 0))
			nvert =polyreduce(2*ix,verts,nvert);
		polysort(s,2*ns);
		for(i=0; i<ns; i++)
		   {
			y1= s[i].on;
			y2= s[i].off;
			if(y1 < iywmin) y1= iywmin;
			if(y2 > iywmax) y2= iywmax;
			if(y2 < y1) continue;
			linefill(ix,y1,y2);
		   }
	   }
	if(release) free(s);
	return(0);
   }

sympolyfill(nvert,verts)
int nvert;
struct intpolygon *verts;
   {
	int release, i, n, ix, ixmin, ixmax, t, y1, y2;
	int np, ns;
	struct on_off *s;
	struct intpolygon temp[MAXSYMVERTS];

	release=0;
	if(nvert < POLYBUFSIZE) s= onoffbuf;
	 else
	   {
		s= (struct on_off *)malloc(nvert*sizeof(struct on_off));
		if(s == NULL)
		   {
			err(WARN,"cannot allocate memory in poly size = %d",nvert);
			return(-1);
		   }
		release= 1;
	   }
	ixmin= ixmax= verts[0].ixv;
	for(i=0; i<nvert; i++)
	   {
		t= verts[i].ixv;
		temp[i].iyv = verts[i].iyv;
		if(ixmin > t) ixmin= t;
		if(ixmax < t) ixmax= t;
		/*verts[i].ixv= 2*t +1; */
		temp[i].ixv = 2*t + 1;
	   }
	if(ixmin < ixwmin) ixmin= ixwmin;
	if(ixmax > ixwmax) ixmax= ixwmax;
	if(ixmax < ixmin) return(0);
	for(ix= ixmin; ix <= ixmax; ix++)
	   {
		ns = intersect(2*ix,temp,nvert,s);
		if(nvert > 8 && ((ix%50) == 0))
			nvert =polyreduce(2*ix,temp,nvert);
		polysort(s,2*ns);
		for(i=0; i<ns; i++)
		   {
			y1= s[i].on;
			y2= s[i].off;
			if(y1 < iywmin) y1= iywmin;
			if(y2 > iywmax) y2= iywmax;
			if(y2 < y1) continue;
			linefill(ix,y1,y2);
		   }
	   }
	if(release) free(s);
	return(0);
   }




intersect(x,p,np,s)
int x, np;
register struct intpolygon *p;
int *s;
   {
	register int i;
	int x1, y1, x2, y2, num, den, ns;
	int dx1, dx2;
	x1= p[np-1].ixv;
	y1= p[np-1].iyv;
	ns=0;
	for(i=0; i<np; i++)
	   {
		x2= p[i].ixv;
		y2= p[i].iyv;
		dx1= x-x1;
		dx2= x-x2;
		if( dx1*dx2 <= 0 )
		   {
			num= y2 * dx1 - y1 * dx2;
			den= x2-x1;
			/* the initial stretch guarantees that den is never 0 */
			s[ns++]= (int)( (float)num / (float)den );
		   }
		x1= x2;
		y1= y2;
	   }
	return(ns/2);
   }

polysort(s,ns)
int *s;
int ns;
   {
	register int *limit, *s1, *s2, tmp;
	limit= s + (ns-1);
	for(s1=s; s1<limit; s1++)
	for(s2=s1+1; s2<=limit; s2++)
	   {
		if(*s1 > *s2) { tmp= *s1; *s1= *s2; *s2= tmp; }
	   }
   }


polyreduce(x,pbase,np)
int x, np;
struct intpolygon *pbase;
   {
	register struct intpolygon *p, *q;
	register int i;
	int npout;

	p= q= pbase;
	if(p[np-1].ixv > x || p[0].ixv > x || p[1].ixv > x)
	   {
		q->ixv= p->ixv;
		q->iyv= p->iyv;
		q++;
	   }
	p++;
	for(i=2; i<np; i++)
	   {
		if(p[-1].ixv > x || p[0].ixv > x || p[1].ixv > x)
		   {
			q->ixv= p->ixv;
			q->iyv= p->iyv;
			q++;
		   }
		p++;
	   }
	if(p[-1].ixv > x || p[0].ixv > x || pbase[0].ixv > x)
	   {
		q->ixv= p->ixv;
		q->iyv= p->iyv;
		q++;
	   }
	npout= q - pbase;
	return(npout);
   }
/*
boxfill(x1,y1,x2,y2)
int x1,y1,x2,y2;
   {
	register int ix, t;
	if(x2 < x1) { t= x1; x1= x2; x2=t; }
	if(y2 < y1) { t= y1; y1= y2; y2=t; }
	if(x1 < ixwmin) x1= ixwmin;
	if(x2 > ixwmax) x2= ixwmax;
	if(x2 < x1) return;
	for(ix=x1; ix<=x2; ix++)
		linefill(ix,y1,y2);
   }
   */
