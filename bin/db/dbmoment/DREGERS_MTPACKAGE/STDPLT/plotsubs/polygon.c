#include	<stdlib.h>
#include	<stdio.h>
#include	"../h/igl.h"
#include	"stdplt.h"

/*extern struct frameinfo *frames, *cfr; */
extern struct frameinfo frames[],*cfr;
extern struct globalinfo glplot;
extern float PIXINCH, INCHPIX;

struct fltpolygon { float  xv,  yv; };
struct intpolygon { int ixv, iyv; };

struct intpolygon polybuf[POLYBUFSIZE];

boxfill(x1,y1,x2,y2)
float x1,y1,x2,y2;
  {
	int ix1,iy1,ix2,iy2,t;
	ix1 = x1*PIXINCH;
	iy1 = y1*PIXINCH;
	ix2 = x2*PIXINCH;
	iy2 = y2*PIXINCH;
	if (ix1>ix2) {t=ix1; ix1=ix2; ix2 =t;}
	if (iy1>iy2) {t=iy1; iy1=iy2; iy2 =t;}
	output10(IGL_BOXFILL,ix1,iy1,ix2,iy2);
   }

uboxfill(x1,y1,x2,y2)
float x1,y1,x2,y2;
  {
	int ix1,iy1,ix2,iy2,t;
	ix1 = XSCL(x1)*PIXINCH;
	iy1 = YSCL(y1)*PIXINCH;
	ix2 = XSCL(x2)*PIXINCH;
	iy2 = YSCL(y2)*PIXINCH;
	if (ix1>ix2) {t=ix1; ix1=ix2; ix2 =t;}
	if (iy1>iy2) {t=iy1; iy1=iy2; iy2 =t;}
	output10(IGL_BOXFILL,ix1,iy1,ix2,iy2);
   }


polyfill(p,np)
struct fltpolygon *p;
int np;
   {
	struct intpolygon *q, *getpolybuf();
	register int i;
	register struct intpolygon *qptr;
	register struct fltpolygon *pptr;

	if(np < 2 || np > MAXVERT)
	   {
		err(WARN,"invalid poly size = %d",np);
		return(-1);
	   }
	q= getpolybuf(np);
	
	qptr= q;
	pptr= p;
	for(i=0; i<np; i++)
	   {
		qptr->ixv= (int)(pptr->xv *PIXINCH);
		qptr->iyv= (int)(pptr->yv *PIXINCH);
		qptr++;
		pptr++;
	   }
	output9(IGL_POLYFILL,q,np);
	freepolybuf(q);
	return(0);
   }

upolyfill(p,np)
struct fltpolygon *p;
int np;
   {
	struct intpolygon *q, *getpolybuf();
	register int i;
	register struct intpolygon *qptr;
	register struct fltpolygon *pptr;

	if(np < 2 || np > MAXVERT)
	   {
		err(WARN,"invalid poly size = %d",np);
		return(-1);
	   }
	q= getpolybuf(np);
	
	qptr= q;
	pptr= p;
	for(i=0; i<np; i++)
	   {
		qptr->ixv= (int)(XSCL(pptr->xv) *PIXINCH);
		qptr->iyv= (int)(YSCL(pptr->yv) *PIXINCH);
		qptr++;
		pptr++;
	   }
	output9(IGL_POLYFILL,q,np);
	freepolybuf(q);
	return(0);
   }



polyfilln(npoly,nverts,verts)
int npoly,  nverts[];
struct fltpolygon **verts;
   {
      struct intpolygon *q, *getpolybuf();
      register int i,j;
      register struct intpolygon *qptr;
      register struct fltpolygon *pptr;

      for (i=0; i<npoly; i++)        /* for each polygon  */
	{
	  if (nverts[i]<2  || nverts[i] > MAXVERT)
	     {    
	       err(WARN, "invalid poly size= %d\n",nverts[i]);
	       return(-1);
             }
          q= getpolybuf(nverts[i]);
	  qptr = q;
	  pptr = *verts;                 /* vertices of polygon */
	  for (j=0; j<nverts[i]; j++)
	     {
	       qptr->ixv = (int) (pptr->xv * PIXINCH);
	       qptr->iyv = (int) (pptr->yv * PIXINCH);
	       qptr++;
	       pptr++;
             }
         if (i==0) output8(IGL_POLYFILLN,npoly);  /* output command, npoly */
	  output13(q,nverts[i]);   /* output poly, no command  */
	  verts++;     
	  freepolybuf(q);
        
	} /*for*/
	return(0);
   } /*polyfilln*/


upolyfilln(npoly,nverts,verts)
int npoly,  nverts[];
struct fltpolygon **verts;
   {
      struct intpolygon *q, *getpolybuf();
      register int i,j;
      register struct intpolygon *qptr;
      register struct fltpolygon *pptr;


      for (i=0; i<npoly; i++)        /* for each polygon  */
	{
	  if (nverts[i]<2  || nverts[i] > MAXVERT)
	     {    
	       err(WARN, "invalid poly size= %d\n",nverts[i]);
	       return(-1);
             }
          q= getpolybuf(nverts[i]);
	  qptr = q;
	  pptr = *verts;                 /* vertices of polygon */

	  for (j=0; j<nverts[i]; j++)
	     {
	       qptr->ixv = (int) (XSCL(pptr->xv) * PIXINCH);
	       qptr->iyv = (int) (YSCL(pptr->yv) * PIXINCH);   
	       qptr++;
	       pptr++;
             }
         if (i==0) output8(IGL_POLYFILLN,npoly);  /* output command, npoly */
	  output13(q,nverts[i]);   /* output poly, no command  */
	  verts++;     
	  freepolybuf(q);
        
	} /*for*/
	return(0);
   } /*upolyfilln*/

  

	      

	       
struct intpolygon *getpolybuf(n)
int n;
   {
	struct intpolygon *q;

	if(n < POLYBUFSIZE) return(polybuf);
	q= (struct intpolygon *)malloc(n*sizeof(struct intpolygon));
	if(q == NULL)
		err(FATAL,"connot allocate memory in poly size = %d",n);
	return(q);
   }

freepolybuf(q)
struct intpolygon *q;
   {
	if(q != polybuf) free(q);
   }
