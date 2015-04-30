#include	<stdio.h>
#include	"igl.h"
#include	"stdplt.h"

extern struct frameinfo *frames, *cfr;
extern struct globalinfo *glcur;
extern float PIXINCH, INCHPIX;
extern int XMAX, YMAX;

setorig(xorig,yorig)
float xorig, yorig;
   {
	cfr->fr_xorig= xorig;
	cfr->fr_yorig= yorig;
   }

setmin(xmin,ymin)
float xmin, ymin;
   {
	cfr->fr_xmin= xmin;
	cfr->fr_ymin= ymin;
   }

setscl(xscl,yscl)
float xscl, yscl;
   {
	if(xscl == 0.0 || yscl == 0.0)
		err(FATAL,"scale value of zero (0.0) specified");
	cfr->fr_xscl= xscl;
	cfr->fr_yscl= yscl;
   }

getorig(xorig,yorig)
float *xorig, *yorig;
   {
	*xorig= cfr->fr_xorig;
	*yorig= cfr->fr_yorig;
   }

getmin(xmin,ymin)
float *xmin, *ymin;
   {
	*xmin= cfr->fr_xmin;
	*ymin= cfr->fr_ymin;
   }

getscl(xscl,yscl)
float *xscl, *yscl;
   {
	*xscl= cfr->fr_xscl;
	*yscl= cfr->fr_yscl;
   }
window(xmin, ymin, xmax, ymax)
float xmin, ymin, xmax, ymax;
   {
	int ixmin, iymin, ixmax, iymax;

	ixmin= (int)(xmin*PIXINCH);
	iymin= (int)(ymin*PIXINCH);
	ixmax= (int)(xmax*PIXINCH);
	iymax= (int)(ymax*PIXINCH);
	iwindow(ixmin, iymin, ixmax, iymax);
   }

uwindow(xmin, ymin, xmax, ymax)
float xmin, ymin, xmax, ymax;
   {
	int ixmin, iymin, ixmax, iymax;

	ixmin= (int)(XSCL(xmin)*PIXINCH);
	iymin= (int)(YSCL(ymin)*PIXINCH);
	ixmax= (int)(XSCL(xmax)*PIXINCH);
	iymax= (int)(YSCL(ymax)*PIXINCH);
	iwindow(ixmin, iymin, ixmax, iymax);
   }

iwindow(xmin, ymin, xmax, ymax)
int xmin, ymin, xmax, ymax;
   {
	int t;
	if(xmin > xmax) { t=xmin; xmin= xmax; xmax=t; }
	if(ymin > ymax) { t=ymin; ymin= ymax; ymax=t; }
	if(xmin<0)    xmin=0;
	if(ymin<0)    ymin=0;
	if(xmax>XMAX) xmax=XMAX;
	if(ymax>YMAX) ymax=YMAX;
	cfr->fr_ixwmin= xmin;
	cfr->fr_iywmin= ymin;
	cfr->fr_ixwmax= xmax;
	cfr->fr_iywmax= ymax;
	output10(IGL_WINDOW,xmin,ymin,xmax,ymax);
   }

unwindow()
   {
	cfr->fr_ixwmin= 0;
	cfr->fr_iywmin= 0;
	cfr->fr_ixwmax= XMAX;
	cfr->fr_iywmax= YMAX;
	output1(IGL_UNWINDOW);
   }

where(xlab,ylab)
float *xlab, *ylab;
   {
	*xlab= XPOS;
	*ylab= YPOS;
   }

uwhere(xlab,ylab)
float *xlab, *ylab;
   {
	*xlab= (XPOS - cfr->fr_xorig)/cfr->fr_xscl + cfr->fr_xmin;
	*ylab= (YPOS - cfr->fr_yorig)/cfr->fr_yscl + cfr->fr_ymin;
   }
