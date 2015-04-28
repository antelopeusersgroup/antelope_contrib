#include	<stdio.h>
#include	"../h/igl.h"
#include	"stdplt.h"

extern struct frameinfo *frames, *cfr;
extern struct globalinfo glplot;
extern float PIXINCH, INCHPIX;

plotdot(xinch,yinch,size,imode)
float xinch,yinch, size;
int imode;
   {
	int ix, iy, isize;

	ix= (int)(xinch*PIXINCH);
	iy= (int)(yinch*PIXINCH);
	isize= (int)(size*FLOATNORM);
	if(imode) output16(IGL_BDOT,ix,iy,isize);
	 else	  output16(IGL_WDOT,ix,iy,isize);
	XPOS= xinch;
	YPOS= yinch;
   }

uplotdot(xuser,yuser,size,imode)
float xuser,yuser, size;
int imode;
   {
	int ix, iy, isize;
	float xinch, yinch;

	xinch= XSCL(xuser);
	yinch= YSCL(yuser);
	ix= (int)(xinch*PIXINCH);
	iy= (int)(yinch*PIXINCH);
	isize= (int)(size*FLOATNORM);
	if(imode) output16(IGL_BDOT,ix,iy,isize);
	 else	  output16(IGL_WDOT,ix,iy,isize);
	XPOS= xinch;
	YPOS= yinch;
   }
