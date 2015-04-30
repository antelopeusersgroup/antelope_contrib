#include	<stdio.h>
#include	"igl.h"
#include	"stdplt.h"

extern struct frameinfo *frames, *cfr;
extern struct globalinfo glplot;
extern float PIXINCH, INCHPIX;

plot(xinch,yinch,ipen)
float xinch,yinch;
int ipen;
   {
	int ix, iy;

	ix= (int)(xinch*PIXINCH);
	iy= (int)(yinch*PIXINCH);
	if(ipen) output2(IGL_DRAW,ix,iy);
	 else	 output2(IGL_MOVE,ix,iy);
	XPOS= xinch;
	YPOS= yinch;
   }

uplot(xuser,yuser,ipen)
float xuser,yuser;
int ipen;
   {
	int ix, iy;
	float xinch, yinch;

	xinch= XSCL(xuser);
	yinch= YSCL(yuser);
	ix= (int)(xinch*PIXINCH);
	iy= (int)(yinch*PIXINCH);
	if(ipen) output2(IGL_DRAW,ix,iy);
	 else	 output2(IGL_MOVE,ix,iy);
	XPOS= xinch;
	YPOS= yinch;
   }

line(x1,y1,x2,y2)
float x1, y1, x2, y2;
   {
	int ix1, iy1, ix2, iy2;

	ix1= (int)(x1*PIXINCH);
	iy1= (int)(y1*PIXINCH);
	ix2= (int)(x2*PIXINCH);
	iy2= (int)(y2*PIXINCH);
	output10(IGL_LINE,ix1,iy1,ix2,iy2);
	XPOS= x2;
	YPOS= y2;
   }

uline(x1,y1,x2,y2)
float x1, y1, x2, y2;
   {
	int ix1, iy1, ix2, iy2;
	float xinch, yinch;

	xinch= XSCL(x1);
	yinch= YSCL(y1);
	ix1= (int)(xinch*PIXINCH);
	iy1= (int)(yinch*PIXINCH);
	xinch= XSCL(x2);
	yinch= YSCL(y2);
	ix2= (int)(xinch*PIXINCH);
	iy2= (int)(yinch*PIXINCH);
	output10(IGL_LINE,ix1,iy1,ix2,iy2);
	XPOS= xinch;
	YPOS= yinch;
   }

box(x1,y1,x2,y2)
float x1, y1, x2, y2;
   {
	int ix1, iy1, ix2, iy2, t;
	ix1= x1*PIXINCH;
	iy1= y1*PIXINCH;
	ix2= x2*PIXINCH;
	iy2= y2*PIXINCH;
	if(ix1>ix2) { t=ix1; ix1=ix2; ix2=t; }
	if(iy1>iy2) { t=iy1; iy1=iy2; iy2=t; }
	output10(IGL_BOX,ix1,iy1,ix2,iy2);
	

   }

ubox(x1,y1,x2,y2)
float x1, y1, x2, y2;
   {
	int ix1, iy1, ix2, iy2, t;
	ix1= XSCL(x1)*PIXINCH;
	iy1= YSCL(y1)*PIXINCH;
	ix2= XSCL(x2)*PIXINCH;
	iy2= YSCL(y2)*PIXINCH;
	if(ix1>ix2) { t=ix1; ix1=ix2; ix2=t; }
	if(iy1>iy2) { t=iy1; iy1=iy2; iy2=t; }
	output10(IGL_BOX,ix1,iy1,ix2,iy2);
   }

point(xinch,yinch)
float xinch, yinch;
   {
	int ix, iy;

	ix= (int)(xinch *PIXINCH);
	iy= (int)(yinch *PIXINCH);
	output2(IGL_POINT,ix,iy);
	XPOS= xinch;
	YPOS= yinch;
   }

upoint(xuser,yuser)
float xuser, yuser;
   {
	int ix, iy;
	float xinch, yinch;

	xinch= XSCL(xuser);
	yinch= YSCL(yuser);
	ix= (int)(xinch *PIXINCH);
	iy= (int)(yinch *PIXINCH);
	output2(IGL_POINT,ix,iy);
	XPOS= xinch;
	YPOS= yinch;
   }
