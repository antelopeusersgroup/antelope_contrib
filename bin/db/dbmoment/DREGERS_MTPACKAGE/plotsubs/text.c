#include	<stdio.h>
#include	"igl.h"
#include	"stdplt.h"

extern struct frameinfo *frames, *cfr;
extern struct globalinfo glplot;
extern float PIXINCH, INCHPIX;

char xfr_line[1024];

#define ARGS	a1,a2,a3,a4,a5,a6
text(xinch,yinch,fmt,ARGS)
float xinch,yinch;
char *fmt;
int ARGS;
   {
	int ix,iy;
	ix = (int) (xinch*PIXINCH);
	iy = (int) (yinch*PIXINCH);
	sprintf(xfr_line,fmt,ARGS);
	output14(IGL_TEXT,ix,iy,xfr_line);
   }

utext(xuser,yuser,fmt,ARGS)
float xuser,yuser;
char *fmt;
int ARGS;
   {
	int ix,iy;
	float xinch,yinch;
	xinch = XSCL(xuser);
	yinch = YSCL(yuser);
	ix = (int) (xinch*PIXINCH);
	iy = (int) (yinch*PIXINCH);
	sprintf(xfr_line,fmt,ARGS);
	output14(IGL_TEXT,ix,iy,xfr_line);
   }


plotlabel(fmt,ARGS)
char *fmt;
int ARGS;
   {
	sprintf(xfr_line,fmt,ARGS);
	output3(IGL_PLOTLABEL,xfr_line);
   }

settextangle(angle)
float angle;
   {
	int iang;
	while(angle >  180.0) angle -= 360.0;
	while(angle < -180.0) angle += 360.0;
	iang= (int)(ANGLENORM * angle/360.0);
	output8(IGL_SETTEXTANGLE,iang);
   }

settextsize(size)
float size;
   {
	int isize;
	isize= (int)(FLOATNORM * size);
	output8(IGL_SETTEXTSIZE,isize);
   }

settextfont(ifont)
int ifont;
   {
	ifont &= 0xff;
	output4(IGL_SETTEXTFONT,ifont);
   }

settextcenter(hpos,vpos)
int hpos, vpos;
   {
	int pos;

	pos= ((vpos & 0xf)<<4) | (hpos & 0xf);
	output4(IGL_TEXTCENTER,pos);
   }
