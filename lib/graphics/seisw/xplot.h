/* Copyright (c) Colorado School of Mines, 2005.*/
/* All rights reserved.                       */

/* include file for X graphics */

#ifndef XPLOT_H
#define XPLOT_H


/* INCLUDES */
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

/* DEFINES */

/* axes drawing */
#define NONE 0
#define DOT 1
#define DASH 2
#define SOLID 3
#define NORMAL 0
#define SEISMIC 1
/* These used to be in cwp.h */
#ifndef ABS
#define ABS(x) ((x) < 0 ? -(x) : (x))
#endif
#ifndef MAX
#define MAX(x,y) ((x) > (y) ? (x) : (y))
#endif
#ifndef MIN
#define MIN(x,y) ((x) < (y) ? (x) : (y))
#endif
#define NINT(x) ((int)((x)>0.0?(x)+0.5:(x)-0.5))



/* 256 pixel values for truecolor model*/
extern unsigned long truecolor_pixel[256];


/* FUNCTION PROTOTYPES */

/* windows */
Window xNewWindow (Display *dpy, int x, int y, int width, int height,
	int border, int background, char *name);

/* images */
XImage *xNewImage (Display *dpy, unsigned long pmin, unsigned long pmax,
	int width, int height, float blank, unsigned char *bytes);


/* colormaps */
Status xCreateRGBDefaultMap (Display *dpy, XStandardColormap *scmap);
unsigned long xGetFirstPixel (Display *dpy);
unsigned long xGetLastPixel (Display *dpy);
Colormap xCreateRGBColormap (Display *dpy, Window win,
	 char *str_cmap, int verbose);
Colormap xCreateHSVColormap (Display *dpy, Window win,
	 char *str_cmap, int verbose);
Colormap xCreateGrayColormap (Display *dpy, Window win);
Colormap xCreateHueColormap (Display *dpy, Window win);
void xDrawLegendBox(Display *dpy, Window win,
        int x, int y, int width, int height,
        float bclip, float wclip, char *units, char *legendfont,
        char *labelfont, char *title, char *titlefont,
        char *axescolor, char *titlecolor, char *gridcolor,
        int style);

/* for xcontour */
void xContour(Display *dpy, Window win,GC gcc, GC gcl, 
	       float *cp,int nx, float x[], int ny, float y[], float z[], 
	       char lcflag,char *lcf,char *lcc, float *w, int nplaces);

/* curve drawing */
void xDrawCurve(Display *dpy, Window win,
		int x, int y, int width, int height,
		float x1beg, float x1end, float p1beg, float p1end,
		float x2beg, float x2end, float p2beg, float p2end,
		float *x1curve, float *x2curve, int ncurve,
		char *curvecolor, int style);
void xDrawCurve_double(Display *dpy, Window win,
		int x, int y, int width, int height,
		float x1beg, float x1end, float p1beg, float p1end,
		float x2beg, float x2end, float p2beg, float p2end,
		double *x1curve, double *x2curve, int ncurve,
		char *curvecolor, int style);
/* added in conversion.  axis routine */
void scaxis (float x1, float x2, int *nxnum, float *dxnum, float *fxnum);


#endif /* XPLOT_H */
