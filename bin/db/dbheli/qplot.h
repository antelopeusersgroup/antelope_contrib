
#include <stdio.h>
#include <X11/Xlib.h>

#define	MAX_PIXELS	10000
#define	MAX_POINTS	20000

#define	QPL_SCALE_FIXED	0
#define	QPL_SCALE_AUTO	1
#define	QPL_SCALE_AUTO0	2

#define	REFRESH_TIME	5

typedef struct qpl_ {
	int npixels;
	int np;
	int istart;
	float *min;
	float *max;
	float *in;
	float *out;
	int *numb;
	int overlap;
	int noverlaps;
	struct qpl_ *overlaps;
} QPlot;

extern QPlot *qpbin (double ts, double dt, int nsamp, float *data, double tmin, double twin, int npixels, QPlot *qpl);
extern int qpfree (QPlot *qpl);
extern int qpfreeoverlaps (QPlot *qpl);
extern int qplot (double ts, double dt, int nsamp, float *data, double tmin, double twin, int npixels, int x, int y, int h, float *ybot, float *ytop, 
       int scale_type, float ygain, XPoint **xp);
extern int qplotsegs (QPlot *qpl, int x, int y, int h, float *ybot, float *ytop, int scale_type, float ygain, Display *display, Drawable drawable, GC gc, GC gcov);
extern int nqplotsegs (QPlot *qpl, float xdim, float ydim, float xlow, float ylow, float *ybot, float *ytop, int scale_type, int iclip, float ygain);
