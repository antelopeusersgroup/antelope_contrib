#include "../../h/igl.h"
#include <xview/xview.h>
#include <xview/canvas.h>

#include "xigl.h"

extern int pencolor;

boxfill(x1,y1,x2,y2)
int x1, y1, x2, y2;
   {
	register int dx, dy, t;
	int x, y;
	int fillwhite = 0;
	extern int xlast, ylast;
        extern int patterns[][32];
	extern int ixwmin, ixwmax, iywmin, iywmax;
	extern int brushmode, brushpat;
	extern int ypagelen;
	short *pat;
	unsigned long saved_color;
	Pixmap pm;

	debug ("entering boxfill...\n");
	if(x1 > x2) { t=x1; x1=x2; x2=t; }
	if(y1 > y2) { t=y1; y1=y2; y2=t; }

	/* box totally out of bounds */
	if ((x1<ixwmin && x2<ixwmin) || (x1>ixwmax && x2>ixwmax)) return;
	if ((y1<iywmin && y2<iywmin) || (y1>iywmax && y2>iywmax)) return;

	/* clip box if necessary */
	if (x1<ixwmin) x1=ixwmin;
	if (x2>ixwmax) x2=ixwmax;
	if (y1<iywmin) y1=iywmin;
	if (y2>iywmax) y2=iywmax;

	dx= x2-x1;
	dy= y2-y1;
	if(dx == 0) dx=1;
	if(dy == 0) dy=1;
	xlast= x1;
	ylast= y1;

 	if (brushmode==FILL_BRWHITE){
		pat = 0;  		/* white fill */
		fillwhite = 1;
		brushmode = FILL_EQU;
	}

	pat = (short *) patterns[brushpat];  
	setpattern(pat);
/*::	gc_val.stipple = pattern_info[pat].pixmap; ::*/
	XChangeGC (display, gc, GCStipple, &gc_val);

	switch(brushmode) {

	case FILL_OR:		/* FillStippled				*/
		gc_val.fill_style = FillStippled;
		XChangeGC (display, gc, GCStipple, &gc_val);
		XFillRectangle (display, xid, gc, x1, ypagelen-y2, dx, dy);
		break;
	case FILL_XOR:		/* Not supported			*/
		gc_val.fill_style = FillStippled;
		XChangeGC (display, gc, GCStipple, &gc_val);
		XFillRectangle (display, xid, gc, x1, ypagelen-y2, dx, dy);
		break;
	case FILL_AND:		/* FillOpaqueStippled			*/
		gc_val.fill_style = FillOpaqueStippled;
		XChangeGC (display, gc, GCStipple, &gc_val);
		XFillRectangle (display, xid, gc, x1, ypagelen-y2, dx, dy);
		break;
	case FILL_EQU:		/* FillSolid				*/
		gc_val.fill_style = FillSolid;
		XChangeGC (display, gc, GCFillStyle, &gc_val);
		if (fillwhite) {
			saved_color = gc_val.foreground;
			gc_val.foreground = gc_val.background;
			XChangeGC (display, gc, GCForeground, &gc_val);
		}
		XFillRectangle (display, xid, gc, x1, ypagelen-y2, dx, dy);
		if (fillwhite) {
			gc_val.foreground = saved_color;
			XChangeGC (display, gc, GCForeground, &gc_val);
		}
		break;
	}
	debug ("leaving boxfill...\n");
}
