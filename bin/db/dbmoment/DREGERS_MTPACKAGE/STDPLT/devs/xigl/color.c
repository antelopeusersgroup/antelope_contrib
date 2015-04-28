#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/cms.h>
#include "devpar.h"

#include "xigl.h"

extern int pencolor;
extern int colordev;

setcolor(icol)
int icol;
   {
	debug ("entering setcolor...\n");
	if (!colordev) return(0);
	pencolor = icol;
	gc_val.foreground = icol;
	XChangeGC (display, gc, GCForeground, &gc_val);
	debug ("leaving setcolor...\n");
	return(0);
   }

defcolor(icol,ir,ig,ib)
int icol,ir,ig,ib;
   {
	unsigned char ur, ug, ub;

	if (!colordev) return(0);
	fprintf (stderr, "entering defcolor, color %i, r=%d g=%d, b=%d\n",
			 icol, ir, ig, ib);

	xcolor[icol].pixel	= icol;
	xcolor[icol].red	= ir << 8;
	xcolor[icol].green	= ig << 8;
	xcolor[icol].blue	= ib << 8;

/*::
	if (XV_OK != xv_set (cms, CMS_X_COLORS, xcolor) ) {
		fprintf (stderr, "error settting color %i, r=%d g=%d, b=%d\n",
			 icol, ir, ig, ib);
	}
::*/
	debug ("leaving defcolor...\n");
	return(0);
   }
