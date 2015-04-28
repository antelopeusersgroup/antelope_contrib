#include <stdio.h>
#include "../com/global.h"

#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/cms.h>
#include "devpar.h"

#include "xigl.h"

typedef	unsigned short	xcolor_type;

extern char colors[][4];
extern int pencolor;
extern int colordev;

/* inits the first 8 colors to be those in globalvar.c */
initcolortable()
{
	int icol;
	int last, predefined;

	debug ("entering initcolortable...\n");
	if (!colordev) {
		debug ("leaving initcolortable...\n");
		return(0);
	}

	predefined = (ncolors > 8) ? 8 : ncolors;
	last = ncolors-1;

	for (icol=0; icol<predefined; icol++) {
		xcolor[icol].pixel	= icol;
		xcolor[icol].red	= (xcolor_type) colors[icol][1] << 8;
		xcolor[icol].green	= (xcolor_type) colors[icol][2] << 8;
		xcolor[icol].blue	= (xcolor_type) colors[icol][3] << 8;
	}
	for (icol=predefined; icol<ncolors; icol++) {
		xcolor[icol].pixel = icol;
		xcolor[icol].red = (xcolor_type) 0;
		xcolor[icol].green = (xcolor_type) 0;
		xcolor[icol].blue = (xcolor_type) 0;
	}

	/* Foreground color (last entry in colormap) */
		xcolor[icol].pixel = last;
		xcolor[icol].red = (xcolor_type)255 << 8;
		xcolor[icol].green = (xcolor_type)255 << 8;
		xcolor[icol].blue = (xcolor_type)255 << 8;

	xv_set (cms, CMS_X_COLORS, xcolor);
	
	setcolor(pencolor);
	debug ("leaving initcolortable...\n");
	return(0);
}
