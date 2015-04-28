#include <stdio.h>
#include "../com/global.h"
#include "../../h/igl.h" 

#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include "devpar.h"

extern Pixwin	*pw ;

extern char colors[][4];
extern int pencolor;
extern int colordev;

/* inits the first 8 colors to be those in globalvar.c */
initcolortable()
{
	int icol;
	unsigned char ur[256],ug[256],ub[256];
	extern Rect igl_canvas_rect;

	if (!colordev) return(0);
	for (icol=0; icol<8; icol++) {
		ur[icol] = colors[icol][1];
		ug[icol] = colors[icol][2];
		ub[icol] = colors[icol][3];
	}
	for (; icol<256; icol++) ur[icol] = ug[icol] = ub[icol] = 0;
	/* foreground color (last entry in colormap) */
	ur[255] = ug[255] = ub[255] = 255;
	pw_unlock(pw);
	pw_putcolormap(pw,0,256,ur,ug,ub);
	pw_lock(pw,igl_canvas_rect);
	setcolor(pencolor);
	return(0);
}
