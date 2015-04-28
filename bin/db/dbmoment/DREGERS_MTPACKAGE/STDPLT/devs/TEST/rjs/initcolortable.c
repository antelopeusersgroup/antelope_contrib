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
	unsigned char ur[8],ug[8],ub[8];
	extern Rect igl_canvas_rect;

	if (!colordev) return(0);
	for (icol=0; icol<8; icol++) {
		ur[icol] = colors[icol][1];
		ug[icol] = colors[icol][2];
		ub[icol] = colors[icol][3];
		pw_putcolormap(pw,0,8,ur,ug,ub);
	}
	pw_unlock(pw);
	setcolor(pencolor);
	pw_lock(pw,igl_canvas_rect);
	return(0);
}
