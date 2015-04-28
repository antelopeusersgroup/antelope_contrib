#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include "devpar.h"

extern Pixwin	*pw ;

extern int pencolor;
extern int colordev;

setcolor(icol)
int icol;
   {
	if (!colordev) return(0);
	pencolor = icol;
	return(0);
   }

defcolor(icol,ir,ig,ib)
int icol,ir,ig,ib;
   {
	unsigned char ur, ug, ub;
	extern Rect igl_canvas_rect;

	if (!colordev) return(0);
	ur= ir;
	ug= ig;
	ub= ib;
	pw_unlock(pw);
	pw_putcolormap(pw,icol,1,&ur,&ug,&ub);
	pw_lock(pw,igl_canvas_rect);
	return(0);
   }
