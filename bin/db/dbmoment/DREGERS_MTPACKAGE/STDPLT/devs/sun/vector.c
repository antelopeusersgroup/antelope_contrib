#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include "devpar.h"

extern Pixwin	*pw ;

extern int ixwmin, ixwmax, iywmin, iywmax;
extern int pencolor;
extern int ypagelen;

move(x,y)
register int x, y;
   {
	extern int xlast, ylast;

	xlast= x;
	ylast= y;
   }

draw(x,y)
register int x, y;
   {
	extern int xlast, ylast;

	pw_vector (pw, xlast, ypagelen - ylast, x, ypagelen - y, PIX_SRC | PIX_COLOR(pencolor), 1) ;
	xlast= x;
	ylast= y;
   }

vector(x1,y1,x2,y2)
register int x1,y1,x2,y2;
   {
	extern int xlast, ylast;

	pw_vector (pw, x1, ypagelen - y1, x2, ypagelen - y2, PIX_SRC | PIX_COLOR(pencolor), 1) ;
	xlast= x2;
	ylast= y2;
   }

linefill(x,y1,y2)
register int x,y1,y2;
   {
	extern int xlast, ylast;
	int t;

	if (y2<y1) {t=y1; y1=y2; y2=t;}

	if (y1<iywmin) y1=iywmin;
	if (y2>iywmax) y2=iywmax;

	pw_vector (pw, x, ypagelen - y1, x, ypagelen - y2, PIX_SRC | PIX_COLOR(pencolor), 1) ;
	xlast= x;
	ylast= y2;
   }

xlinefill(y,x1,x2)
register int y,x1,x2;
   {
	extern int xlast, ylast;
	int t;

	if (x2<x1) {t=x1; x1=x2; x2=t;}

	if (x1<ixwmin) x1=ixwmin;
	if (x2>ixwmax) x2=ixwmax;

	pw_vector (pw, x1, ypagelen - y, x2, ypagelen - y, PIX_SRC | PIX_COLOR(pencolor), 1) ;
	xlast= x2;
	ylast= y;
   }
