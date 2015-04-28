#include	"../../h/igl.h"
#include <suntool/sunview.h>
#include <suntool/canvas.h>

extern Pixwin	*pw ;
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
	Pixrect *igl_pr_pat;

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
	igl_pr_pat = mem_point(32,32,1,pat);

	switch(brushmode) {

	case FILL_OR:
		pw_replrop(pw,x1,ypagelen-y2,dx,dy,PIX_SRC | PIX_DST | PIX_COLOR(pencolor),igl_pr_pat,0,0);
		break;
	case FILL_XOR:
		pw_replrop(pw,x1,ypagelen-y2,dx,dy,PIX_SRC ^ PIX_DST | PIX_COLOR(pencolor),igl_pr_pat,0,0);
		break;
	case FILL_AND:
		pw_replrop(pw,x1,ypagelen-y2,dx,dy,PIX_SRC & PIX_DST | PIX_COLOR(pencolor),igl_pr_pat,0,0);
		break;
	case FILL_EQU:
		pw_replrop(pw,x1,ypagelen-y2,dx,dy,PIX_SRC | PIX_COLOR(pencolor),igl_pr_pat,0,0);
		if (fillwhite) {
			brushmode = FILL_BRWHITE;
			fillwhite = 0;				
		}
		break;
	}
}
