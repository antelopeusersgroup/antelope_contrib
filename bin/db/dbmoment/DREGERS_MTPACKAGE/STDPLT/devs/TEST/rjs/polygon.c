#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include "devpar.h"
#include	"../com/global.h"
#include	"../../h/igl.h"

extern int ixwmin, ixwmax, iywmin, iywmax;
extern int pencolor;

extern Pixwin	*pw ;

polyfilln(npoly,nverts,verts)
int npoly, *nverts;
struct intpolygon **verts;
   {
	int release, i, n;
	int np;
	struct intpolygon *p;
	struct pr_pos *VecList, *pVecList;
	int fillwhite = 0;
        extern int patterns[][32];
	extern int brushmode, brushpat;
	extern int ymax;
	short *pat;
	Pixrect *igl_pr_pat;

	np= 0;
	for(n=0; n<npoly; n++) np += nverts[n];
	if ((VecList = (struct pr_pos *)malloc(np*sizeof(struct pr_pos))) == NULL) {
		err(WARN,"cannot allocate memory in poly size = %d",np);
		return(-1);
	}
	pVecList = VecList;
	for (n=0; n<npoly; n++) {
		p = verts[n];
		for(i=0; i<nverts[n]; i++) {
			pVecList->x = p->ixv;
			pVecList->y = ymax - p->iyv;
			pVecList++;      p++;
		}
	}

 	if (brushmode==FILL_BRWHITE){
		pat = 0;  		/* white fill */
		fillwhite = 1;
		brushmode = FILL_EQU;
	}

	pat = (short *) patterns[brushpat];  
	igl_pr_pat = mem_point(32,32,1,pat);

	switch(brushmode) {

	case FILL_OR:
		pw_polygon_2(pw,0,0,npoly,nverts,VecList,PIX_SRC | PIX_DST | PIX_COLOR(pencolor),igl_pr_pat,0,0);
		break;
	case FILL_XOR:
		pw_polygon_2(pw,0,0,npoly,nverts,VecList,PIX_SRC ^ PIX_DST | PIX_COLOR(pencolor),igl_pr_pat,0,0);
		break;
	case FILL_AND:
		pw_polygon_2(pw,0,0,npoly,nverts,VecList,PIX_SRC & PIX_DST | PIX_COLOR(pencolor),igl_pr_pat,0,0);
		break;
	case FILL_EQU:
		pw_polygon_2(pw,0,0,npoly,nverts,VecList,PIX_SRC | PIX_COLOR(pencolor),igl_pr_pat,0,0);
		if (fillwhite) {
			brushmode = FILL_BRWHITE;
			fillwhite = 0;				
		}
		break;
	}

	free(VecList);
	return(0);
}

polyfill(nvert,verts)
int nvert;
struct intpolygon *verts;
{
	return(polyfilln(1,&nvert,&verts));
}

sympolyfill(nvert,verts)
int nvert;
struct intpolygon *verts;
{
	return(polyfilln(1,&nvert,&verts));
}
