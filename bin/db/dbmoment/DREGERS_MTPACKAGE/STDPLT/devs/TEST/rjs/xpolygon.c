#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include "devpar.h"
#include	"../com/global.h"
#include	"../../h/igl.h"

/*
 * ../com/readcom.c should be modified so that the window values are
 * set by subroutine.  This way, a device, such as a sun display, that
 * knows about windowing can window automatically via a primitive call.
 */

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
	int fix_n, nvo[1000], *pnv, new_npoly;

	np= 0;
	for(n=0; n<npoly; n++) np += nverts[n];
	if ((VecList = (struct pr_pos *)malloc(np*sizeof(struct pr_pos))) == NULL) {
		err(WARN,"cannot allocate memory in poly size = %d",np);
		return(-1);
	}
	pVecList = VecList;
	pnv = nvo;
	new_npoly = 0;
	for (n=0; n<npoly; n++) {
		p = verts[n];
		fix_n = fix_verts(p,pVecList,nverts[n]);
		if (fix_n > 2) {
			new_npoly++;
			*pnv++ = fix_n;
			pVecList += fix_n;
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
		pw_polygon_2(pw,0,0,new_npoly,nvo,VecList,PIX_SRC | PIX_DST | PIX_COLOR(pencolor),igl_pr_pat,0,0);
		break;
	case FILL_XOR:
		pw_polygon_2(pw,0,0,new_npoly,nvo,VecList,PIX_SRC ^ PIX_DST | PIX_COLOR(pencolor),igl_pr_pat,0,0);
		break;
	case FILL_AND:
		pw_polygon_2(pw,0,0,new_npoly,nvo,VecList,PIX_SRC & PIX_DST | PIX_COLOR(pencolor),igl_pr_pat,0,0);
		break;
	case FILL_EQU:
		pw_polygon_2(pw,0,0,new_npoly,nvo,VecList,PIX_SRC | PIX_COLOR(pencolor),igl_pr_pat,0,0);
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

int
fix_verts(vin,vout,nv)
int nv;
struct pr_pos *vout;
struct intpolygon *vin;
{
	int i, at_x_bound, at_y_bound, startx, starty, savex, savey;
	int vx1_in, vy1_in, vx2_in, vy2_in, vx1, vy1, vx2, vy2;
	int vxold, vyold, nvout;

	if (nv < 3) return(nv);

	/* find first point */
	startx = vx1_in = vx1 = vx2_in = vx2 = vin->ixv;
	starty = vy1_in = vy1 = vy2_in = vy2 = vin->iyv;
	vin++;
	i = 1;
	if (vx1 < ixwmin || vx1 > ixwmax || vy1 < iywmin || vy1 > iywmax) {
		vx2_in = vx2 = vin->ixv;
		vy2_in = vy2 = vin->iyv;
		vin++;
		i++;
		while (clip(&vx1,&vy1,&vx2,&vy2)) {
			if ( i++ >= nv ) return(0);
			vx1 = vx2_in;
			vy1 = vy2_in;
			vx2_in = vx2 = vin->ixv;
			vy2_in = vy2 = vin->iyv;
			vin++;
		}
	}
	if ((nv - i) < 2) return(nv - i);
	if (vx2 == ixwmax || vx2 == ixwmin) at_x_bound = 1;
	if (vy2 == iywmax || vy2 == iywmin) at_y_bound = 1;
	savex = vout->x = vxold = vx2;
	savey = vyold = vy2;
	vout->y = ymax - vy2;
	vout++;
	nvout = 1;

	for (; i < nv; i++) {
		vx1_in = vx1 = vx2_in;
		vy1_in = vy1 = vy2_in;
		vx2_in = vx2 = vin->ixv;
		vy2_in = vy2 = vin->iyv;
		vin++;
		if (clip(&vx1,&vy1,&vx2,&vy2)) break;
		if (vx1 == vx2 && vy1 == vy2) break;
		if (vx2 == vxold && vy2 == vyold) break;
		if (at_y_bound && vx1 != vxold) {
			vout->x = vx1;
			vout->y = ymax - vy1_in;
			vout++;
			nvout++;
			vxold = vx1;
		}
		if (at_x_bound && vy1 != vyold) {
			vout->x = vx1_in;
			vout->y = ymax - vy1;
			vout++;
			nvout++;
			vyold = vy1;
		}
		if (vx2 == ixwmax || vx2 == ixwmin) at_x_bound = 1;
		if (vy2 == iywmax || vy2 == iywmin) at_y_bound = 1;
		vout->x = vxold = vx2;
		vyold = vy2;
		vout->y = ymax - vy2;
		vout++;
		nvout++;
	}

	/* do last point */
	vx1_in = vx1 = vx2_in;
	vy1_in = vy1 = vy2_in;
	vx2_in = vx2 = startx;
	vy2_in = vy2 = starty;
	if (clip(&vx1,&vy1,&vx2,&vy2)) goto verylast;
	if (vx1 == vx2 && vy1 == vy2) goto verylast;
	if (vx2 == vxold && vy2 == vyold) goto verylast;
	if (at_y_bound && vx1 != vxold) {
		vout->x = vx1;
		vout->y = ymax - vy1_in;
		vout++;
		nvout++;
		vxold = vx1;
	}
	if (at_x_bound && vy1 != vyold) {
		vout->x = vx1_in;
		vout->y = ymax - vy1;
		vout++;
		nvout++;
		vyold = vy1;
	}
	if (vx2 == ixwmax || vx2 == ixwmin) at_x_bound = 1;
	if (vy2 == iywmax || vy2 == iywmin) at_y_bound = 1;
	if (savex != vx2 || savey != vy2) {
		vout->x = vxold = vx2;
		vyold = vy2;
		vout->y = ymax - vy2;
		vout++;
		nvout++;
	}

verylast:
	/* may still need to do a window vertex */
	vx1_in = vx1 = vx2_in;
	vy1_in = vy1 = vy2_in;
	vx2_in = vx2 = savex;
	vy2_in = vy2 = savey;
	if (clip(&vx1,&vy1,&vx2,&vy2)) return(nvout);
	if (vx1 == vx2 && vy1 == vy2) return(nvout);
	if (vx2 == vxold && vy2 == vyold) return(nvout);
	if (at_y_bound && vx1 != vxold) {
		vout->x = vx1;
		vout->y = ymax - vy1_in;
		vout++;
		nvout++;
		vxold = vx1;
	}
	if (at_x_bound && vy1 != vyold) {
		vout->x = vx1_in;
		vout->y = ymax - vy1;
		vout++;
		nvout++;
		vyold = vy1;
	}
	return(nvout);
}
