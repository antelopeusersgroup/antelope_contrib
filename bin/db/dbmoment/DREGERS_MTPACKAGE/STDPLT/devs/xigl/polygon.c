#include <xview/xview.h>
#include <xview/canvas.h>
#include "devpar.h"
#include	"../com/global.h"

/*
 * ../com/readcom.c should be modified so that the window values are
 * set by subroutine.  This way, a device, such as a sun display, that
 * knows about windowing can window automatically via a primitive call.
 */

extern int ixwmin, ixwmax, iywmin, iywmax;
extern int ypagelen;

struct Xpoint	*vtemp;

polyfilln(npoly,nverts,verts)
int npoly, *nverts;
struct intpolygon **verts;
   {
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

