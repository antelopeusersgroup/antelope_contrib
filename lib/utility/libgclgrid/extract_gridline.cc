#include "gclgrid.h"
#include "dmatrix.h"
/* Extracts the Cartesian components of a GCLgrid3d object along
a desired gridline from a specified offset.  The result is returned
as a 3xn matrix (dmatrix object) with n computed from the offsets
(see below)

Arguments:
	grid - GCLgrid3d object to extract from.
	ix1,ix2, and ix3  define the offset into the grid that 
		extraction is to proceed from.  
	comp - component to extract (1, 2, or 3)  1 extracts line along
		x1 direction, 2 for x2 direction, and 3 for x3 direction.
	reverse - if true, matrix will in reverse order from the original
		path.  (i.e. first will become the last on output)

The offset variables (ix1,ix2,ix3) determine which part of the 
parent grid is extracted.  When reverse is false the grid line will
be extracted from ixi with the other offset variables constant where
i is 1,2, or 3.  For example, to extract the complete path defined by the
3 axis for ix1=3 and ix2=4 this function would be called as:
	path = extract_gridline(grid,3,4,0,3,1);
to extract from the x3 offset of 8 use
	path = extract_gridline(grid,3,4,8,3,1);

Some testing is performed on the offset values. If the request is 
inconsistent with a dimensions of the grid the routine will throw a
GCLgrid_error object with a diagnostic message.

Author:  Gary Pavlis
Written:  June 2003
*/

dmatrix *extract_gridline(GCLgrid3d& grid, int ix1, int ix2, int ix3,
        int comp, bool reverse) throw (GCLgrid_error)
{
	dmatrix coords;
	int npts, i0, di;
	int i,ii;
	if(reverse)
		di = -1;
	else
		di = 1;
	switch (comp)
	{
	case 1:
		if(reverse)
			npts = ix1 + 1;
		else
			npts = grid.n1 - ix1;
		if(npts<0)throw(GCLgrid_error("extract_gridline:  requested offset inconsistent with grid dimensions\n"));
		coords=dmatrix(3,npts);
		for(i=ix1,ii=0;ii<npts;i+=di,++ii)
		{
			coords(0,ii)=grid.x1[i][ix2][ix3];
			coords(1,ii)=grid.x2[i][ix2][ix3];
			coords(2,ii)=grid.x3[i][ix2][ix3];
		}
		break;
	case 2:
		if(reverse)
			npts = ix2 + 1;
		else
			npts = grid.n2 - ix2;
		if(npts<0)throw(GCLgrid_error("extract_gridline:  requested offset inconsistent with grid dimensions\n"));
		coords=dmatrix(3,npts);
		for(i=ix2,ii=0;ii<npts;i+=di,++ii)
		{
			coords(0,ii)=grid.x1[ix1][i][ix3];
			coords(1,ii)=grid.x2[ix1][i][ix3];
			coords(2,ii)=grid.x3[ix1][i][ix3];
		}
		break;
	case 3:
		if(reverse)
			npts = ix3 + 1;
		else
			npts = grid.n3 - ix3;
		if(npts<0)throw(GCLgrid_error("extract_gridline:  requested offset inconsistent with grid dimensions\n"));
		coords=dmatrix(3,npts);
		for(i=ix3,ii=0;ii<npts;i+=di,++ii)
		{
			coords(0,ii)=grid.x1[ix1][ix2][i];
			coords(1,ii)=grid.x2[ix1][ix2][i];
			coords(2,ii)=grid.x3[ix1][ix2][i];
		}
		break;
	default:
		throw(GCLgrid_error("extract_gridline function: Illegal component requested.  Must be 1 2 or 3"));
	}
	dmatrix *dmptr=new dmatrix(coords);
	return dmptr;
}
