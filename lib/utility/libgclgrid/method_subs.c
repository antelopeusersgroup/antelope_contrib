/* This group of modules are what might be called "methods" if I wanted
to make a C++ monster out of this. */

#include <sunperf.h>
#include "db.h"
#include "gclgrid.h"
/* function prototype used only here (FORTRAN routine) */
void fme_interpolate_(int *,double *,double *,double *,double *);
/* This is an indexing routine for finding the location of a point in 3 space
within what I'm calling here a geographical curvilinear grid (gclgrid).  
This algorithm will work only if the grid defines an object that is 
best thought of as a distorted box filled with bricks of approximately 
uniform initial size.  The grid points index the location of the bricks.
Distortion means the bricks (and object) can be distorted in to 
objects with nonorthogonal sides and variable spacing.  They are still
indexable, however, because the grid points are assumed to be laid
out in a regular order (C order with the last index varying most 
rapidly.  The indices map positions in generalized coordinates in
the grid.)  The algorith used here works only if the grid is not folded 
or multivalued in any sense.  

The basic  algorithm is an iterative one that computes the local 
transformation matrix at each step from a simple forward difference.
That is, it essentially does a shift to the current grid position 
defined by the generalized coordinate index positions i, j, and k.  
At that point it computes a vector direction of a +1 shift in each
grid position to define the number of unit cells to jump from the 
current position.  Because the grid spacing is not assumed to be
uniform or rectilinear this in general requires an iteration.
This is repeated until the requested point is in the cell 
defined by a bounding box defined by the location 
x1[i+1][j+1][k+1], x2[i+1][j+1][k+1], x3[i+1][j+1][k+1]  to
x1[i][j][k], x2[i][j][k], x3[i][j][k]  This algorith converges
rapidly if the initial starting point is not far from the final
point.  For this reason we return the index positions through 
the argument list.  When tracking a regular curve this approach
should be reasonably fast if the previous index position is passed
as the starting point for the search.  

Arguments:

	g - GRLgrid structure object to find requested point int
	x,y,z - cartesian coordinates of requested point in GRCgrid
		reference frame
	ix, iy, iz - grid index locations for voxel containing x,y,z.
		On entry these values are used as a starting point
		for the search.  On exit they contain the requested
		index position (see above).  
Returns:
Normal return is 0.  A nonzero return indicates an error that the
calling program will need to handle as desired.
+1 return indicates the point is located outside the grid and the 
returned indices are meaninless
-1 return indicates the algorithm failed to converge even though the 
final point was located inside the grid.  

Both nonzero returns indicate the returned index values are not 
to be ued.  

Author:  Gary Pavlis
Written:  June 2000
*/
#define MAXIT_GRID 50   /* convergence count limit */
int GCL3Dgrid_index_lookup(GCL3Dgrid *g, 
	double x, double y, double z,
	int *ix, int *iy, int *iz)
{
	int i,j,k;
	int ilast,jlast,klast;
	double xp[3],yp[3],zp[3];
	double dx,dy,dz;
	double xg,yg,zg;
	int di, dj, dk;
	int ctest;
	int count=0;
	double delta[3];

	/* return immediately if outside the extents bounding box */
	if( (x > (g->xhigh)) || (x < (g->xlow)) 
	  ||  (y > (g->yhigh)) || (y < (g->ylow)) 
	  ||  (z > (g->zhigh)) || (z < (g->zlow)) ) return(1);

	i = *ix;
	j = *iy;
	k = *iz;
	if(i<0) i=0;
	if(j<0) j=0;
	if(k<0) k=0;
	if(i>=((g->n1)-1)) i = (g->n1)-2;
	if(j>=((g->n2)-1)) j = (g->n2)-2;
	if(k>=((g->n3)-1)) k = (g->n3)-2;


	do
	{
		/* This is the unit step vector in x, y, and z directions*/
		xp[0] = (g->x1[i+1][j][k]) - (g->x1[i][j][k]);
		xp[1] = (g->x1[i][j+1][k]) - (g->x1[i][j][k]);
		xp[2] = (g->x1[i][j][k+1]) - (g->x1[i][j][k]);
		yp[0] = (g->x2[i+1][j][k]) - (g->x2[i][j][k]);
		yp[1] = (g->x2[i][j+1][k]) - (g->x2[i][j][k]);
		yp[2] = (g->x2[i][j][k+1]) - (g->x2[i][j][k]);
		zp[0] = (g->x3[i+1][j][k]) - (g->x3[i][j][k]);
		zp[1] = (g->x3[i][j+1][k]) - (g->x3[i][j][k]);
		zp[2] = (g->x3[i][j][k+1]) - (g->x3[i][j][k]);

		dx = dnrm2(3,xp,1);
		dy = dnrm2(3,yp,1);
		dz = dnrm2(3,zp,1);
		delta[0] = x - (g->x1[i][j][k]);
		delta[1] = y - (g->x2[i][j][k]);
		delta[2] = z - (g->x3[i][j][k]);
		/* This might seem wrong, but it is correct to divide
		by dx^2.  One is dot product normalize, second is 
		to get distance in cell units */
		di = (int)ddot(3,delta,1,xp,1)/(dx*dx);
		dj = (int)ddot(3,delta,1,yp,1)/(dy*dy);
		dk = (int)ddot(3,delta,1,zp,1)/(dz*dz);
		/* This is necessary because integer truncation jumps
		opposite directions for positive and negative numbers.
		An alternative is nint, but then you have to mess with 
		a special condition between -0.5 and 0 to converge
		correctly.  Here it is automatic.*/
		if(di<0) --di;
		if(dj<0) --dj;
		if(dk<0) --dk;
		i += di;
		j += dj;
		k += dk;
		/* We reset i, j, or k if they move outside the grid
		and just continue the iteration.  We catch nonconvergence
		when the loop is exited and try to decide if the 
		nonconvergence is nonconvergence or a point in the 
		grey zone between the bounding box and the actual grid. */
		if(i >= ((g->n1)-1) ) i = (g->n1)-2;
		if(j >= ((g->n2)-1) ) j = (g->n2)-2;
		if(k >= ((g->n3)-1) ) k = (g->n3)-2;
		if(i<0) i=0;
		if(j<0) j=0;
		if(k<0) k=0;
		ctest = abs(di)+abs(dj)+abs(dk);
		++count;
	}
	while( (ctest>0) && (count<MAXIT_GRID) );
	/* points in the grey area between the bounding box and
	the edges of the grid are assumed to never converge and
	reach here.  We first exit immediately for convergence
	then handle the complexities of returning the correct
	error code only if necessary. */
	if(ctest == 0)
	{
		*ix = i;
		*iy = j;
		*iz = k;
		return(0);
	}
	if((di<=0) && (i==0) )return(1);
	if((dj<=0) && (j==0) )return(1);
	if((dk<=0) && (k==0) )return(1);
	if( di && (i>=((g->n1)-2))) return(1);
	if( dj && (j>=((g->n2)-2))) return(1);
	if( dk && (k>=((g->n3)-2))) return(1);

	/* We should only get here when we have a convergence error */
	return(-1);
}

/*This is an interpolation function for GCL3dgrid objects.  
This function is mostly an interface function to the fme_interpolate
(FORTRAN) function that actually does the interpolation.  
that routine uses distorted box, finite element interpolation 
functions to obtain an interpolation of the vector valued function
fg defining the values of the function at the grid points defined
in g.  It is assumed the coordinates of the point to be interpolated
(xp) are within the box defined by [i]->[i+1], [j]->[j+1] and
[k]->[k+1].  The function may return an answer if this isn't so, 
but generally if the point is more than one approximate cell unit
outside the box it is likely to be totally garbage.  
We interpolate all nf points in a vector field simultaneously 
because there is a fair amount of work evaluating the Jacobian for
the tranformation of the interpolation functions to standardized
coordinates.  Note that the vector component is the first index of
the four D array defined the functional points, fg.  This is done
so that if the fg array is actually only scalar it need only be
passed as &fg and the function will work properly.  


Arguments:
xp-three vector defining location in space at which the value of the
	desired function is to be interpolated (Note: It is blindly 
	assumed this is a valid 3 vector within the interior or at
	least nearby the box defined by the i,j,k index --below).
g - grid to interpolate for answer
i, j, k - grid index returned by above function 
fg - 4d array of values to be interpolated (leftmost index is vector
	element index so length 1 resolves correctly).
f - vector to hold result
nf - length of f

Function always returns 0

Author:  G Pavlis, 
Written:  December 2000
*/
int GCL3Dgrid_interpolate(double *xp, GCL3Dgrid *g, int i, int j, int k, 
double ****fg, double *f, int nf)
{
	double coord[24];
	double *fvals;   /* work space holding function values for nf */
	int iv;
	allot(double *,fvals,8*nf);

	/* point 0 */
	coord[0] = g->x1[i][j][k];
	coord[8] = g->x2[i][j][k];
	coord[16] = g->x3[i][j][k];
	/* point 1 */
	coord[1] = g->x1[i][j][k+1];
	coord[9] = g->x2[i][j][k+1];
	coord[17] = g->x3[i][j][k+1];
	/* point 2 */
	coord[2] = g->x1[i+1][j][k+1];
	coord[10] = g->x2[i+1][j][k+1];
	coord[18] = g->x3[i+1][j][k+1];
	/* point 3 */
	coord[3] = g->x1[i+1][j][k];
	coord[11] = g->x2[i+1][j][k];
	coord[19] = g->x3[i+1][j][k];
	/* point 4 (other side of element in dimension 2) */
	coord[4] = g->x1[i][j+1][k];
	coord[12] = g->x2[i][j+1][k];
	coord[20] = g->x3[i][j+1][k];
	/* point 5 */
	coord[5] = g->x1[i][j+1][k+1];
	coord[13] = g->x2[i][j+1][k+1];
	coord[21] = g->x3[i][j+1][k+1];
	/* point 6 */
	coord[6] = g->x1[i+1][j+1][k+1];
	coord[14] = g->x2[i+1][j+1][k+1];
	coord[22] = g->x3[i+1][j+1][k+1];
	/* point 7 */
	coord[7] = g->x1[i+1][j+1][k];
	coord[15] = g->x2[i+1][j+1][k];
	coord[23] = g->x3[i+1][j+1][k];
	for(iv=0;iv<nf;++iv)
	{
		fvals[iv*8] = fg[iv][i][j][k];
		fvals[1+iv*8] = fg[iv][i][j][k+1];
		fvals[2+iv*8] = fg[iv][i+1][j][k+1];
		fvals[3+iv*8] = fg[iv][i+1][j][k];
		fvals[4+iv*8] = fg[iv][i][j+1][k];
		fvals[5+iv*8] = fg[iv][i][j+1][k+1];
		fvals[6+iv*8] = fg[iv][i+1][j+1][k+1];
		fvals[7+iv*8] = fg[iv][i+1][j+1][k];
	}
	fme_interpolate_(&nf,xp,fvals,coord,f);
	free(fvals);
	return(0);
}
/* This function maps an input grid onto an output grid using 
interpolation and indexing functions immediately above.  The 
algorithm basically scans the grid in order filling in all valid
values that are properly interpolated.  The initial grid is cleared
to zero to allow straight sums to occur without being burdened by
conditionals.  To maintain a record of which cells could not be filled
a parallel 3d int array is filled with 1 for a hit and 0 for nonhit.  


Author:  Gary Pavlis
Written:  June 2000
*/
int map_full_grid(GCL3Dgrid *gin, double ****fin, 
	GCL3Dgrid *gout, double ****fout, int nv, int ***hit)
{
	int i,j,k;
	int ix, iy, iz;
	int iv;  /* vector component = subscript 1 */
	double *fwork;
	int nzeros=0;
	int retcode;
	double xp[3];

	allot(double *,fwork,nv);

	/* initialize */
	for(iv=0;iv<nv;++iv)
		for(i=0;i<(gout->n1);++i)
		for(j=0;j<(gout->n2);++j)
		for(k=0;k<(gout->n3);++k)
			fout[iv][i][j][k] = 0.0;
	for(i=0;i<(gout->n1);++i)
		for(j=0;j<(gout->n2);++j)
		for(k=0;k<(gout->n3);++k)
			hit[i][j][k] = 0;

	ix = gin->i0;
	iy = gin->j0;
	iz = gin->k0;

	for(i=0;i<(gout->n1);++i)
	{
		for(j=0;j<(gout->n2);++j)
		{
			for(k=0;k<(gout->n3);++k)
			{
			retcode = GCL3Dgrid_index_lookup(gin,
					gout->x1[i][j][k],
					gout->x2[i][j][k],
					gout->x3[i][j][k],
					&ix,&iy,&iz);
			if(retcode)
			{
				++nzeros;
				if(retcode<0) elog_complain(0,
				  "Convergence failure in GCLgrid_index_lookup:  interpolation gap artifacts are likely\nError at output grid index %d,%d,%d\n",
				  i,j,k);
			}
			else
			{
				xp[0] = gout->x1[i][j][k];
				xp[1] = gout->x2[i][j][k];
				xp[2] = gout->x3[i][j][k];

				GCL3Dgrid_interpolate(xp,gin,ix,iy,iz,
					fout, fwork, nv);
				for(iv=0;iv<nv;++iv)fout[iv][i][j][k]=fwork[iv];
				++hit[i][j][k];
			}
			}
		}
	}

	free(fwork);
	return(nzeros);
}
/* Full indexed form of a routine to accumulate one 3d gridded funtion,
(in) to another (sum) updating the hitcount (hitsum is hitcount of
current sum ).  Grid dimensions are assumed n1,n2, n3 and it is 
blindly assumed indexing will work.  A potentially faster form
of this routine could be used if one guaranteed the memory defined
by in and sum was contiguous and could be treated as a single 
vector.  I decided it was not necessary to add this confusion unless
it proved necessary */

void GCL3D_grid_stack(double ***in, double ***sum, int ***hit, int ***hitsum,
	int n1, int n2, int n3)
{
	int i,j,k;
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j)
			for(k=0;k<n3;++k)
			{
				sum[i][j][k] += in[i][j][k];
				hitsum[i][j][k] += hit[i][j][k];
			}
}

