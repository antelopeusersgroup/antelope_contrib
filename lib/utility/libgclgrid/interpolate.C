#include "gclgrid.h"
/* function prototype used only here (FORTRAN routine) */
void fme_weights_(double *,double *,double *);
/*This is an interpolation function for 3d grids.
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

Most used here are part of the object they act on.

Function always returns an nv vector of interpolated values.
IMPORTANT:  The return vector is created with new and must
be freed.

IMPORTANT ASSUMPTION:  no range checking is done here.  This function
is assumed low-level and the caller was careful to pass it a valid
index (i,j,k) for entry into fg and the x1, x2, and x3 grids.

Author:  G Pavlis, 
Written:  December 2000
Modified:  December 2002
Made into a more generic function to use as a helper to C++ 
implementation.  Algorithm is changed but it is much
simpler than the older subroutine like implementation.

One more.  Added BLAS_LIMIT to use the BLAS to interpolate 
large vectors.  This was done in anticipation of using this
function for interpolation of 3D travel time tables.
*/
#define BLAS_LIMIT 10
double *GCLvectorfield3d::interpolate(double xp1, double xp2, double xp3)
{
	double coord[24];
	int l;
	/* This depends on a static being initialized once on first call */
	static double xplast[3]={0.0,0.0,0.0};
	static double weights[8];
	double *f = new double[nv];
	int ix[3];
	int i,j,k;
	double xp[3];

	get_index(ix);
	//
	//This is done to make this readable.  subscripted subscripts without
	//this substitution are very hard to read
	//
	i=ix[0];
	j=ix[1];
	k=ix[2];


		
	if( (xp1!=xplast[0]) || (xp2!=xplast[1]) || (xp3!=xplast[2]) )
	{
		/* point 0 */
		coord[0] = x1[i][j][k];
		coord[8] = x2[i][j][k];
		coord[16] = x3[i][j][k];
		/* point 1 */
		coord[1] = x1[i][j][k+1];
		coord[9] = x2[i][j][k+1];
		coord[17] = x3[i][j][k+1];
		/* point 2 */
		coord[2] = x1[i+1][j][k+1];
		coord[10] = x2[i+1][j][k+1];
		coord[18] = x3[i+1][j][k+1];
		/* point 3 */
		coord[3] = x1[i+1][j][k];
		coord[11] = x2[i+1][j][k];
		coord[19] = x3[i+1][j][k];
		/* point 4 (other side of element in dimension 2) */
		coord[4] = x1[i][j+1][k];
		coord[12] = x2[i][j+1][k];
		coord[20] = x3[i][j+1][k];
		/* point 5 */
		coord[5] = x1[i][j+1][k+1];
		coord[13] = x2[i][j+1][k+1];
		coord[21] = x3[i][j+1][k+1];
		/* point 6 */
		coord[6] = x1[i+1][j+1][k+1];
		coord[14] = x2[i+1][j+1][k+1];
		coord[22] = x3[i+1][j+1][k+1];
		/* point 7 */
		coord[7] = x1[i+1][j+1][k];
		coord[15] = x2[i+1][j+1][k];
		coord[23] = x3[i+1][j+1][k];
		xp[0]=xp1;
		xp[1]=xp2;
		xp[2]=xp3;
		fme_weights_(xp,coord,weights);
	}
	/* Compute interpolated vector as a linear combination of the
	 * corners using weights just computed (or from the last pass)
	 * Use the BLAS for large vectors, but use the scalar form for
	 * smaller vectors.
	 */
	if(nv>=BLAS_LIMIT)
	{
		for(l=0;l<nv;++l) f[l]=0.0;
		daxpy(nv,weights[0],val[i][j][k],1,f,1);
		daxpy(nv,weights[1],val[i][j][k+1],1,f,1);
		daxpy(nv,weights[2],val[i+1][j][k+1],1,f,1);
		daxpy(nv,weights[3],val[i+1][j][k],1,f,1);
		daxpy(nv,weights[4],val[i][j+1][k],1,f,1);
		daxpy(nv,weights[5],val[i][j+1][k+1],1,f,1);
		daxpy(nv,weights[6],val[i+1][j+1][k+1],1,f,1);
		daxpy(nv,weights[7],val[i+1][j+1][k],1,f,1);
	}
	else
	{
	    for(l=0;l<nv;++l)
	    {
		f[l]=0.0;
		f[l]+=weights[0]* val[i][j][k][l];
		f[l]+=weights[1]*val[i][j][k+1][l]; 
		f[l]+=weights[2]*val[i+1][j][k+1][l];
		f[l]+=weights[3]*val[i+1][j][k][l];
		f[l]+=weights[4]*val[i][j+1][k][l];
		f[l]+=weights[5]*val[i][j+1][k+1][l];
		f[l]+=weights[6]*val[i+1][j+1][k+1][l];
		f[l]+=weights[7]*val[i+1][j+1][k][l];
	    }
	}
	for(l=0;l<3;++l) xplast[l]=xp[l];
	return(f);
}
//
// painfully parallel routine for scalars.  There may be a way to do this
// elegantly in C++ but it is beyond my current understanding.
//
double GCLscalarfield3d::interpolate(double xp1, double xp2, double xp3)
{
	double coord[24];
	/* This depends on a static being initialized once on first call */
	static double xplast[3]={0.0,0.0,0.0};
	static double weights[8];
	double f;
	int ix[3];
	int i,j,k,l;
	double xp[3];

	get_index(ix);
	i=ix[0];
	j=ix[1];
	k=ix[2];
	
	if( (xp1!=xplast[0]) || (xp2!=xplast[1]) || (xp3!=xplast[2]) )
	{
		/* point 0 */
		coord[0] = x1[i][j][k];
		coord[8] = x2[i][j][k];
		coord[16] = x3[i][j][k];
		/* point 1 */
		coord[1] = x1[i][j][k+1];
		coord[9] = x2[i][j][k+1];
		coord[17] = x3[i][j][k+1];
		/* point 2 */
		coord[2] = x1[i+1][j][k+1];
		coord[10] = x2[i+1][j][k+1];
		coord[18] = x3[i+1][j][k+1];
		/* point 3 */
		coord[3] = x1[i+1][j][k];
		coord[11] = x2[i+1][j][k];
		coord[19] = x3[i+1][j][k];
		/* point 4 (other side of element in dimension 2) */
		coord[4] = x1[i][j+1][k];
		coord[12] = x2[i][j+1][k];
		coord[20] = x3[i][j+1][k];
		/* point 5 */
		coord[5] = x1[i][j+1][k+1];
		coord[13] = x2[i][j+1][k+1];
		coord[21] = x3[i][j+1][k+1];
		/* point 6 */
		coord[6] = x1[i+1][j+1][k+1];
		coord[14] = x2[i+1][j+1][k+1];
		coord[22] = x3[i+1][j+1][k+1];
		/* point 7 */
		coord[7] = x1[i+1][j+1][k];
		coord[15] = x2[i+1][j+1][k];
		coord[23] = x3[i+1][j+1][k];
		xp[0]=xp1;
		xp[1]=xp2;
		xp[2]=xp3;
		fme_weights_(xp,coord,weights);
	}

	f=0.0;
	f+=weights[0]* val[i][j][k];
	f+=weights[1]*val[i][j][k+1]; 
	f+=weights[2]*val[i+1][j][k+1];
	f+=weights[3]*val[i+1][j][k];
	f+=weights[4]*val[i][j+1][k];
	f+=weights[5]*val[i][j+1][k+1];
	f+=weights[6]*val[i+1][j+1][k+1];
	f+=weights[7]*val[i+1][j+1][k];

	for(l=0;l<3;++l) xplast[l]=xp[l];
	return(f);
}

double *GCLvectorfield::interpolate(double xp1, double xp2, double xp3)
{
	double coord[24];
	int l;
	/* This depends on a static being initialized once on first call */
	static double xplast[3]={0.0,0.0,0.0};
	static double weights[8];
	double *f = new double[nv];
	int i,j;
	double xp[3];
	int ix[2];

	get_index(ix);
	i=ix[0];
	j=ix[1];

		
	if( (xp1!=xplast[0]) || (xp2!=xplast[1]) || (xp3!=xplast[2]) )
	{
		double dx1[3], dx2[3];
		Cartesian_point cp;
		double cell_size;
		//compute the average cell size 
		dx1[0]=x1[i+1][j]-x1[i][j];
		dx1[1]=x2[i+1][j]-x2[i][j];
		dx1[2]=x3[i+1][j]-x3[i][j];
		dx2[0]=x1[i][j+1]-x1[i][j];
		dx2[1]=x2[i][j+1]-x2[i][j];
		dx2[2]=x3[i][j+1]-x3[i][j];
		cell_size=(dnrm2(3,dx1,1)+dnrm2(3,dx2,1))/2.0;
		//
		//We now find coordinates for fake points one cell_size below 
		//each grid point.  We use this for a box for the interpolator.
		//This is not as efficient as a 2d interpolator but is always
		//correct even when the 4 points are not coplanar.
		//
		//start with point 0
		cp=gtoc(lat[i][j], lon[i][j],r[i][j]-cell_size);
		coord[0]=cp.x1;
		coord[8]=cp.x2;
		coord[16]=cp.x3;
		/* point 1 */
		coord[1] = x1[i][j];
		coord[9] = x2[i][j];
		coord[17] = x3[i][j];
		/*point 2 */
		coord[2] = x1[i+1][j];
		coord[10] = x2[i+1][j];
		coord[18] = x3[i+1][j];	
		/* point 3 */
		cp=gtoc(lat[i+1][j], 
			lon[i+1][j],r[i+1][j]-cell_size);
		coord[3]=cp.x1;
		coord[11]=cp.x2;
		coord[19]=cp.x3;
		/* point 4 (other side of element in dimension 2) */
		cp=gtoc(lat[i][j+1], 
			lon[i][j+1],r[i][j+1]-cell_size);
		coord[4]=cp.x1;
		coord[12]=cp.x2;
		coord[20]=cp.x3;
		/* point 5 */
		coord[5] = x1[i][j+1];
		coord[13] = x2[i][j+1];
		coord[21] = x3[i][j+1];
		/* point 6 */
		coord[6] = x1[i+1][j+1];
		coord[14] = x2[i+1][j+1];
		coord[22] = x3[i+1][j+1];
		/* point 7 */
		cp=gtoc(lat[i+1][j+1], 
			lon[i+1][j+1],r[i+1][j+1]-cell_size);
		coord[7]=cp.x1;
		coord[15]=cp.x2;
		coord[23]=cp.x3;
		xp[0]=xp1;
		xp[1]=xp2;
		xp[2]=xp3;
		fme_weights_(xp,coord,weights);
	}
	for(l=0;l<nv;++l)
	{
		f[l]=0.0;
		f[l]+=weights[0]* val[i][j][l];
		f[l]+=weights[1]*val[i][j][l]; 
		f[l]+=weights[2]*val[i+1][j][l];
		f[l]+=weights[3]*val[i+1][j][l];
		f[l]+=weights[4]*val[i][j+1][l];
		f[l]+=weights[5]*val[i][j+1][l];
		f[l]+=weights[6]*val[i+1][j+1][l];
		f[l]+=weights[7]*val[i+1][j+1][l];
	}
	for(l=0;l<3;++l) xplast[l]=xp[l];
	return(f);
}

double GCLscalarfield::interpolate(double xp1, double xp2, double xp3)
{
	double coord[24];
	/* This depends on a static being initialized once on first call */
	static double xplast[3]={0.0,0.0,0.0};
	static double weights[8];
	double f;
	int ix[2];
	int i,j,l;
	double xp[3];

	get_index(ix);
	i=ix[0];
	j=ix[1];
		
	if( (xp1!=xplast[0]) || (xp2!=xplast[1]) || (xp3!=xplast[2]) )
	{
		double dx1[3], dx2[3];
		Cartesian_point cp;
		double cell_size;
		//compute the average cell size 
		dx1[0]=x1[i+1][j]-x1[i][j];
		dx1[1]=x2[i+1][j]-x2[i][j];
		dx1[2]=x3[i+1][j]-x3[i][j];
		dx2[0]=x1[i][j+1]-x1[i][j];
		dx2[1]=x2[i][j+1]-x2[i][j];
		dx2[2]=x3[i][j+1]-x3[i][j];
		cell_size=(dnrm2(3,dx1,1)+dnrm2(3,dx2,1))/2.0;
		//
		//We now find coordinates for fake points one cell_size below 
		//each grid point.  We use this for a box for the interpolator.
		//This is not as efficient as a 2d interpolator but is always
		//correct even when the 4 points are not coplanar.
		//
		//start with point 0
		cp=gtoc(lat[i][j], lon[i][j],r[i][j]-cell_size);
		coord[0]=cp.x1;
		coord[8]=cp.x2;
		coord[16]=cp.x3;
		/* point 1 */
		coord[1] = x1[i][j];
		coord[9] = x2[i][j];
		coord[17] = x3[i][j];
		/*point 2 */
		coord[2] = x1[i+1][j];
		coord[10] = x2[i+1][j];
		coord[18] = x3[i+1][j];	
		/* point 3 */
		cp=gtoc(lat[i+1][j], 
			lon[i+1][j],r[i+1][j]-cell_size);
		coord[3]=cp.x1;
		coord[11]=cp.x2;
		coord[19]=cp.x3;
		/* point 4 (other side of element in dimension 2) */
		cp=gtoc(lat[i][j+1], 
			lon[i][j+1],r[i][j+1]-cell_size);
		coord[4]=cp.x1;
		coord[12]=cp.x2;
		coord[20]=cp.x3;
		/* point 5 */
		coord[5] = x1[i][j+1];
		coord[13] = x2[i][j+1];
		coord[21] = x3[i][j+1];
		/* point 6 */
		coord[6] = x1[i+1][j+1];
		coord[14] = x2[i+1][j+1];
		coord[22] = x3[i+1][j+1];
		/* point 7 */
		cp=gtoc(lat[i+1][j+1], 
			lon[i+1][j+1],r[i+1][j+1]-cell_size);
		coord[7]=cp.x1;
		coord[15]=cp.x2;
		coord[23]=cp.x3;
		xp[0]=xp1;
		xp[1]=xp2;
		xp[2]=xp3;
		fme_weights_(xp,coord,weights);
	}
	f=0.0;
	f+=weights[0]* val[i][j];
	f+=weights[1]*val[i][j]; 
	f+=weights[2]*val[i+1][j];
	f+=weights[3]*val[i+1][j];
	f+=weights[4]*val[i][j+1];
	f+=weights[5]*val[i][j+1];
	f+=weights[6]*val[i+1][j+1];
	f+=weights[7]*val[i+1][j+1];
	for(l=0;l<3;++l) xplast[l]=xp[l];
	return(f);
}

