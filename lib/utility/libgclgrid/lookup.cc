#include <math.h>
#include <list>
/* This fortran routine is used to invert Jacobian.  It is
preferable to a general inversion routine as it is analytic for
a 3x3 system making it faster. */
extern "C" {
extern void treex3_(double *, int *, double *, int *, double *);
}

/* We use 3 element vector dot product, L2 norm, and cross products
from this Antelope library.  This is for efficiency as blas versions of
same have no advantage for 3 element vectors */
#include "coords.h"
#include "gclgrid.h"
class GridCell
{
public:
	int ii;
	int jj;
	int kk;

	dmatrix points;
	// convenient pointers
	double *point1, *point2, *point3, *point4, *point5,
		*point6, *point7, *point8;
	dmatrix normals;
	double *front, *back, *right, *left, *top, *bottom;
	GridCell(GCLgrid3d& g, int ii, int jj, int kk);
	GridCell(const GridCell& parent);
	GridCell& operator=(const GridCell& parent);
	bool InsideTest(double x, double y, double z, double tolerance);
};
GridCell::GridCell(GCLgrid3d& g, int i, int j, int k) 
		: points(3,8),normals(3,6)
{
	int l;
	ii=i;
	jj=j;
	kk=k;
	// point 1
	points(0,0)=g.x1[i][j][k];
	points(1,0)=g.x2[i][j][k];
	points(2,0)=g.x3[i][j][k];
	point1=points.get_address(0,0);
	// point 2
	points(0,1)=g.x1[i][j][k+1];
	points(1,1)=g.x2[i][j][k+1];
	points(2,1)=g.x3[i][j][k+1];
	point2=points.get_address(0,1);
	// point 3
	points(0,2)=g.x1[i+1][j][k+1];
	points(1,2)=g.x2[i+1][j][k+1];
	points(2,2)=g.x3[i+1][j][k+1];
	point3=points.get_address(0,2);
	// point 4
	points(0,3)=g.x1[i+1][j][k];
	points(1,3)=g.x2[i+1][j][k];
	points(2,3)=g.x3[i+1][j][k];
	point4=points.get_address(0,3);
	// point 5
	points(0,4)=g.x1[i][j+1][k];
	points(1,4)=g.x2[i][j+1][k];
	points(2,4)=g.x3[i][j+1][k];
	point5=points.get_address(0,4);
	// point 6
	points(0,5)=g.x1[i][j+1][k+1];
	points(1,5)=g.x2[i][j+1][k+1];
	points(2,5)=g.x3[i][j+1][k+1];
	point6=points.get_address(0,5);
	// point 7
	points(0,6)=g.x1[i+1][j+1][k+1];
	points(1,6)=g.x2[i+1][j+1][k+1];
	points(2,6)=g.x3[i+1][j+1][k+1];
	point7=points.get_address(0,6);
	// point 8
	points(0,7)=g.x1[i+1][j+1][k];
	points(1,7)=g.x2[i+1][j+1][k];
	points(2,7)=g.x3[i+1][j+1][k];	
	point8=points.get_address(0,7);
	// Set up similar pointers right away for normals 
	front=normals.get_address(0,0);
	back=normals.get_address(0,1);
	right=normals.get_address(0,2);
	left=normals.get_address(0,3);
	top=normals.get_address(0,4);
	bottom=normals.get_address(0,5);
	// now compute normals as average of cross products from alternate 
	// pairs of vectors 
	double a[3],b[3],cross1[3],cross2[3];
	dr3sub(point4,point1,a);
	dr3sub(point2,point1,b);	
	dr3cros(a,b,cross1);
	dr3sub(point2,point3,a);
	dr3sub(point4,point3,b);
	dr3cros(a,b,cross2);
	for(l=0;l<3;++l)
		front[l]=(cross1[l]+cross2[l])/2.0;

	dr3sub(point6,point5,a);
	dr3sub(point6,point5,a);
	dr3sub(point8,point5,b);
	dr3cros(a,b,cross1);
	dr3sub(point8,point7,a);
	dr3sub(point6,point7,b);
	dr3cros(a,b,cross2);
	for(l=0;l<3;++l)
		back[l]=(cross1[l]+cross2[l])/2.0;

	dr3sub(point8,point4,a);
	dr3sub(point3,point4,b);
	dr3cros(a,b,cross1);
	dr3sub(point3,point7,a);
	dr3sub(point8,point7,b);
	dr3cros(a,b,cross2);
	for(l=0;l<3;++l)
		right[l]=(cross1[l]+cross2[l])/2.0;

	dr3sub(point3,point2,a);
	dr3sub(point6,point2,b);
	dr3cros(a,b,cross1);
	dr3sub(point6,point7,a);
	dr3sub(point3,point7,b);
	dr3cros(a,b,cross2);
	for(l=0;l<3;++l)
		top[l]=(cross1[l]+cross2[l])/2.0;

	dr3sub(point2,point1,a);
	dr3sub(point5,point1,b);
	dr3cros(a,b,cross1);
	dr3sub(point5,point6,a);
	dr3sub(point2,point6,b);
	dr3cros(a,b,cross2);
	for(l=0;l<3;++l)
		left[l]=(cross1[l]+cross2[l])/2.0;

	dr3sub(point5,point1,a);
	dr3sub(point4,point1,b);
	dr3cros(a,b,cross1);
	dr3sub(point4,point8,a);
	dr3sub(point5,point8,b);
	dr3cros(a,b,cross2);
	for(l=0;l<3;++l)
		bottom[l]=(cross1[l]+cross2[l])/2.0;
	// Finally normalize all these to unit vectors
	dr3norm(front);
	dr3norm(back);
	dr3norm(right);
	dr3norm(left);
	dr3norm(top);
	dr3norm(bottom);
}
GridCell::GridCell(const GridCell& parent)
{
	ii=parent.ii;
	jj=parent.jj;
	kk=parent.kk;
	points=parent.points;
	normals=parent.normals;
	// We can't just copy the pointers as they just point
	// back to the dmatrix parents.  These are now copies.
	// Might work sometimes, but very dangerous to not 
	// refresh these
        point1=points.get_address(0,0);
        point2=points.get_address(0,1);
        point3=points.get_address(0,2);
        point4=points.get_address(0,3);
        point5=points.get_address(0,4);
        point6=points.get_address(0,5);
        point7=points.get_address(0,6);
        point8=points.get_address(0,7);
	front=normals.get_address(0,0);
	back=normals.get_address(0,1);
	right=normals.get_address(0,2);
	left=normals.get_address(0,3);
	top=normals.get_address(0,4);
	bottom=normals.get_address(0,5);
}
GridCell& GridCell::operator=(const GridCell& parent)
{
    if(this != &parent)
    {
	ii=parent.ii;
	jj=parent.jj;
	kk=parent.kk;
	points=parent.points;
	normals=parent.normals;
	// We can't just copy the pointers as they just point
	// back to the dmatrix parents.  These are now copies.
	// Might work sometimes, but very dangerous to not 
	// refresh these
        point1=points.get_address(0,0);
        point2=points.get_address(0,1);
        point3=points.get_address(0,2);
        point4=points.get_address(0,3);
        point5=points.get_address(0,4);
        point6=points.get_address(0,5);
        point7=points.get_address(0,6);
        point8=points.get_address(0,7);
	front=normals.get_address(0,0);
	back=normals.get_address(0,1);
	right=normals.get_address(0,2);
	left=normals.get_address(0,3);
	top=normals.get_address(0,4);
	bottom=normals.get_address(0,5);
    }
    return *this;
}
/* Test if a point x,y,z is inside this cell object (distorted box).
Tolerance gives the allowed slop.  Any point with a negative dot product 
on all 6 faces is considered inside by definition.  Outside is allowed
a fraction value of tolerance.  By fractional I mean a dot product of 
a unit vector from point 1 or point 7 (depending on face) with the unit
normal has a projection less than that the tolerance number.  e.g. 0.1
would mean the largest allowed outside projection is 0.1.
*/
bool GridCell::InsideTest(double x, double y, double z, double tolerance)
{
	double dx0[3];
	double dxproj;
	dx0[0]=x-point1[0];
	dx0[1]=y-point1[1];
	dx0[2]=z-point1[2];
	dr3norm(dx0);
	dxproj=dr3dot(left,dx0);
	if(dxproj>tolerance) return(false);	
	dxproj=dr3dot(bottom,dx0);
	if(dxproj>tolerance) return(false);	
	dxproj=dr3dot(front,dx0);
	if(dxproj>tolerance) return(false);
	// Need vector from point 7 for other 3 sides
	dx0[0]=x-point7[0];
	dx0[1]=y-point7[1];
	dx0[2]=z-point7[2];
	dr3norm(dx0);
	dxproj=dr3dot(top,dx0);
	if(dxproj>tolerance) return(false);	
	dxproj=dr3dot(back,dx0);
	if(dxproj>tolerance) return(false);
	dxproj=dr3dot(right,dx0);
	if(dxproj>tolerance) return(false);	
	return(true);
}	
	
	
static const double FeasibleTest(0.1);
static const double AcceptableTest(1.0E-2);
static const double UnambiguousTest(1.0E-5);

/* This is a recovery routines. The lookup function uses
the direction set method which is known to fail in some situations in 
strongly curved grid lines and at edges.  When the direction set method 
fails this procedures is called.  The algorithm uses a double search
with a more loose feasability test followed by a more rigorous acceptance
test.  The algorithm uses the GridCell object defined above.

Arguments:
	g is the grid to search
	x,y,z is the point in the Cartesian space for g to search for
	i0, j0, k0 lookup iteration result
	dr current computed distance range in unit cells between x,y,z
		and lookup iteration final estimate.  Used to limit
		grid search to + to - int(dr+1.5) 
		Note it is a 3 component vector as I found a need to 
		make search distance variable in different directions
		to reduce search times.
*/
int *recover(GCLgrid3d& g, double x, double y, double z, 
	int i0, int j0, int k0,double *dr)
{
	int i,j,k;
	int search_range;
	int *result=new int[3];
	/* We first search for all but positive direction border
	area.  We have to handle the positive side border in a special
	way due to the method used here. */
	int imin,imax,jmin,jmax,kmin,kmax;
	search_range=static_cast<int>(dr[0]+1.5);
	imin=i0-search_range;
	if(imin<0) imin=0;
	imax=i0+search_range;
	if(imax>=(g.n1-1))imax=g.n1-2;
	search_range=static_cast<int>(dr[1]+1.5);
	jmin=j0-search_range;
	if(jmin<0) jmin=0;
	jmax=j0+search_range;
	if(jmax>=(g.n2-1))jmax=g.n2-2;
	search_range=static_cast<int>(dr[2]+1.5);
	kmin=k0-search_range;
	if(kmin<0) kmin=0;
	kmax=k0+search_range;
	if(kmax>=(g.n3-1))kmax=g.n3-2;
	list<GridCell> feasible;
	list<GridCell>::iterator fptr;
	double drm;
	for(i=imin;i<=imax;++i)
	  for(j=jmin;j<=jmax;++j)
	    for(k=kmin;k<=kmax;++k)
	    {
		GridCell cell(g,i,j,k);
		if(cell.InsideTest(x,y,z,FeasibleTest))
			feasible.push_back(cell);
	    }
	int nfeasible=feasible.size();
	if(nfeasible<=0)
	{
		for(k=0;k<3;++k) result[k]=-1;
	}
	else if (nfeasible==1)
	{
		fptr=feasible.begin();
		result[0]=fptr->ii;
		result[1]=fptr->jj;
		result[2]=fptr->kk;
	}
	else
	{

		// This will silently take the first element in the list
		// if none of the cells in the list pass the full 
		// acceptance test.  Assumption is that feasible means within
		// extrapolation tolerance from any edge.
		fptr=feasible.begin();
		result[0]=fptr->ii;
		result[1]=fptr->jj;
		result[2]=fptr->kk;
		for(fptr=feasible.begin();fptr!=feasible.end();
			fptr++)
		{
			// hunt for the first call passing the unambiguous
			// test.  If that fails we'll try again with 
			// a lower tolerance 
			if(fptr->InsideTest(x,y,z,UnambiguousTest))
			{
				result[0]=fptr->ii;
				result[1]=fptr->jj;
				result[2]=fptr->kk;
				return(result);
			}
		}
		for(fptr=feasible.begin();fptr!=feasible.end();
			fptr++)
		{
			// hunt for the first call passing the unambiguous
			// test.  If that fails we'll try again with 
			// a lower tolerance 
			if(fptr->InsideTest(x,y,z,AcceptableTest))
			{
				result[0]=fptr->ii;
				result[1]=fptr->jj;
				result[2]=fptr->kk;
				break;
			}
		}
	}
	return(result);
}
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

	x,y,z - cartesian coordinates of requested point in GRCgrid
		reference frame

The primary return of this function is the variables ix1, ix2, ix3
defined internally in the GCLgrid3d structure.  They are the index
positions locating the cell that can be interpolated for the
point x,y,z.  

This function returns an integer that defines the outcome of 
the lookup attempt.  If the lookup was successful it returns 0.
If not, the following error codes are defined.
	1 - requested point is outside the bounding box (normally this
		should be handled silently)
	-1 - convergence error;  could not locate x,y,z in the grid 
		(this most likely indicates a ill defined grid that
		does not match the expectations of the library)
The following former return codes have been depricated May 2007:
	2 - requested point is in a grey area near the edge of the 
		grid.  Caller may want to attempt interpolation, but
		should be warned it is dangerous.  i.e. it is 
		algorithm dependent how this case should be handled.
	-2 - total failure.  Attempted nothing because either the
		cartesian grid is not defind.

Note the basic pattern is that a positive error should be handled but
not considered a serious problem while a negative code should cause
at least a diagnostic to be issued.

Author:  Gary Pavlis
Written:  June 2000
Modified:  Debug with minor changes during fall 2002.  Converted to
C++ December 2002.
This brought several major changes too pervasive to be worth documenting.
Modified:  May-June 2007  Major changes.
Found that the previous version failed to converge often and somewhat
randomly when applied to grids with strongly distorted cells.  The
problem was eventually tracked to a fundamental error in the original
version of this algorithm.  Previously I used the projection of points
onto the cell edges as basis vectors for generalized coordinates
defined by grid lines.  This failed in curved grids because the 
correct approach is to compute the Jacobian of the transformation for
each cell.  When the cells have nearly orthogonal sides this algorithm
and the older one are nearly equivalent because then the Jacobian is
nearly diagonal.  When the cells are strongly distorted, however,
the inverse of the Jacobian (used here) differs strongly from the
simple projection operator used in the previous version.
*/
const int MAXIT=50;	//convergence count limit
// Max and min search distances.  Each are effectively one larger
// with current magic number of 1.5 added to this number in recover
// function above. Change these if that number changes.
const double maximum_search_distance(5.0);
const double minimum_search_distance(1.0);  
// If convergence final delta is more than this many grid cells away in any
// generalized coordinate direction, recover is not attempted
// This saves time in curved grids inside the bounding box
const double border_cutoff(2.0);
int GCLgrid3d::lookup(double x, double y, double z) 
{
	int i,j,k;
	int ilast, jlast, klast;
	int ii;
	double dxi[3],dxj[3],dxk[3];
	double nrmdxi,nrmdxj,nrmdxk;
	double dxiunit,dxjunit,dxkunit;
	int di, dj, dk;
	int ctest;
	int count=0;
	/* When inside this number of cell units, increasing distance
	from the target point will break the iterative loop.  Found
	necessary to stop excessive iterations on edge points */
	const double InnerRCut(4.0);
	double drunit,drunit_last;
	dmatrix J(3,3),Jinv(3,3);  // Jacobian and it's inverse
	dvector dxraw(3),dxunit(3);
	int three(3);
	double det;


	/* return immediately if outside the extents bounding box */
	if( (x > (x1high)) || (x < (x1low)) 
	  ||  (y > (x2high)) || (y < (x2low)) 
	  ||  (z > (x3high)) || (z < (x3low)) ) return(1);

	i = ix1;
	j = ix2;
	k = ix3;
	if(i<0) i=0;
	if(j<0) j=0;
	if(k<0) k=0;
	if(i>=((n1)-1)) i = (n1)-2;
	if(j>=((n2)-1)) j = (n2)-2;
	if(k>=((n3)-1)) k = (n3)-2;
	ilast =i;  jlast=j;  klast=k;
	// A conservative large value for drunit to start the loop
	drunit_last=static_cast<double>(n1+n2+n3);


	do
	{
		/* This is the unit cell step directions along x1, x2, and x3 grid lines*/
		dxi[0] = (x1[i+1][j][k]) - (x1[i][j][k]);
		dxi[1] = (x2[i+1][j][k]) - (x2[i][j][k]);
		dxi[2] = (x3[i+1][j][k]) - (x3[i][j][k]);

		dxj[0] = (x1[i][j+1][k]) - (x1[i][j][k]);
		dxj[1] = (x2[i][j+1][k]) - (x2[i][j][k]);
		dxj[2] = (x3[i][j+1][k]) - (x3[i][j][k]);


		dxk[0] = (x1[i][j][k+1]) - (x1[i][j][k]);
		dxk[1] = (x2[i][j][k+1]) - (x2[i][j][k]);
		dxk[2] = (x3[i][j][k+1]) - (x3[i][j][k]);

		nrmdxi = dr3mag(dxi);
		nrmdxj = dr3mag(dxj);
		nrmdxk = dr3mag(dxk);

		dxraw(0) = x - (x1[i][j][k]);
		dxraw(1) = y - (x2[i][j][k]);
		dxraw(2) = z - (x3[i][j][k]);

		// Now compute and use the local Jacobian
		// to compute the number of grid cells to jump.
		for(ii=0;ii<3;++ii)
		{
			J(ii,0)=dxi[ii];
			J(ii,1)=dxj[ii];
			J(ii,2)=dxk[ii];
		}
		// This is a FORTRAN function to invert a 3x3 matrix
		// with an analytic form. It is the same routine
		// called in the interpolate method in this library.
		// All the address references are because this is
		// FORTRAN which requires passing pointers.
		treex3_(J.get_address(0,0),&three,
			Jinv.get_address(0,0),&three,&det);
		dxunit=Jinv*dxraw;
		
		// This is necessary as int truncation for
		// negative numbers removes the fractional
		// part.  The effectively rounds the opposite
		//direction as it does for positive numbers
		// Necessary as we are search for the lower right corner
		// of each cell.
		if(dxunit(0)<0) dxunit(0)-=1.0;
		if(dxunit(1)<0) dxunit(1)-=1.0;
		if(dxunit(2)<0) dxunit(2)-=1.0;
		di=static_cast<int>(dxunit(0));
		dj=static_cast<int>(dxunit(1));
		dk=static_cast<int>(dxunit(2));

		i += di;
		j += dj;
		k += dk;
		drunit=dr3mag(dxunit.get_address(0,0));
		if( (drunit<InnerRCut) && (drunit_last<drunit) )
		{
			// crude, but assures recovery loop will
			// be entered
			ctest=-1;
			break;
		}
		/* We reset i, j, or k if they move outside the grid
		and just continue the iteration.  We catch nonconvergence
		when the loop is exited and try to decide if the 
		nonconvergence is nonconvergence or a point in the 
		grey zone between the bounding box and the actual grid. */
		if(i >= (n1-1) ) 
			i = n1-2;
		if(j >= (n2-1) ) 
			j = n2-2;
		if(k >= (n3-1) ) 
			k = n3-2;
		if(i<0) 
			i=0;
		if(j<0) 
			j=0;
		if(k<0) 
			k=0;
		drunit_last=drunit;
		ctest = abs(di)+abs(dj)+abs(dk);
		++count;
		if(i==ilast && j==jlast && k==klast && ctest>0) 
			break;
		else
		{
			ilast=i;
			jlast=j;
			klast=k;
		}
	}
	while( (ctest>0) && (count<MAXIT) );
	ix1 = i;
	ix2 = j;
	ix3 = k;
 
	if(ctest==0)
	{
		GridCell cell(*this, ix1,ix2,ix3);
		if(cell.InsideTest(x,y,z,UnambiguousTest))
		{
			return(0);
		}
	}

	// Use dxunit values to define search distance in each direction
	double nrmdel,search_distance[3]; 
	for(ii=0;ii<3;++ii)search_distance[ii]=fabs(dxunit(ii));
	// This is aimed to reduce search time for points outside the actual
	// boundary.
	if((ix1==0) || (ix2==0) || (ix3==0)
		||(ix1==n1-2) || (ix2==n2-2) || (ix3==n3-2) )
	{
		for(ii=0;ii<3;++ii)
		{
			if(search_distance[ii]>border_cutoff) 
			{
				return(1);
			}
		}
	}
	for(ii=0;ii<3;++ii)
	{
		if(search_distance[ii]>maximum_search_distance)
			search_distance[ii]=maximum_search_distance;
		else if(search_distance[ii]<minimum_search_distance)
			search_distance[ii]=minimum_search_distance;

	}

	
	int *irecov=recover(*this,x,y,z,ix1,ix2,ix3,search_distance);
	int iret;
	if(irecov[0]<0) 
	{
		this->reset_index();
		iret=-1;
	}
	else
	{
		ix1=irecov[0];
		ix2=irecov[1];
		ix3=irecov[2];
		iret=0;
	}
	delete [] irecov;
	return(iret);
}
//
// 2d version here.  General algorithm and symbols are the same but 
// there are major differences in details.  These are noted below.
// Error's thrown have the same pattern as 3d case.
//
int GCLgrid::lookup(double target_lat, double target_lon) 
{
	int i,j;
	double dxi[3],dxj[3];
	double nrmdxi,nrmdxj;
	double dxiunit,dxjunit;
	int di, dj;
	int ctest;
	int count=0;
	double delta[3];
	Cartesian_point target_x;
	double r0p;
	double x,y,z;

	//
	// This can incorrectly throw an error if the 2d grid is highly
	// warped, but it seems the safest initial value for this 
	//
	r0p=r0;
	target_x = gtoc(target_lat, target_lon, r0p);
	x=target_x.x1;
	y=target_x.x2;
	z=target_x.x3;

	/* return immediately if outside the extents bounding box */
	if( (x > (x1high)) || (x < (x1low)) 
	  ||  (y > (x2high)) || (y < (x2low)) 
	  ||  (z > (x3high)) || (z < (x3low)) ) return(1);

	i = ix1;
	j = ix2;
	if(i<0) i=0;
	if(j<0) j=0;
	if(i>=((n1)-1)) i = (n1)-2;
	if(j>=((n2)-1)) j = (n2)-2;


	do
	{
		//
		//  A 2d GCLgrid is a warped surface.  To allow the surface
		//  to be not just be radial shell we have to constantly 
		//  update the radius vector and the effective target vector
		//  Confusing difference from 3D case, but necessary
		//
		r0p=r(i,j);
		target_x = gtoc(target_lat, target_lon, r0p);
		x=target_x.x1;
		y=target_x.x2;
		z=target_x.x3;

		//compute the cell vectors as above, but only need 2 now
		dxi[0] = (x1[i+1][j]) - (x1[i][j]);
		dxi[1] = (x2[i+1][j]) - (x2[i][j]);
		dxi[2] = (x3[i+1][j]) - (x3[i][j]);

		dxj[0] = (x1[i][j+1]) - (x1[i][j]);
		dxj[1] = (x2[i][j+1]) - (x2[i][j]);
		dxj[2] = (x3[i][j+1]) - (x3[i][j]);

		nrmdxi = dnrm2(3,dxi,1);
		nrmdxj = dnrm2(3,dxj,1);

		delta[0] = x - (x1[i][j]);
		delta[1] = y - (x2[i][j]);
		delta[2] = z - (x3[i][j]);

		dxiunit = ddot(3,delta,1,dxi,1)/(nrmdxi*nrmdxi);
		dxjunit = ddot(3,delta,1,dxj,1)/(nrmdxj*nrmdxj);
                di = static_cast<int>(dxiunit);
                dj = static_cast<int>(dxjunit);
		if(di<0) --di;
		if(dj<0) --dj;
		i += di;
		j += dj;

		if(i >= ((n1)-1) ) i = (n1)-2;
		if(j >= ((n2)-1) ) j = (n2)-2;
		if(i<0) i=0;
		if(j<0) j=0;
		ctest = abs(di)+abs(dj);
		++count;
	}
	while( (ctest>0) && (count<MAXIT) );
	ix1 = i;
	ix2 = j;
	if(ctest==0) return(0);
	if((abs(di)<=1) && (abs(dj)<=1))
		return(2);
	if((count>=MAXIT)) 
		return(-1);
	else
		return(1);
}
