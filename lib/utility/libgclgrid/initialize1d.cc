#include "interpolator1d.h"
#include "gclgrid.h"
using namespace INTERPOLATOR1D;
/* This group of functions initialize 3d fields with an input 1d function.  This can be used, for
example, to initialize a grid defining a 3d velocity model with a 1d velocity model background.
When an externally defined model is then registered on the grid unfilled points can have a default
value of the 1d background.  This is common, for example, when trying to define an externally produced
model on a different grid geometry.  
*/

/* This function takes the input GCLscalarfield object (field) and fills it with 
interpolated values from the scale, 1d model defined by the input vectors val1d and
z1d.  Note that the order of z1d is expected to be backwards from the grid.  This is
because the grid (see man gclgrid(3)) requires that the x3 component be arranged from
the deepest depth upward while 1d earth model convention universally start at the surface.
This complicates the algorithm, but should be transparent to the user provided the
input recognizes this. */
void  initialize_1Dscalar(GCLscalarfield3d& field, 
	vector<double>val1d,vector<double> z1d, vector<double>grad)
{
	int i,j,k,kk;
	// First make sure the input is valid
	if( (val1d.size()!= z1d.size()) || (z1d.size()!=grad.size()) )
		throw(GCLgrid_error(string("initialize_1Dscalar:  input 1d model vector sizes do not match")));
	// a few useful variables that are best set once as they are used many times below
	int n1dsize=val1d.size();
	int grid_top=field.n3-1;  // index to top surface of grid
	int igrid;
	// Use a simple array here to interface with 1d interpolation library
	double grid_depth;
	/* Compute a match tolerance to nodes using the nominal dx3 grid spacing as 
	a scale factor */
	double dz_match(field.dx3_nom*0.01);
	// To be completely correct and general we have to loop over each surface point because the top
	// surface is not guaranteed to be at Earth's surface.  
	for(i=0;i<field.n1;++i)
		for(j=0;j<field.n2;++j)
		{
			for(k=0;k<field.n3;++k)
			{
				grid_depth=field.depth(i,j,k);
				igrid=irregular_lookup(grid_depth,&(z1d[0]),n1dsize);
				if(igrid<0)
					// case for above top 1d grid point (surface)
					field.val[i][j][k]=val1d[0];
				else if(igrid>(n1dsize-1))
				{
					// case for below bottom of 1d grid definition
					// Note this extends the grid down using a gradient
					field.val[i][j][k]=val1d[n1dsize-1]
						+ grad[n1dsize-1]*(grid_depth-z1d[n1dsize-1]);
				}
				else
				{
					/* We have to be careful of lookup errors creaated
					by roundoff errors.  This was found the hard way to 
					be an issue with constant velocity layered models with
					grad=0 */
					if(igrid>1)
					{
						if(fabs(z1d[igrid-1]-grid_depth)<dz_match) 
							--igrid;
					}
					if(igrid<(n1dsize-1))
					{
						if(fabs(z1d[igrid+1]-grid_depth)<dz_match)
							++igrid;
					}
					// normal point 
					field.val[i][j][k]=val1d[igrid]
						+ grad[igrid]*(grid_depth-z1d[igrid]);
				}
			}
		}
				
}
	
	
	
// Overloaded version for case with 0 gradient in all "layers".  i.e. constant property
// layers.  Implemented by just creating 0 gradient and calling the above.  
void  initialize_1Dscalar(GCLscalarfield3d& field, vector<double>val1d,vector<double> z1d)
{
	vector<double> grad;
	int n=val1d.size();
	grad.reserve(n);
	for(int i=0;i<n;++i)
		grad.push_back(0.0);
	initialize_1Dscalar(field,val1d,z1d,grad);
}
