#include <stdio.h>
#include <float.h>
#include "elog.h"
#include "gclgrid.h"
/* modified Nov. 2003.  Original assumed cartesian frames for g and the current object were
identical.  We have no reason to believe this should always be so.  Now all these operators
are paranoid and use the geographhic points and convert them to the cartesian components.

Modified Nov 2004:  added == and != operators 
Modified April 2004:  
1.  modified += operators for 3d objects to improve efficiency by 
	dropping unnecessary geographic conversions.  (algorithm change)
2.  Moved = operators from create_destroy to this file.  Found I had
	missed the fact that operator = was not defined before for 
	field objects.  These were added.
*/

// First the assignment operators.
// This code is very repetitious as I don't see how to avoid the
// duplication of code necessitated by inheritance mechanism.
// That is, I am not aware of the equivalent of what can be done
// for a copy constructor 

GCLgrid& GCLgrid::operator=(const GCLgrid& g)
{
	if(this != &g)  // avoid self assignment
	{
		// This has serious problems if the x1, x2, and x3 pointers
		// aren't initialized to NULL
		if(x1!=NULL) free_2dgrid_contiguous(x1,n1);
		if(x2!=NULL) free_2dgrid_contiguous(x2,n1);
		if(x3!=NULL) free_2dgrid_contiguous(x3,n1);

		int i,j;
		name=g.name;
		lat0=g.lat0;
		lon0=g.lon0;
		r0=g.r0;
		azimuth_y=g.azimuth_y;
		dx1_nom=g.dx1_nom;
		dx2_nom=g.dx2_nom;
		n1=g.n1;
		n2=g.n2;
		i0=g.i0;
		j0=g.j0;
		x1low=g.x1low;
		x1high=g.x1high;
		x2low=g.x2low;
		x2high=g.x2high;
		x3low=g.x3low;
		x3high=g.x3high;
		for(i=0;i<3;++i)
		{
			translation_vector[i]=g.translation_vector[i];
			for(j=0;j<3;++j) gtoc_rmatrix[i][j]=g.gtoc_rmatrix[i][j];
		}
		ix1=g.ix1;
		ix2=g.ix2;
		x1=create_2dgrid_contiguous(n1,n2);
		x2=create_2dgrid_contiguous(n1,n2);
		x3=create_2dgrid_contiguous(n1,n2);
		//
		//I use separate loops for each array here as this is highly
		//optimized on most compilers 
		//
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) x1[i][j]=g.x1[i][j];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) x2[i][j]=g.x2[i][j];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) x3[i][j]=g.x3[i][j];
	}
	return *this;
}
//
//Same for 3D grid
//
GCLgrid3d& GCLgrid3d::operator=(const GCLgrid3d& g)
{
	if(this != &g)  // avoid self assignment
	{
		int i,j,k;
		// This has serious problems if the x1, x2, and x3 pointers
		// aren't initialized to NULL
		if(x1!=NULL) free_3dgrid_contiguous(x1,n1,n2);
		if(x2!=NULL) free_3dgrid_contiguous(x2,n1,n2);
		if(x3!=NULL) free_3dgrid_contiguous(x3,n1,n2);

		name=g.name;
		lat0=g.lat0;
		lon0=g.lon0;
		r0=g.r0;
		azimuth_y=g.azimuth_y;
		dx1_nom=g.dx1_nom;
		dx2_nom=g.dx2_nom;
		dx3_nom=g.dx3_nom;
		n1=g.n1;
		n2=g.n2;
		n3=g.n3;
		i0=g.i0;
		j0=g.j0;
		k0=g.k0;
		x1low=g.x1low;
		x1high=g.x1high;
		x2low=g.x2low;
		x2high=g.x2high;
		x3low=g.x3low;
		x3high=g.x3high;
		x1=create_3dgrid_contiguous(n1,n2,n3);
		x2=create_3dgrid_contiguous(n1,n2,n3);
		x3=create_3dgrid_contiguous(n1,n2,n3);
		for(i=0;i<3;++i)
		{
			translation_vector[i]=g.translation_vector[i];
			for(j=0;j<3;++j) gtoc_rmatrix[i][j]=g.gtoc_rmatrix[i][j];
		}
		ix1=g.ix1; ix2=g.ix2; ix3=g.ix3;
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) 
				for(k=0;k<n3;++k) x1[i][j][k]=g.x1[i][j][k];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) 
				for(k=0;k<n3;++k) x2[i][j][k]=g.x2[i][j][k];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) 
				for(k=0;k<n3;++k) x3[i][j][k]=g.x3[i][j][k];
	}
	return *this;
}
// Now assignment for field objects.  Repetitious, but don't know how
// to avoid this.
// First the 2d field objects.
GCLscalarfield& GCLscalarfield::operator=(const GCLscalarfield& g)
{
	if(this != &g)  // avoid self assignment
	{
		// This has serious problems if the x1, x2, and x3 pointers
		// aren't initialized to NULL
		if(x1!=NULL) free_2dgrid_contiguous(x1,n1);
		if(x2!=NULL) free_2dgrid_contiguous(x2,n1);
		if(x3!=NULL) free_2dgrid_contiguous(x3,n1);
		if(val!=NULL) free_2dgrid_contiguous(val,n1);

		int i,j;
		name=g.name;
		lat0=g.lat0;
		lon0=g.lon0;
		r0=g.r0;
		azimuth_y=g.azimuth_y;
		dx1_nom=g.dx1_nom;
		dx2_nom=g.dx2_nom;
		n1=g.n1;
		n2=g.n2;
		i0=g.i0;
		j0=g.j0;
		x1low=g.x1low;
		x1high=g.x1high;
		x2low=g.x2low;
		x2high=g.x2high;
		x3low=g.x3low;
		x3high=g.x3high;
		for(i=0;i<3;++i)
		{
			translation_vector[i]=g.translation_vector[i];
			for(j=0;j<3;++j) gtoc_rmatrix[i][j]=g.gtoc_rmatrix[i][j];
		}
		ix1=g.ix1;
		ix2=g.ix2;
		x1=create_2dgrid_contiguous(n1,n2);
		x2=create_2dgrid_contiguous(n1,n2);
		x3=create_2dgrid_contiguous(n1,n2);
		val=create_2dgrid_contiguous(n1,n2);
		//
		//I use separate loops for each array here as this is highly
		//optimized on most compilers 
		//
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) x1[i][j]=g.x1[i][j];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) x2[i][j]=g.x2[i][j];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) x3[i][j]=g.x3[i][j];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) val[i][j]=g.val[i][j];
	}
	return *this;
}
GCLvectorfield& GCLvectorfield::operator=(const GCLvectorfield& g)
{
	if(this != &g)  // avoid self assignment
	{
		// This has serious problems if the x1, x2, and x3 pointers
		// aren't initialized to NULL
		if(x1!=NULL) free_2dgrid_contiguous(x1,n1);
		if(x2!=NULL) free_2dgrid_contiguous(x2,n1);
		if(x3!=NULL) free_2dgrid_contiguous(x3,n1);
		if(val!=NULL) free_3dgrid_contiguous(val,n1,n2);

		int i,j,k;
		name=g.name;
		lat0=g.lat0;
		lon0=g.lon0;
		r0=g.r0;
		azimuth_y=g.azimuth_y;
		dx1_nom=g.dx1_nom;
		dx2_nom=g.dx2_nom;
		n1=g.n1;
		n2=g.n2;
		nv=g.nv;
		i0=g.i0;
		j0=g.j0;
		x1low=g.x1low;
		x1high=g.x1high;
		x2low=g.x2low;
		x2high=g.x2high;
		x3low=g.x3low;
		x3high=g.x3high;
		for(i=0;i<3;++i)
		{
			translation_vector[i]=g.translation_vector[i];
			for(j=0;j<3;++j) gtoc_rmatrix[i][j]=g.gtoc_rmatrix[i][j];
		}
		ix1=g.ix1;
		ix2=g.ix2;
		x1=create_2dgrid_contiguous(n1,n2);
		x2=create_2dgrid_contiguous(n1,n2);
		x3=create_2dgrid_contiguous(n1,n2);
		val=create_3dgrid_contiguous(n1,n2,nv);
		//
		//I use separate loops for each array here as this is highly
		//optimized on most compilers 
		//
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) x1[i][j]=g.x1[i][j];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) x2[i][j]=g.x2[i][j];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) x3[i][j]=g.x3[i][j];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) 
				for(k=0;k<nv;++nv) val[i][j][k]=g.val[i][j][k];
	}
	return *this;
}
// Finally operator = for 3d field objects
GCLscalarfield3d& GCLscalarfield3d::operator=(const GCLscalarfield3d& g)
{
	if(this != &g)  // avoid self assignment
	{
		int i,j,k;
		// This has serious problems if the x1, x2, and x3 pointers
		// aren't initialized to NULL
		if(x1!=NULL) free_3dgrid_contiguous(x1,n1,n2);
		if(x2!=NULL) free_3dgrid_contiguous(x2,n1,n2);
		if(x3!=NULL) free_3dgrid_contiguous(x3,n1,n2);
		if(val!=NULL) free_3dgrid_contiguous(val,n1,n2);

		name=g.name;
		lat0=g.lat0;
		lon0=g.lon0;
		r0=g.r0;
		azimuth_y=g.azimuth_y;
		dx1_nom=g.dx1_nom;
		dx2_nom=g.dx2_nom;
		dx3_nom=g.dx3_nom;
		n1=g.n1;
		n2=g.n2;
		n3=g.n3;
		i0=g.i0;
		j0=g.j0;
		k0=g.k0;
		x1low=g.x1low;
		x1high=g.x1high;
		x2low=g.x2low;
		x2high=g.x2high;
		x3low=g.x3low;
		x3high=g.x3high;
		x1=create_3dgrid_contiguous(n1,n2,n3);
		x2=create_3dgrid_contiguous(n1,n2,n3);
		x3=create_3dgrid_contiguous(n1,n2,n3);
		val=create_3dgrid_contiguous(n1,n2,n3);
		for(i=0;i<3;++i)
		{
			translation_vector[i]=g.translation_vector[i];
			for(j=0;j<3;++j) gtoc_rmatrix[i][j]=g.gtoc_rmatrix[i][j];
		}
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) 
				for(k=0;k<n3;++k) x1[i][j][k]=g.x1[i][j][k];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) 
				for(k=0;k<n3;++k) x2[i][j][k]=g.x2[i][j][k];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) 
				for(k=0;k<n3;++k) x3[i][j][k]=g.x3[i][j][k];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) 
				for(k=0;k<n3;++k) val[i][j][k]=g.val[i][j][k];
	}
	return *this;
}
GCLvectorfield3d& GCLvectorfield3d::operator=(const GCLvectorfield3d& g)
{
	if(this != &g)  // avoid self assignment
	{
		int i,j,k,l;
		// This has serious problems if the x1, x2, and x3 pointers
		// aren't initialized to NULL
		if(x1!=NULL) free_3dgrid_contiguous(x1,n1,n2);
		if(x2!=NULL) free_3dgrid_contiguous(x2,n1,n2);
		if(x3!=NULL) free_3dgrid_contiguous(x3,n1,n2);
		if(val!=NULL) free_4dgrid_contiguous(val,n1,n2,n3);

		name=g.name;
		lat0=g.lat0;
		lon0=g.lon0;
		r0=g.r0;
		azimuth_y=g.azimuth_y;
		dx1_nom=g.dx1_nom;
		dx2_nom=g.dx2_nom;
		dx3_nom=g.dx3_nom;
		n1=g.n1;
		n2=g.n2;
		n3=g.n3;
		nv=g.nv;
		i0=g.i0;
		j0=g.j0;
		k0=g.k0;
		x1low=g.x1low;
		x1high=g.x1high;
		x2low=g.x2low;
		x2high=g.x2high;
		x3low=g.x3low;
		x3high=g.x3high;
		x1=create_3dgrid_contiguous(n1,n2,n3);
		x2=create_3dgrid_contiguous(n1,n2,n3);
		x3=create_3dgrid_contiguous(n1,n2,n3);
		val=create_4dgrid_contiguous(n1,n2,n3,nv);
		for(i=0;i<3;++i)
		{
			translation_vector[i]=g.translation_vector[i];
			for(j=0;j<3;++j) gtoc_rmatrix[i][j]=g.gtoc_rmatrix[i][j];
		}
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) 
				for(k=0;k<n3;++k) x1[i][j][k]=g.x1[i][j][k];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) 
				for(k=0;k<n3;++k) x2[i][j][k]=g.x2[i][j][k];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) 
				for(k=0;k<n3;++k) x3[i][j][k]=g.x3[i][j][k];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) 
				for(k=0;k<n3;++k) 
					for(l=0;l<nv;++l)
						val[i][j][k][l]=g.val[i][j][k][l];
	}
	return *this;
}


/*  The == and != operators do NOT test every element of the object.
Equality is defined if the two grids use the same Cartesian coordinate
system.  In this library this is totally defined by the origin of
the coordinate system and the azimuth_y parameter of the grid.
The routine then simply tests these for equality.

Note a previous version tested the transformation matrix and the 
translation vector, but that required far more work and was more
subject to roundoff error mistakes.

delta_cutoff defines the distance in cartesian units between
the origin of the two grids.  Note assumption that these
are stored in units of km.
*/
const double delta_cutoff(0.1);  
// A relatively safe equality test
bool values_differ(double a, double b)
{
	if(fabs(a-b)==0.0) 
		return false;
	else if(fabs((a-b)/b)<(10.0*DBL_EPSILON))
		return false;
	else
		return true;
}
// This is a common routine for == and != that computes distance
double delta_origin(BasicGCLgrid& a, BasicGCLgrid& b)
{
	Cartesian_point pa=b.gtoc(b.lat0,b.lon0,b.r0);
	Cartesian_point pb=b.gtoc(a.lat0,a.lon0,a.r0);
	double dp=(pa.x1-pb.x1)*(pa.x1-pb.x1)
			+ (pa.x2-pb.x2)*(pa.x2-pb.x2)
			+ (pa.x3-pb.x3)*(pa.x3-pb.x3);
	return(sqrt(dp));
}
bool BasicGCLgrid::operator==(const BasicGCLgrid& b)
{
	double dp;
	if(values_differ(azimuth_y,b.azimuth_y) )return(false);
	dp=delta_origin(*this,const_cast<BasicGCLgrid&>(b));
	if(dp<=delta_cutoff) 
		return(true);
	else
		return(false);
}
bool BasicGCLgrid::operator!=(const BasicGCLgrid& b)
{
	double dp;
	dp=delta_origin(*this,const_cast<BasicGCLgrid&>(b));
	if(dp>delta_cutoff) 
		return(true);
	else if(values_differ(azimuth_y,b.azimuth_y))
		return(true);
	else
		return(false);
}
void GCLscalarfield3d::operator+=(GCLscalarfield3d& g)
{
	int i,j,k;
	double valnew;
	int err;
	Cartesian_point cx;
	Geographic_point gp;
	bool remap;

	if(*this==g)
		remap=false;
	else
		remap=true;

	g.reset_index(); 

	for(i=0;i<n1;++i)
	{
		for(j=0;j<n2;++j)
		{
			for(k=0;k<n3;++k)
			{
				if(remap)
				{
					gp = geo_coordinates(i,j,k);
					cx = g.gtoc(gp.lat,gp.lon,gp.r);
				}
				else
				{
					cx.x1=x1[i][j][k];	
					cx.x2=x2[i][j][k];	
					cx.x3=x3[i][j][k];	
				}
				err=g.lookup(cx.x1,cx.x2,cx.x3);
				switch(err)
				{
					
				case -2:
				case 2:
					elog_die(0,(char*)"Coding error:  return code %d from GCLgrid3d::lookup method depricated\n",err);
				case 1:
				case -1:
					g.reset_index();
					break;
				case 0:
					valnew = g.interpolate(cx.x1,cx.x2,cx.x3);
					val[i][j][k]+=valnew;

					break;
				default:
					elog_die(0,(char*)"Illegal return code %d from GCLgrid3d::lookup function\n",err);
				};
			}
		}
	}
}
// Modified April 14, 2005:  now tests for grid consistency and
// bypasses geographic conversion when grids have same coordinate
// system.  This was found important as early versions spent a lot
// of cpu time doing geographic conversions.

void GCLvectorfield3d::operator += (GCLvectorfield3d& g)
{
	int i,j,k,l;
	double *valnew;
	int err;
	bool remap;
	Cartesian_point cx;
	Geographic_point gp;

	g.reset_index(); 

	if(*this==g)
		remap=false;
	else
		remap=true;

	for(i=0;i<n1;++i)
	{
		for(j=0;j<n2;++j)
		{
			for(k=0;k<n3;++k)
			{
				if(remap)
				{
					gp = geo_coordinates(i,j,k);
					cx = g.gtoc(gp.lat,gp.lon,gp.r);
				}
				else
				{
					cx.x1=x1[i][j][k];	
					cx.x2=x2[i][j][k];	
					cx.x3=x3[i][j][k];	
				}

				err=g.lookup(cx.x1,cx.x2,cx.x3);
				switch(err)
				{
				case -2:
				case 2:
					elog_die(0,(char*)"Coding error:  return code %d from GCLgrid3d::lookup method depricated\n",err);
				case 1:
				case -1:
					g.reset_index();
					break;
				case 0:
					valnew = g.interpolate(cx.x1,cx.x2,cx.x3);
					for(l=0;l<nv;++l) val[i][j][k][l]+=valnew[l];
					delete [] valnew;
					break;
				default:
					elog_die(0,(char*)"Illegal return code %d from GCLgrid3d::lookup function\n",err);
				};
			}
		}
	}
}
void GCLscalarfield::operator += (GCLscalarfield& g)
{
	int i,j;
	double valnew;
	int err;

	g.reset_index(); 

	for(i=0;i<n1;++i)
	{
		for(j=0;j<n2;++j)
		{
			Cartesian_point cx;

			err=g.lookup(lat(i,j),lon(i,j));
			switch(err)
			{
			case 1:
			case 2:
				g.reset_index();
				break;
			case -2:
				elog_die(0,(char*)"Coding error:  incomplete GCLgrid object cannot be mapped\n");
			case -1:
				g.reset_index();
				// Try again after a reset index
				// if nonconvergence (this case)
				// If found, fall into interpolation block
				if(g.lookup(lat(i,j),lon(i,j))!=0) break;
			case 0:
				cx = g.gtoc(lat(i,j),lon(i,j),r(i,j));
				valnew = g.interpolate(cx.x1,cx.x2,cx.x3);
				val[i][j]+=valnew;

				break;
			default:
				elog_die(0,(char*)"Illegal return code %d from lookup function\n",err);
			};
		}
	}
}

void GCLvectorfield::operator += (GCLvectorfield& g)
{
	int i,j,l;
	double *valnew;
	int err;

	g.reset_index(); 

	for(i=0;i<n1;++i)
	{
		for(j=0;j<n2;++j)
		{
                        Cartesian_point cx;

			err=g.lookup(lat(i,j),lon(i,j));
			switch(err)
			{
			case 1:
			case 2:
				g.reset_index();
				break;
			case -2:
				elog_die(0,(char*)"Coding error:  incomplete GCLgrid object cannot be mapped\n");
			case -1:
				g.reset_index();
				// Try again after a reset index
				// if nonconvergence (this case)
				// If found, fall into interpolation block
				if(g.lookup(lat(i,j),lon(i,j))!=0) break;
			case 0:
				cx = g.gtoc(lat(i,j),lon(i,j),r(i,j));
				valnew = g.interpolate(cx.x1,cx.x2,cx.x3);
				for(l=0;l<nv;++l) val[i][j][l]+=valnew[l];
				delete [] valnew;

				break;
			default:
				elog_die(0,(char*)"Illegal return code %d from lookup function\n",err);
			};
		}
	}
}
// Multiplication by scalar operators
void GCLscalarfield3d::operator *= (double c1)
{
	int i,j,k;
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j)
			for(k=0;k<n3;++k)
				val[i][j][k]*=c1;
}
void GCLvectorfield3d::operator *= (double c1)
{
	int i,j,k,l;
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j)
			for(k=0;k<n3;++k)
				for(l=0;l<nv;++l)
					val[i][j][k][l]*=c1;
}
void GCLscalarfield::operator *= (double c1)
{
	int i,j,k;
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j)
			val[i][j]*=c1;
}
void GCLvectorfield::operator *= (double c1)
{
	int i,j,k,l;
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j)
			for(l=0;l<nv;++l)
				val[i][j][l]*=c1;
}

ostream& operator << (ostream& fout, GCLscalarfield& g)
{
	int i,j;

	fout << g.n1 << " " << g.n2 << endl;
	for(i=0;i<g.n1;++i)
	{
		for(j=0;j<g.n2;++j)
		{
			fout << g.x1[i][j]
				<< " "
				<< g.x2[i][j]
				<< " "
				<< g.x3[i][j]
				<< " "
				<< deg(g.lat(i,j))
				<< " "
				<< deg(g.lon(i,j))
				<< " "
				<< g.r(i,j)
				<< " "
				<< g.depth(i,j)
				<< " "
				<< g.val[i][j]
				<< endl;
		}
	}
	return fout;
}
ostream& operator << (ostream& fout, GCLscalarfield3d& g)
{
	int i,j,k;

	fout << g.n1 << " " << g.n2 << " " << g.n3 << endl;
	for(i=0;i<g.n1;++i)
	{
		for(j=0;j<g.n2;++j)
		{
			for(k=0;k<g.n3;++k)
			{
				fout << g.x1[i][j][k]
				<< " "
					<< g.x2[i][j][k]
				<< " "
					<< g.x3[i][j][k]
				<< " "
					<< deg(g.lat(i,j,k))
				<< " "
					<< deg(g.lon(i,j,k))
				<< " "
					<< g.r(i,j,k)
				<< " "
					<< g.depth(i,j,k)
				<< " "
					<< g.val[i][j][k]
					<< endl;
			}
		}
	}
	return fout;
}
ostream& operator << (ostream& fout, GCLvectorfield& g)
{
	int i,j,k;

	fout << g.n1 << " " << g.n2 << " " << g.nv << endl;
	for(i=0;i<g.n1;++i)
	{
		for(j=0;j<g.n2;++j)
		{
			fout << g.x1[i][j]
				<< " "
				<< g.x2[i][j]
				<< " "
				<< g.x3[i][j]
				<< " "
				<< deg(g.lat(i,j))
				<< " "
				<< deg(g.lon(i,j))
				<< " "
				<< g.r(i,j) 
				<< " "
				<< g.depth(i,j); 
			for(k=0;k<g.nv;++k)
				fout<< " " << g.val[i][j][k];
			fout << endl;
		}
	}
	return fout;
}
ostream& operator << (ostream& fout, GCLvectorfield3d& g)
{
	int i,j,k,l;

	fout << g.n1 << " " << g.n2 << " " << g.n3 << " " << g.nv << endl;
	for(i=0;i<g.n1;++i)
	{
		for(j=0;j<g.n2;++j)
		{
			for(k=0;k<g.n3;++k)
			{
				fout << g.x1[i][j][k]
					<< " "
					<< g.x2[i][j][k]
					<< " "
					<< g.x3[i][j][k]
					<< " "
					<< deg(g.lat(i,j,k))
					<< " "
					<< deg(g.lon(i,j,k))
					<< " "
					<< g.r(i,j,k)
					<< " "
					<< g.depth(i,j,k);
				for(l=0;l<g.nv;++l) fout << " " << g.val[i][j][k][l];
				fout << endl;
			}
		}
	}
	return fout;
}
