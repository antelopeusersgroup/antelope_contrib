#include <stdio.h>
#include <limits>
#include "elog.h"
#include "gclgrid.h"
/* modified Nov. 2003.  Original assumed cartesian frames for g and the current object were
identical.  We have no reason to believe this should always be so.  Now all these operators
are paranoid and use the geographhic points and convert them to the cartesian components.

Modified Nov 2004:  added == and != operators 
*/
void GCLscalarfield3d::operator+=(GCLscalarfield3d& g)
{
	int i,j,k;
	double valnew;
	int err;

	reset_index(); 

	for(i=0;i<g.n1;++i)
	{
		for(j=0;j<g.n2;++j)
		{
			for(k=0;k<g.n3;++j)
			{
				Cartesian_point cx;
				Geographic_point gp;
				gp = g.geo_coordinates(i,j,k);
				cx = gtoc(gp.lat,gp.lon,gp.r);
				err=lookup(cx.x1,cx.x2,cx.x3);
				switch(err)
				{
				case -1:
				case 2:
					reset_index();
					break;
				case -2:
					elog_die(0,(char*)"Coding error:  incomplete GCLgrid object cannot be mapped\n");
				case 0:
					valnew = interpolate(cx.x1,cx.x2,cx.x3);
					val[i][j][k]+=valnew;

					break;
				default:
					elog_die(0,(char*)"Illegal return code %d from lookup function\n",err);
				};
			}
		}
	}
}

void GCLvectorfield3d::operator += (GCLvectorfield3d& g)
{
	int i,j,k,l;
	double *valnew;
	int err;

	reset_index(); 

	for(i=0;i<g.n1;++i)
	{
		for(j=0;j<g.n2;++j)
		{
			for(k=0;k<g.n3;++j)
			{
				Cartesian_point cx;
				Geographic_point gp;
				gp = g.geo_coordinates(i,j,k);
				cx = gtoc(gp.lat,gp.lon,gp.r);

				err=lookup(cx.x1,cx.x2,cx.x3);
				switch(err)
				{
				case -1:
				case 2:
					reset_index();
					break;
				case -2:
					elog_die(0,(char*)"Coding error:  incomplete GCLgrid object cannot be mapped\n");
				case 0:

					valnew = interpolate(cx.x1,cx.x2,cx.x3);
					for(l=0;l<nv;++nv) val[i][j][k][l]=valnew[l];
					delete valnew;
					break;
				default:
					elog_die(0,(char*)"Illegal return code %d from lookup function\n",err);
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

	reset_index(); 

	for(i=0;i<g.n1;++i)
	{
		for(j=0;j<g.n2;++j)
		{
			Cartesian_point cx;

			err=lookup(g.lat(i,j),g.lon(i,j));
			switch(err)
			{
			case -1:
			case 2:
				reset_index();
				break;
			case -2:
				elog_die(0,(char*)"Coding error:  incomplete GCLgrid object cannot be mapped\n");
			case 0:
				cx = gtoc(g.lat(i,j),g.lon(i,j),g.r(i,j));
				valnew = interpolate(cx.x1,cx.x2,cx.x3);
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

	reset_index(); 

	for(i=0;i<g.n1;++i)
	{
		for(j=0;j<g.n2;++j)
		{
                        Cartesian_point cx;

			err=lookup(g.lat(i,j),g.lon(i,j));
			switch(err)
			{
			case -1:
			case 2:
				reset_index();
				break;
			case -2:
				elog_die(0,(char*)"Coding error:  incomplete GCLgrid object cannot be mapped\n");
			case 0:
				cx = gtoc(g.lat(i,j),g.lon(i,j),g.r(i,j));
				valnew = interpolate(cx.x1,cx.x2,cx.x3);
				for(l=0;l<nv;++nv) val[i][j][l]=valnew[l];
				delete valnew;

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
/* The == and != operators do NOT test every element of the object. Equality is defined
as being defined on the same Cartesian system.  This is done by testing for equality of
the transformation matrix and translation vectors.  These define the Cartesian reference
frame completely.  We could test lat0, lon0, and r0 but that would be redundant since
they are defined precisely by the translation vector.  Note since these quantities
are defined in the base class, derived classes can use a dynamic_cast to use these
operators so I don't bother to define them for fields or even the GCLgrid3d object. */
bool GCLgrid::operator==(const GCLgrid& b)
{
	numeric_limits<double> test;
	int i,j;
	for(i=0;i<3;++i)
	{
		if(fabs(translation_vector[i]-b.translation_vector[i]) > test.epsilon())
			return(false);
		for(j=0;j<3;++j)
		{
			if(fabs(gtoc_rmatrix[i][j]-b.gtoc_rmatrix[i][j]) >  test.epsilon())
				return(false);
		}
	}
	return(true);
}
bool GCLgrid::operator!=(const GCLgrid& b)
{
	numeric_limits<double> test;
	int i,j;
	for(i=0;i<3;++i)
	{
		if(fabs(translation_vector[i]-b.translation_vector[i]) > test.epsilon())
			return(true);
		for(j=0;j<3;++j)
		{
			if(fabs(gtoc_rmatrix[i][j]-b.gtoc_rmatrix[i][j]) >  test.epsilon())
				return(true);
		}
	}
	return(false);
}
