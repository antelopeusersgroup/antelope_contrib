#include <stdio.h>
#include "elog.h"
#include "gclgrid.h"
/* modified Nov. 2003.  Original assumed cartesian frames for g and the current object were
identical.  We have no reason to believe this should always be so.  Now all these operators
are paranoid and use the geographhic points and convert them to the cartesian components.
*/
void GCLscalarfield3d::operator+=(const GCLscalarfield3d& g)
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

				cx = gtoc(g.lat[i][j][k],g.lon[i][j][k], g.r[i][j][k]);
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

void GCLvectorfield3d::operator += (const GCLvectorfield3d& g)
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

				cx = gtoc(g.lat[i][j][k],g.lon[i][j][k], g.r[i][j][k]);
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
void GCLscalarfield::operator += (const GCLscalarfield& g)
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

			err=lookup(g.lat[i][j],g.lon[i][j]);
			switch(err)
			{
			case -1:
			case 2:
				reset_index();
				break;
			case -2:
				elog_die(0,(char*)"Coding error:  incomplete GCLgrid object cannot be mapped\n");
			case 0:
				cx = gtoc(g.lat[i][j],g.lon[i][j],g.r[i][j]);
				valnew = interpolate(cx.x1,cx.x2,cx.x3);
				val[i][j]+=valnew;

				break;
			default:
				elog_die(0,(char*)"Illegal return code %d from lookup function\n",err);
			};
		}
	}
}

void GCLvectorfield::operator += (const GCLvectorfield& g)
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

			err=lookup(g.lat[i][j],g.lon[i][j]);
			switch(err)
			{
			case -1:
			case 2:
				reset_index();
				break;
			case -2:
				elog_die(0,(char*)"Coding error:  incomplete GCLgrid object cannot be mapped\n");
			case 0:
				cx = gtoc(g.lat[i][j],g.lon[i][j],g.r[i][j]);
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
