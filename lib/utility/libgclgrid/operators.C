#include <stdio.h>
#include "elog.h"
#include "gclgrid.h"
void GCLscalarfield3d::operator+=(const GCLscalarfield3d& g)
{
	int i,j,k;
	double valnew;
	int err;

	reset_index(); 

	for(i=0;i<n1;++i)
	{
		for(j=0;j<n2;++j)
		{
			for(k=0;k<n3;++j)
			{
				err=lookup(g.x1[i][j][k],g.x2[i][j][k],g.x3[i][j][k]);
				switch(err)
				{
				case -1:
				case 2:
					reset_index();
					break;
				case -2:
					elog_die(0,(char*)"Coding error:  incomplete GCLgrid object cannot be mapped\n");
				case 0:
					valnew=interpolate(g.x1[i][j][k],g.x2[i][j][k],g.x3[i][j][k]);
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

	for(i=0;i<n1;++i)
	{
		for(j=0;j<n2;++j)
		{
			for(k=0;k<n3;++j)
			{
				err=lookup(g.x1[i][j][k],g.x2[i][j][k],g.x3[i][j][k]);
				switch(err)
				{
				case -1:
				case 2:
					reset_index();
					break;
				case -2:
					elog_die(0,(char*)"Coding error:  incomplete GCLgrid object cannot be mapped\n");
				case 0:

					valnew=interpolate(g.x1[i][j][k],g.x2[i][j][k],g.x3[i][j][k]);
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

	for(i=0;i<n1;++i)
	{
		for(j=0;j<n2;++j)
		{
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
				valnew=interpolate(g.x1[i][j],g.x2[i][j],g.x3[i][j]);
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

	for(i=0;i<n1;++i)
	{
		for(j=0;j<n2;++j)
		{
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
				valnew=interpolate(g.x1[i][j],g.x2[i][j],g.x3[i][j]);
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
