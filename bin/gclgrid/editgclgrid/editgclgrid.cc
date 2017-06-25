#include <string.h>
#include <iostream>
#include "stock.h"
#include "pf.h"
#include "seispp.h"
#include "dbpp.h"
#include "gclgrid.h"
using namespace std;
using namespace SEISPP;
void usage(string prog)
{
  cerr << "usage:   editgclgrid db [-pf pfname]" <<endl
      << "decimate and/or carve out smaller block of a gclgrid object stored in db"
      <<endl;
  exit(-1);
}
GCLgrid *makenewgrid(GCLgrid& parent,
	int x1skip, int x2skip, 
	int x1dec, int x2dec, 
	int nx1out, int nx2out)
{
	int i,j,k,ii,jj,kk;
	// Use the standard grid constructor to match
	// required size and clone origin.  We'll then
	// not depend on this to copy coordinates.
	char decstring[20];
	sprintf(decstring,"%d_%d",x1dec,x2dec);
	string newname=parent.name+string("dec=")+string(decstring);
	int x1snew,x2snew;
	x1snew=(parent.i0-x1skip)/x1dec;
	x2snew=(parent.j0-x2skip)/x2dec;
	// An oddity of the r origin definition requires this
	double r0new;
	r0new=parent.r0+parent.depth(parent.i0,parent.j0);
	GCLgrid *newgrid=new GCLgrid(nx1out,nx2out,
			newname,parent.lat0,parent.lon0,
			r0new,0.0,
			parent.dx1_nom*(static_cast<double>(x1dec)),
			parent.dx2_nom*(static_cast<double>(x2dec)),
			x1snew,x2snew);
	// We use geographic coordinates for the conversion to 
	// avoid clashes with the coordinate system.  This is just
	// a safer algorithm although it will be slower than if we
	// just decimated the grid without the intermediary of going
	// to geographic coordinates
	// This blindly assumes input has been checked with the VerifyInput
	// function immediately below.
	Geographic_point p;
	Cartesian_point cp;
	for(i=x1skip,ii=0;(i<parent.n1)&&(ii<nx1out);i+=x1dec,++ii)
		for(j=x2skip,jj=0;(j<parent.n2)&&(jj<nx2out);j+=x2dec,++jj)
			{
				p=parent.geo_coordinates(i,j);
				cp=newgrid->gtoc(p);
				newgrid->x1[ii][jj]=cp.x1;
				newgrid->x2[ii][jj]=cp.x2;
				newgrid->x3[ii][jj]=cp.x3;
			}
	return(newgrid);
}
GCLgrid3d *makenewgrid3d(GCLgrid3d& parent,
	int x1skip, int x2skip, int x3skip,
	int x1dec, int x2dec, int x3dec,
	int nx1out, int nx2out, int nx3out)
{
	int i,j,k,ii,jj,kk;
	// Use the standard grid constructor to match
	// required size and clone origin.  We'll then
	// not depend on this to copy coordinates.
	char decstring[20];
	sprintf(decstring,"%d_%d_%d",x1dec,x2dec,x3dec);
	string newname=parent.name+string("dec=")+string(decstring);
	int x1snew,x2snew;
	x1snew=(parent.i0-x1skip)/x1dec;
	x2snew=(parent.j0-x2skip)/x2dec;
	// An oddity of the r origin definition requires this
	double r0new;
	r0new=parent.r0+parent.depth(parent.i0,parent.j0,0);
	GCLgrid3d *newgrid=new GCLgrid3d(nx1out,nx2out,nx3out,
			newname,parent.lat0,parent.lon0,
			r0new,0.0,
			parent.dx1_nom*(static_cast<double>(x1dec)),
			parent.dx2_nom*(static_cast<double>(x2dec)),
			parent.dx3_nom*(static_cast<double>(x3dec)),
			x1snew,x2snew);
	// We use geographic coordinates for the conversion to 
	// avoid clashes with the coordinate system.  This is just
	// a safer algorithm although it will be slower than if we
	// just decimated the grid without the intermediary of going
	// to geographic coordinates
	// This blindly assumes input has been checked with the VerifyInput
	// function immediately below.
	Geographic_point p;
	Cartesian_point cp;
	for(i=x1skip,ii=0;(i<parent.n1)&&(ii<nx1out);i+=x1dec,++ii)
		for(j=x2skip,jj=0;(j<parent.n2)&&(jj<nx2out);j+=x2dec,++jj)
			for(k=x3skip,kk=0;(k<parent.n3)&&(kk<nx3out);k+=x3dec,++kk)
			{
				p=parent.geo_coordinates(i,j,k);
				cp=newgrid->gtoc(p);
				newgrid->x1[ii][jj][kk]=cp.x1;
				newgrid->x2[ii][jj][kk]=cp.x2;
				newgrid->x3[ii][jj][kk]=cp.x3;
			}
	return(newgrid);
}

//
// If there is a problem with input this function prints and
// error message to stderr and returns true.  Otherwise it
// silently returns false
//
bool InputNotValid(string comp,int skip, int dec, int nin, int nout,
	int skip0)
{
	int ilast=skip+(nout-1)*dec;
	if( (ilast>=nin) || ( (skip0-skip)%dec) )
	{
		cerr << "Invalid input for component "<<comp<<endl;
		if(ilast>=nin)
		{
			cerr << "Grid overrun:  computed final index="
				<< ilast<<endl
				<< "Grid limit for "<<comp<<"="
				<<nin<<endl;
		}
		if( (skip0-skip)%dec )
		{
			cerr << "Invalid skip factor:  ="<<skip<<endl
				<< "Parent grid origin offset="<<skip0
				<<endl
				<< "Requested skip="<<skip
				<<endl
				<< "Input decimation factor="<<dec
				<<endl
				<< "Difference skip-parent_skip must be "
				<< "divisible by decimation factor" <<endl;
		}
		return true;
	}
	else
		return false;
}
	

bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
	const string prog("editgclgrid");
	int i;
	if(argc<2) usage(prog);
	string dbname(argv[1]);
	string pfname(prog);
	for(i=2;i<argc;++i)
	{
		string argtest(argv[i]);
		if(argv[i]==string("-pf") )
		{
			++i;
			pfname=string(argv[i]);
		}
		else
			usage(prog);
	}
	// parse the parameter file
	Pf *pf;
	if(pfread(const_cast<char *>(pfname.c_str()),&pf))
	{
		cerr << "pfread error"<<endl;
		exit(-1);
	}
	int x1skip=pfget_int(pf,"x1skip");
	int x2skip=pfget_int(pf,"x2skip");
	// This is computed now
	//int x3skip=pfget_int(pf,"x3skip");
	int x3skip;
	int x1dec=pfget_int(pf,"x1dec");
	int x2dec=pfget_int(pf,"x2dec");
	int x3dec=pfget_int(pf,"x3dec");
	int nx1out=pfget_int(pf,"nx1out");
	int nx2out=pfget_int(pf,"nx2out");
	int nx3out=pfget_int(pf,"nx3out");

	string fieldtype(pfget_string(pf,"fieldtype"));
	string gridname(pfget_string(pf,"gridname"));
	string fieldname(pfget_string(pf,"fieldname"));
	string griddir(pfget_string(pf,"grid_directory"));
	string fielddir(pfget_string(pf,"field_directory"));
	string dfile(pfget_string(pf,"output_field_file_name"));
	GCLgrid *newgrid2d;
	GCLgrid3d *newgrid3d;
	int ii,j,jj,k,kk,l;
	try {
                DatascopeHandle dbh(dbname,false);
		if(fieldtype=="GCLscalarfield")
		{
			GCLscalarfield grid(dbh,gridname,fieldname);
			if( InputNotValid(string("x1"),x1skip,x1dec,
					grid.n1,nx1out,grid.i0)
				|| InputNotValid(string("x2"),x2skip,x2dec,
					grid.n2,nx2out,grid.j0) )exit(-1);
					
			newgrid2d=makenewgrid(dynamic_cast<GCLgrid&>(grid),
				x1skip,x2skip,
				x1dec,x2dec,
				nx1out,nx2out);
			GCLscalarfield newfield(*newgrid2d);
			for(i=x1skip,ii=0;(i<grid.n1)&&(ii<nx1out); i+=x1dec,++ii)
				for(j=x2skip,jj=0;(j<grid.n2)&&(jj<nx2out);
                                        j+=x2dec,++jj)
						newfield.val[ii][jj]=grid.val[i][j];
			newfield.compute_extents();
			newfield.save(dbh,griddir,fielddir,fieldname,dfile);
		}
		else if(fieldtype=="GCLvectorfield")
		{
			GCLvectorfield grid(dbh,gridname,fieldname);
			if( InputNotValid(string("x1"),x1skip,x1dec,
					grid.n1,nx1out,grid.i0)
				|| InputNotValid(string("x2"),x2skip,x2dec,
					grid.n2,nx2out,grid.j0) )exit(-1);
			newgrid2d=makenewgrid(dynamic_cast<GCLgrid&>(grid),
				x1skip,x2skip,
				x1dec,x2dec,
				nx1out,nx2out);
			GCLvectorfield newfield(*newgrid2d,grid.nv);
			for(i=x1skip,ii=0;(i<grid.n1)&&(ii<nx1out);i+=x1dec,++ii)
				for(j=x2skip,jj=0;(j<grid.n2)&&(jj<nx2out);j+=x2dec,++jj)
					for(l=0;l<grid.nv;++l)
					{
						newfield.val[ii][jj][l]=grid.val[i][j][l];
					}
			newfield.compute_extents();
			newfield.save(dbh,griddir,fielddir,fieldname,dfile);
		}
		else if(fieldtype=="GCLscalarfield3d")	
		{
			GCLscalarfield3d grid(dbh,gridname,fieldname);
			// x3skip needs to be computed
			// Assume negative skips are caught by 
			// InputNotValid function below

			x3skip=grid.n3-(nx3out*x3dec);
			
			if( InputNotValid(string("x1"),x1skip,x1dec,
					grid.n1,nx1out,grid.i0)
				|| InputNotValid(string("x2"),x2skip,x2dec,
					grid.n2,nx2out,grid.j0) 
				|| InputNotValid(string("x3"),x3skip,x3dec,
					grid.n3,nx3out,grid.k0) )exit(-1);
			newgrid3d=makenewgrid3d(dynamic_cast<GCLgrid3d&>(grid),
				x1skip,x2skip,x3skip,
				x1dec,x2dec,x3dec,
				nx1out,nx2out,nx3out);
			GCLscalarfield3d newfield(*newgrid3d);
			delete newgrid3d;
			for(i=x1skip,ii=0;(i<grid.n1)&&(ii<nx1out);i+=x1dec,++ii)
				for(j=x2skip,jj=0;(j<grid.n2)&&(jj<nx2out);j+=x2dec,++jj)
					for(k=x3skip,kk=0;(k<grid.n3)&&(kk<nx3out);
                                                                k+=x3dec,++kk)
						newfield.val[ii][jj][kk]=grid.val[i][j][k];
			newfield.compute_extents();
			newfield.save(dbh,griddir,fielddir,fieldname,dfile);
		}
		else if(fieldtype=="GCLvectorfield3d")	
		{
			GCLvectorfield3d grid(dbh,gridname,fieldname);
			//
			// x3skip needs to be computed
			// Assume negative skips are caught by 
			// InputNotValid function below
			//
			x3skip=grid.n3-(nx3out*x3dec);
			
			if( InputNotValid(string("x1"),x1skip,x1dec,
					grid.n1,nx1out,grid.i0)
				|| InputNotValid(string("x2"),x2skip,x2dec,
					grid.n2,nx2out,grid.j0) 
				|| InputNotValid(string("x3"),x3skip,x3dec,
					grid.n3,nx3out,grid.k0) )exit(-1);
			newgrid3d=makenewgrid3d(dynamic_cast<GCLgrid3d&>(grid),
				x1skip,x2skip,x3skip,
				x1dec,x2dec,x3dec,
				nx1out,nx2out,nx3out);
			GCLvectorfield3d newfield(*newgrid3d,grid.nv);
			delete newgrid3d;
			for(i=x1skip,ii=0;(i<grid.n1)&&(ii<nx1out);i+=x1dec,++ii)
				for(j=x2skip,jj=0;(j<grid.n2)&&(jj<nx2out);j+=x2dec,++jj)
					for(k=x3skip,kk=0;(k<grid.n3)&&(kk<nx3out);
                                                            k+=x3dec,++kk)
						for(l=0;l<grid.nv;++l)
						{
							newfield.val[ii][jj][kk][l]=grid.val[i][j][k][l];
						}
			newfield.compute_extents();
			newfield.save(dbh,griddir,fielddir,fieldname,dfile);
		}
		else
		{
			cerr << "Illegal fieldtype="<<fieldtype<<endl
				<< "Exiting with no output"<<endl;
			exit(-1);
		}
	}
	catch (int ierr)
	{
		elog_die(0,"GCLgrid error number %d",ierr);
		exit(-1);
	}
}
			
