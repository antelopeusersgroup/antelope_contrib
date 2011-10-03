#include <iostream>
#include <fstream>
#include <list>
#include <string>
#include "db.h"
#include "VelocityModel_1d.h"
using namespace std;
namespace SEISPP
{

using namespace SEISPP;

double VelocityModel_1d::getv(double zin)
{
	double dz;
        /* if above top, return top model value */
        if(zin<z[0]) return(v[0]);
	/* Assume at layer sigularity when within dz*this factor */
	const double dzeqfactor(0.01);
	for(int i=1;i<nlayers;++i)
	{
		if(zin<z[i]) 
		{
                        dz=zin-z[i-1];
			if(fabs((zin-z[i])/dz)<dzeqfactor)
				return(v[i]);
			else
				return(v[i-1]+dz*grad[i-1]);
		}
	}
	dz=zin-v[nlayers-1];
	return(v[nlayers-1]+dz*grad[nlayers-1]);
}
// database constructor. 
// property must be P or S.  Errors are thrown for a range of
// likely problems.
VelocityModel_1d::VelocityModel_1d(Dbptr dbi,string name,string property)
    throw(VelocityModel_1d_Dberror)
{
	Dbptr dbs1,dbs2;
	Tbl *sortkeys;
	string s1="(modname =~ /";
	string s2="/) && (paramname=~/";
	string s3="velocity/)";
	int i;

	if(property=="S" || property=="P")
	{

		Dbptr db=dblookup(dbi,0,(char *)"mod1d",0,0);
		if(db.table==dbINVALID) 
			throw(VelocityModel_1d_Dberror(name,
				"dbopen failure for mod1d table"));
		// this builds a subset condition like this:
		// (modname=~/name/ && (paramname=~/Pvelocity/)
		string sstr = s1+name+s2+property+s3;
		dbs1 = dbsubset(db,(char *)sstr.c_str(),0);
		sortkeys=strtbl((char *)"depth",NULL);
		dbs2=dbsort(dbs1,sortkeys,0,0);
		dbquery(dbs2,dbRECORD_COUNT,&nlayers);
		if(nlayers<=0)
		{
			if(dbs1.record!=dbINVALID) dbfree(dbs1);
			if(dbs2.record!=dbINVALID) dbfree(dbs2);
			freetbl(sortkeys,0);
			throw(VelocityModel_1d_Dberror(name,
			  "Error forming working view for P velocity"));
		}
		z.reserve(nlayers);
		v.reserve(nlayers);
		grad.reserve(nlayers);
		for(i=0,dbs2.record=0;dbs2.record<nlayers;++dbs2.record,++i)
		{
			double vin, zin, gradin;
			if(dbgetv(dbs2,0,"paramval",&vin, 
				"depth",&zin,
				"grad",&gradin,NULL) == dbINVALID)
			{
				dbfree(dbs1);
				dbfree(dbs2);
				freetbl(sortkeys,0);
				throw(VelocityModel_1d_Dberror(name,
				  "dbgetv error reading P velocities"));
			}
			z.push_back(zin);
			v.push_back(vin);
			grad.push_back(gradin);
		}
		dbfree(dbs1);
		dbfree(dbs2);
		freetbl(sortkeys,0);
	}
	else
	{
		throw(VelocityModel_1d_Dberror(name,
		 "Coding error:  property passed to database constructor must be either P or S"));
	}
}
/* Read from a file constructor.  fname is the file name to be 
read and form is a format name.  Currently supports two 
names:  rbh - Herrmmann synthetics package format; and 
plain - simple ascii depth, velocity pairs.  
For plain the order is thickness,P,S.  i.e. three columns of 
ascii numbers, free form with blank separators, layer thicknes
is in column 1, P velocity is in column 2, and S velocity
is in column 3.  (no gradients in this format allowed). 
property must be "P" or "S".  IMPORTANT:  note the use of
layer THICKNESS not DEPTH. */
VelocityModel_1d::VelocityModel_1d(string fname,
	string form, string property) throw(VelocityModel_1d_IOerror)
{
	int i;

	ifstream input;

	input.open(fname.c_str(), ios::in);
	if(input.fail())
		throw(VelocityModel_1d_IOerror("Cannot open file "+fname,
			"VelocityModel_1d constructor failed"));	
	if(form=="rbh" || form=="plain")
	{
		char line[255];
		double znow;

		// throw away the first few lines for rbh format
		if(form=="rbh")
		{
			for(i=0;i<12;++i) input.getline(line,255);
		}
		while(!input.eof())
		{
			double f1,f2,f3;
			double skipper;
			input >> f1;
			input >> f2;
			input >> f3;
			if(form=="rbh")
				for(i=0;i<7;++i) input >> skipper;
			if(input.eof()) break;
			if(property=="P")
			{
				z.push_back(f1);
				v.push_back(f2);
			}
			else if(property == "S")
			{
				z.push_back(f1);
				v.push_back(f3);
			}
			else
			{
				input.close();
				throw(VelocityModel_1d_IOerror("Illegal property parameter = "+property,
					"VelocityModel_1d constructor failed"));
			}
			// all values are 0 for gradient here
			grad.push_back(0.0);
		}
		input.close();
		nlayers = z.size();
		// replace z values (currently intervals) with accumulated
		// depth to top of each layer
		for(i=1,z[0]=0.0;i<nlayers;++i)  z[i]+=z[i-1];
	}
	else
	{
		input.close();
		throw(VelocityModel_1d_IOerror("Unrecognized format name = "+form,
				"VelocityModel_1d constructor failed"));
	}
}
/* Standard copy constructor */
VelocityModel_1d::VelocityModel_1d(const VelocityModel_1d& old)
{
	nlayers=old.nlayers;
	grad=old.grad;
	v=old.v;
	z=old.z;
}
/* Standard assignment operator */
VelocityModel_1d& VelocityModel_1d::operator=(const VelocityModel_1d& old)
{
	if(this!=&old)
	{
		nlayers=old.nlayers;
		grad=old.grad;
		v=old.v;
		z=old.z;
	}
	return(*this);
}
/* Save a model to a database */
void dbsave(VelocityModel_1d& mod, Dbptr db, string name,string property)
	throw (VelocityModel_1d_Dberror)
{
	db=dblookup(db,0,const_cast<char *>("mod1d"),0,0);
	if(db.table==dbINVALID) 
		throw(VelocityModel_1d_Dberror(name,
				"dbopen failure for mod1d table"));
	/* Don't trust the nlayers variable.  Use the bombproof 
	size method of the vector container.  Assume v, z, and grad
	are all the same size.*/
	int i,ierr;
	string units("km/s");  //frozen units of km per second
	/* Antelope routine to fetch username */
        char username[20];
        my_username(username);

	for(i=0;i<mod.z.size();++i)
	{
		ierr=dbaddv(db,0,"modname",name.c_str(),
			"paramname",property.c_str(),
			"depth",mod.z[i],
			"paramval",mod.v[i],
			"grad",mod.grad[i],
			"units",units.c_str(),
			"auth",username,NULL);
		if(ierr<0) throw (VelocityModel_1d_Dberror(name,
				"dbaddv error while saving model"));
	}

}
} // Termination of namespace SEISPP definitions
