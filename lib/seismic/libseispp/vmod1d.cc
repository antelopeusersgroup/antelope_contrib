#include <iostream>
#include <fstream>
#include <list>
#include <strings.h>
#include "db.h"
#include "seispp.h"

double Velocity_Model_1d::getv(double zin)
{
	double dz;
	for(int i=1;i<nlayers;++i)
	{
		if(zin<z[i]) 
		{
			dz=zin-z[i-1];
			return(v[i-1]+dz*grad[i-1]);
		}
	}
	dz=zin-v[nlayers-1];
	return(v[nlayers-1]+dz*grad[nlayers-1]);
}
// database constructor. 
// property must be P or S.  Errors are thrown for a range of
// likely problems.
Velocity_Model_1d::Velocity_Model_1d(Dbptr db,string name,string property)
    throw(Velocity_Model_1d_dberror)
{
	Dbptr dbs1,dbs2;
	Tbl *sortkeys;
	string s1="(modname =~ /";
	string s2="/) && (paramname=~/";
	string s3="velocity/)";
	int i;

	if(property=="S" || property=="P")
	{

		db=dblookup(db,0,(char *)"mod1d",0,0);
		if(db.table==dbINVALID) 
			throw(Velocity_Model_1d_dberror(name,
				"dbopen failure for mod1d table"));
		// this builds a subset condition like this:
		// (modname=~/name/ && (paramname=~/Pvelocity/)
		string sstr = s1+name+s2+property+s3;
		dbs1 = dbsubset(db,(char *)sstr.c_str(),0);
		sortkeys=strtbl((char *)"depth",0);
		dbs2=dbsort(dbs1,sortkeys,0,0);
		dbquery(dbs2,dbRECORD_COUNT,&nlayers);
		if(nlayers<=0)
		{
			if(dbs1.record!=dbINVALID) dbfree(dbs1);
			if(dbs2.record!=dbINVALID) dbfree(dbs2);
			freetbl(sortkeys,0);
			throw(Velocity_Model_1d_dberror(name,
			  "Error forming working view for P velocity"));
		}
		z=new double[nlayers];
		v=new double[nlayers];
                grad=new double[nlayers];
		for(i=0,dbs2.record=0;dbs2.record<nlayers;++dbs2.record,++i)
		{
			if(dbgetv(dbs2,0,"paramval",&(v[i]),
					"depth",&(z[i]),
					"grad",&(grad[i]),0) == dbINVALID)
			{
				dbfree(dbs1);
				dbfree(dbs2);
				freetbl(sortkeys,0);
				delete [] v;
				delete [] z;
				throw(Velocity_Model_1d_dberror(name,
				  "dbgetv error reading P velocities"));
			}
		}
		dbfree(dbs1);
		dbfree(dbs2);
		freetbl(sortkeys,0);
	}
	else
	{
		throw(Velocity_Model_1d_dberror(name,
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
Velocity_Model_1d::Velocity_Model_1d(string fname,
	string form, string property) throw(Velocity_Model_1d_ioerror)
{
	int i;

	ifstream input;

	try{
		input.open(fname.c_str(), ios::in);
	} catch (ios_base::failure& var)
	{
		string mess;
		mess = var.what();
		throw(Velocity_Model_1d_ioerror("Cannot open file "+fname,
                        "Velocity_Model_1d constructor failed"));
	}

	if(form=="rbh" || form=="plain")
	{
		char line[255];
		list <double> zin,vin;
		list <double>::iterator znow,vnow;

		// throw away the first few lines for rbh format
		if(form=="rbh")
		{
			for(i=0;i<12;++i) input.getline(line,255);
		}
		while(input.good())
		{
			double f1,f2,f3;
			double skipper;
			input >> f1;
			input >> f2;
			input >> f3;
			if(form=="rbh")
				for(i=0;i<7;++i) input >> skipper;
			if(property=="P")
			{
				zin.push_back(f1);
				vin.push_back(f2);
			}
			else if(property == "S")
			{
				zin.push_back(f1);
				vin.push_back(f3);
			}
			else
			{
				input.close();
				throw(Velocity_Model_1d_ioerror("Illegal property parameter = "+property,
					"Velocity_Model_1d constructor failed"));
			}
		}
		input.close();
		nlayers = zin.size();
		z=new double[nlayers];
		v=new double[nlayers];
		// this function only handles constant velocity layers
                grad=new double[nlayers];
		for(i=0;i<nlayers;++i)grad[i]=0.0;
		for(i=0,znow=zin.begin(),vnow=vin.begin();
			znow!=zin.end(),vnow!=vin.end();++znow,++vnow,++i)
		{
			z[i]=*znow;
			v[i]=*vnow;
		}
		if(i!=nlayers)
		{
			nlayers = i-1;
			input.close();
			throw(Velocity_Model_1d_ioerror("Format error:  need at least z,vp,vs lines in the input model description lines",
				"Velocity_Model_1d object only partially constructed"));
		}
		// replace z values (currently intervals) with accumulated
		// depth to top of each layer
		double zbot,ztmp;
		zbot = z[0];
		z[0]=0.0;
		for(i=1;i<nlayers;++i)
		{
			ztmp = z[i];
			z[i]=zbot;
			zbot += ztmp;
		}
	}
	else
	{
		input.close();
		throw(Velocity_Model_1d_ioerror("Unrecognized format namea = "+form,
				"Velocity_Model_1d constructor failed"));
	}
	input.close();
}
