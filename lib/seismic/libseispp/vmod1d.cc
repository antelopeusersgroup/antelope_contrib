#include <iostream>
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

		db=dblookup(db,0,"mod1d",0,0);
		if(db.table==dbINVALID) 
			throw(Velocity_Model_1d_dberror(name,
				"dbopen failure for mod1d table"));
		// this builds a subset condition like this:
		// (modname=~/name/ && (paramname=~/Pvelocity/)
		string sstr = s1+name+s2+property+s3;
		dbs1 = dbsubset(db,(char *)sstr.c_str(),0);
		sortkeys=strtbl("depth",0);
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
