#include <stdio.h>
#include <string>
#include "stock.h"
#include "seispp.h"
namespace SEISPP
{

/* This is an overloaded version of a file of the same name below.  It provides an
alternative interface using dir/dfile as opposed to the simple file name in the
function it calls.  Note this is a strange case of a function calling another of
the same name with different argments.
*/
long int vector_fwrite(double *x,int n, string dir, string dfile) throw(seispp_error)
{
	string fname;
	long int foff;
	fname = dir + "/" + dfile;
	try {
		foff = vector_fwrite(x,n,fname);
	} catch ( seispp_error err) { throw err;};
	
	return(foff);
}

/*  Low level, bombproof write function for C++ for a vector of doubles.  
The function blindly saves a vector of doubles x of length n to a 
specified file.  The algorithm used is this:  
The file is openned, we seek to the file end, get the position, and then write
new data to the end of this file.  The file offset, in bytes, from ftell is
returned as a long int.  This provides a fairly bombproof way to save data 
because data are simply appended.  This avoids file name collision problems under
an assumption that the caller is saving foff to a database table like wfdisc.
*/
long int vector_fwrite(double *x,int n, string fname) throw(seispp_error)
{
	FILE *fp;
	long int foff;

	if((fp=fopen(fname.c_str(),"a")) == NULL)
		throw seispp_error("Open failed on file "+fname);
	fseek(fp,0L,2);
	foff = ftell(fp);
	if( fwrite(x,sizeof(double),n,fp)!=n) 
	{
		fclose(fp);
		throw seispp_error("fwrite error to file "+fname);
	}
	fclose(fp);
	return(foff);
}
// similar to above but for a float vector input 
long int vector_fwrite(float *x,int n, string dir, string dfile) throw(seispp_error)
{
	string fname;
	long int foff;
	fname = dir + "/" + dfile;
	try {
		foff = vector_fwrite(x,n,fname);
	} catch ( seispp_error err) { throw err;};
	
	return(foff);
}

// Again same as above but for a float vector
long int vector_fwrite(float *x,int n, string fname) throw(seispp_error)
{
	FILE *fp;
	long int foff;

	if((fp=fopen(fname.c_str(),"a")) == NULL)
		throw seispp_error("Open failed on file "+fname);
	fseek(fp,0L,2);
	foff = ftell(fp);
	if( fwrite(x,sizeof(float),n,fp)!=n) 
	{
		fclose(fp);
		throw seispp_error("fwrite error to file "+fname);
	}
	return(foff);
}

/*
// Antelope database output routine.  Uses trputwf which is 
// listed as depricated, but this seems preferable to being
// forced in the trace library format with the newer trsave_wf.
// Arguments:
//	ts = times series object to be saved
//	db = Antelope database handle
//	table = database table to write results to
//	mdl = defines names to be extracted from metadata to
//		write to database output
//	am = Attribute_map object defining internal to external namespace
//		mapping
//  The "table" argument drives the database puts.  That is the 
//  basic algorithm is: 
	dbopen table
	for each element of mdl
		if mdl-> am -> dbtable_name == table
			save
		else
			skip
	end foreach
	call trputwf to save data
 
*/
void dbsave(Time_Series& ts, 
	Dbptr db,
		string table, 
			Metadata_list& mdl, 
				Attribute_map& am)
{
	Metadata_list::iterator mdli;
	map<string,Attribute_Properties>::iterator ami,amie=am.attributes.end();
	int recnumber;
	
	db = dblookup(db,0,const_cast<char *>(table.c_str()),0,0);
	recnumber = dbaddnull(db);
	if(recnumber==dbINVALID) throw seispp_error(string("dbsave:  dbaddnull failed on table "+table));
	db.record=recnumber;

	for(mdli=mdl.begin();mdli!=mdl.end();++mdli)
	{
		string field_name;
		double dval;
		int ival;
		string cval;
		ami = am.attributes.find((*mdli).tag);
		if(ami==amie) 
		{
			dbmark(db);
			throw seispp_error(
				string("Required attribute ")
				+(*mdli).tag
				+string(" cannot be mapped to output namespace"));
		}
		if( (ami->second.db_table_name) != table)
		{
			dbmark(db);
			throw seispp_error( 
				string("dbsave (database table mismatch): attribute ")
				+ ami->second.db_attribute_name
				+ string(" is tagged with table name ")
				+ ami->second.db_table_name
				+ string("expected to find ")
				+ table);
		}
		switch(ami->second.mdt)
		{
		case MDint:
			ival = ts.md.get_int(ami->second.internal_name);
			dbputv(db,0,ami->second.db_attribute_name.c_str(),
				ival);
			break;
		case MDreal:
			dval = ts.md.get_double(ami->second.internal_name);
			dbputv(db,0,ami->second.db_attribute_name.c_str(),
				dval);
			break;
		case MDstring:
			cval = ts.md.get_int(ami->second.internal_name);
			dbputv(db,0,ami->second.db_attribute_name.c_str(),
				cval.c_str());
			break;
		case MDboolean:
			// treat booleans as ints for external representation
			// This isn't really necessary as Antelope
			// doesn't support boolean attributes
			if(ts.md.get_bool(ami->second.internal_name))
				ival = 1;
			else
				ival = 0;
			dbputv(db,0,ami->second.db_attribute_name.c_str(),
				ival);
			break;
			
		case MDlist: 
		case MDmap:
			/* We don't throw an exception here but instead just
			output a warning message always.  The reason for this
			is that in a list of attributes to be output the 
			user could make an error.  We don't want the output
			to fail on a single bad definition, but do want
			the user to be aware of the problem.  Same is 
			true of each of the cases below.*/
			cerr << "dbsave: database attribute "
				<< ami->second.db_attribute_name
				<< " is marked as a map or list\n"
				<< "These types cannot be mapped to a db\n"
				<< "Data for this attribute not saved"
				<< endl;
		case MDinvalid:
			cerr << "dbsave: database attribute "
				<< ami->second.db_attribute_name
				<< " was marked as invalid\n"
				<< "Data for this attribute not saved"
				<< endl;
		
		default:
			cerr << "dbsave: database attribute "
				<< ami->second.db_attribute_name
				<< " has unknown data type\n"
				<< "Data for this attribute not saved"
				<< endl;
		}

	}
	// Specifying the output data type is problematic
	// Here I take a dogmatic view that the default should be
	// host floats for efficiency of retrieval into routines
	// based on this library.  Note doubles might seem the
	// best choice, but they collide with Antelope as they do
	// not support external doubles for data.  i.e. output of
	// this function to wfdisc could not, for example, be displayed
	// with Antelope programs.  Using 4 byte output allows this.
	// We first have to decide the byte order of the machine
	// we are running on.
#ifdef sun
	string sdtype("s4");
#elif defined linux
	string sdtype("i4");
#elif defined __APPLE__
	string sdtype("i4");
#elif defined __arm__
	string sdtype("i4");
#else
	string sdtype("i4");
#endif
	// Check the existing datatype field and if it is shown
	// as a double type (t8 or u8) leave it alone and write
	// double output.  Otherwise convert to floats
	// While we are at it we grab the actual dir dfile values 
	// and use these for file name creation as a safe method 
	// Will work when trucation, for example, would cause problems
	char dtype[4];
	char dir[65],dfile[33];  //css3.0 wfdisc attribute sizes
	long int foff;  // actual foff reset in db record
	dbgetv(db,0,"datatype",dtype,"dir",dir,"dfile",dfile,0);
	try {
		if(dtype[1]=='8') 
		{
			// In this case we assume datatype is set for
			// byte order of this host already.  Not
			// bombproof, but this is not normal output
			// anyway.  i.e. this has to be forced
			// trust ns to be set correctly.  could use t.s.size()
			foff = vector_fwrite(&(ts.s[0]),ts.ns,
				string(dir), string(dfile));
		}
		else
		{
			// note this uses sdtype set in indefs above
			dbputv(db,0,"datatype",sdtype.c_str(),0);
			float *outbuf = new float[ts.ns];
			for(int i=0;i<ts.ns;++i) 
				outbuf[i]=static_cast<float>(ts.s[i]);
			foff = vector_fwrite(outbuf,ts.ns,
				string(dir), string(dfile));
		}
		// all cases return an foff that we need to set
		// in the database as the absolutely correct value
		// Reasons is that if the file exists these functions
		// always append and return foff.
		dbputv(db,0,"foff",foff,0);
	}
	catch (seispp_error serr)
	{
		// delete this database row if we had an error
		dbmark(db);
		throw serr;
	}
}
} // Termination of namespace SEISPP definitions

