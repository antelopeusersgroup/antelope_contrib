#include <memory>
#include <vector>
#include <stdio.h>
#include <string>
#include "stock.h"
#include "tr.h"
#include "seispp.h"
namespace SEISPP
{
using namespace SEISPP;
using namespace std;

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
	} catch ( seispp_error& err) { throw err;};
	
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
	} catch ( seispp_error& err) { throw err;};
	
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
	fclose(fp);
	return(foff);
}

// Small helper for function immediately below avoids 
// repetitious and confusing test
bool trdata_is_gap(Wftype *wft,Trsample value)
{
	if((value<=wft->lower) || (value>=wft->upper) )
		return true;
	else
		return false;
}
/* Sets gap values in a Time_Series object using methods from
BRTT's trace library.  That is, in seispp I use a totally different
method to mark gaps compared to the trace library.  The trace
library uses magic numbers while I use a set of Time_Window 
objects.  This function scans an input vector of Trsample (float)
values looking for gaps defined for the input datatype.  
This is essentially a translation routine from external format
methods for defining gaps and ones used here.
Arguments:
	ts - Time_Series object to define gaps for
	data_raw - input array from BRTT trace library of data
		samples to be scanned for gaps.
	nsamp - length of data_raw
	datatype - BRTT defined datatype (normally from wfdisc).

Routine checks for consistency in nsamp and datatype and throws
an exception if the they are not consistent.  

*/

void set_gaps(Time_Series& ts, 
	Trsample *data_raw, 
		int nsamp,
			string datatype)
				throw(seispp_error)
{
	bool in_gap_now;
	double gap_start,gap_end;
	// some sanity checks.  Throw an exception if 
	// any of these occur
	if(ts.s.size()!=nsamp || ts.ns!=nsamp)
		throw seispp_error("set_gaps:  Time_Series data size does not match pattern from raw input");
	string tsdtype=ts.get_string("datatype");
	if(tsdtype!=datatype)
		throw seispp_error("set_gaps:  Time_Series metadata definition of parent data type does not match pattern");
	Wftype *wft=trwftype(const_cast<char *>(datatype.c_str()));
	// This should not be necessary but better to initialize it
	// anyway rather than depend on a random value being accidentally set
	gap_start = ts.t0;
	for(int i=0,in_gap_now=false;i<nsamp;++i)
	{
		if(in_gap_now)
		{
			if(trdata_is_gap(wft,data_raw[i]))
				continue;
			else
			{
				gap_end = ts.time(i-1);
				Time_Window *tw=new Time_Window(gap_start,
							gap_end);
				ts.add_gap(*tw);
				delete tw;
				in_gap_now=false;
			}
		}
		else
		{
			if(trdata_is_gap(wft,data_raw[i]))
			{
				in_gap_now=true;
				gap_start=ts.time(i);
			}
		}
	}
	free((void *)wft);
}


Time_Series *Load_Time_Series_Using_Pf(Pf *pf)
{
	Metadata md(pf);
	Time_Series *ts = new Time_Series(md,true);
	return(ts);
}
/* This function is used by both the Time_Series and Three_Component
versions of dbsave below.  It builds a database row from a metadata
object, which is produced in both cases by  casting up to Metadata,
and pushing attributes out driven by the list, mdl, and the 
namespace map, am.  

Arguments:
	md = Metadata object containing attributes to be written to 
		database
	db = Antelope Dbptr.  It MUST point at a valid single row that
		is to contain the attributes to be copied there.  
	table = name of table these attributes are being written to
		(needed for consistency check).
	mdl = defines names to be extracted from metadata to
		write to database output
	am = Attribute_Map object defining internal to external namespace
		mapping

The basic algorithm is:
	for each element of mdl
		if mdl-> am -> dbtable_name == table
			save
		else
			skip
	end foreach
*/
void save_metadata_for_object(Metadata& md,
	Dbptr db,
		string table,
			Metadata_list& mdl, 
				Attribute_Map& am)
		throw(seispp_error)
{
	Metadata_list::iterator mdli;
	map<string,Attribute_Properties>::iterator ami,amie=am.attributes.end();
	string cval;

	for(mdli=mdl.begin();mdli!=mdl.end();++mdli)
	{
		double dval;
		int ival;
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
		try {
			switch(ami->second.mdt)
			{
			case MDint:
				if(ami->second.is_key)
				{
					ival = dbnextid(db,
					  const_cast<char *>
					   (ami->second.db_attribute_name.c_str()) );
					if(ival<0)throw seispp_error(
					  	string("dbsave:  ")
						+ ami->second.db_attribute_name
						+ string(" is defined as integer key for table ")
						+ ami->second.db_table_name
						+ string(" but dbnextid failed") );
					
				}
				else
					ival = md.get_int(ami->second.internal_name);
				dbputv(db,0,ami->second.db_attribute_name.c_str(),
					ival,0);
				// In this case we need to push this back to metadata
				// so it can be used downstream
				md.put_metadata(ami->second.db_attribute_name,ival);
				break;
			case MDreal:
				dval = md.get_double(ami->second.internal_name);
				dbputv(db,0,ami->second.db_attribute_name.c_str(),
					dval,0);
				break;
			case MDstring:
				cval = md.get_string(ami->second.internal_name);
				dbputv(db,0,ami->second.db_attribute_name.c_str(),
					cval.c_str(),0);
				break;
			case MDboolean:
				// treat booleans as ints for external representation
				// This isn't really necessary as Antelope
				// doesn't support boolean attributes
				if(md.get_bool(ami->second.internal_name))
					ival = 1;
				else
					ival = 0;
				dbputv(db,0,ami->second.db_attribute_name.c_str(),
					ival,0);
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
		catch (Metadata_get_error& mderr)
		{
			mderr.log_error();
			throw seispp_error(
			    string("dbsave object failure from problem in metadata components"));
		}
	}
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
//	am = Attribute_Map object defining internal to external namespace
//		mapping
//  The "table" argument drives the database puts.  That is the 
//  basic algorithm is: 
	dbopen table
	add a null record
	save metadata to new row driven by mdl and am
	call trputwf to save data
 
*/
int dbsave(Time_Series& ts, 
	Dbptr db,
		string table, 
			Metadata_list& mdl, 
				Attribute_Map& am)
		throw(seispp_error)
{
	int recnumber;
	string field_name;

	if(!ts.live) return(-1);  // return immediately if this is marked dead
	
	db = dblookup(db,0,const_cast<char *>(table.c_str()),0,0);
	recnumber = dbaddnull(db);
	if(recnumber==dbINVALID) throw seispp_error(string("dbsave:  dbaddnull failed on table "+table));
	db.record=recnumber;
	try {
		save_metadata_for_object(dynamic_cast<Metadata&>(ts),
			db,table,mdl,am);
	} catch (seispp_error& serr)
	{
		dbmark(db);
		throw serr;
	}
	// Even if they were written in the above loop the contents 
	// of the object just override the metadata versions.  
	// This is safer than depending on the metadata
	double etime;
	etime = ts.endtime();
	dbputv(db,0,"time",ts.t0,
		"endtime",etime,
		"samprate",1.0/ts.dt,
		"nsamp",ts.ns,0);
	
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
#ifdef WORDS_BIGENDIAN
	string sdtype("t4");
#else
	string sdtype("u4");
#endif
	// Check the existing datatype field and if it is shown
	// as a float type (t8 or u8) leave it alone and force write
	// as double output.  Otherwise (normal behaviour) convert to floats
	// While we are at it we grab the actual dir dfile values 
	// and use these for file name creation as a safe method 
	// Will work when trucation, for example, would cause problems
	
	char dtype[4];
	char dir[65],dfile[33];  //css3.0 wfdisc attribute sizes
	long int foff;  // actual foff reset in db record
	dbgetv(db,0,"datatype",dtype,"dir",dir,"dfile",dfile,0);
	// make sure the directory is present
	if(makedir(dir))
	{
		throw seispp_error(string("makedir(dir) failed with dir=")
				+ string(dir));
	}
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
		dbputv(db,0,"foff",static_cast<int>(foff),0);
		return(recnumber);
	}
	catch (seispp_error& serr)
	{
		// delete this database row if we had an error
		dbmark(db);
		throw serr;
	}
}
/*
// Antelope database output routine.  Fragments three component
// seismograms into scalar time series object and then uses the
// scalar Time_Series version of dbsave.  
// Arguments:
//	ts = times series object to be saved
//	db = Antelope database handle
//	table = database table to write results to
//	mdl = defines names to be extracted from metadata to
//		write to database output
//	am = Attribute_Map object defining internal to external namespace
//		mapping

// mdl and am define what data are pushed to the output database.
// Two quantities are always pushed to Metadata space for each 
// component before dbsave(Time_Series ...) is called:  hang an vang.
// These are the component directions using naming scheme of css3.0

Author:  G Pavlis
Written:  summer 2004
 
*/
int dbsave(Three_Component_Seismogram& tcs, 
	Dbptr db,
		string table, 
			Metadata_list& mdl, 
				Attribute_Map& am,
					vector<string>chanmap,
						bool output_as_standard)
{
	int irec;
	try {
		if(output_as_standard) tcs.rotate_to_standard();
		auto_ptr<Time_Series>x1(Extract_Component(tcs,0));
		x1->put_metadata("vang",90.0);
		x1->put_metadata("hang",90.0);
		x1->put_metadata("chan",chanmap[0]);
		auto_ptr<Time_Series>x2(Extract_Component(tcs,1));
		x2->put_metadata("vang",90.0);
		x2->put_metadata("hang",90.0);
		x2->put_metadata("chan",chanmap[1]);
		auto_ptr<Time_Series>x3(Extract_Component(tcs,2));
		x3->put_metadata("vang",90.0);
		x3->put_metadata("hang",90.0);
		x3->put_metadata("chan",chanmap[2]);
		irec=dbsave(*x1,db,table,mdl,am);
		irec=dbsave(*x2,db,table,mdl,am);
		irec=dbsave(*x3,db,table,mdl,am);
		return(irec);
		// delete not needed because of auto_ptr
	// catch all exceptions and just rethrow them
	} catch (...)
	{ throw;}
}
// Writes a 3C trace out in a fortran matrix, binary format.
// Arguments are as in above function but there is not 
// channel map function in this case.  Note the data are
// always forced to standard coordinates so we don't need
// to save the transformation matrix.  
// Uses a trick to get at the contents of the dmatrix.
// VERY IMPORTANT portability restriction if the dmatrix
// implementation every changes.  Assumes the contents of
// the dmatrix are in FORTRAN order with components of 
// the seismogram in row order.  (i.e. data are multiplexed
// in the vector sequence stored in output)  
int dbsave(Three_Component_Seismogram& tcs, 
	Dbptr db,
		string table, 
			Metadata_list& mdl, 
				Attribute_Map& am)
{
	int recnumber;
	string field_name;

	if(!tcs.live) return(-1);  // return immediately if this is marked dead
	if(table=="wfdisc")
		throw seispp_error(string("dbsave:  Using wrong dbsave function ")
			+string("for Three_Component_Seismogram object.\n")
			+string("Cannot save to wfdisc with this function.\n"));
	
	db = dblookup(db,0,const_cast<char *>(table.c_str()),0,0);
	recnumber = dbaddnull(db);
	if(recnumber==dbINVALID) 
		throw seispp_error(string("dbsave:  dbaddnull failed on table "+table));
	db.record=recnumber;
	try {
		save_metadata_for_object(dynamic_cast<Metadata&>(tcs),
			db,table,mdl,am);
		// Even if they were written in the above loop the contents 
		// of the object just override the metadata versions.  
		// This is safer than depending on the metadata
		double etime;
		etime = tcs.endtime();
		string sdtype("3c"); // speciall datatype signals dmatrix format
		dbputv(db,0,"time",tcs.t0,
			"endtime",etime,
			"samprate",1.0/tcs.dt,
			"nsamp",tcs.ns,
			"datatype",sdtype.c_str(),0);
		char dir[65],dfile[33];  //css3.0 wfdisc attribute sizes
		long int foff;  // actual foff reset in db record
		// assume these were set in mdl.  probably should have a cross check
		dbgetv(db,0,"dir",dir,"dfile",dfile,0);
		// make sure the directory is present
		if(makedir(dir))
		{
			dbmark(db);
			throw seispp_error(string("makedir(dir) failed with dir=")
					+ string(dir));
		}
		// Note we always write these as doubles.  I don't
		// expect to save a datatype with this class of 
		// object database writers.
		foff=vector_fwrite(tcs.u.get_address(0,0),tcs.ns*3,
			string(dir),string(dfile));
		// Above returns and off that we need to set 
		// in the database as the absolutely correct value
		// Reasons is that if the file exists these functions
		// always append and return foff.
		dbputv(db,0,"foff",static_cast<int>(foff),0);
		return(recnumber);
	}
	catch (seispp_error& serr)
	{
		// delete this database row if we had an error
		dbmark(db);
		throw serr;
	}
}

} // Termination of namespace SEISPP definitions

