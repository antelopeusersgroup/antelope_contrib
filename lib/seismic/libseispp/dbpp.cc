#include "dbpp.h"
#include "seispp.h"
using namespace std;
using namespace SEISPP;
// oddity needed to work with DanQs freetbl function
extern "C" {
void
myfree(char *p)
{
    free((void *) p) ;
}
}
namespace SEISPP
{
// Simple function needed by constructors below.  Returns true if
// the string dbgroup is found in the command to be send to dbprocess.
// Throws an int exception if the dbgroup clause is anywhere but 
// the last line
bool dbgroup_used(Tbl *t)
{
	string s;
	int ierr;
	int n=maxtbl(t);
	for(int i=0;i<n;++i)
	{
		s = string(static_cast<char *>(gettbl(t,i)));
		ierr=s.find("dbgroup");
		if(ierr>=0)
		{
			if(i==(n-1))
				return true;
			else
				throw 1;
		}
	}
	return false;
}
/* Tricky function to test if an Antelope Dbptr is a bundle 
pointer.  It uses a trick to suppress an error describe by 
Dan Quinlan to glp on Feb. 23, 2005.  

Function returns true of the Dbptr is the result of dbgroup.
Returns false otherwise.
*/
bool is_a_bundle_pointer(Dbptr db)
{
	int logmark;
	Dbptr dbbundle;
	bool ret;
	logmark=elog_mark();
	if(dbgetv(db,0,"bundle",&dbbundle,0)==dbINVALID)
		ret=false;
	else
		ret=true;
	elog_flush(0,logmark);
	return(ret);
}
// Default constructor needs to initialize the pointer to 
// mark it as invalid
Datascope_Handle::Datascope_Handle()
{
	db.database=dbINVALID;
	db.table=dbINVALID;
	db.field=dbINVALID;
	db.record=dbINVALID;
	is_bundle=false;
	close_on_destruction=false;
	parent_table.database=dbINVALID;
	parent_table.table=dbINVALID;
	parent_table.field=dbINVALID;
	parent_table.record=dbINVALID;
}
	
/* dbprocess driven constructor for an Antelope_Database (handle) object.
	dbname = name of database to be openned (always openned r+)
	pf = pf (parameter file object) pointer containing dbprocess list
	tag = name of Tbl in pf for list of commands passed to dbprocess.
Will throw a seispp_dberror if problems happen in dbprocess, pf routines,
or in dbprocess.  This is an example of a constructor that acquires
a resource (in this case valid pointers to a view into a database)
*/
Datascope_Handle::Datascope_Handle(string dbname,
	string pfname, 
		string tag,
			bool readonly)
{
	if(readonly)
	{
		if(dbopen(const_cast<char*>(dbname.c_str()),"r",&db))
			throw seispp_dberror("Failure in dbopen",db,complain);
	}
	else
	{
		if(dbopen(const_cast<char*>(dbname.c_str()),"r+",&db))
			throw seispp_dberror("Failure in dbopen",db,complain);
	}
	close_on_destruction=true;
	// Probably needs to be an auto_ptr so pffree can be called
	// Maybe should use a Metadata object
	Pf *pf;
	if(pfread(const_cast<char*>(pfname.c_str()),&pf))
		throw seispp_error("Failure in pfread");
	Tbl *process_list;
	process_list = pfget_tbl(pf,const_cast<char*>(tag.c_str()));
	if(process_list==NULL)
	{
		freetbl(process_list,0);
		throw seispp_error("Tag name = "+tag+" not in parameter file");
	}
	try {
		is_bundle = dbgroup_used(process_list);
	} catch (...)
	{
		freetbl(process_list,0);
		throw seispp_error("Error in process list specification:  dbgroup can only be used as last command");
	}
	db = dbprocess(db,process_list,0);
	freetbl(process_list,0);
	if(db.table == dbINVALID)
		throw seispp_dberror("dbprocess failed",db,complain);
	pffree(pf);
	// Always initialize -- better than garbage
	parent_table=db;
	if(is_bundle) --parent_table.table;
	close_on_destruction=false;
}
// somewhat repetitious, but a useful subset of above
// Convenient sometimes to use with Plain Jane version immediately below
Datascope_Handle::Datascope_Handle(Dbptr dbi, Pf *pf, string tag)
{
	Tbl *process_list;
	process_list = pfget_tbl(pf,const_cast<char*>(tag.c_str()));
        if(process_list==NULL)
                throw seispp_error("Tag name = "+tag+" not in parameter file");
	try {
		is_bundle = dbgroup_used(process_list);
	} catch (...)
	{
		throw seispp_error("Error in process list specification:  dbgroup can only be used as last command");
	}
        db = dbprocess(dbi,process_list,0);
        if(db.table == dbINVALID)
                throw seispp_dberror("dbprocess failed",db,complain);
	// Always initialize -- better than garbage
	parent_table=db;
	if(is_bundle) --parent_table.table;
	close_on_destruction=false;
}

// Plain Jane version just opens the database and sets the Dbptr
Datascope_Handle::Datascope_Handle(string dbname,bool readonly)
{
	if(readonly)
	{
		if(dbopen(const_cast<char*>(dbname.c_str()),"r",&db))
			throw seispp_dberror("Failure in dbopen",db,complain);
	}
	else
	{
		if(dbopen(const_cast<char*>(dbname.c_str()),"r+",&db))
			throw seispp_dberror("Failure in dbopen",db,complain);
	}
	is_bundle = false;
	close_on_destruction=false;
}
// copy constructor 
Datascope_Handle::Datascope_Handle(Datascope_Handle& dbi)
{
	db = dbi.db;
	is_bundle=dbi.is_bundle;
	close_on_destruction=false; // copies should be this by default
	parent_table=dbi.parent_table;
}
/* An odd specialized copy constructor for Datascope db.  
Handle is constructed using an existing db pointer, dbi, 
and setting the "parent" for this handle using dbip.  
A special problem is that the function has to do an independent
test to see if dbi is a bundle pointer or not to set the is_bundle
variable.  This is necessary as Datascope has no way to independently
tell if a Dbptr is a bundle or just a plain table or view.  
*/

Datascope_Handle::Datascope_Handle(Dbptr dbi,Dbptr dbip)
{
	db = dbi;
	parent_table = dbip;
	is_bundle=is_a_bundle_pointer(dbi);
	close_on_destruction=false; // copies should be this by default
}
	
Datascope_Handle::~Datascope_Handle()
{
	if(close_on_destruction)dbclose(db);
}
// function that needs to be called to force closing database
void Datascope_Handle::close()
{
	close_on_destruction=true;
}
double Datascope_Handle::get_double(string name)
{
	double val;
	if(dbgetv(db,0,name.c_str(),&val,0))
		throw seispp_dberror(string("dbgetv error extracting attribute ")
			+name,
			db,complain);
	return(val);
}
int Datascope_Handle::get_int(string name)
{
	int val;
	if(dbgetv(db,0,name.c_str(),&val,0))
		throw seispp_dberror(string("dbgetv error extracting attribute ")
			+name,
			db,complain);
	return(val);
}
string Datascope_Handle::get_string(string name)
{
	char val[128];
	if(dbgetv(db,0,name.c_str(),&val,0))
		throw seispp_dberror(string("dbgetv error extracting attribute ")
			+name,
			db,complain);
	return(string(val));
}
string  Datascope_Handle::get_filename()
{
	char str[256];

	if(dbgetv(db,0,"dir",str,0) )
		throw seispp_dberror("dbgetv error trying to read dir",db,complain);
	string dir(str);
	if(dbgetv(db,0,"dfile",str,0) )
		throw seispp_dberror("dbgetv error trying to read dfile",db,complain);
	return(dir+"/"+string(str));
}
	
int Datascope_Handle::number_tuples()
{
	int ierr;
	int nrec;
	ierr = dbquery(db,dbRECORD_COUNT,&nrec);
	if(ierr<0)
		throw seispp_dberror(string("dbquery of record count failed"),
			db,complain);
	return(nrec);
}
int Datascope_Handle::number_attributes()
{
	int ierr;
	int natt;
	ierr = dbquery(db,dbFIELD_COUNT,&natt);
	if(ierr<0)
		throw seispp_dberror(string("number_attributes:  dbquery failed"),
			db,complain);
	return(natt);
}
Datascope_Handle& Datascope_Handle::operator=(const Datascope_Handle& dbi)
{
	if(this!=&dbi)
	{
		db=dbi.db;
		is_bundle=dbi.is_bundle;
		close_on_destruction=false;
		parent_table=dbi.parent_table;
	}
	return(*this);
}
void Datascope_Handle::operator ++()
{
	++db.record;
}
// add row function.  Adds a blank row to db and returns the
// record number of the new row.  Assumes Dbptr points at
// one table.  Throws an exception if the add fails

int Datascope_Handle::append()
{
	int row_added;
	row_added = dbaddnull(db);
	if(row_added==dbINVALID)
		throw seispp_dberror(string("Datascope_Handle::append: dbaddnull failure"),
			db,complain);
	db.record=row_added;
	return(row_added);
}
// Put functions.  All can have the same name because of overloading
// in all put functions first arg is the attribute name and the second
// is the value to be deposited in the current row.
// I could have used a template except some versions require a cast
// Maybe there is a way around that but I don't know it
void Datascope_Handle::put(string name, double value)
{
	if(dbputv(db,0,name.c_str(),value,0) == dbINVALID)
		throw seispp_dberror(string("dbputv error with attribute "+name),
				db, complain);
}
void Datascope_Handle::put(string name, float value)
{
	if(dbputv(db,0,name.c_str(),static_cast<double>(value),0) == dbINVALID)
		throw seispp_dberror(string("dbputv error with attribute "+name),
				db, complain);
}
void Datascope_Handle::put(string name, int value)
{
	if(dbputv(db,0,name.c_str(),value,0) == dbINVALID)
		throw seispp_dberror(string("dbputv error with attribute "+name),
				db, complain);
}
void Datascope_Handle::put(string name, string value)
{
	if(dbputv(db,0,name.c_str(),value.c_str(),0) == dbINVALID)
		throw seispp_dberror(string("dbputv error with attribute "+name),
				db, complain);
}
void Datascope_Handle::put(string name, char *value)
{
	if(dbputv(db,0,name.c_str(),value,0) == dbINVALID)
		throw seispp_dberror(string("dbputv error with attribute "+name),
				db, complain);
}
void Datascope_Handle::put(string name, const char *value)
{
	if(dbputv(db,0,name.c_str(),const_cast<char *>(value),0) == dbINVALID)
		throw seispp_dberror(string("dbputv error with attribute "+name),
				db, complain);
}
Tbl *list_to_tbl(list<string> slist)
{
	list<string>::iterator sl;
	Tbl *t;
	t=newtbl(slist.size());
	for(sl=slist.begin();sl!=slist.end();++sl)
	{
		char *s;
		s = strdup((*sl).c_str());
		pushtbl(t,s);
	}
	return(t);
}
	
void Datascope_Handle::sort(list<string> sortkeys)
{
	Tbl *t;
	t = list_to_tbl(sortkeys);
	db = dbsort(db,t,0,0);
	if(db.table == dbINVALID)
		throw seispp_dberror("dbsort failed",db,complain);
	freetbl(t,myfree);
}
	
// natural join requires no join keys
void Datascope_Handle::natural_join(string table1, string table2)
{
	Dbptr dbj1, dbj2;
	dbj1 = dblookup(db,0,const_cast<char *>(table1.c_str()),0,0);
	dbj2 = dblookup(db,0,const_cast<char *>(table2.c_str()),0,0);
	db = dbjoin(dbj1, dbj2,0,0,0,0,0);
	if(db.table==dbINVALID)
		throw seispp_dberror(string("dbjoin of tables ")
			+ table1 
			+ string(" and ") 
			+ table2 
			+ string("failed"),
			db,complain);
}
// special case appending to current db
void Datascope_Handle::natural_join(string table)
{
	db = dbjoin(db,dblookup(db,0,const_cast<char *>(table.c_str()),0,0),
		0,0,0,0,0);
	if(db.table==dbINVALID)
		throw seispp_dberror(string("dbjoin: append to current view with table") 
			+ table 
			+ string("failed"),
			db,complain);
}

// full case with different lists for table 1 and table 2
void Datascope_Handle::join(string table1, string table2, 
	list<string> joinkeys1, list<string> joinkeys2)
{
	Tbl *t1,*t2;
	t1 = list_to_tbl(joinkeys1);
	t2 = list_to_tbl(joinkeys2);
	Dbptr dbj1, dbj2;
	dbj1 = dblookup(db,0,const_cast<char *>(table1.c_str()),0,0);
	dbj2 = dblookup(db,0,const_cast<char *>(table2.c_str()),0,0);
	db = dbjoin(dbj1, dbj2,&t1,&t2,0,0,0);
	freetbl(t1,myfree);
	freetbl(t2,myfree);
	if(db.table==dbINVALID)
		throw seispp_dberror(string("dbjoin of tables ")
			+ table1 
			+ string(" and ") 
			+ table2 
			+ string("failed"),
			db,complain);
}

void Datascope_Handle::group(list<string> groupkeys)
{
	Tbl *t;
	t = list_to_tbl(groupkeys);
	parent_table=db;
	db = dbgroup(db,t,0,0);
	is_bundle=true;
	freetbl(t,myfree);
	if(db.table==dbINVALID)
		throw seispp_dberror(string("dbgroup failed"),
			db,complain);
}
void Datascope_Handle::subset(string sstr)
{
	db = dbsubset(db,const_cast<char *>(sstr.c_str()),0);
	if(db.table==dbINVALID)
		throw seispp_dberror(string("dbsubset failed"),
			db,complain);
}
void Datascope_Handle::lookup(string t)
{
	db = dblookup(db,0,const_cast<char *>(t.c_str()),
		0,0);
	if(db.table==dbINVALID)
		throw seispp_dberror(string("lookup:  lookup failed for table"
			+ t),db,complain);
}
DBBundle Datascope_Handle::get_range()
{
	DBBundle bundle;
	int s,e;
	if(is_bundle)
	{
		Dbptr dbbundle;
		int ierr;
		ierr = dbgetv(db,0,"bundle",&dbbundle,0);
		dbget_range(dbbundle,&s,&e);
		bundle.start_record = s;
		bundle.end_record = e;
		bundle.parent = dbbundle;
	}
	else
		throw seispp_dberror(
			string("get_range:  handle is not a bundle pointer"),
			db,complain);
	return(bundle);
}


// error object functions 
seispp_dberror::seispp_dberror(const string mess, Dbptr dbi)
		: seispp_error(mess)
{
	db=dbi;
	error_type=unknown;
}
seispp_dberror::seispp_dberror(const string mess, Dbptr dbi,
		error_severity et) : seispp_error(mess)
{
	db=dbi;
	error_type=et;
}

void seispp_dberror::log_error()
{
	cerr<<"Database related error:"<<endl
		<<message<<endl
	<< "Current database pointer:"
	<<"Current database pointer:"
	<<"Database="<<db.database
	<<", Table="<<db.table
	<<", Record="<<db.record
	<<", Field="<<db.field<<endl
	<<"severity level=";
	switch (error_type)
	{
	case fault:
		cerr << " Fault" << endl;
		break;
	case fatal:
		cerr << " Fatal" << endl;
		break;
	case complain:
		cerr << " Complain" << endl;
		break;
	case notify:
		cerr << " Notify" << endl;
		break;
	case log:
		cerr << " Log" << endl;
		break;
	default:
		cerr << " Unknown" << endl;
	}
	clear_register(1);
}
}

