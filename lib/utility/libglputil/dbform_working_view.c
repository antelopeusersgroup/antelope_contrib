#include "stock.h"
#include "pf.h"
#include "db.h"
/* This short routine simplifies the messy process of creating
the complex view required by many programs by using dbprocess.
That is, many datascope programs require a long series of join,
subsets, etc. to form a working view and the processing proceeds
by working through this working view or through group pointers.
Instead of cluttering up the logic of the working code with a
long string of db calls, this small function automates the process
being driven by a parameter file definition.  

The function returns a valid db pointer on sucess.  Errors result
cause db.record to be set to dbINVALID.

Arguments:
	db - pointer to a datascope database that is ASSUMED to
		have already been opened for access with dbopen.
	pf - parameter file object to be parsed to build working view
	tname - Tbl parameter entry name that holds the instructions
		to be passed to dbprocess.  

An example should make this clearer.  This is a prototype view
defintion for the initial version of dbpmel:

dbpmel_dbview &Tbl{
	dbopen hypocentroid
	dbsubset gridname==specified
	dbjoin cluster
	dbjoin event
	dbjoin origin
	dbsubset orid==prefor
	dbjoin assoc
	dbjoin arrival
	dbjoin site
	dbsort gridid evid
	dbgroup gridid 
}

where gridname is defined in the parameter file..

Here tname="dbpmel_dbview". 

Author:  Gary Pavlis
Written:  October 2000
*/
Dbptr dbform_working_view(Dbptr db, Pf *pf, char *tname)
{
	Dbptr dbv;
	Tbl *t;

	t = pfget_tbl(pf,tname);
	if(maxtbl(t)<=0)
	{
		elog_notify(0,"dbform_working_view: Tbl parameter %s defining commands for dbprocess not found.\n",tname);
		dbv.record = dbINVALID;
		return(dbv);
	}

	dbv = dbprocess(db,t,0);
	freetbl(t,0);
	/* Assume that dbprocess either returns a null db pointer or
	one set with db.record = dbINVALID in the event of errors.
	Thus we just return now. */
	return(dbv);
}

