#include "db.h"
/* A very common construct in working with the css3.0 schema is
to form the view 
event->origin->assoc->arrival
subsetting the table with orid==prefor

This function returns that view.  If any problems occur the
record field of the returned dbpointer is set to dbINVALID.

Author:  Gary Pavlis
Date: August 2000
*/
Dbptr dbarrival_view(Dbptr db)
{
	Dbptr dba, dbb, dbc, dbd;  
	char *module="dbarrival_view";

	/* All the joins here are "natural" in the css3.0 schema */
	dba = dbjoin(dblookup(db,0,"event",0,0),
			dblookup(db,0,"origin",0,0), 0,0,0,0,0);
	if(dba.record == dbINVALID) 
	{
		elog_notify(0,"%s:  event->origin join failed\n",module);
		return(dba);
	}
	dbb = dbsubset(dba,"orid == prefor",0);
	if(dbb.record == dbINVALID)
	{
		elog_notify(0,"%s:  event->origin subset to prefor failed\n",module);
		dbfree(dba);
		return(dbb);
	}
	
	dbc = dbjoin(dbb,dblookup(db,0,"assoc",0,0),0,0,0,0,0);
	if(dbc.record == dbINVALID)
	{
		elog_notify(0,"%s:  event->origin->assoc join failed\n",module);
		dbfree(dba);
		dbfree(dbb);
		return(dbc);
	}
	dbd = dbjoin(dbc,dblookup(db,0,"arrival",0,0),0,0,0,0,0);
	if(dbd.record == dbINVALID)
	{
		elog_notify(0,"%s:  event->origin->assoc->arrival join failed\n",module);
	}

	dbfree(dba);
	dbfree(dbb);
	dbfree(dbc);
	return(dbd);
}
