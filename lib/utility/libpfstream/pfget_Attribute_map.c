/* function to build an Attribute_map object from a parameter file
description.  

Arguments:

pf - parameter file object handle containing description of the 
	map to be built.
name - name attached to the block defining the Attribute_map to 
	be constructed. 

Return:

normal return is a Tbl list of pointers to Attribute_map objects
that define the complete mapping operator.  Errors cause a NULL
pointer routine with messages sent with elog.  

Author:  Gary L. Pavlis
Written:  September 2002

*/

#include <string.h>
#include "stock.h"
#include "elog.h"
#include "brttutil.h"
#include "pfstream.h"  /*Attribute_map object is defined here */

Tbl *pfget_Attribute_map(Pf *pf, char *name)
{
	Attribute_map *amap;
	Tbl *t;
	Tbl *tout;
	int nlist;
	int i;

	t=pfget_tbl(pf,name);
	if(t==NULL)
	{
		elog_complain(0,"parameter %s to define Attribute_map not in parameter file\n",name);
		freetbl(t,0);
		return(NULL);
	}
	nlist=maxtbl(t);
	tout=newtbl(nlist);
	for(i=0;i<nlist;++i)
	{
		char dbname[80],type[20],pfname[80];
		char *s;
		s=gettbl(t,i);
		allot(Attribute_map *,amap,1);
		if(sscanf(s,"%s %s %s",dbname,type,pfname)!=3)
		{
			elog_complain(0,"Syntax error in input line for input list %s defining attribute mapping\nBadline:  %s\n",
				name,s);
			freetbl(t,0);
			freetbl(tout,free);
		}
		/*the type fields here are defined by an enum in db2pfstream.h
		minor added code to make the input more flexible here. */
		if((!strcmp(type,"int")) || (!strcmp(type,"INT"))
				|| (!strcmp(type,"integer")) )
			amap->type=DBINT;
		else if((!strcmp(type,"real")) || (!strcmp(type,"REAL"))
				|| (!strcmp(type,"double")) 
				|| (!strcmp(type,"DOUBLE"))
				|| (!strcmp(type,"float")) 
				|| (!strcmp(type,"FLOAT")))
			amap->type=DBREAL;
		else if((!strcmp(type,"string")) || (!strcmp(type,"STRING"))
				|| (!strcmp(type,"str"))
				|| (!strcmp(type,"STR")) )
			amap->type=DBSTR;
		else if((!strcmp(type,"time")) || (!strcmp(type,"TIME")) )
			amap->type=DBTIME;
		else
		{
			elog_complain(0,"Syntax error:  unknown data type %s used to define Attribute mapping\n",
				type);
			freetbl(t,0);
			freetbl(tout,free);
			return(NULL);
		}
		amap->dbname = strdup(dbname);
		amap->pfname = strdup(pfname);
		pushtbl(tout,amap);
	}

	freetbl(t,0);
	return(tout);
}
