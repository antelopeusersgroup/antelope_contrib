/* This program reads a pfstream and saves attributes in a relational database
with a dictionary defined by an control parameter file.  

Usage:  pfstream2db file db [-v -V -pf pffile]

file - input pfstream. This can be a fifo or regular file provided it 
	follows the pfstream format.
-v verbose flag
-V exit with usage line 
-pf use alternative pf control file pffile.  

Primary control is driven by a complex parameter file described in a
man page (or at least will be).
Author:  Gary Pavlis
Written:  October 2002
*/
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include "stock.h"
#include "db.h"
#include "elog.h"
#include "brttutil.h"
#include "pfstream.h"
int PF2DBSVerbose;

void usage()
{
	cbanner("1.0",
		"file db [-V -v -pf pffile -sift expression]",
		"Gary L. Pavlis", "Indiana University",
		"pavlis@indiana.edu");
	exit(-1);
}
Arr *pfget_table_map(Pf *pf)
{
	char pfname[50],dbname[50];
	char *l;
	int i;
	Tbl *list_aliases;
	Arr *table_map;

	list_aliases = pfget_tbl(pf,"table_name_maps");
	table_map = newarr(0);
	for(i=0;i<maxtbl(list_aliases);++i)
	{
		l=(char *)gettbl(list_aliases,i);
		sscanf(l,"%s%s",pfname,dbname);
		/* This is keyed by dfname because the pfstream
		may contain a view with multiple tables to be extracted*/
		setarr(table_map,dbname,strdup(pfname));
	}
	freetbl(list_aliases,0);
	return(table_map);
}
/* Cracks a description for a group of attributes headed by "tag" 
and surrounded by &Arr{}.  The tagged group is assumed to be a set of
database table names.  Within the Tbl tagged by that list are attribute
map definitions for that table.  A small example will make the format
clearer:

save_by_row &Arr{
arrival &Tbl{
	arid int arid
	phase string phase
}
assoc &Tbl{
	orid int orid
	delta double delta 
}
}

In this example char *tag="save_by_row"

IMPORTANT:  note because we use the same function used for the
inverse operation in db2pfstream, the entries for each table
are in the order:  dbname type pfname
This is somewhat backward from a copy convention one might
guess (it isn't from to it s to from).  

Author:  Gary Pavlis
Written:  October 2002
*/
Arr *build_attribute_list(char *tag,Pf *pfi)
{
	Tbl *t;
	Arr *a;
	Pf *pf;
	Tbl *table_list;
	int i;

	a = newarr(0);
	/* silently return an empty list if the tag is not defined.
	This allows loops to be driven cleanly by range of the arr*/
	if(pfget(pfi,tag,(void **)&pf) != PFARR) return(a);
	table_list = pfkeys(pf);
	for(i=0;i<maxtbl(table_list);++i)
	{
		char *table;
		table = (char *)gettbl(table_list,i);
		/* this libpfstream function pushes Attribute_map objects 
		onto the current list */
		t=pfget_Attribute_map(pf,table);
		/* this places the list of Attribute_map pointers into 
		an associative array keyed by the table name */
		setarr(a,table,t);
	}
	return(a);
}
/* adds one row to table pointed to by db using attributes stored in
pf.  Names space mapping is defined by the list of Attribute_map
pointers stored in the input list t.  The algorithm is an iterator
on the list t.  First add a null row then fill in each attribute
plucked from pf one by one using dbputv.  

Note because of the use of dbaddnull there is no checking in the
output db for matching primary keys.  The routine blindly assumes
this will be ok.

Normal return with all ok is 0.  Returns dbINVALID if nothing 
could be appended to the table.  A positive number indicates the
number of errors posted to the error log caused by one of two
possible attribute mapping problems.  
*/
		
int dbadd_row_pfe(Dbptr db, Pf *pf, Tbl *t)
{
	Attribute_map *m;
	int i;
	int record;
	int put_errors=0;

	record = dbaddnull(db);
	if(record==dbINVALID)
		return(dbINVALID);
	db.record=record;

	for(i=0;i<maxtbl(t);++i)
	{
		Pf *test;
		int itest;
		double dvalue;
		int ivalue;
		char *cvalue;

		m = (Attribute_map *)gettbl(t,i);
		itest=pfget(pf,m->pfname,(void **)&test);
		if(itest==PFINVALID) 
		{
			if(PF2DBSVerbose)
			  elog_notify(0,
				"pfname=%s, dbname=%s, not in pfstream\n",
				m->pfname,m->dbname);
			++put_errors;
			continue;
		}
		switch(m->type)
		{
		case DBREAL:
			dvalue = pfget_double(pf,m->pfname);
			itest=dbputv(db,0,m->dbname,dvalue,0);
			break;
		case DBINT:
			ivalue = pfget_int(pf,m->pfname);
			itest=dbputv(db,0,m->dbname,ivalue,0);
			break;
		case DBTIME:
			dvalue = pfget_time(pf,m->pfname);
			itest=dbputv(db,0,m->dbname,dvalue,0);
			break;
		case DBSTR:
		default:
			cvalue = pfget_string(pf,m->pfname);
			itest=dbputv(db,0,m->dbname,cvalue,0);
		}
		if(itest==dbINVALID)
		{
			++put_errors;
			if(PF2DBSVerbose)
			  elog_notify(0,"dbputv error for %s\n",
				m->dbname);
		}
	}
	return(put_errors);
}

/* Bombproof method to acquire database table name from 
parameter space view name using the map defined by tmap.
It is bombproof in the sense that if tmap does not have
an entry for the name passed as dbtable a default of 
dbname=pfname is used. 
*/
char *get_table_name(char *dbtable,Arr *tmap)
{
	char *pftable;
	pftable=(char *)getarr(tmap,dbtable);
	if(pftable==NULL)
	{
		if( PF2DBSVerbose)elog_notify(0,"Namespace mapping not defined for pfstream table %s\nDefaulting to output=input\n",
			dbtable);
		return(dbtable);
	}
	else
		return(pftable);
}
/* This function updates database id parameters in a Pf_ensemble.  
The algorithm is a bit complex, but is driven by the Arr called
required_newids.  The routine first checks this Arr, which is
keyed by database output table name, for an id name entry.  If
there is none, it returns immediately doing nothing.  If a attribute
name is found it will update the Pf_ensemble for the range is to 
ie (inclusive) with the value returned by called dbnextid on the
Antelope database pointer db.  The rest of the code is some
minor complications related to namespace mapping between parameter
file namespace and database attribute namespace.  This is done
by a linear search through the list tam.

Author:  Gary Pavlis
*/
int change_id(Pf_ensemble *pfe,
	int is, 
		int ie, 
			Arr *required_newids,
				Dbptr db,
					char *table,
						Tbl  *tam)
{
	Attribute_map *a;
	char *idname;
	int idvalue;
	int i,itam;
	char *idn_here;
	
	idname=(char *)getarr(required_newids,table);
	if(idname==NULL) return(0);

	idvalue = dbnextid(db,idname);
	if(idvalue<0) return(-1);
	/* linear search for table in Attribute map list */
	for(itam=0;itam<maxtbl(tam);++itam)
	{
		a=(Attribute_map *)gettbl(tam,itam);
		if(!strcmp(idname,a->dbname)) 
		{
			idn_here = a->pfname;
			break;
		}
	}
	/* if not found, use the name assuming input=output */
	if(itam==maxtbl(tam))
	{
		idn_here = idname;
		if( PF2DBSVerbose)elog_notify(0,"Requested id=%s for table %s to update is not in list of output attributes\nBlundering on but database updates may fail\n",
			idname,table);
	}
	for(i=is;i<=ie;++i)
	{
		pfput_int(pfe->pf[i],idn_here,idvalue);
	}
	return(0);
}
	
void main(int argc, char **argv)
{
	Dbptr db;
	char *pfi=NULL;
	/* These lists contain secondary lists containing pointers
	to Attribute_maps.  They are built from input parameter file 
	descriptions */
	Arr *save_by_row,*save_by_group,*save_by_ensemble;
	Pf_ensemble *pfe;
	int fd;
	Tbl *table_list;
	int i,j;
	Pf *pf,*pfis;
	char *streamin;
	Arr *table_map;
	Arr *required_newids;
	Pfstream_handle *pfshi;
	

	PF2DBSVerbose=0;

/*crack the command line*/
	if(argc<3) usage();
	elog_init(argc,argv);
	if(dbopen(argv[2],"r+",&db)==dbINVALID)
	{
		elog_complain(0,"dbopen failed on database %s\n",argv[1]);
		usage();
	}
	streamin=argv[1];
	for(i=3;i<argc;++i)
	{
		if(!strcmp(argv[i],"-pf"))
		{
			++i;
			if(i>argc) usage();
			pfi=argv[i];
		}
		else if(!strcmp(argv[i],"-V"))
			usage();
		else if(!strcmp(argv[i],"-v"))
			PF2DBSVerbose=1;
		else
			usage();
	}
	
	if(pfi==NULL) pfi=strdup("pfstream2db");
	if(pfread(pfi,&pf))
		elog_die(0,"Error reading parameter file %s.pf\n",pfi);

	save_by_row = build_attribute_list("save_by_row",pf);
	save_by_group = build_attribute_list("save_by_group",pf);
	save_by_ensemble = build_attribute_list("save_by_ensemble",pf);
	/* The above define tags assigned to a group of attributes.
	When mapping between schemas or extracting data from a view
	it will be necessary to provide a map between parameter space
	tags and output table names.  This block does this by using
	a list of aliases defined in the parameter file.  */
	table_map=pfget_table_map(pf);

	/* Another nasty detail of databases is keeping integer
	ids consistently mapped.  The following NOT a general solution
	to this problem, but will work provided the ids listed are
	actually keys to the table AND were used as keys to define
	the grouping and/or ensemble blocking.  Chaos will follow if
	someone uses the wrong key for the wrong table.  I see now
	easy way to verify validity within this program.  The
	process is driven by a (not so simple) list of table name
	and id attributes.  Note if none are defined this silently 
	creates an empty Arr*/
	required_newids=pfget_arr(pf,"newids_required");
	if(required_newids==NULL)
		required_newids=newarr(0);

	/* This uses the Pfstream library multithreaded read method.
	The following routine launches a reader thread to handle
	input while the loop below does the reformatting to put things
	into the output. */
	pfshi = pfstream_start_read_thread(streamin);
        if(pfshi==NULL) 
		elog_die(1,"Read from %s thread create failed\n",argv[2]);

	while((pfis=pfstream_get_next_ensemble(pfshi))!=NULL)
	{
		char *pftable,*table;
		Tbl *tam;  /* list of Attribute maps */
		int chide;
		Arr *pfearr=newarr(0);

		/* note the order of processing here is critical
		for the id updates to work correctly */
		table_list = keysarr(save_by_row);
		/* This is a workaround for a spooky memory problem I never
		solved.  Following calls to pfget_Pf_ensemble the contents of
		table_map kept getting corrupted.  Workshop did not detect 
		any access violations and I was unable to track the problem 
		down.  I made up this workaround (and duplicates of it later
		in this loope) to work around the problem for now.  It will
		probably surface in another form later. */
		freearr(table_map,free);
		table_map=pfget_table_map(pf);

		for(i=0;i<maxtbl(table_list);++i)
		{
			table = (char *)gettbl(table_list,i);
			pftable=get_table_name(table,table_map);
			tam = (Tbl *)getarr(save_by_row,table);
			pfe=(Pf_ensemble *)getarr(pfearr,pftable);
			if(pfe==NULL)
			{
				pfe = pfget_Pf_ensemble(pfis,pftable);
				if(pfe!=NULL)
					setarr(pfearr,pftable,pfe);
				else
				{
					elog_complain(0,"Search in this data block for ensemble tag %s failed\nData from table %s ill not be saved\n",
						pftable,table);
					break;
				}
			}
			db = dblookup(db,0,table,0,0);
			for(j=0;j<pfe->nmembers;++j)
			{
				chide=change_id(pfe,j,j,
				          required_newids,db,table,tam);
				if((chide!=0) && PF2DBSVerbose)
				  elog_notify(0,"Cannot update id for table %s\n",
					table);
				if(pfget_boolean(pfe->pf[j],"data_valid"))
				      dbadd_row_pfe(db,pfe->pf[j],tam);
			}
		}
		table_list = keysarr(save_by_group);
		freearr(table_map,free);
		table_map=pfget_table_map(pf);
		for(i=0;i<maxtbl(table_list);++i)
		{
			table = (char *)gettbl(table_list,i);
			pftable=get_table_name(table,table_map);
			tam = (Tbl *)getarr(save_by_group,table);
			pfe=(Pf_ensemble *)getarr(pfearr,pftable);
			if(pfe==NULL)
			{
				pfe = pfget_Pf_ensemble(pfis,pftable);
				if(pfe!=NULL)
					setarr(pfearr,pftable,pfe);
				else
				{
					elog_complain(0,"Search in this data block for ensemble tag %s failed\nData from table %s ill not be saved\n",
						pftable,table);
					break;
				}
			}
			db = dblookup(db,0,table,0,0);
			for(j=0;j<pfe->ngroups;++j)
			{
				chide=change_id(pfe,
				  pfe->group_start[j],
				  pfe->group_end[j],
				  required_newids,db,table,tam);
				if((chide!=0) && PF2DBSVerbose && (j==0))
				   elog_notify(0,"Cannot update id for table %s\n",
					table);
				if(pfget_boolean(pfe->pf[pfe->group_start[j]],"data_valid"))
				    dbadd_row_pfe(db,
					pfe->pf[pfe->group_start[j]],tam);
			}
		}
		table_list = keysarr(save_by_ensemble);
		freearr(table_map,free);
		table_map=pfget_table_map(pf);
		for(i=0;i<maxtbl(table_list);++i)
		{
			table = (char *)gettbl(table_list,i);
			pftable=get_table_name(table,table_map);
			tam = (Tbl *)getarr(save_by_ensemble,table);
			pfe=(Pf_ensemble *)getarr(pfearr,pftable);
			if(pfe==NULL) 
			{
				pfe = pfget_Pf_ensemble(pfis,pftable);
				if(pfe!=NULL)
					setarr(pfearr,pftable,pfe);
				else
				{
					elog_complain(0,"Search in this data block for ensemble tag %s failed\nData from table %s ill not be saved\n",
						pftable,table);
					break;
				}
			}
			chide=change_id(pfe,0,0,required_newids,db,table,tam);
			if((chide!=0) && PF2DBSVerbose)
				elog_notify(0,"Cannot update id for table %s\n",
                                        table);
			db = dblookup(db,0,table,0,0);
			if(pfget_boolean(pfe->pf[0],"data_valid"))
				dbadd_row_pfe(db,pfe->pf[0],tam);
		}
		freearr(pfearr,free_Pf_ensemble);
	}
	exit(0);
}
