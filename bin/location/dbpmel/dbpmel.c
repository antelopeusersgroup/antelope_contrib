#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include "elog.h" 
#include "stock.h"
#include "arrays.h"
#include "pf.h"
#include "db.h"
#include "coords.h"
#include "glputil.h"
#include "location.h"
#include "pmel.h"
#include "dbpmel.h"


/* This small function parses an input string made up of
a comma seperated list of integer ranges returning a Tbl
of ints after expanding the full range defined by the 
string.  An example will make this clearer:
if *gstr is "1,2,4-6" The output Tbl list will be
1,2,4,5,6.  No test is made for overlapping integer
ranges.  

Author: Gary Pavlis
Written:  Sept 2000
*/
Tbl *parse_gridlist_string(char *gstr)
{
	Tbl *t;
	char *sp;
	char *sie;
	long is,ie,i;

	t = newtbl(0);
	sp = strtok(gstr,",");
	do
	{
		if( (sie=strchr(sp,'-')) == NULL)
		{
			is = atol(sp);
			ie = is;
		}
		else
		{
			*sie = '\0';
			++sie;
			is = atol(sp);
			ie = atol(sie);
		}
		for(i=is;i<=ie;++i)
		{
			pushtbl(t,(void *)i);
		}
	} while((sp = strtok(NULL,","))!=NULL);
	return(t);
}
/* This small functions scans the gridlist tbl to get the maximum
and minimum gridid values.  These are used to subset the working view
automatically to reduce the size of the working view (found to be
excessive otherwise .

gmin and gmax are returned as the maximum and minimum grid id

Author:  Gary Pavlis
Written: July 2001
*/
void get_gridid_range(Tbl *gridlist,long *gmin,long *gmax)
{
	long gidmin,gidmax;
	long gridid;
	long i;
	
	if(maxtbl(gridlist)<=0) elog_die(0,"Empty grid id list\nProbable usage error\n");
	gidmin = (long)gettbl(gridlist,0);
	gidmax = gidmin;
	for(i=1;i<maxtbl(gridlist);++i)
	{
		gridid = (long)gettbl(gridlist,i);
		gidmin = MIN(gidmin,gridid);
		gidmax = MAX(gidmax,gridid);
	}
	*gmin = gidmin;
	*gmax = gidmax;
}
/* small companion function to main.  It basically dumps the contents of
the parameter space, pf, to a special database table.  It is assumed
to be called early on on execution so it is a fragile little program 
that will die if you look at it wrong.  The checks against the string
parameters are redundant because of the use of check_required_pf, but 
since it is possible to mess this up by errors in pf double checks here
are ok.
*/

void save_run_parameters(Dbptr db,Pf *pf)
{
	char *dir,*dfile;
	char filename[512];
	char *vm,*vm3d;
	int ierr;
	
	dir = pfget_string(pf,"pmelrun_archive_directory");
	if(dir==NULL)elog_die(0,"Parameter pmelrun_archive_directory not in parameter file\n");
	if(makedir(dir))
		elog_die(0,"makedir failed on directory %s\n",dir);
	dfile = pfget_string(pf,"pmel_run_name");

	
	vm = pfget_string(pf,"travel_time_model");
	vm3d=pfget_string(pf,"3Dreference_model");
	if( (vm==NULL) || (vm3d==NULL) )
		elog_die(0,"Missing required velocity model definitions\nCheck parameters travel_time model and 3Dreference_model\n");
	db = dblookup(db,0,"pmelruns",0,0);
	ierr=dbaddv(db,0,"pmelrun",dfile,
		"vmodel",vm,
		"vmodel3d",vm3d,
		"dir",dir,
		"dfile",dfile,NULL );
	if(ierr < 0) elog_die(0,
		   "dbaddv error on pmelrun table\nVerify schema extensions for dbpmel and that the pmel_run_name parameter is unique\n");

	strcpy(filename,dir);
	strcat(filename,"/");
	strcat(filename,dfile);
	if(pfwrite(filename,pf))
		elog_die(0,"pfwrite error for file %s\n",filename);
}
	

void usage()
{
	elog_die(0,"Usage:  dbpmel db gridlist [-sift expression -pf file]\n\twhere gridlist is a comma seperated list of grid points\n");
}
/* main for dbpmel*/	
int
main(int argc, char **argv)
{
	char *dbin;  /* Input db name */
	Tbl *gridlist;
	Dbptr db;  /* input db pointer */
	Dbptr dbv;  /* set to view formed by join */
	char *pfin=NULL;  /* input parameter file */
	char *sift_exp;  /* sift expression for subset */
	int sift = 0;  /* default is no sift.  */
	Tbl *sortkeys;
	/* db row variables */
	long nrows, nrows_raw;

	Pf *pf;
	char *version="1.0";
	int i;
	long gmin,gmax;
	char sstring[128];
	char *gridname;
	Tbl *proctbl;


	/* Initialize the error log and write a version notice */
	elog_init (argc, argv) ;
	fprintf (stdout, "%s version %s\n", argv[0], version) ;

	if(argc < 3) usage();
	dbin = argv[1];
	gridlist = parse_gridlist_string(argv[2]);
	get_gridid_range(gridlist,&gmin,&gmax);
	
	for(i=3;i<argc;++i)
	{
		if(!strcmp(argv[i],"-pf"))
		{
			++i;
			if(i>=argc) usage();
			pfin = argv[i];
		}
		else if(!strcmp(argv[i],"-sift"))
		{
			++i;
			if(i>=argc) usage();
			sift_exp = argv[i];
			sift = 1;
		}
		else
			usage();
	}
	/* set default this way*/
	if(pfin == NULL) pfin = (char *)strdup("dbpmel");
	i = pfread(pfin,&pf);
	if(i != 0) elog_die(1,"Pfread error\n");
	check_required_pf(pf);


	/* Set up main database view.  This is a derived from code
	in the related genloc program called relocate.
	Always join assoc, arrival, and site.  We join site 
	to make sure station table is properly dynamic to account for
	time changes.  With this setup, the stations can even move
	around and this should still work.*/
	gridname = pfget_string(pf,"gridname");
	if(dbopen(dbin,"r+",&db) == dbINVALID) 
		elog_die(1,"Unable to open input database %s\n",dbin);
	
	/* We save the pf object into archive files that document the
	complex state of this program.  This small function does this
	and saves the results in a special db table */
	save_run_parameters(db,pf);	
		
	db = dblookup(db,0,"hypocentroid",0,0);
	sprintf(sstring,"gridid>=%ld && gridid<=%ld && (gridname=~/%s/)",gmin,gmax,gridname);
	db = dbsubset(db,sstring,0);
	dbquery(db, dbRECORD_COUNT, &nrows);
	if(nrows<=0) 
		elog_die(0,"No hypocentroid records in requested gridid range of %ld to %ld for grid called %s\n",
				gmin,gmax,gridname);
	/* This forms the working view for this program */
	proctbl = strtbl("dbjoin cluster",
		"dbjoin event",
		"dbjoin origin",
		"dbsubset orid==prefor",
		"dbjoin assoc",
		"dbjoin arrival",NULL );
	dbv = dbprocess(db,proctbl,0);
	dbquery(dbv, dbRECORD_COUNT, &nrows);
	fprintf(stdout,"Raw working database view has %ld rows\n",nrows);

	/* Subset using sift_key if requested */
	if(sift)
	{
		dbv = dbsubset(dbv,sift_exp,0);
		if(dbv.record == dbINVALID)
			elog_die(1,"dbsubset of %s with expression %s failed\n",
				dbin, sift_exp);
	}

	/* First we have to run a unique key sort in the following order
	to remove redundant picks made on multiple channels.  We will
	issue a warning if the record count changes.  This was found
	to be a common problem that had to be repaired automatically.*/
	dbquery(dbv, dbRECORD_COUNT, &nrows_raw);
	sortkeys = newtbl(0);
	pushtbl(sortkeys,"gridid");
	pushtbl(sortkeys,"evid");
	pushtbl(sortkeys,"sta");
	pushtbl(sortkeys,"phase");
	dbv = dbsort(dbv,sortkeys,dbSORT_UNIQUE,0);
	dbquery(dbv, dbRECORD_COUNT, &nrows);

	if(nrows != nrows_raw)
		elog_complain(0,"Input database has duplicate picks of one or more phases on multiple channels\n\
Which picks will be used here is unpredictable\n\
%ld total picks, %ld unique\nContinuing\n", nrows_raw, nrows);

	fprintf(stdout,"Final working view has %ld rows\n",nrows);

	if(dbpmel_process(dbv,gridlist,pf))
	{
		elog_complain(0,"Errors in dbpmel_process\n");
		exit(-1);
	}
	exit(0);
}
