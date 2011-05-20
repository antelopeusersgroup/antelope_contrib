#include <stdio.h>
#include <strings.h>
#include "multiwavelet.h"
#include "mwap.h"

FILE *MWpin=0,*MWpout=0;

void usage(char *prog)
{
	elog_die(0,"usage: %s db [-phase phase -sift expression -pf pfname]\n",
		prog);
} 
/* This is ugly, but necessary because I want to avoid a mixed tcl and
perl script for the mwap gui.  This routine opens and input and output
fifo whose names are taken from the mwap parameter file.  These are
passed around in an ugly way with the external variables MWpin and MWpout.
A nasty solution but preferable to changing argument lists as this method
may go away some day.  
*/
void MWopen_pipes(Pf *pf)
{
	extern FILE *MWpin,*MWpout;
	char *pname;
	pname = pfget_string(pf,"mwap_inpipe");
	if(pname==NULL)elog_die(0,"Parameter file is missing mwap_inpipe parameter\n");
	MWpin = fopen(pname,"r+");
	if(MWpin==NULL) elog_die(0,"Cannot open mwap input pipe %s\n",
				pname);
	pname = pfget_string(pf,"mwap_outpipe");
	if(pname==NULL)elog_die(0,"Parameter file is missing mwap_outpipe parameter\n");
	MWpout = fopen(pname,"r+");
	if(MWpout==NULL) elog_die(0,"Cannot open mwap output pipe %s\n",
				pname);
}
int main(int argc, char **argv)
{
	char *version="2.0 August 2000 \nAuthor:  Gary Pavlis";
	char *dbname;  /* input database name */
	Dbptr db;  /* base db pointer */
	Dbptr dbj, dbevid_group, dbsta_group;

	int i;
	char *pfin=NULL,*phase=NULL,*sift_exp=NULL;
	int sift=0;
	char subset_string[128];

	int nrows_total, nevents;  /* rows in join and # of groups */
	Tbl *grp_tbl;
	Tbl *sortkeys;

	Pf *pf;


        /* Initialize the error log and write a version notice */
        elog_init (argc, argv);
        elog_log (0, "%s version %s\n", argv[0], version) ;

	/* usual cracking of command line */
	if(argc < 2) usage(argv[0]);
	dbname = argv[1];

	for(i=2;i<argc;++i)
	{
              if(!strcmp(argv[i],"-pf"))
                {
                        ++i;
                        if(i>=argc) usage(argv[0]);
                        pfin = argv[i];
                }
                else if(!strcmp(argv[i],"-sift"))
                {
                        ++i;
                        if(i>=argc) usage(argv[0]);
                        sift_exp = argv[i];
                        sift = 1;
                }
                else if(!strcmp(argv[i],"-phase"))
		{
                        ++i;
                        if(i>=argc) usage(argv[0]);
			phase = argv[i];
		}
                else
                        usage(argv[0]);
        }
	/* this sets defaults */
	if(pfin == NULL) pfin = strdup("mwap");
	if(phase == NULL) phase = strdup("P");

	i = pfread(pfin,&pf);
        if(i != 0) elog_die(1,"Pfread error\n");
	MWopen_pipes(pf);

	/* This utility causes the program to die if required parameters
	are missing */
	check_required_pf(pf);

	/* We open the db and do the join of
	event->origin->assoc->arrival
	This view is then subsetted by the combination of phase and the 
	sift parameter (if given) and grouped by station/channel */

	if(dbopen(dbname,"r+",&db) == dbINVALID)
                elog_die(1,"Unable to open input database %s\n",dbname);
	dbj = dbjoin ( dblookup(db,0,"event",0,0),
                dblookup(db,0,"origin",0,0),
                0,0,0,0,0);
 	if(dbj.table == dbINVALID)
                elog_die(1,"event->origin join failed\n");
        dbj = dbjoin ( dbj, dblookup(db,0,"assoc",0,0),
                        0,0,0,0,0);
        if(dbj.table == dbINVALID)
                elog_die(1,"event->origin->assoc join failed\n");
        dbj = dbjoin ( dbj, dblookup(db,0,"arrival",0,0),
                        0,0,0,0,0);
        if(dbj.table == dbINVALID)
                elog_die(1,"event->origin->assoc->arrival join failed\n");
	sprintf(subset_string,
		"(arrival.iphase =~ /%s/) && (orid==prefor)",phase);
	if(sift_exp != NULL)
	{
		strcat(subset_string," && ");
		strcat(subset_string,sift_exp);
		elog_log(0,"Subsetting input db with expression %s\n",subset_string);
	}
	dbj = dbsubset(dbj,subset_string,0);
	if(dbj.record == dbINVALID)
               elog_die(1,"dbsubset of %s with expression %s failed\n",
                             dbname, sift_exp);
	dbquery(dbj,dbRECORD_COUNT,&nrows_total);
	fprintf(stdout,"Working database has %d arrivals\n",nrows_total);

	sortkeys = newtbl(4);
	pushtbl(sortkeys,"evid");
	pushtbl(sortkeys,"sta");
	pushtbl(sortkeys,"chan");
	dbj = dbsort(dbj,sortkeys,0,0);
	if(dbj.record == dbINVALID)
		elog_die(0,"dbsort of input db failed\n");

	grp_tbl = newtbl(2);
	pushtbl(grp_tbl,"evid");
	dbevid_group = dbgroup(dbj,grp_tbl,EVIDBDLNAME,EVIDBUNDLE);
	dbquery(dbevid_group,dbRECORD_COUNT,&nevents);
	if(nevents <= 0) elog_die(0,"dbgroup failed -- no data to process\n");
	fprintf(stdout,"%s will attempt to process %d events in this run\n",argv[0],nevents);
	/*This view is used repeatedly, so we create it here and
	then look it up later.  It is a join of wfdisc and sitechan 
	sorted and grouped by station */
        db = dbjoin ( dblookup(db,0,"wfdisc",0,0),
                dblookup(db,0,"sitechan",0,0),
                0,0,0,0,0);
	if(db.table == dbINVALID)
                elog_die(1,"wfdisc->sitechan join failed\n");
	clrtbl(sortkeys,0);
	sortkeys=strtbl("sta","chan","time",0);
	db = dbsort(db,sortkeys,0,WFVIEW);
	if(db.record == dbINVALID)
		elog_die(0,"dbsort of input db failed\n");

	freetbl(sortkeys,0);
	freetbl(grp_tbl,0);

	/* Now we call the function that actually does all the work
	passing the grouped db pointer and the parameter space */

	mwap_process(dbj,phase,pf);

	dbclose(db);
	return(0);
}
