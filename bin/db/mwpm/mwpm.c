#include <stdio.h>
#include <strings.h>
#include <sunmath.h>
#include "multiwavelet.h"
void usage(char *prog)
{
	die(0,"usage: %s db [-phase phase -sift expression -pf pfname]\n",
		prog);
} 
int main(int argc, char **argv)
{
	char *version="1.0 April 2000 \nAuthor:  Gary Pavlis";
	char *dbname;  /* input database name */
	Dbptr db;  /* base db pointer */
	Dbptr dbj, dbsta_group;

	int i;
	char *pfin=NULL,*phase=NULL,*sift_exp=NULL;
	int sift=0;
	char subset_string[128];

	int nrows_total, nevents;  /* rows in join and # of groups */
	Tbl *sortkeys;
	Tbl *proc_tbl;  /* passed to dbprocess */

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
	if(pfin == NULL) pfin = strdup("mwpm");
	if(phase == NULL) phase = strdup("P");

	i = pfread(pfin,&pf);
        if(i != 0) die(1,"Pfread error\n");

	/* This utility causes the program to die if required parameters
	are missing */
	check_required_pf(pf);


	if(dbopen(dbname,"r+",&db) == dbINVALID)
                die(1,"Unable to open input database %s\n",dbname);

	proc_tbl = strtbl("dbopen event",
			"dbjoin origin",
			"dbsubset orid==prefor",
			"dbjoin assoc",
			"dbjoin arrival",0);
	db = dbprocess(db,proc_tbl,0);
	if(db.record == dbINVALID) elog_die(0,
		"dbprocess failure forming working arrival view\n");	
	sprintf(subset_string,
		"(iphase =~ /%s/)",phase);
	if(sift_exp != NULL)
	{
		strcat(subset_string," && ");
		strcat(subset_string,sift_exp);
		elog_log(0,"Subsetting input db with expression %s\n",subset_string);
	}
	dbj = dbsubset(db,subset_string,"arrival_subset");
	if(dbj.record == dbINVALID)
                die(1,"dbsubset of %s with expression %s failed\n",
                                dbname, sift_exp);

	dbquery(dbj,dbRECORD_COUNT,&nrows_total);
	elog_log(0,"Working database view has %d arrivals\n",nrows_total);

        db = dbjoin ( dblookup(db,0,"wfdisc",0,0),
                dblookup(db,0,"sitechan",0,0),
                0,0,0,0,0);
        if(db.table == dbINVALID)
                die(1,"wfdisc->sitechan join failed\n");
        sortkeys=strtbl("sta","chan","time",0);
        db = dbsort(db,sortkeys,0,WFVIEW);
        if(db.record == dbINVALID)
                die(0,"dbsort of input db failed\n");

	freetbl(sortkeys,0);

	/* Now we call the function that actually does all the work
	passing the grouped db pointer and the parameter space */

	mwpm_process(dbj,phase,pf);

	dbclose(db);
}
