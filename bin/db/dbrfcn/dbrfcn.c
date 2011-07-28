/* dbrfcn.c   receiver fcn calculation (frequency domain)
	call with:  dbrfcn db orid sta [parameter-file]
	see manpages for details

	Limitations:
	1.  rot() will screw up for odd sensor orientations/channel names
	2.  No way to deal with gappy data/more than 1 trace per channel; just exits
	3.  doesn't save parameters in any reasonable way (e.g. in a new table)
	
	Geoff Abers 1/96	

	11/27/00   modified so saves amplitude rescaling (correcting for filtering) in calib  GAA
 */
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <string.h>

#include "db.h"
#include "tr.h"
#include "pf.h"
#include "tttaup.h"
#include "response.h"
#include "coords.h"
#include "stock.h"

extern int filter3c(), num_traces() ;
extern int rot(), rfcn_calc(), trsubset(), trdecimate();
extern float *rfcn();

main (argc, argv)

int argc;
char **argv;

{
	float	phshift=10., wlev=.01;					/* rfcn-parameters */
	double tst=-10., ten=50., gfreq=0.25, hpfreq=0.02;		/* more settable params */
	int idecim=8, dographics=1, doarid=0;				/* still more */
	char *dbname, *output_database;
	Pf  *pf;
	char pfile[132];
	void *pfstr;
	char *sta;
	char *chan;
	char *oridstr, *aridstr, yesno;
	char	fgcolor[32], line[100];
	char	title[256], subtitle[256];
	int orid=-1, arid=-1;

	Dbptr db, dbo, dbs, dbwf, dbsc, tr, dbout;
	double evlat, evlon, dep,evtime;
	double slat, slon, del=0.0, az, azdeg;
	double tt, t0, t1, t0ref, t1ref, tpred, t0dat, t1dat, t0m, t1m, samprate ;
	int i, nsamp, nwf, nwfo, izero=0 , wfid, ier;
	int itran = 0;	/* Portrait */
	int nplt,  *plotlist;
         Tbl           *sortkeys ;
         float    zero = 0.0 , one = 1.0, xx, yy;
	float *rfcn1;
	char	time_str[24], endtime_str[24];
	char *outpath = "wfrf/%Y/%j/%{sta}.%{chan}.%Y:%j:%H:%M:%S";
	Tbl *stachan ;

/* DEBUG stuff.. */
	int rs, re, bundletype, bs, be;
	char chan0[10];
	Dbptr	bundle;

	printf(" %s Version of 2000.11.27 - GAA \n", argv[0]);
/*
 *	Get command args.
 */
	if (argc!=5 && argc !=6 ) {
		fprintf(stderr," %d arguments invalid\n",argc);
		usage();
		exit(1);
	}
	dbname = argv[1];
	if (!strcmp(dbname, "-H")) {
		fprintf(stderr," help arguments\n");
		usage();
		exit(1);
	}
	output_database = argv[2];
	oridstr = argv[3];
	printf ("DBRFCN db: %s odb: %s ",dbname, output_database);
	if (strchr(oridstr, ':') ) {
	    doarid = 1;
	    aridstr = strtok(oridstr,":");
	    arid = atoi(aridstr);
	    azdeg = atof(strtok(0,":"));
	    printf(" arid:az %s ",oridstr);
	} else {
	    orid = atoi(argv[3]);
	    printf (" orid: %d ",orid);
	}
	sta = argv[4];
	printf ("sta %s ",sta);
	if (argc == 6 && ((int)strlen(argv[5]))>1) {
	    strcpy(pfile, argv[5]);
	    /* Open and read pf file */
	    if ( (ier=pfread(argv[5], &pf))<0 ) {
		  printf(" pf error %d on %s\n",ier, pfile);
		  elog_die(0, " pfread error.. %s", argv[5]);
	    }
	    printf ("pfile %s\n",pfile);
	    if (pfget(pf, "tstart", &pfstr)!= PFINVALID) tst = pfget_double(pf, "tstart");
	    if (pfget(pf, "tend", &pfstr)!= PFINVALID) ten = pfget_double(pf, "tend");
	    if (pfget(pf, "gaussfreq", &pfstr)!= PFINVALID) gfreq = pfget_double(pf, "gaussfreq");
	    if (pfget(pf, "hpfreq", &pfstr)!= PFINVALID) hpfreq = pfget_double(pf, "hpfreq");
	    if (pfget(pf, "phaseshift", &pfstr)!= PFINVALID) phshift = pfget_double(pf, "phaseshift");
	    if (pfget(pf, "waterlevel", &pfstr)!= PFINVALID) wlev = pfget_double(pf, "waterlevel");
	    if (pfget(pf, "decimate", &pfstr)!= PFINVALID) idecim = pfget_int(pf, "decimate");
	    if (pfget(pf, "graphics", &pfstr)!= PFINVALID) dographics = pfget_int(pf, "graphics");
	} else {
	    printf ("Default-settings...\n");
	}
	printf ("Arguments: tstart = %.3f  tend = %.3f\n",tst, ten);
	printf ("           gaussfreq = %.3f  hpfreq = %.3f  phaseshift = %.3f waterlevel = %.4f\n", gfreq, hpfreq, phshift, wlev);

/*
 *	Open database.
 */
	if (dbopen (dbname, "r+", &db) == dbINVALID) {
		elog_clear_register(1);
		fprintf (stderr, "dbspgram: Unable to open database.\n");
		exit (1);
	}

/* get origin and set rotation 
 */
	if (!doarid) {
	    printf ("Doing orid %s station %s in db %s\n", oridstr, sta, dbname);
	    dbo = dblookup (db, 0, "origin", "orid", oridstr);
 		if (dbo.record == dbINVALID) {
 			elog_clear_register(1);
 			fprintf (stderr, "dbrfcn: Cannot find orid %d in %s.\n", orid, dbname);
 			usage();
 			exit (1);
 		}
	    dbo.field = dbALL;
	    dbgetv (dbo, 0, "lat", &evlat, "lon", &evlon, "depth",&dep, "time",&evtime, 0);
	    dbs = dblookup (db, 0, "site", "sta", sta);
 	    if (dbs.record == dbINVALID) {
 		elog_clear_register(1);
 		fprintf (stderr, "dbspgram: Cannot find sta %s in %s.\n", sta, dbname);
 		usage();
 		exit (1);
 	    }
 	    dbs.field = dbALL;
 	    dbgetv (dbs, 0, "lat", &slat, "lon", &slon, 0);
	    printf("Origin: %.3f %.3f  Site: %.3f %.3f ",evlat,evlon,slat,slon);
 	    slat *= M_PI/180.0;
 	    slon *= M_PI/180.0;
	    evlat *= M_PI/180.0;
 	    evlon *= M_PI/180.0;
	    dist (slat, slon, evlat, evlon, &del, &az);
	    del *= 180.0/M_PI;
	    azdeg = az*180.0/M_PI;
	    tpred = evtime + ptime(del, dep);
	    printf("Del: %.2f, Az: %.2f %s\n",del,azdeg, strtime(tpred));
	} else {
	    dbo = dblookup(db, 0, "arrival", "arid", aridstr);
 		if (dbo.record == dbINVALID) {
 			elog_clear_register(1);
 			fprintf (stderr, "dbrfcn: Cannot find arid %d in %s.\n", arid, dbname);
 			usage();
 			exit (1);
 		}
	    dbo.field = dbALL;
	    dbgetv (dbo, 0, "time", &tpred, 0);
	    printf("ARID: %d, Baz: %.2f %s\n",arid, azdeg, strtime(tpred));
	}
	t0 = tpred + tst;
	t1 = tpred + ten;

	dbwf=dblookup(db, 0, "wfdisc", 0, 0);
	dbquery (dbwf, dbRECORD_COUNT, &nwf);
	printf("%d records in original WF...\n ",nwf);
	if (nwf<1) { 
	 	fprintf(stderr," No data\n");
		exit(1);
	}

	/* subset the station */
	sprintf(line,"(sta == \"%s\")",sta);
	dbwf = dbsubset(dbwf,line,0);
	dbquery (dbwf, dbRECORD_COUNT, &nwf);
	printf("%d records after sta subset...\n ",nwf);
	if (nwf<1) { 
	 	fprintf(stderr," No data\n");
		exit(1);
	}

	/* subset the time window */ 
	sprintf(line,"(time < %f && endtime > %f)", t1, t0);
	dbwf = dbsubset(dbwf,line,0);
	dbquery (dbwf, dbRECORD_COUNT, &nwf);
	printf("%d records after time subset...\n ",nwf);
	if (nwf<1) { 
	 	fprintf(stderr," No data\n");
		exit(1);
	}
	dbsc=dblookup(db, 0, "sitechan", 0, 0);
	dbwf=dbjoin(dbwf, dbsc, 0, 0, 0, 0, 0);
	dbquery (dbwf, dbRECORD_COUNT, &nwf);
	printf("%d records after sitechan join...\n ",nwf);
	if (nwf!=3) { 
	 	fprintf(stderr," Needs 3 channels; quitting\n");
		exit(1);
	}


	for (i=0; i<nwf; i++) {
	   dbwf.record = i;
	   dbgetv(dbwf, 0, 
  	   "time", &t0dat,
	   "nsamp", &nsamp,
	   "samprate", &samprate,  0);
	   t1dat = t0dat + (double)(nsamp-1)/samprate;
	   if (t0<t0dat) t0=t0dat;
	   if (t1>t1dat) t1=t1dat;
	 }

	/* SORT the subset */
         sortkeys = newtbl ( 3 ) ;
         pushtbl ( sortkeys, "sta" ) ;
         pushtbl ( sortkeys, "chan" ) ;
         pushtbl ( sortkeys, "time" ) ;
         dbwf = dbsort ( dbwf, sortkeys, 0, 0 ) ;

	/* Make trace database */
	tr = dbinvalid() ;
	sprintf(time_str,"%f",t0);
	sprintf(endtime_str,"%f",t1);
	if (trload_css ( dbwf, time_str, endtime_str, &tr, 0, 0) < 0) 
		elog_die( 0, "Problems loading traces\n") ;
	/* For CNET:  Split out datagaps */
	trsplit (tr, 0, 0);
	dbquery (tr, dbRECORD_COUNT, &nwf);
	if (nwf != 3) {
	    fprintf(stderr," ERROR - GAPS: need 3 traces, have %d, Quitting...\n", nwf);
	    return 0;
	} 

         /* group tr table by sta-chan */
	stachan = strtbl("sta", "chan", 0 ) ;
	tr = dbgroup(tr, stachan, 0, 1) ;

	printf("after trgroup %d traces\n",num_traces(tr));

	if (dographics>0) {
	    /* Plot raw */
	    sprintf(title,"db: %s orid: %d sta: %s t0: %s Depth: %.0f",dbname, orid, sta, strtime(t0), dep);
	    sprintf(subtitle,"Delta: %.2f BAz: %.2f Flp: %.3f Fhp: %.3f WLev %.3f",del, azdeg, gfreq, hpfreq, wlev);
	    init_plot(0, 0.9, "dbrfcn", "dbrfcn", title, subtitle);

	    /* flag the traces to plot : first 3 */
	    nplt = num_traces(tr);
	    printf("... %d records in final tr\n ",nplt);
	    if (nplt != 3) fprintf ( stderr, "WARNING:  Unexpected # of traces %d\n",nplt ) ;
	    plotlist = (int *)malloc(nplt*sizeof(int));
	    plotlist[0] = 1;		/* E */
	    plotlist[1] = 1;		/* N */
	    plotlist[2] = 1;		/* Z */
 
	    strcpy(fgcolor, "black");
	    setfg_ (fgcolor, strlen(fgcolor));
	    plot_tr(tr, 0.675, 0.9, plotlist, nplt);
	}

	if (idecim > 0) trdecimate(tr, idecim);
	trdemean(tr);
	rot(tr, azdeg); 

	if (dographics>0) {
	    plotlist[2] = 0;
	    strcpy(fgcolor, "red");
	    setfg_ (fgcolor, strlen(fgcolor));
	    plot_tr(tr, 0.525, 0.675, plotlist, nplt);
	}

         /* take a look at the data */
/*         trdisp ( tr, 0 ) ;
 */

	/** Calculate RECEIVER FUNCTION **/
	rfcn_calc(tr, phshift, wlev, gfreq, hpfreq);

	if (dographics>0) {
	    strcpy(fgcolor, "blue");
	    setfg_ (fgcolor, strlen(fgcolor));
	    plotlist[2] = 1;
	    plot_tr(tr, 0.25, 0.5, plotlist, nplt);

	    /* Save the receiver functions ( Radial + tangential ) */
	    printf ("Save this result? (y/n) ");
	    scanf("%c",&yesno);
	} else {
	    yesno = 'y';
	}

	if (yesno=='y' || yesno=='Y') {

   	    dbget_range(tr, &rs, &re);
    	    for (tr.record = rs; tr.record < re; tr.record++)
    	    {
		dbgetv(tr, 0,
	       		"chan", &chan0,
	       		0);
		if (!(!strcmp(chan0,"rf_T") || !strcmp(chan0,"rfcn"))) dbmark(tr);
	    }
	    dbcrunch(tr);
    	    
	    /* FIX THE TIMES: tpred should be at zero lag */
	    tt = tpred - phshift;
	    nplt = num_traces(tr);
   	    dbget_range(tr, &rs, &re);
    	    for (tr.record = rs; tr.record < re; tr.record++)
    	    {
	      	dbgetv(tr, 0, "bundle", &bundle, 0 );
	   	dbget_range(bundle, &bs, &be);
		for (bundle.record = bs; bundle.record < be; bundle.record ++)
		{
		    dbgetv(bundle, 0, "samprate", &samprate, "nsamp", &nsamp, 0);
		    t1dat = tt + (double)(nsamp - 1)/samprate;
		    dbputv(bundle, 0,
			"time", tt,
			"endtime", t1dat,
			0);
		    dbgetv(bundle, 0, "time", &t0, 0);
		    if (abs(tt-t0) > 0.001) fprintf(stderr,"problem fixing origin time dt= %f\n", tt-t0);
		}
	    }

	    printf ("  ..saving %d to %s via %s\n ",nplt,output_database,line);
	    if ( dbopen ( output_database, "r+", &dbout ) )
		elog_die( 0, "Can't open database %s\n", output_database ) ;

	    dbout = dblookup ( dbout, 0, "wfdisc", 0, 0 ) ;
	    dbquery (dbout, dbRECORD_COUNT, &nwfo);
	    if ( trsave_wf ( tr, dbout, "t4", outpath, 0 ) )
		elog_die( 0, "Couldn't save waveforms\n" ) ;
	    dbquery (dbout, dbRECORD_COUNT, &nwf);
	    for (dbout.record=nwfo; dbout.record<nwf; dbout.record ++) {
		wfid = dbnextid(dbout, "wfid");
		dbputv(dbout, 0, "wfid", wfid, 0);


	    }
	    dbfree(dbout);
	    if (dographics>0) {
		    finitt_();
		    hdkild_();
	    }
	} else {
	    if (dographics>0) killbutton(itran);
	}

         /* free the data segments */
	 tr.table=dbALL;
         trfree ( tr ) ;
	dbfree(db);
	return 0;
}


/* count # of traces in tr */
int num_traces(tr)
Dbptr tr;
{
	int ntr, rs, re, bs, be;
	Dbptr bundle;

	dbget_range(tr, &rs, &re);
	ntr = 0;
	printf(" num_tr outer range %d - %d \n",rs, re);
	for (tr.record = rs; tr.record<re; tr.record++) {
	    dbgetv(tr, 0, "bundle", &bundle, 0 );
	    dbget_range(bundle, &bs, &be);
	    ntr += be - bs;
	}
	return (ntr);
}

int
usage()

{
	fprintf (stderr, "usage: dbrfcn db dbout {orid | arid:azim} sta [pf-file-prefix]\n");
	fprintf (stderr, "	orid:  must be in origin table in db\n");
	fprintf (stderr, "	arid:azim  arid gives time, azim gives back-az for rotation\n");
	fprintf (stderr, "	sta:  station; must be in site table in db\n");
	fprintf (stderr, "	pf-file:  parameter file (default is dbrfcn.pf) May have\n");
	fprintf (stderr, "	tstart, tend, gaussfreq, hpfreq, phaseshift, waterlevel\n");
	fprintf (stderr, " Receiver Function deconvolution: from R and T comps\n");
}
