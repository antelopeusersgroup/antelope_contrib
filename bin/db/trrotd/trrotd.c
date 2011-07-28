/* trrotd.c   rotates, decimates and detrends traces (front of dbrfcn)
	call with:  trrotd db dbout {orid | arid:azim} sta [pf-file-prefix]
	rotates/windows EITHER on orid-site matchup OR on keyed arid:azim
	( uses same parameter file as dbrfcn)
	1/01   upgraded to Antelope4.2
	GAA 1/96	*/
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

extern int num_traces() ;
extern int rot(), rfcn_calc(), trsubset(), trdecimate();
extern float *rfcn();
extern int *fill();

main (argc, argv)

int argc;
char **argv;

{
	float	phshift=5., wlev=.01;					/* rfcn-parameters */
	double tst=-10., ten=50., gfreq=0.5, hpfreq=0.02;		/* more settable params */
	int idecim=0, dographics=1, doarid=0;				/* still more */
	char *dbname, *output_database;
	Pf  *pf;
	char pfile[132], *substr;
	void *pfstr;
	char *sta;
	char *chan;
	char *oridstr, *aridstr, yesno;
	char *outpath = "wfrot/%Y/%j/%{sta}.%{chan}.%Y:%j:%H:%M:%S";
	char	fgcolor[32], line[100];
	char	time_str[24], endtime_str[24], title[100];
	int orid=-1, arid=-1, wfid;

	Dbptr db, dbo, dbs, dbwf, dbsc, tr, dbout;
	double evlat, evlon, dep,evtime;
	double slat, slon, del=0.0, az, azdeg;
	double t0, t1, t0ref, t1ref, tpred, t0dat, t1dat, t0m, t1m, samprate ;
	int i, nsamp, nwf, izero=0 ;
	int nplt,  *plotlist;
         Tbl           *sortkeys ;
         float    zero = 0.0 , one = 1.0, xx, yy;
	float *rfcn1;

/* DEBUG stuff.. */
	int rs, re, bundletype, bs, be;
	char chan0[10];
	Dbptr	bundle;


/* 	double ptime();  Now in library */

/*
 *	Get command args.
 */
	if (argc!=6 && argc !=7 ) {
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
	if (strchr(oridstr, ':') ) {
	    doarid = 1;
	    aridstr = strtok(oridstr,":");
	    arid = atoi(aridstr);
	    azdeg = atof(strtok(0,":"));
	} else {
	    orid = atoi(argv[3]);
	}
	sta = argv[4];
	substr = argv[5];
	if (argc == 7) {
		strcpy(pfile, argv[6]);
		/* Open and read pf file */
		if (pfread(pfile, &pf)) elog_die(0, " pfread error on %s\n", pfile);
		if (pfget(pf, "tstart", &pfstr)!= PFINVALID) tst = pfget_double(pf, "tstart");
		if (pfget(pf, "tend", &pfstr)!= PFINVALID) ten = pfget_double(pf, "tend");
		if (pfget(pf, "decimate", &pfstr)!= PFINVALID) idecim = pfget_int(pf, "decimate");
		if (pfget(pf, "graphics", &pfstr)!= PFINVALID) dographics = pfget_int(pf, "graphics");
	}

	printf ("Arguments: tstart = %f  tend = %f  decimation factor = %d\n",tst, ten, idecim);

/*
 *	Open database.
 */
	if (dbopen (dbname, "r+", &db) == dbINVALID) {
		elog_clear_register(1);
		fprintf (stderr, "trrotd: Unable to open database.\n");
		exit (1);
	}

/* get origin and set rotation 
 */
	if (!doarid) {
	    dbo = dblookup (db, 0, "origin", "orid", oridstr);
 		if (dbo.record == dbINVALID) {
 			elog_clear_register(1);
 			fprintf (stderr, "trrotd: Cannot find orid %d in %s.\n", orid, dbname);
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

	/* subset the station and subsetstring */
/* 	printf ("length of subset string is %d\n",strlen(substr)); */
	if (strlen(substr)<1) {
		sprintf(line,"(sta =~ /%s/)",sta);
	} else {
		sprintf(line,"(sta =~ /%s/ && %s)",sta, substr);
	}
	dbwf = dbsubset(dbwf,line,0);
	dbquery (dbwf, dbRECORD_COUNT, &nwf);
	printf("%d records after sta and substr subset...\n ",nwf);
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
/*	if (nwf!=3) { 
	 	fprintf(stderr," Needs 3 channels; quitting\n");
		exit(1);
	}
*/
/*	for (i=0; i<nwf; i++) {
	   dbwf.record = i;
	   dbgetv(dbwf, 0, 
  	   "time", &t0dat,
	   "nsamp", &nsamp,
	   "samprate", &samprate,  0);
	   t1dat = t0dat + (double)(nsamp-1)/samprate;
	   if (i==0 || t0m>t0dat) t0m = t0dat;
	   if (i==0 || t1m<t1dat) t1m = t1dat;
	 }
	if (t0<t0dat) t0=t0dat;
	if (t1>t1dat) t1=t1dat;
 */
	/* SORT the subset */
         sortkeys = newtbl ( 3 ) ;
         pushtbl ( sortkeys, "sta" ) ;
         pushtbl ( sortkeys, "chan" ) ;
         pushtbl ( sortkeys, "time" ) ;
         dbwf = dbsort ( dbwf, sortkeys, 0, 0 ) ;

	/* Make trace database */

	tr = dbinvalid();
	sprintf(time_str,"%f",t0);
	sprintf(endtime_str,"%f",t1);
	printf(" ... Asking trload for data from %s to %s should be %f s\n",time_str, endtime_str, t1-t0);
	if (trload_css ( dbwf, time_str, endtime_str, &tr, 0, 0) < 0) 
		elog_die( 0, "Problems loading traces\n") ;

	/* PROBLEM:  trsplice is setting added points to a very bad value */
	/* trsplice( tr, 1.0, 0, 0); */
	trsplice( tr, 1.0, fill, 0);
	dbquery(tr, dbRECORD_COUNT, &nwf);
	printf ("%d records after load and splice...\n", nwf);
	for (i=0; i<nwf; i++) {
		tr.record=i;
	 	dbgetv(tr, 0, "time", &t0dat, "endtime", &t1dat, "nsamp", &nsamp, "samprate", &samprate, 0);
		printf(" record %d from %f to %f but dt= %f \n", i, t0dat, t1dat, (double)(nsamp-1)/samprate);
	}
	trwavestats(tr);

         /* group tr table by sta-chan */
      /*   tr = trgroup ( tr ) ; */  
	printf("At start  %d traces\n",num_traces(tr));

	if (dographics>0) {
	    /* Plot raw */
	    sprintf(title,"db: %s orid: %d sta: %s t0: %s",dbname, orid, sta, strtime(t0));
	    init_plot(0, 0.9, "trrotd", "trrotd", title);

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
	printf (" ready to detrend\n");
	mytr_detrend(tr);
	printf (" Ready to rotate by %f\n",azdeg);
	rot(tr, azdeg); 
	printf (" Done rotating by %f\n",azdeg);

	if (dographics>0) {
	    plotlist[2] = 1;
	    strcpy(fgcolor, "red");
	    setfg_ (fgcolor, strlen(fgcolor));
	    plot_tr(tr, 0.425, 0.65, plotlist, nplt);
	}
	

	if (dographics>0) {
	    /* Save the receiver functions ( Radial + tangential ) */
	    savebutton(0, &yesno);
	  /*  printf ("Save this result? (y/n) ");
	    scanf("%c",&yesno); */
	} else {
	    strcpy(&yesno,"y");
	}

	if (yesno=='y' || yesno=='Y') {

    	    
	    nplt = num_traces(tr);
	    printf ("  ..saving %d to %s via %s\n ",nplt,output_database,line);
	    if ( dbopen ( output_database, "r+", &dbout ) )
		elog_die( 0, "Can't open database %s\n", output_database ) ;

	    dbout = dblookup ( dbout, 0, "wfdisc", 0, 0 ) ;
	    printf ("Last Check of data before save: \n");
	    trwavestats(tr);

	    if ( trsave_wf ( tr, dbout, "t4", outpath , 0) )
		elog_die( 0, "Couldn't save waveforms\n" ) ;
	    dbquery (dbout, dbRECORD_COUNT, &nwf);
	    for (dbout.record=0; dbout.record<nwf; dbout.record ++) {
		wfid = dbnextid(dbout, "wfid");
		dbputv(dbout, 0, "wfid", wfid, 0);
	    }
	}

         /* free the data segments */
        trfree ( tr ) ;
	if (dographics>0) {
	    finitt_();
	    hdkild_();
	}
	return 0;

}


/* count # of traces in tr */
int num_traces(tr)
Dbptr tr;
{
	int ntr, rs, re, bs, be, btype;
	Dbptr bundle;

	dbget_range(tr, &rs, &re);
	ntr = 0;
	for (tr.record = rs; tr.record<re; tr.record++) {
		dbgetv(tr,  0, "bundletype", &btype, 0);
		if (btype==0) {
			ntr += 1;
		} else {
			dbgetv(tr, 0, "bundle", &bundle, 0 );
			ntr += num_traces(bundle);
		}
	}
	return ntr;
}

int
usage()

{
	fprintf (stderr, "usage: trrotd db dbout {orid | arid:azim} sta substr [pf-file-prefix]\n");
	fprintf (stderr, "	orid:  must be in origin table in db\n");
	fprintf (stderr, "	arid:azim  arid gives time, azim gives back-az for rotation\n");
	fprintf (stderr, "	sta:  station; must be in site table in db\n");
	fprintf (stderr, "	substr:  subset string on wfdisc, use doublequote for none \n");
	fprintf (stderr, "	pf-file:  parameter file (default is dbrfcn.pf) May have\n");
	fprintf (stderr, "	tstart, tend, decimate\n");
	fprintf (stderr, " Rotates, detrends, windows, and optionally decimates traces\n");
}
