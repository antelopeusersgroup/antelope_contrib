/* Dumb stacking program for a tr-dataset */

#include "db.h"
#include "tr.h"
#include "tttaup.h"
#include "pf.h"
#include "tttaup.h"
#include "response.h"
#include "coords.h"
#include "stock.h"

extern int num_traces(), tr_stack() ;
main (argc, argv)

int argc;
char **argv;

{
	char *dbname, *output_database;
	char *subsetstr;
	char *outpath = "wfstack/%Y/%j/%{sta}.%{chan}.%Y:%j:%H:%M:%S";
	Pf  *pf;
	int imode;
	Dbptr db, dbout, dbwf, dbwrec, dbo, dbs, dbos;
	Dbptr tr;
	void *pfstr;
	int nwf, ntr=0, nsamp, nsampmax, i, j, nos, *ikeeps;
	double samp0, samprate, hpfreq=-1., lpfreq=-1.;
	double slat, slon, olat, olon, odep, otime;
	double t1, t2, tst, ten, tst0, ten0, del, *ptims, baz;
         float    zero = 0.0 , one = 1.0, xx, yy, y1, y2;
	float summ1, summ2, varm1, varm2;
	float *sumout, *varout, *data;
	float eps = 0.00001;
	int rs, re, bs, be, *plotlist;
	int itran = 0;	/* Portrait */
	Dbptr  bundle;
	int bundletype;
	char chan[10], sta[10], sta0[10];
	char	title[255];
	char	time_str[24], endtime_str[24], *newpath ;
	int doorid=0, idum=5;
	double azmin=0., azmax=360., delmin=0., delmax=180.;

/*
 *	Get command args.
 */
	if (argc<5 ) {
		fprintf(stderr," %d arguments invalid\n",argc);
		usage();
		exit(1);
	}
	dbname = argv[1];
	argc--; argv++;
	output_database = argv[1];
	argc--; argv++;
	subsetstr = argv[1];
	argc--; argv++;
	printf(" Subset using {%s}\n",subsetstr);
	imode = atoi(argv[1]);
	argc--; argv++;

	while ((argc>1) && (argv[1][0]== '-') ) {
	    if (!strncmp(&argv[1][1],"azmin=",6)) {
		doorid=1;
		azmin=atof(&argv[1][7]);
	    }
	    if (!strncmp(&argv[1][1],"azmax=",6)) {
		doorid=1;
		azmax=atof(&argv[1][7]);
	    }
	    if (!strncmp(&argv[1][1],"delmin=",7)) {
		doorid=1;
		delmin=atof(&argv[1][8]);
	    }
	    if (!strncmp(&argv[1][1],"delmin=",7)) {
		doorid=1;
		delmax=atof(&argv[1][8]);
	    }
	    argc--; argv++;
	    idum++;
	}
	if (doorid) printf("Az range %.1f - %.1f Del range %.1f - %.1f\n", azmin,azmax,delmin,delmax);
	  
	if (argc>1) {
            if (pfread(argv[1], &pf)) elog_die(0, " pfread error..");
            if (pfget(pf, "hpfreq", &pfstr)!= PFINVALID) hpfreq = pfget_double(pf, "hpfreq");
            if (pfget(pf, "lpfreq", &pfstr)!= PFINVALID) lpfreq = pfget_double(pf, "lpfreq");
	}
 /*	Open database.
 */
	if (dbopen (dbname, "r+", &db) == dbINVALID) {
		elog_clear_register(1);
		fprintf (stderr, "dbspgram: Unable to open database.\n");
		exit (1);
	}

	/* subset */
	dbwf=dblookup(db, 0, "wfdisc", 0, 0);
	dbquery (dbwf, dbRECORD_COUNT, &nwf);
	printf("%d records in original WF...\n ",nwf);
	dbwf = dbsubset(dbwf, subsetstr, 0);
	dbquery (dbwf, dbRECORD_COUNT, &nwf);
	printf("%d records in subsetted WF...\n ",nwf);

	if (doorid) {
	    dbo=dblookup(db,0, "origin", 0, 0);
	    dbs=dblookup(db,0, "site", 0, 0);
	    dbos=dbtheta(dbo, dbs, 0, 0, 0 ) ;
            if ( dbos.table == dbINVALID )
                 elog_die( 1, "Join fails at table site + origin\n"  ) ;
	    dbquery(dbos, dbRECORD_COUNT, &nos);
		printf("%d records in site-origin jooin...\n ",nos);
	    if ((ptims= (double *)calloc(nos, sizeof(double)) )==0) elog_die(0," calloc failed for %d\n",nos);
	    if ((ikeeps= (int *)calloc(nos, sizeof(int)) )==0) elog_die(0," calloc failed for %d\n",nos);
	    for (i=0; i<nos; i++) {
	  	dbos.record = i;
		dbgetv(dbos, 0, "site.lat", &slat, "site.lon", &slon, 
		    "origin.lat", &olat, "origin.lon", &olon, "depth", &odep, "time", &otime,
		    0);
 	      	slat *= M_PI/180.0;
		slon *= M_PI/180.0;
		olat *= M_PI/180.0;
		olon *= M_PI/180.0;
	    	dist (slat, slon, olat, olon, &del, &baz);
		del *= 180.0/M_PI;
		baz *= 180.0/M_PI;
		if (baz<0.0) baz += 360.;
		ptims[i]=ptime(del, odep) + otime;
		ikeeps[i] = 1;
		if (del<delmin || del>delmax) ikeeps[i] = 0;
		if (baz<azmin || baz>azmax) ikeeps[i] = 0;
	    }

	    for (i=0; i<nwf; i++) {
		dbwf.record = i;
	 	dbgetv(dbwf, 0, "sta", sta, "time", &t1, "endtime", &t2, 0);
		for (j=0; j<nos; j++) {
		    dbos.record=j;
		    dbgetv(dbos, 0, "sta", sta0, 0);
		    if ((!strcmp(sta, sta0)) && t1<ptims[j] && ptims[j]<t2)  {
			if (ikeeps[j]==0) j=nos;
			break;
		    }
		}
		if (j>=nos) dbmark(dbwf);
	    }
	    dbcrunch(dbwf);
	    dbwf.record = dbALL;   
	    dbquery (dbwf, dbRECORD_COUNT, &nwf);
	    printf("%d records in distaz-limited WF...\n ",nwf);
	    

	}
	if (nwf<1) { 
	 	fprintf(stderr," No data\n");
		exit(1);
	}

	/* set up output array */
	dbwrec = dblookup(dbwf, 0, "wfdisc", 0, 0);
	for (dbwrec.record=0; dbwrec.record<nwf; dbwrec.record ++) {
	  dbgetv(dbwrec, 0,
		"nsamp", &nsamp, "samprate", &samprate, 0);
	  if (dbwrec.record==0 || nsamp < nsampmax) nsampmax = nsamp; 
	  if (dbwrec.record==0) samp0 = samprate;
	  if (fabs(samprate-samp0)>eps) {
		fprintf(stderr,"warning Sample rates inconsistent: %f vs %f \n",
			samprate, samp0);
	  }
	}
	/* dbex_evalstr (dbwf, "min(nsamp)", dbINTEGER, &nsampmax); */
	sumout= (float *) malloc(nsampmax*sizeof(float));	
	varout= (float *) malloc(nsampmax*sizeof(float));	
	for (i=0; i<nsampmax; i++ ){ 
		sumout[i]=0.;
		varout[i]=0.;
	}
	/* Get time bounds */
	dbex_evalstr ( dbwf, "min(time)", dbTIME, &tst0 ) ; 
	dbex_evalstr ( dbwf, "max(endtime)", dbTIME, &ten0 ) ;
	sprintf(time_str,"%f",tst0);
	sprintf(endtime_str,"%f",ten0);

	/* Make trace database */
	wfdump(dbwf);

	tr = dbinvalid();

	if (trload_css ( dbwf, time_str, endtime_str, &tr, 0, 0) < 0) 
		elog_die( 0, "Problems loading traces\n") ;

	/* printf ("  Retrieved %d traces\n",num_traces(tr)); */
	trdump(tr);

	/* Do the stacking */
	dbget_range(tr, &rs, &re);
	if (re - rs < 2 ) elog_die(0," Not enough records to be worthwhile");
	for (tr.record = rs; tr.record < re; tr.record++)  {
	
	    dbgetv(tr, 0,
	       "bundletype", &bundletype,
	       0);
	    if (bundletype != 0) elog_die(0,"trstack: bundletype != 0");
	    dbgetv(tr, 0, 
		   "nsamp", &nsamp,
		   "data", &data,
		   0);

	    if (hpfreq>0. || lpfreq>0.) filtrec(nsamp, samprate, data, hpfreq, lpfreq);

	    for (i=0; i<nsampmax; i++) {
	  	sumout[i] += data[i];
		varout[i] += data[i]*data[i];
	    }
	    ntr += 1;
	}
	if (ntr==0) elog_die(0," Number of processed traces = 0");
	printf(" Processed %d traces \n",ntr);
	summ1 = 0.;
	summ2 = 0.;
	for (i=0; i<nsampmax; i++) {
	  	sumout[i] /= (float)ntr;
		summ1 += varout[i]/(float)(ntr);
		if (i<nsampmax/2) summ2 += varout[i]/(float)(ntr);
		if (ntr>1) {
			varout[i] = sqrt((varout[i]-sumout[i]*sumout[i]*(float)ntr)/(float)(ntr-1));
		} else {
			varout[i]=0.;
		}
		if (i==0) {
			y1 = sumout[0]-varout[0];
			y2 = sumout[0]+varout[0];
		} else {
        		if (sumout[i]-varout[i]<y1) y1 = sumout[i]-varout[i];
        		if (sumout[i]+varout[i]>y2) y2 = sumout[i]+varout[i];
		}
	}
	/* Plot raw */
	if (imode != 1) {
	  sprintf(title,"db: %s rule: ( %s ) Az: %.1f - %.1f Del: %.1f - %.1f", dbname, subsetstr, azmin, azmax, delmin, delmax);
	   init_plot(itran, 0.9, "trstack", "trstack", title);
	   plotlist = (int *)malloc(ntr*sizeof(int));
	  for (i=0; i<ntr; i++) plotlist[i]=1;
	  plot_tr(tr, 0.5, 0.9, plotlist, ntr);


	    /* plot stack */
	    plot_stack(0.3, 0.4, sumout, varout, nsampmax, samp0);
	}

	/* Calculate mean values */
	varm1 = 0.;
	varm2 = 0.;
	for (i=0; i<nsampmax; i++) {
		varm1 += varout[i]*varout[i];
		if (i<nsampmax/2)  varm2 += varout[i]*varout[i];
	}
	summ1 = sqrt(summ1/(float)nsampmax);
	varm1 = sqrt(varm1/(float)nsampmax);
	summ2 = sqrt(summ2/(float)(nsampmax/2));
	varm2 = sqrt(varm2/(float)(nsampmax/2));
	printf(" total variance %f  normalized= %.4f  Yrange: %f - %f\n",varm1, varm1/summ1, y1, y2);
	printf(" 50%% variance %f  normalized= %.4f\n",varm2, varm2/summ2);

	/* Kluge with output: 1st is stack, 2nd is stdEr... */
	for (tr.record = rs+2; tr.record < re; tr.record++) dbmark(tr);
	dbcrunch(tr);
	tr.record = rs;
	dbgetv(tr, 0, 
		"sta", sta,
		"chan", chan,
		"data", &data,
		"time", &t1,
		0);
	for (i=0; i<nsampmax; i++) data[i]=sumout[i];
	t2 = t1 + (double)(nsampmax-1)/samprate;
	strcat(chan,"_s");
	dbputv(tr, 0, "chan", chan, 
		"endtime", t2, 
		"nsamp", nsampmax, 
		0);

	tr.record +=1;
	dbgetv(tr, 0, 
		"chan", &chan,
		"data", &data,
		0);
	for (i=0; i<nsampmax; i++) data[i]=varout[i];
	strcat(chan,"_e");
	dbputv(tr, 0, "sta", sta,
		"chan", chan, 
		"time", t1, 
		"endtime", t2, 
		"nsamp", nsampmax, 
		0);

	    if ( dbopen ( output_database, "r+", &dbout ) )
		elog_die( 0, "Can't open database %s\n", output_database ) ;

	    dbout = dblookup ( dbout, 0, "wfdisc", 0, 0 ) ;
	    /* trwfname(tr, 0, &newpath); DOES NOTHING */
	    /* if ( trsave_wf ( tr, dbout, 0, "rfstack" ) ) */
	    if ( trsave_wf ( tr, dbout, "t4", outpath, 0 ) )
		elog_die( 0, "Couldn't save waveforms\n" ) ;


	if (imode != 1) killbutton(itran);  
         trfree ( tr ) ;
	dbfree(db);
	return 0;
}




/* count # of traces in tr IF Bundletype =1 */
int num_traces(tr)
Dbptr tr;
{
	int ntr, rs, re, bs, be, bundletype;
	Dbptr bundle;

	dbget_range(tr, &rs, &re);
	ntr = 0;
	printf(" num_tr outer range %d - %d \n",rs, re);
	dbgetv(tr, 0, "bundletype", &bundletype, 0 );

	if (bundletype == 0) {
	    ntr = re - rs;
	} else {

	    for (tr.record = rs; tr.record<re; tr.record++) {
	    	dbgetv(tr, 0, "bundle", &bundle, 0 );
	   	 dbget_range(bundle, &bs, &be);
	   	 ntr += be - bs;
	    }
	}
	return (ntr);
}


int
usage()

{
	fprintf (stderr, "usage: trstack dbin dbout subset imode [-azmin=xx] [-azmax=xx] [-delmin=xx] [-delmax=xx] [pf-file]\n");
	fprintf (stderr, "	subset:  logical expression for subsetting wfdisc\n");
	fprintf (stderr, "	imode:  0 - regular, 1 - nographics\n");
	fprintf (stderr, "	pf-file settings: hpfreq, lpfreq\n");
}


int 
wfdump(db)
Dbptr db;
{
	int n, i, nsamp, wfid;
	char sta[10], chan[10];
	double time, endtime, samprate;

	dbquery (db, dbRECORD_COUNT, &n);
	for (db.record=0;db.record<n;db.record++) {
	  dbgetv(db, 0,
	    "sta", sta, "chan", chan,
	    "time", &time, 
	    "nsamp", &nsamp,
	    "samprate", &samprate,
	    "endtime", &endtime,
	    "wfid", &wfid, 
		0);
	  printf("%d: %s %s time:%.3f endtime:%.3f nsamp:%d samprate:%.5f wfid:%d\n",
		db.record, sta, chan, time, endtime, nsamp, samprate, wfid);
	}
}
int 
trdump(tr)
Dbptr tr;
{
	int n, i, nsamp, wfid, bundletype, rs, re;
	char sta[10], chan[10];
	double time, endtime, samprate;
	Dbptr bundle;

	dbget_range(tr, &rs, &re);
	for (tr.record = rs; tr.record < re; tr.record ++) {
	dbgetv(tr, 0,
	       "bundletype", &bundletype,
	       0);

	switch (bundletype)
	  {
	  case 0:
	    dbgetv(tr, 0,
	    "sta", sta, "chan", chan,
	    "time", &time, 
	    "nsamp", &nsamp,
	    "samprate", &samprate,
	    "endtime", &endtime,
	    "wfid", &wfid, 
		0);
	  printf("%d: %s %s time:%.3f endtime:%.3f nsamp:%d samprate:%.5f wfid:%d\n",
		tr.record, sta, chan, time, endtime, nsamp, samprate, wfid);
	    break;
	  default:
	    dbgetv(tr, 0, "bundle", &bundle, 0);
	    trdump(bundle);
	    break;
	  }
	}
}
