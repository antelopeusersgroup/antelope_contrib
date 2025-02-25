#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "tr.h"
#include "db.h"
#include "stock.h"

typedef struct Flags { 
    unsigned int autodrm : 1 ;
    unsigned int z_channel_only : 1 ;
    unsigned int percentage : 1 ;
    unsigned int total : 1 ;
    unsigned int seconds : 1 ;
    unsigned int channels : 1 ;
    unsigned int stations : 1 ;
    unsigned int networks : 1 ;
} Flags ;

typedef struct By_network { 
    char snet[16] ;
    long nsta ; 
    long nchan ; 
    double missing ; 
} By_network ; 

char *Gap_database = 0 ; 
Dbptr Gap_db = INVALID_DBPTR ;
double Range, Stop, Start ; 

#if DEBUG
void
print_list ( char *msg, Tbl *list) 
{
    long i ; 
    printf ( "\n%d %s\n", maxtbl(list), msg ) ;
    for (i=0 ; i<maxtbl(list) ; i++ ) { 
	char *entry ; 
	entry = gettbl(list, i) ; 
	printf ( "   %s\n", entry ) ; 
    }
    printf ( "\n" ) ; 
}
#endif


static void
usage ()
{
    fprintf (stderr, "\nUsage: %s [-AcCpStz] [-n network] [-d gaps-db] [-P maxpts] [-s subset] [-x deployment_subset] database start end\n", 
	    Program_Name);
    banner (Program_Name, 0) ;
    exit (1);
}

static char *
tdel ( double gap_time, Flags flags ) 
{
    static char string[96] ; 
    char *s ;
    if ( flags.seconds ) { 
	sprintf ( string, "%7.1f seconds", gap_time ) ; 
    } else { 
	s = strtdelta(gap_time) ; 
	strcpy ( string, s ) ; 
	free(s) ;
    }
    s = string ;
    return s ;
} 

static void
print_autodrm_header (double start, double stop)
{
    char *start_str, *stop_str ;
    start_str = epoch2str(start, "%Y/%m/%d %H:%M:%S.%s") ;
    stop_str = epoch2str(stop, "%Y/%m/%d %H:%M:%S.%s") ;
    fprintf ( stdout, "Report period from %s to %s\n", start_str, stop_str ) ; 
    free(start_str) ;
    free(stop_str) ;

    fprintf(stdout, 
"Net       Sta  Chan Aux      Start Date Time          End Date Time        Duration Comment\n" ) ;
}

static void
print_gap ( char *stachan, double start, double stop, Flags flags, 
		double *gap_time, double *max_gap ) 
{

    if ( flags.autodrm ) { 
	char *comment = "" ;
	char sta[MAX_STA_SIZE], *chan, anet[MAX_STA_SIZE], fsta[MAX_STA_SIZE], fchan[MAX_CHAN_SIZE], aux[MAX_CHAN_SIZE] ; 
	char *start_str, *stop_str ;

	strcpy(sta, stachan ) ; 
	chan = strchr(sta, ':') ; 
	*chan++ = 0 ;

	autodrm_net ( sta, anet, fsta ) ;
	autodrm_aux ( sta, chan, fchan, aux ) ;

	start_str = epoch2str(start, "%Y/%m/%d %H:%M:%S.%s") ;
	stop_str = epoch2str(stop, "%Y/%m/%d %H:%M:%S.%s") ;
	fprintf ( stdout, "%-9.9s %-5.5s %-3.3s %-4.4s %s %s %10.3f %s\n",
		anet, fsta, fchan, aux, 
		    start_str, stop_str, stop-start, comment );
	free(start_str) ; 
	free(stop_str) ; 
    } else if ( ! flags.total ) { 
	char *s ;
	fprintf ( stdout, "%-15s %s %s\n", 
	    stachan, s=strydtime(start), tdel(stop-start, flags) ) ; 
	free(s) ; 
    }
    if ( Gap_db.database >= 0 ) { 
	char sta[MAX_STA_SIZE], *chan, *s1, *s2 ;
	strcpy(sta, stachan ) ; 
	chan = strchr(sta, ':') ; 
	*chan++ = 0 ;
	if ( dbaddv(Gap_db, 0, 
	    "sta", sta, 
	    "chan", chan, 
	    "time", start, 
	    "tgap", stop-start,
	    NULL ) < 0 ) { 
	    die (0, "Can't add new record for %s:%s at %s for %s\n", 
	    	sta, chan, s1=strydtime(start), s2=strtdelta(stop-start)) ; 
	    free(s1) ; 
	    free(s2) ;
	}
    }
    if ( stop-start > *max_gap ) { 
	*max_gap = stop-start ;
    }
    if ( stop-start > 0 ) { 
	*gap_time += ABS(stop-start) ;
    }
}

static void
show_gaps ( char *stachan, Dbptr tr, 
	Flags flags, double *gap_time, double *last_time, double *max_gap ) 
{
    long n ;

    dbquery ( tr, dbRECORD_COUNT, &n )  ; 
    if ( n > 0 ) { 
	double time, time1, samprate, samprate1, te ;
	long nsamp, nsamp1 ; 
	tr.record = 0 ;
	dbgetv ( tr, 0, "time", &time, "samprate", &samprate, 
	    "nsamp", &nsamp, NULL ) ; 
	if ( (time-*last_time)*samprate > 1.5) { 
	    print_gap(stachan, *last_time, time, flags, gap_time, max_gap ) ; 
	}

	for ( tr.record = 1 ; tr.record < n ; tr.record++ ) { 
	    dbgetv ( tr, 0, "time", &time1, "samprate", &samprate1, 
		"nsamp", &nsamp1, NULL ) ; 
	    if ( ! TRCONTIGUOUS(time, time1, samprate, nsamp) && 
	    	time1 > SAMP2TIME(time,samprate,nsamp) ) {
		*last_time = te = SAMP2TIME(time,samprate,nsamp) ;
		print_gap(stachan, te, time1, flags, gap_time, max_gap ) ; 
	    }
	    time = time1 ;
	    nsamp = nsamp1 ; 
	    samprate = samprate1 ;
	}

	*last_time = te = SAMP2TIME(time,samprate,nsamp) ;
    }
}

static void
print_stachan_gap ( char *next_stachan, double gap_time, Flags flags ) 
{
    double perfval = -1;

    if ( ! flags.autodrm && gap_time > 0.0 ) { 
	fprintf (stdout, "    %-15s  %s\n", next_stachan, 
	    tdel(gap_time, flags)) ;
    }
    if ( Gap_db.database >= 0 ) { 
	char sta[MAX_STA_SIZE], *chan ; 
	strcpy(sta, next_stachan ) ; 
	chan = strchr(sta, ':') ; 
	*chan++ = 0 ;
	if( Range != 0 ) {
	    perfval = 100.0 - gap_time*100./Range;
	}
	if ( dbaddv(Gap_db, "chanperf", 
	    "sta", sta,
	    "chan", chan,
	    "time", Start, 
	    "endtime", Stop, 
	    "perf", perfval,
	    NULL ) < 0 ) { 
	    complain (0, "Couldn't add record to chanperf table\n") ;
	}
    }
}

Dbptr 
find_this_stachan ( Dbptr dbg, int ngroup, char *this_sta, char *this_chan ) 
{ 
    Dbptr bundle ;
    char sta[MAX_STA_SIZE], chan[MAX_CHAN_SIZE] ;
    for ( dbg.record = 0 ; dbg.record < ngroup ; dbg.record++ ){
	insist(dbgetv ( dbg, 0, 
		"bundle", &bundle, "sta", sta, "chan", chan, NULL ) == 0 ) ; 
	if ( strcmp(sta, this_sta) == 0 
	    && strcmp(chan, this_chan) == 0 ) { 
	    return bundle ;
	}
    }
    return dbinvalid() ;
}

int
main (int argc, char **argv)
{
    int             c,
                    errflg = 0;
    Dbptr           dbwanted, dbwfdisc, dbg, bundle, tr ;
    Dbvalue	   dbwanted_dbname;
    char           *table;
    Arr		   *wanted, *wanted_sta, *reporting_stations, *reporting_channels, *down ;
    char	   *start_str, *stop_str ;
    double	    start, stop ; 
    double 	    mintime;
    int		    errors = 0 ;
    Tbl		   *stachan_list, *sta_list, *reporting_list, *channels_list ;
    long	    i, result, nwanted, ngroup ;
    char	    sta[MAX_STA_SIZE], chan[MAX_CHAN_SIZE] ;
    char 	   time_subset[STRSZ] ;
    Tbl		   *keys ;
    Flags	    flags ;
    char	   *subset = 0 ; 
    char	   *deploy_subset = 0 ; 
    long	    index ;
    char	   *this_stachan, *this_sta, *this_chan ;
    double	   gap_time = 0.0, total_gap_time=0.0, max_gap=0.0 ;
    double	   max_period = 10 ; 
    long	   maxpts = 40 * 86400 ;
    long	   nchannels = 0, nstations = 0, nreporting = 0 ;
    char	  *netcode = 0 ; 
    Arr 	  *counted, *networks ;
    By_network    *netcount ; 
    char          *anet ; 
    // Tbl		  *prtfields ;

    elog_init ( argc, argv ) ; 
    elog_set(ELOG_MAXMSG, 0, 0) ; 
    memset ( &flags, 0, sizeof(flags)) ;

    while ((c = getopt (argc, argv, "AcCd:Nn:P:pSs:tVx:z")) != -1) {
	switch (c) {

	case 'A':
	    flags.autodrm = 1 ; 
	    break ;

	case 'c':
	    flags.stations = 1 ; 
	    break; 

	case 'C':
	    flags.channels = 1 ; 
	    break; 

	case 'd':
	    Gap_database = optarg ; 
	    break ;

	case 'N':
	    flags.networks = 1 ; 
	    break ;

	case 'n':
	    netcode = optarg ; 
	    break ;

	case 'P':
	    maxpts = atol(optarg) ; 
	    break ;

	case 'p':
	    flags.percentage = 1 ; 
	    break ;

	case 'S':
	    flags.seconds = 1 ;
	    break ;

	case 's':
	    subset = optarg ; 
	    break ;

	case 't':
	    flags.total = 1 ;
	    break ;

	case 'x':
	    deploy_subset = optarg ; 
	    break ;

	case 'z':
	    flags.z_channel_only = 1 ; 
	    break ;

	case 'V':
	    banner (Program_Name, 0) ;
	    exit (0);

	default:
	    errflg++;
	    break ;
	}
    }

    if (errflg || argc-optind != 3)
	usage ();

    table = argv[optind++];
    result = dbopen_database (table, "r", &dbwanted);
    switch (result) {

      case dbINVALID:
	die (0, "Can't open input table '%s'", table);
	break;

      case 0:
	if ( dbwanted.table < 0 ) { 
	    dbwanted = dblookup ( dbwanted, 0, "sensor", 0, 0 ) ; 
	    dbquery ( dbwanted, dbRECORD_COUNT, &nwanted ) ; 
	    if ( nwanted == 0 ) { 
		die (0, "No records in sensor table for '%s'", table);
	    }
	} else { 
	    die (0, "No records in input table '%s'", table);
	}
	break;
    }
    if( dbquery( dbwanted, dbDATABASE_NAME, &dbwanted_dbname ) < 0 ) {

    	elog_die ( 0, "Couldn't query '%s' for base database name\n", table );
    }
    if ( Gap_database != 0 ) { 
	if ( ! strcmp( dbwanted_dbname.t, Gap_database ) ) {
    		elog_die ( 0, "Gaps database '%s' cannot be the same as input database '%s'\n", Gap_database, dbwanted_dbname.t );
	} 
	if ( ! is_present ( Gap_database ) ) { 
	    char *schema_name ;
	    dbquery(dbwanted, dbSCHEMA_NAME, &schema_name) ; 
	    if ( dbcreate(Gap_database, schema_name, 0, 0, 0, 0, 0)) { 
		elog_die ( 0, "Can't create new database %s with schema %s", 
			Gap_database, schema_name ) ; 
	    }
	}
	if ( dbopen ( Gap_database, "r+", &Gap_db ) ) { 
	    die (0, "Can't open gap database %s", Gap_database ) ; 
	}
	Gap_db = dblookup (Gap_db, 0, "gap", 0, 0 ) ; 
    }

    finit_db(dbwanted) ; 

    start_str = argv[optind++] ; 
    stop_str = argv[optind++] ; 
    start = str2epoch(start_str) ; 
    stop = str2epoch(stop_str) ; 
    mintime = str2epoch("1/1/1980") ; 
    if( stop >= mintime && stop <= start ) {
    	die (0, "End time must be greater than start time (or alternatively an interval number of seconds past the start time)" );
    }
    if ( stop < start ) { 
	stop += start ; 
    }
    Start = start ;
    Stop = stop ; 
    Range = stop - start ;

    sprintf( time_subset, 
	"(time < %.5f && endtime >= %.5f)||(%.5f <= time && %.5f > time)",
        start, start, start, stop ) ; 

    dbwanted = dbsubset ( dbwanted, time_subset, 0 ) ; 
    dbquery ( dbwanted, dbRECORD_COUNT, &nwanted ) ; 
    if ( nwanted == 0 ) { 
	die (0, "No channels running during specified time range" ) ;
    }

    if ( subset ) { 
	dbwanted = dbsubset(dbwanted, subset, 0 ) ; 
	dbquery ( dbwanted, dbRECORD_COUNT, &nwanted ) ; 
	if ( nwanted == 0 ) { 
	    die (0, "No records after subset '%s'", subset);
	}
    }

    if ( netcode ) { 
	Dbptr dbnetsta ; 
	char net_subset[256] ;
	dbnetsta = dblookup(dbwanted, 0, "snetsta", 0, 0) ; 
	dbwanted = dbjoin(dbwanted, dbnetsta, 0, 0, 0, 0, 0) ; 
	sprintf(net_subset, "snet =~ /%s/", netcode ) ; 
	dbwanted = dbsubset(dbwanted, net_subset, 0 ) ; 
	dbquery ( dbwanted, dbRECORD_COUNT, &nwanted ) ; 
	if ( nwanted == 0 ) { 
	    die (0, "No records after network subset '%s'", net_subset);
	}
    }

    if ( deploy_subset ) { 
	Dbptr dbdeploy ; 
	dbdeploy = dblookup(dbwanted, 0, "deployment", 0, 0) ; 
	dbwanted = dbjoin(dbwanted, dbdeploy, 0, 0, 0, 0, 0) ; 
	dbwanted = dbsubset(dbwanted, deploy_subset, 0 ) ; 
	dbquery ( dbwanted, dbRECORD_COUNT, &nwanted ) ; 
	if ( nwanted == 0 ) { 
	    die (0, "No records after deployment subset '%s'", deploy_subset);
	}
    }

    if ( flags.z_channel_only ) { 
	dbwanted = dbsubset(dbwanted, "chan=~/.*[zZ]|..[zZ].*/", 0) ; 
	dbquery ( dbwanted, dbRECORD_COUNT, &nwanted ) ; 
	if ( nwanted == 0 ) { 
	    die (0, "No records after z channel subset");
	}
    }

    wanted = newarr(0) ;
    wanted_sta = newarr(0) ;
    counted = newarr(0) ;
    networks = newarr(0) ; 
    down = newarr(0) ;
    for (dbwanted.record = 0 ; dbwanted.record < nwanted ; dbwanted.record++ ) { 
	char snet[MAX_STA_SIZE], fsta[MAX_STA_SIZE] ; 
	dbgetv(dbwanted, 0, "sta", sta, "chan", chan, NULL ) ; 
	seed_net ( sta, snet, fsta ) ;
	netcount = getarr(networks, snet) ; 
	if ( netcount == 0 ) { 
	    allot (By_network *, netcount, 1) ; 
	    memset(netcount, 0, sizeof(*netcount)) ;
	    strcpy(netcount->snet, snet) ; 
	    setarr(networks, snet, netcount) ; 
	}
	if ( getarr(counted, sta) == 0 ) { 
	    netcount->nsta++ ; 
	    setarr(counted, sta, (char *) -1) ; 
	    setarr(down, sta, (char *) -1) ;
	}
	netcount->nchan++ ;
#if DEBUG
	printf ( "%2s %8s %2d %3s %2d\n", 
		snet, sta, netcount->nsta, chan, netcount->nchan ) ; 
#endif
	setarr ( wanted_sta, sta, strdup(snet)) ; 
	strcat ( sta, ":" ) ; 
	strcat ( sta, chan ) ;
	setarr ( wanted, sta, (char *) -1 ) ; 
    }
    stachan_list = keysarr(wanted) ;
    nchannels = maxtbl(stachan_list) ; 
    sta_list = keysarr(wanted_sta) ; 
    nstations = maxtbl(sta_list) ;
#ifdef DEBUG
    print_list("wanted stations", sta_list) ;
#endif

    dbwfdisc = dblookup ( dbwanted, 0, "wfdisc", 0, 0 ) ; 
    dbwfdisc = dbsubset ( dbwfdisc, time_subset, 0 ) ; 
    keys = strtbl("sta", "chan", "time", NULL ) ; 
    dbwfdisc = dbsort ( dbwfdisc, keys, 0, 0 ) ; 
    poptbl(keys) ; 

    // prtfields = strtbl("sta", "chan", "time", "endtime", NULL ) ;
    // dbselect_hdr (dbwfdisc, prtfields, stdout) ;
    // dbselect ( dbwfdisc, prtfields, stdout) ;
    dbg = dbgroup ( dbwfdisc, keys, 0, 1 ) ; 
    dbquery ( dbg, dbRECORD_COUNT, &ngroup ) ; 


    if ( flags.autodrm ) { 
	print_autodrm_header (start, stop) ; 
    }

    reporting_stations = newarr(0) ;
    reporting_channels = newarr(0) ;
    tr = trnew(0,0) ;
    tr = dblookup (tr, 0, "trace", 0, 0)  ;
    // working through list of channels wanted ?
    for ( index = 0 ; index < maxtbl(stachan_list) ; index++ ) { 
	this_stachan = gettbl(stachan_list, index) ; 
	this_sta = strdup(this_stachan) ; 
	this_chan = strchr(this_sta, ':') ; 
	insist(this_chan != 0) ;
	*this_chan++ = 0 ;
	bundle = find_this_stachan ( dbg, ngroup, this_sta, this_chan ) ; 

	if ( bundle.record < 0 ) { 
	    print_gap ( this_stachan, start, stop, flags, &gap_time, &max_gap ) ; 
	    print_stachan_gap ( this_stachan, gap_time, flags ) ;

	} else { 
	    double t0, t1, last_time, samprate ; 
	    char t0_str[32], t1_str[32] ;

	    gap_time = 0.0 ;

	    setarr ( reporting_stations, this_sta, (char *) -1 ) ;
	    delarr ( down, this_sta ) ;  
	    setarr ( reporting_channels, this_stachan, (char *) -1 ) ;

	    last_time = start ;
	    dbgetv ( bundle, 0, "samprate", &samprate, NULL ) ;
	    max_period = maxpts / samprate ; 
	    for ( t0 = start, t1 = MIN(start+max_period, stop) ; 
	    	  t1 != t0 ;
		  t0 = t1, t1 = MIN(t0+max_period, stop) )  {
		long mark ; 

		sprintf ( t0_str, "%.8f", t0 ) ;
		sprintf ( t1_str, "%.8f", t1 ) ;
		mark = elog_mark() ;
		if ( trload_cssgrp ( bundle, t0_str, t1_str, &tr, 0, 0 ) < -1) { 
		    complain ( 0, "Can't load trace data for %s", this_stachan ) ;
		    errors++ ;
		} 
		elog_truncate(mark) ;
		insist(trsplit(tr, 0, 0 ) ==0); 
		insist(trsplice(tr, trTOLERANCE, 0, 0 )==0) ; 
		insist(trsplit(tr, 0, 0 ) ==0); 
		show_gaps ( this_stachan, tr, flags, &gap_time, &last_time, &max_gap ) ; 
		insist(trtruncate(tr, 0) ==0);
	    }
	    if ( (stop-last_time) * samprate > 1.5 ) { 
		print_gap ( this_stachan, last_time, stop, flags, &gap_time, &max_gap ) ; 
	    }

	    print_stachan_gap ( this_stachan, gap_time, flags ) ;

	}

	anet = getarr(wanted_sta, this_sta) ; 
	if ( anet == 0 ) { 
	    printf ( "anet sta = %s\n", this_sta ) ; 
	    die(0, "should never happen\n" ) ; 
	}
	netcount = getarr(networks, anet) ;
	if ( netcount == 0 ) { 
	    printf ( "netcount anet=%s sta = %s\n", anet, this_sta ) ; 
	    die(0, "should never happen #2\n" ) ; 
	}
	free(this_sta) ; 

	netcount->missing += gap_time ; 
	total_gap_time += ABS(gap_time) ;

	gap_time = 0.0 ;
    }

    reporting_list = keysarr(reporting_stations) ; 
#ifdef DEBUG
    print_list("reporting stations", reporting_list) ;
#endif
    nreporting = maxtbl(reporting_list) ;

    channels_list = keysarr(reporting_channels) ; 

    if ( flags.percentage ) { 
	double total ;
	total = (stop - start) * nchannels ;
	fprintf ( stdout, "Missing average of %s of data from %ld channels (%ld/%ld stations reporting)\n", 
		    tdel(total_gap_time/nchannels, flags), nchannels, nreporting, nstations ) ; 

	fprintf ( stdout, "%4.2f%% data recovered over %s\n", 
		100.0 * (total - total_gap_time)/total, 
		tdel(stop-start, flags) ) ; 

	fprintf ( stdout, "Largest gap = %s\n", tdel(max_gap, flags) ) ; 

    }

    if ( flags.stations ) { 
	fprintf ( stdout, "\n> Reporting stations\n>" ) ;
	for ( i=0 ; i<maxtbl(reporting_list) ; i++ ) { 
	    char *asta ;
	    if ( i % 5 == 0 ) { 
		fprintf ( stdout, "\n>\t" ) ; 
	    }
	    asta = gettbl(reporting_list, i) ; 
	    fprintf ( stdout, " %-8s", asta ) ; 
	}
	fprintf ( stdout, "\n" ) ; 
    }

    if ( flags.channels ) { 
	fprintf ( stdout, "\n>> Reporting channels\n>>" ) ;
	for ( i=0 ; i<maxtbl(channels_list) ; i++ ) { 
	    char *achan ;
	    if ( i % 4 == 0 ) { 
		fprintf ( stdout, "\n>>\t" ) ; 
	    }
	    achan = gettbl(channels_list, i) ; 
	    fprintf ( stdout, " %-15s", achan ) ; 
	}
	fprintf ( stdout, "\n" ) ; 
    }

    if ( flags.networks ) { 
	Tbl *list ; 
	double range ; 
	long i ;
	range = stop - start ; 
	fprintf ( stdout, "\n> Network Performance %10.0f seconds\n", 
		range ) ; 
	list = keysarr(networks) ; 
	for (i=0 ; i<maxtbl(list) ; i++ ) { 
	    anet = gettbl(list, i) ; 
	    netcount = getarr(networks, anet) ;
	    fprintf ( stdout, "%% %s %4ld sta %4ld chan %10.0f seconds  %7.2f%%\n", 
	    	netcount->snet, netcount->nsta, netcount->nchan, netcount->missing, 
		100.0 - netcount->missing*100. /(netcount->nchan * range) ) ; 
	    if ( Gap_db.database >= 0 ) { 
		if ( dbaddv(Gap_db, "netperf", 
		    "snet", netcount->snet, 
		    "time", start, 
		    "endtime", stop, 
		    "npsta", netcount->nsta, 
		    "npchan", netcount->nchan, 
		    "perf", 100.0 - netcount->missing*100. /
				(netcount->nchan * range), 
		    NULL ) < 0 ) { 
		    complain (0, "Couldn't add record to netperf table\n") ;
		}
	    }
	}
	freetbl(list, 0) ; 

	list = keysarr(down) ; 
	if ( maxtbl(list) > 0 ) { 
	    printf ( "\nStations down:\n" ) ; 
	    for (i=0 ; i<maxtbl(list) ; i++ ) { 
		char *asta ;
		asta = gettbl(list, i) ; 
		fprintf(stdout, "# %s\n", asta ) ; 
	    }
	}
	freetbl(list, 0) ;
    }
		
    trdestroy(&tr) ; 
    freetbl(stachan_list, 0) ;
    freearr(wanted, 0) ;
    freetbl(sta_list, 0) ;
    freearr(wanted_sta, free) ;
    freearr(reporting_stations, 0) ;
    freetbl(reporting_list, 0) ;
    freetbl(keys,0) ;
    freearr(counted, 0) ; 
    freearr(down, 0) ; 
    freearr(networks, free) ; 
    dbclose (dbwanted) ;

    return (errors == 0 ? 0 : 1);
}
