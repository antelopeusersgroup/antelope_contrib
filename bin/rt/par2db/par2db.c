/* @(#)par2db.c	1.2 12/30/96  */

#include "par2db.h"
#include "header.h"

FILE *F1;
int DCSP = 0;
int GPS_COOR = 0;
FILE *fplcmd;

int Verbose = 0;
int DINTV = 1;

Source *
new_source ( int maxpkts )
{
    Source *source ;

    allot ( Source *, source, 1 ) ; 
    source->apipe = new_orbpipe (maxpkts);
    source->last = 0.0 ; 
    return source ; 
}

double 
get_last_dbtimes (Dbptr db)
{
    Tbl *keys ; 
    int nstachan ;
    Dbptr dbs, dbg ; 
    Expression *expr ;
    double after, min_after ;
    char sta[25], chan[25] ;

    dbquery ( db, dbRECORD_COUNT, &nstachan ) ; 
    if ( nstachan < 1 ) 
	return 0.0 ; 

    keys = strtbl( "sta", "chan", 0 ) ; 
    dbs = dbsort ( db, keys, 0, 0 ) ; 
    dbg = dbgroup ( dbs, keys, 0, 1 ) ; 

    min_after = VERY_LARGE_DOUBLE ;
    dbquery ( dbg, dbRECORD_COUNT, &nstachan ) ; 
    dbex_compile ( dbg, "max(endtime)", &expr, dbREAL ) ;
    for ( dbg.record = 0 ; dbg.record < nstachan ; dbg.record++ ) {
	dbgetv ( dbg, 0, "sta", &sta, "chan", &chan, 0 ) ; 
	if ( dbex_eval(dbg, expr, 0, &after ) < 0 ) 
	    die ( 0, "failed to read maximum endtime for %s_%s\n", 
		sta, chan ) ; 
	min_after = MIN(after, min_after) ; 
    }

    dbex_free ( expr ) ; 
    freetbl(keys, 0) ;
    return min_after ;
}


int
main (argc, argv)
int             argc;
char          **argv;
{

    extern char    *optarg;
    extern int      optind;
    int             done = 0;
    char           *orbname = 0;
    int             errreap, c,
                    errflg = 0;
    int             rdorb;
    int             nselect,
                    pktid;
    char	   *after_str, *window_str ;
    double	    window ;
    double          after = 0.0, until=VERY_LARGE_DOUBLE;
    char           *match = ".*/CBBHS";
    int             version;
    Dbptr           db;
    char           *database;
    int             maxpkts;
    Save_params     params;
    char           *packet=0;
    char           *s, lcmdfile[64];
    int             nbytes, bufsize = 0;
    double          pkttime;
    char            srcname[ORBSRCNAME_SIZE];
    double	    eps = .001 ;
    Arr            *sources ;
    Source	   *asource;
    Ste 	   *site;
    int		    start_pktid = -1 ;
    int 	   i,
    	           nsid,
		   dinterval = 0,
		   id;
    static Packet *unstuffed=0 ;
    
    
    Program_Name = argv[0];
    elog_init (argc, argv);
    elog_notify ( 0, "%s Version Version 1.6 10/28/96\n", Program_Name ) ; 

    DiagPar = 0;
    params.memsize = 60.0;
    params.segment_size = 86400.0;
    params.wfname = 0;
    strcpy (params.datatype, "s4");
    params.gapmax = 120.0 ;
    maxpkts = 5;
    
    while ((c = getopt (argc, argv, "cgm:i:u:Vvw:")) != -1)
	switch (c) {

	case 'c':
	    DCSP = 1;
	    break;

	case 'g':
	    GPS_COOR = 1;
	    break;

	case 'm':
	    match = optarg;
	    break;

	case 'i':
	    DINTV = atoi(optarg);
	    break;

	case 'u':
	    until = str2epoch ( optarg ) ; 
	    break ;

	case 'V':
	    usage ();
	    break;

	case 'v':
	    Verbose++;
	    break;

	case 'w':
	    params.wfname = optarg;
	    break;

	case '?':
	    errflg++;
	}

    params.datacode = trINT;
    params.gap_value = TRGAP_VALUE_S4 ; 
    unstuffed = newpkt();


    /*
    F1 = fopen( "pardump", "w" );
*/ 
    if (errflg || argc - optind < 2 || argc - optind > 4)
	usage ();

    if ( params.memsize < 1. )
	die ( 0, "memory buffer size = %f seconds, but must be more than 1 second\n", 
	    params.memsize ) ;

    orbname = argv[optind++];
    if ((rdorb = orbopen (orbname, "r")) < 0)
	die (0, "Can't open ring buffer '%s'\n", orbname);

    if( DCSP )  
        match = ".*/BSP";
    if (match) {
	if ((nselect = orbselect (rdorb, match)) < 0)
	    die (1, "orbselect '%s' failed\n", match);
    }

    database = argv[optind++];
    if (dbopen_database (database, "r+", &db) < 0)
	die (0, "Can't open output database %s\n", database);
    if (db.table < 0) {
	db = dblookup (db, 0, "wfdisc", 0, 0);
	if (db.table < 0)
	    die (0, "Can't open output table '%s'\n", database);
    }
    params.db = db ;

    sources = newarr (0);

    if (argc - optind >= 1) {
	after_str = argv[optind++] ; 
	if (!strcmp (after_str, "now") == 0) {
	    after = str2epoch (after_str);
	} else {
	    if (orbget (rdorb, ORBCURRENT,
		    &pktid, srcname, &after, &packet, &nbytes, &bufsize)) 
		die(0,"orbget to get current server time fails\n") ; 
	}
	if ((pktid = orbafter (rdorb, after-eps)) < 0) 
	    die (1, "orbafter to %s failed\n", strtime (after));
	else
	    printf ("starting pktid is #%d\n", pktid);

	if ( argc - optind == 1 ) {
	    window_str = argv[optind++] ;
	    window = str2epoch ( window_str ) ; 
	    until = params.segment_size = after + window ;
	}
    } else if ( start_pktid != -1 ) {
	if (orbget (rdorb, start_pktid,
		&pktid, srcname, &after, &packet, &nbytes, &bufsize)) 
	    die(0,"orbget to get current server time fails\n") ; 
    } else {
	after = get_last_dbtimes (db);
	if ((pktid = orbafter (rdorb, after)) < 0) {
	    die (1, "orbafter to %s failed\n", strtime (after));
	} else
	    printf ("starting pktid is #%d\n", pktid);
    }

    if( DCSP ) { 
       sprintf( lcmdfile, "%s.LCMD\0", database);
       if( ( fplcmd = fopen( lcmdfile, "a+")) == NULL )
          die( 1, "can't open '%s' for LAST COMMAND rerords.\n", lcmdfile);
    }
    /* init( pffile );   */
    errreap = 0;
    do {
	if (orbreap (rdorb, &pktid, srcname, &pkttime, &packet, &nbytes, &bufsize)) {
	    errreap++; 
            complain( 0, "orbreap failed.\n" );
            if( errreap > 900 )  {
                   orbclose( rdorb );
		   if( (rdorb = orbopen( orbname, "r")) < 0)
		       die( 0, "Can't open ORB\n");
		   if ((nselect = orbselect ( rdorb, match)) < 1 )
		        die (1, "orbselect '%s' failed\n", match); 
		   if (orbseek (rdorb, ORBCURRENT ) < 0 ) 
		       die(0,"orbseek to ORBCURRENT failed .\n") ; 
		    
    	    }
	} else errreap = 0;
/*
	if( (site = ( Ste *) getarr( Pid, srcname)) == 0 ) continue;
*/
	if (Verbose > 1) {
	    printf ("%5d %s %s\n", pktid, srcname, s = strtime (pkttime));
	    free (s);
	}

	if ((asource = (Source *) getarr (sources, srcname)) == 0) {
	    asource = new_source (maxpkts);
	    setarr (sources, srcname, asource);
	}

        if( (pkttime - asource->last ) >= DINTV )
	      asource->last = pkttime;
        else continue;


	if ( pkttime < after ) {
	    continue;                         

	} else {

	    switch (orbsort (asource->apipe, &pktid, &pkttime, srcname, 
	                     &packet, &nbytes, &bufsize)) {

	    case 0:
		bufsize = 0;
		break;
	    case 2:
	    case 3:
		complain ( 0, "orbsort error at %lf for %s\n", 
		    pkttime, srcname) ; 
		break ;

	    default:
		if ( pkttime >= until) 
			done = 1 ; 
		else 
		  if (pkt2db ( &unstuffed, srcname, pkttime, packet, &params))
		     die (0, "pkt2db fails\n");
	    }
	}
    } while (!done);

    
    if (orbclose (rdorb))
	complain (1, "Error closing read fd\n");
   
    return 0;
}
