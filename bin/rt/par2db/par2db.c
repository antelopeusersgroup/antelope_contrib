#include "par2db.h"
#include "header.h"

int DCSP = 0;
int GPS_COOR = 0;
FILE *fplcmd;

int Verbose = 0;
int DINTV = 1;
int Log = 0;
int DCFlag = 1;


int
main (argc, argv)
int             argc;
char          **argv;
{

    extern char    *optarg;
    extern int      optind;
    double          pkttime;
    double          after = 0.0;
    int             err=0, c;
    int             rdorb;
    int             npkt = 15,
		    nselect,
                    pktid;
    char	   *after_str=0 ;
    char           *orbname = 0;
    char           *match = ".*/CBBHS";
    char           *database;
    char           *packet=0;
    char            srcname[ORBSRCNAME_SIZE];
    char   	   acomp[64];
    char           *s, lcmdfile[64];
    Dbptr           db;
    Save_params     params;
    int             nbytes, bufsize = 0;
    Arr            *Parms ;
    Arr            *sources ;
    Source	   *asource;
    int 	   i, par;
    PktChannel 	   *achan ;
    Db_buffer 	   *buffer ;
    static Packet  *Pkt=0 ;
    
    
    Program_Name = argv[0];
    elog_init (argc, argv);
    elog_notify ( 0, "%s Version Version 1.6 10/28/96\n", Program_Name ) ; 

    params.memsize = 180.0;
    params.segment_size = 86400.0;
    params.wfname = 0;
    params.gapmax = 600.0 ;
    
    while ((c = getopt (argc, argv, "Ocgm:i:s:v")) != -1)
	switch (c) {

	case 'c':
	    DCSP = 1;
	    DINTV = 5;
	    break;

	case 'g':
	    GPS_COOR = 1;
	    break;

	case 'O':
	    DCFlag = 0;
	    break;

	case 'm':
	    match = optarg;
	    break;

	case 'i':
	    DINTV = atoi(optarg);
	    break;

	case 's':
	    npkt = atoi(optarg);
	    break;

	case 'v':
	    Log=1;
	    break;

	case '?':
	    err++;
	}

    if (err || argc - optind < 2 || argc - optind > 4)
	usage ();

    if ( params.memsize < 1. )
	die ( 0, "memory buffer size = %f seconds, but must be more than 1 second\n", 
	    params.memsize ) ;

    orbname = argv[optind++];
    if ((rdorb = orbopen (orbname, "r")) < 0)
	die (0, "Can't open ring buffer '%s'\n", orbname);

    database = argv[optind++];
    if (dbopen_database (database, "r+", &db) < 0)
	die (0, "Can't open output database %s\n", database);
    if (db.table < 0) {
	db = dblookup (db, 0, "wfdisc", 0, 0);
	if (db.table < 0)
	    die (0, "Can't open output table '%s'\n", database);
    }
    params.db = db ;

    if( DCSP )  { 
       sprintf( lcmdfile, "%s.LCMD\0", database);
       if( ( fplcmd = fopen( lcmdfile, "a+")) == NULL )
          die( 1, "can't open '%s' for LAST COMMAND rerords.\n", lcmdfile);
   
       match = ".*/BSP";
    }
    
    if (match) {
	if ((nselect = orbselect (rdorb, match)) < 0)
	    die (1, "orbselect '%s' failed\n", match);
    }

    Pkt = newpkt();
    Parms = newarr (0);
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
	if ((pktid = orbafter (rdorb, after-0.001)) < 0) 
	    die (1, "orbafter to %s failed\n", strtime (after));
	else
	    printf ("starting pktid is #%d\n", pktid);

    } else {
	after = get_last_dbtimes (db);
	if ((pktid = orbafter (rdorb, after)) < 0) {
	    die (1, "orbafter to %s failed\n", strtime (after));
	} else
	    printf ("starting pktid is #%d\n", pktid);
    }

    
    err = 0;
    while(1) {

	if (orbreap (rdorb, &pktid, srcname, &pkttime, &packet, &nbytes, &bufsize)) {
	    err++; 
            complain( 0, "orbreap failed.\n" );
            if( err > 900 )  {
                   orbclose( rdorb );
		   if( (rdorb = orbopen( orbname, "r")) < 0)
		       die( 0, "Can't open ORB\n");
		   if ((nselect = orbselect ( rdorb, match)) < 1 )
		        die (1, "orbselect '%s' failed\n", match); 
		   if (orbseek (rdorb, ORBCURRENT ) < 0 ) 
		       die(0,"orbseek to ORBCURRENT failed .\n") ; 
		    
    	    }
	} else err = 0;
	if (Verbose > 1) {
	    printf ("%5d %s %s\n", pktid, srcname, s = strtime (pkttime));
	    free (s);
	}

	if ((asource = (Source *) getarr (sources, srcname)) == 0) {
	    asource = new_source (npkt);
	    setarr (sources, srcname, asource);
	}

        if( (pkttime - asource->last ) >= DINTV )
	      asource->last = pkttime;
        else continue;

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

                switch ( unstuffpar (packet, pkttime, &Pkt, srcname )) {
	            case 1:
	               for (par = 0; par < Pkt->nchannels; par++) {
	                  achan = (PktChannel *) gettbl (Pkt->chan, par);
	                  sprintf( &acomp[0], "%s_%s_%s\0", 
			           achan->net, achan->sta, achan->chan);
	                  if( (buffer = (Db_buffer *) getarr( Parms, acomp )) != 0 ) 
	                       seg_append (achan, buffer) ;
	                  else {
	                       buffer = new_db_buffer( achan, &params);
	                       setarr( Parms, acomp, buffer );
	                  }
	                }
	                break;

	             default:
	                break;
                 }
                 break;
	 }
    } 

}


