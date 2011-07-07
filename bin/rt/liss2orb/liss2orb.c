
#include "liss2orb.h"

Arr *NewCh;
int PSize;
int Log;
struct sockaddr_in peer_in;
char *Debug = 0 ;

static void 
usage ()
{
    fprintf (stderr,
    "Usage: %s [-d database] [-m match] [-r] [-s size] [-t timeout] [-v] liss orb\n",
	     Program_Name);
    exit (1);
}

#define BAD_LISSPACKETS 1
/*

The routine getpkt below attempts to read the 1000 blockette
and derive the packet size from that information.  However, 
currently, the liss servers are putting the wrong information
into that blockette, so instead we must fall back on using the 
liss server name or explicit command line argument to decide on 
the packet size.  See the message below:

    Date: Wed, 15 Sep 1999 10:54:01 -0600
    To: "Daniel M. Quinlan" <danq@brtt.com>, bolton@asl.cr.usgs.gov
    From: Scott Halbert <scott@asl.cr.usgs.gov>
    Subject: Re: questions about LISS output format
    Cc: vernon@brtt.com

    All GTSN (.gt) stations have 256 byte records, *all* others (at this time) have
    512---despite what that blockette 1000 says.

    We're checking into that, but since GTSN support was taken over by the
    air force, we might not have any control of it.  I may have to write a module
    in the LISS that repairs the blockette 1000 on the fly.

    ---Scott Halbert
    Albuqueque Seismological Lab

*/

static int 
getpkt (Bns *bns, char **seedp, int fixedsize, int *pktsize, int *seedsize) 
{
    int retcode ;
    if ( fixedsize != 0 ) {
	SIZE_BUFFER (char *, *seedp, *seedsize, fixedsize ) ; 
	retcode = bnsget(bns, *seedp, BYTES, fixedsize ) ;
	*pktsize = fixedsize ;
    } else if ( (retcode = bnsget ( bns, *seedp, BYTES, 64 )) == 0 ) {
	unsigned short type=0 ; 
	int log2_record_length = 0 ; 
	memcpy(&type, *seedp+48, 2) ;
	if ( type == 0x3e8 || type == 0xe803 ) { 
	    int size ;
	    log2_record_length = *((unsigned char *) *seedp+54) ;
	    *pktsize = size = 1 << log2_record_length ;
	    RESIZE_BUFFER (char *, *seedp, *seedsize, size ) ;
	    if ( *pktsize > 0 && *pktsize < 1<<14 ) {
		retcode = bnsget(bns, *seedp+64, BYTES, size-64 ) ;
	    } else { 
		elog_complain( 0, "read record length=%d => packet size=%d", 
			log2_record_length, *pktsize ) ; 
		hexdump ( stderr, *seedp, 64 ) ; 
		retcode = -1; 
	    }
	} else { 
	    elog_complain( 0, "no blockette 1000!" ) ; 
	    hexdump ( stderr, *seedp, 64 ) ; 
	    retcode = -1 ; 
	}
    }
    return retcode ;
}

int
main (int argc, char **argv)
{
    int		   c ;
    char           *match = 0;
    char           *liss_server ;
    int            timeout = 300;
    char           *orbname ;
    int		   verbose = 0 ;
    int 	   orb ;
    int		   fixedsize=0, pktsize=0 ;
    int		   defaultport = 4000 ;
    char	  *database = 0 ;
    char	  *seed ;
    int		   seedsize = 4096, bufsize = 0 ; 
    int 	   fd = -1 ;
    int 	   failures = 0 ;
    char 	   srcname[ORBSRCNAME_SIZE] ;
    double 	   time ; 
    char 	  *packet ; 
    int 	   nbytes = 0 ;
    int 	   cnt =0, npkts = 0 ;
    int		   remap = 0 ;
    Bns 	   *bns=0 ;
    Hook	   *hook=0 ;
    long	   start, nchars ;

    elog_init (argc, argv);
    announce(0,0) ;

    while ((c = getopt (argc, argv, "D:d:m:n:rs:t:vV")) != -1) {
	switch (c) {
	  case 'D':
	    Debug = optarg ; 
	    break ;

	  case 'd':
	    database = optarg ; 
	    break ;

	  case 'm':
	    match = optarg;
	    break;

	  case 'n':
	    npkts = atoi(optarg) ; 
	    break ;

	  case 'r':
	    remap = 1 ; 
	    break ;

	  case 's':
	    fixedsize = atoi(optarg) ; 
	    break ;

	  case 't':
	    timeout = atoi(optarg) ; 
	    break ;

	  case 'v':
	    verbose++ ;
	    break;

	  case 'V':
	    cbanner ( 0, 0,
		"Daniel Quinlan", "BRTT", "danq@brtt.com" ) ;
	    usage() ;
	    break ;

	  default:
	    usage ();
	}
    }
    if (argc - optind != 2)
	usage ();

    liss_server = argv[optind++];
    orbname = argv[optind++];

    orb = orbopen(orbname, "w&" ) ; 
    if ( orb < 0 ) { 
	elog_die(0, "Can't open output orb %s", orbname ) ; 
    }

    if (database) {
	Dbptr db;

	if (dbopen(database, "r+", &db) == dbINVALID) {
		elog_die(0, "dbopen(%s) error.\n", database);
	}
	finit_db (db);
    }

    allot(char *, seed, seedsize) ; 
    for(;;) { 
	while ( fd < 0 ) { 
	    fd = open_socket ( liss_server, defaultport ) ; 
	    if ( fd < 0 ) { 
		if ( failures==0) {
		    elog_complain( 1, "Can't open liss server %s", liss_server ) ;
		}
		snooze ( 60. ) ;
		failures++ ;
	    } else { 
		if ( failures ) { 
		    elog_notify ( 0, "connected to %s after %d failures", 
		    	liss_server, failures ) ;
		    failures = 0  ;
		}
		bns = bnsnew(fd, 8192) ;
		bnsuse_sockio(bns) ;
		bnstimeout ( bns, timeout*1000 ) ;
		bnsclr(bns) ;
		bns->fd = fd ; 
	    }
	}

	if (getpkt(bns, &seed, fixedsize, &pktsize, &seedsize) == 0) { 
	    if ( Debug != 0 ) { 
		static int debug = -1 ; 
		static long cnt = 0 ;
		if ( debug < 0 ) { 
		    debug = reopen (Debug, O_RDWR | O_CREAT, 0664);
		    if ( debug < 0 ) { 
			elog_die( 1, "Can't open %s to write out packets!", Debug ) ; 
		    }
		    printf ( "\n" ) ;
		}
		fprintf ( stderr, "\rPacket #%ld", cnt++ ) ; 
		if ( write ( debug, seed, pktsize ) != pktsize ) { 
		    elog_die( 1, "Failed to write %d bytes to %s", pktsize, Debug ) ; 
		}
	    }
	    if ( liss2orbpkt ( seed, pktsize, database, remap,
		    srcname, &time, &packet, &nbytes, &bufsize ) == 0 ) { 
		if ( match == 0 || strcontains ( srcname, match, &hook, &start, &nchars) ) { 
		    orbput ( orb, srcname, time, packet, nbytes ) ; 
		    if ( verbose ) { 
			char *s ;
			fprintf ( stderr, "%-20s %s %4d => %4d\n", srcname, s=strydtime(time), pktsize, nbytes ) ;
			free(s) ;
		    }
		    cnt++ ; 
		    if ( npkts > 0 && cnt >= npkts ) { 
			break ; 
		    }
		}
	    }
	} else { 
	    elog_complain( 1, "Closing connection to liss server %s", liss_server ) ;
	    bnsclose ( bns ) ;
	    fd = -1 ; 
	}
    }

    return 0 ;
}
