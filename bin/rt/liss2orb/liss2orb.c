#include <stdlib.h>
#include <unistd.h>

#include "stock.h"
#include "orb.h"
#include "coords.h"
#include "liss2orb.h"

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

#ifndef BAD_LISSPACKETS
static int 
getpkt(Bns *bns, char *seed, int *pktsize) 
{
    int retcode ;
    if ( (retcode = bnsget ( bns, seed, BYTES, 64 )) == 0 ) {
	short type=0 ; 
	int log2_record_length = 0 ; 
	memcpy(&type, seed+48, 2) ;
	if ( type == 0x3e8 || type == 0xe803 ) { 
	    int size ;
	    log2_record_length = *((unsigned char *) seed+54) ;
	    *pktsize = size = 1 << log2_record_length ;
	    if ( *pktsize > 0 && *pktsize < 1<<14 ) {
		retcode = bnsget(bns, seed+64, BYTES, size-64 ) ;
	    } else { 
		complain ( 0, "read record length=%d => packet size=%d", 
			log2_record_length, *pktsize ) ; 
		hexdump ( stderr, seed, 64 ) ; 
		retcode = -1; 
	    }
	} else { 
	    complain ( 0, "no blockette 1000!" ) ; 
	    hexdump ( stderr, seed, 64 ) ; 
	    retcode = -1 ; 
	}
    }
    return retcode ;
}
#endif

static int
matches ( char *srcname, char *match)
{
    int retcode ;
    if ( match != 0 ) { 
	static regex_t *pgm = 0 ;
	if ( pgm == 0 ) { 
	    allot ( regex_t *, pgm, 1 ) ;
	    if ( regcomp(pgm, match, REG_EXTENDED ) != 0) {
		die ( 1, "can't compile regular expression '%s'", match ) ; 
	    }
	}
	if ( regexec ( pgm, srcname, 0, 0, 0) == 0 ) {
	    retcode = 1 ; 
	}
    } else { 
	retcode = 1 ; 
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
    int		   size = 1024, pktsize=0 ;
    int		   defaultport = 4000 ;
    char	  *database = 0 ;
    char	  *seed ;
    int 	   fd = -1 ;
    int 	   failures = 0 ;
    char 	   srcname[ORBSRCNAME_SIZE] ;
    double 	   time ; 
    char 	  *packet ; 
    int 	   nbytes = 0, bufsize = 0 ;
    int		   remap = 0 ;

    elog_init (argc, argv);
    elog_notify ( 0, "%s $Revision$ $Date$\n", Program_Name ) ; 

    while ((c = getopt (argc, argv, "d:m:rs:t:vV")) != -1) {
	switch (c) {
	  case 'd':
	    database = optarg ; 
	    break ;

	  case 'm':
	    match = optarg;
	    break;

	  case 'r':
	    remap = 1 ; 
	    break ;

	  case 's':
	    pktsize = atoi(optarg) ; 
	    break ;

	  case 't':
	    timeout = atoi(optarg) ; 
	    break ;

	  case 'v':
	    verbose++ ;
	    break;

	  case 'V':
	    cbanner ( "$Revision$", 0,
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

#ifdef BAD_LISSPACKETS
    if ( pktsize == 0 ) { 
	char *lower_liss ;
	lower_liss = strdup ( liss_server) ;
	str2lower(lower_liss) ; 
	if ( strstr(lower_liss, ".gt." ) != 0 ) { 
	    pktsize = 256 ; 
	} else { 
	    pktsize = 512 ; 
	}
	free(lower_liss) ;
    }
#endif

    orb = orbopen(orbname, "w&" ) ; 
    if ( orb < 0 ) { 
	die (0, "Can't open output orb %s", orbname ) ; 
    }

    allot ( char *, seed, size ) ;
    for(;;) { 
	Bns *bns ;
	
	while ( fd < 0 ) { 
	    fd = open_socket ( liss_server, defaultport ) ; 
	    if ( fd < 0 ) { 
		if ( failures==0) {
		    complain ( 1, "Can't open liss server %s", liss_server ) ;
		    sleep ( 60 ) ;
		}
		failures++ ;
	    } else { 
		if ( failures ) { 
		    complain ( 1, "connected to %s", liss_server ) ;
		    failures = 0  ;
		}
		bns = bnsnew(fd, size) ;
		bnsuse_sockio(bns) ;
		bnstimeout ( bns, timeout*1000 ) ;
		bnsclr(bns) ;
		bns->fd = fd ; 
	    }
	}

#ifndef BAD_LISSPACKETS
	if (getpkt(bns, seed, &pktsize) == 0) { 
#else
	if (bnsget(bns, seed, BYTES, pktsize) == 0) { 
#endif
	    if ( liss2orbpkt ( seed, pktsize, database, remap,
		    srcname, &time, &packet, &nbytes, &bufsize ) == 0 ) { 
		if ( matches ( srcname, match) ) { 
		    orbput ( orb, srcname, time, packet, nbytes ) ; 
		    if ( verbose ) { 
			char *s ;
			fprintf ( stderr, "%s %s %d\n", srcname, s=strydtime(time), nbytes ) ;
			free(s) ;
		    }
		}
	    } 
	} else { 
	    complain ( 1, "Closing connection to liss server %s", liss_server ) ;
	    bnsclose ( bns ) ;
	    fd = -1 ; 
	}
    }

    return 0 ;
}
