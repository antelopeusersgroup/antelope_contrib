#include <stdlib.h>
#include <unistd.h>

#include "stock.h"
#include "bns.h"

#define SIZE 4096

static void 
usage ()
{
    fprintf (stderr,
    "Usage: %s [-v] liss ...\n",
	     Program_Name);
    exit (1);
}

extern int open_socket ( char *name, int default_port )  ;

int
main (int argc, char **argv)
{
    int		   c ;
    char           *liss_server ;
    int            timeout = 10;
    int		   verbose = 0 ;
    int		   defaultport = 4000 ;
    unsigned char *seed ;
    int 	   fd = -1 ;
    Bns 	   *bns=0 ;

    elog_init (argc, argv);
    announce(0,0) ;

    while ((c = getopt (argc, argv, "v")) != -1) {
	switch (c) {

	  case 'v':
	    verbose++ ;
	    break;

	  default:
	    usage ();
	}
    }
    if (argc - optind < 1)
	usage ();

    allot(unsigned char *, seed, SIZE) ; 
    for(;optind < argc;optind++) { 
	liss_server = argv[optind] ;
	elog_notify(0, "opening %s\n", liss_server) ; 
	fd = open_socket ( liss_server, defaultport ) ; 
	if ( fd < 0 ) { 
	    elog_complain ( 1, "Can't open liss server %s", liss_server ) ;
	} else { 
	    int out ;
	    bns = bnsnew(fd, 8192) ;
	    bnsuse_sockio(bns) ;
	    bnstimeout ( bns, timeout*1000 ) ;
	    bnsclr(bns) ;
	    bns->fd = fd ; 
	    elog_notify(0, "reading %d bytes from %s\n", SIZE, liss_server) ; 
	    if ( bnsget(bns, seed, BYTES, SIZE ) == 0 ) { 
		out = open(liss_server, O_WRONLY | O_CREAT, 0664 ) ;
		if ( out == 0 ) { 
		    elog_die (0, "Can't open %s to write", liss_server) ;
		}
		elog_notify(0, "writing %d bytes from %s\n", SIZE, liss_server) ; 
		if ( write(out, seed, SIZE) != SIZE ) { 
		   elog_complain(0, "failed to write %d bytes to %s", SIZE, liss_server) ; 
		}
		if ( close(out) != 0 ) { 
		    elog_complain(0, "failed to close %s", liss_server) ; 
		}
	    } else { 
		elog_complain(0, "Failed to read data from %s", liss_server) ; 
	    }
	    if ( bnsclose(bns) != 0 ) { 
		elog_complain(0, "failed to close bns #%d for %s", fd, liss_server) ; 
	    }
	}
    }

    return 0 ;
}
