#include <stdio.h>

#include <errno.h>
#include <netdb.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "db.h"
#include "stock.h"
#include "dbptolemy.h"
#include "dbxml.h"

/* This code is taken directly from the dbprocess(3) manpage's
   'try.c' example program.
*/

static int debug = 1;

static int verbose = 0 ;

/* Perform one or more queries to the given dbname from the given
   file descriptor (which can be stdin, a socket, a pipe, etc).
*/

void dbserve(char *database, FILE *in_stream, FILE *out_stream) {

    Dbptr db;
    
    char *fmt ;
    Pf *pf = 0 ;
    Tbl *recipe, *keys, *values ;

    fprintf(out_stream, "HELLO\n");

    if ( dbopen(database, "r", &db ) != 0)
	elog_die(0, "Couldn't open database %s\n", database ) ;
    
    if (pfin(in_stream, &pf) != 0)
	elog_die(0, "Can't read parameter file\n");

    elog_debug(0, "reading the recipe.\n");
    recipe = pfget_tbl (pf, "recipe" ) ;

    if (recipe == 0) 
	elog_die(0, "Didn't find a 'recipe' entry in the parameter file." );

    elog_debug(0, "performing dbprocess.\n");
    db = dbprocess ( db, recipe, 0 ) ;

    elog_debug(0, "getting 'keys' and 'values'.\n");
    keys = pfget_tbl ( pf, "keys" ) ;
    values = pfget_tbl ( pf, "values" ) ;

    fmt = pfget_string ( pf, "format" ) ;

    if ( strcmp(fmt, "xml") == 0 ) {

	char *xml = 0;
        elog_debug(0, "outputting results as xml\n");
	db2xml( db, 0, 0, keys, values, (void **) &xml, 0 );
	fprintf(out_stream, "%s\n", xml );

    } else if ( strcmp(fmt, "ptolemy") == 0 ) {

	char *pt_expression = 0;
        elog_debug(0, "outputting results as ptolemy expressions\n");
	db2ptolemy( db, keys, values, (void **) &pt_expression, 0 );
	fprintf(out_stream, "%s", pt_expression);	

    } else {

        elog_debug(0, "outputting results as dbselect\n");
	dbselect (db, values, out_stream ) ;

    }
}

/** Listen for incoming TCP connections on the given port on all available
    interfaces. */

void daemonize(char *database, int port) {

    int sock;
    int connection;
    struct sockaddr_in local_addr, remote_addr;
    
    sock = socket (PF_INET, SOCK_STREAM, 0);
    local_addr.sin_addr.s_addr = INADDR_ANY;
    local_addr.sin_family = PF_INET;
    local_addr.sin_port = htons((short)port);
    
    remote_addr.sin_addr.s_addr = INADDR_ANY;
    remote_addr.sin_family = PF_INET;
    remote_addr.sin_port = INADDR_ANY;
    
    if (-1 == bind(sock, (struct sockaddr *)(&local_addr), 
		   sizeof(struct sockaddr_in) )) 
	elog_die(1, "could not bind to socket.");
    
    if (-1 == listen(sock, SOMAXCONN)) 
	elog_die(1, "could not listen on socket.");
    
    while (1) {
	int connection;
	pid_t fork_result;
	socklen_t addrlen;

	addrlen = sizeof(struct sockaddr);
	connection = accept(sock, (struct sockaddr *)&remote_addr, &addrlen);
	
	if (-1 == connection) 
	    elog_die(1, "accept() failed.");
	
	elog_log(0, "incoming connection from %s:%u\n",
		 inet_ntoa(remote_addr.sin_addr),
		 ntohs(remote_addr.sin_port));
    
	
	switch ( fork() ) {
	    case 0: {
		FILE *stream;
		stream = fdopen( connection, "rw" );
                setvbuf( stream, NULL, _IOLBF, 0 );
		dbserve( database, stream, stream );
		exit( 0 );
	    }
	    case -1: 
		elog_die(1, "fork() failed. (%s)", strerror(errno));
	    default:
		/* success */
		break;
	}
    }
}


static void usage() {
    fprintf(stderr, "Usage: %s [-d port] dbname\n", Program_Name);
    exit(1);
}


int main(int argc, char **argv) {

    extern char *optarg;
    extern int optind;
    int c, errflg = 0 ;
    char *database;    
    int port;
    int opt_daemonize = 0;
    
    Program_Name = argv[0];

    elog_init( argc, argv ) ;
    
    while ((c = getopt (argc, argv, "d:v")) != -1) {
	switch (c) {
	    
	    case 'v':
		verbose++;
		break;
		
	    case 'd':
		opt_daemonize = 1;
		port = atoi( optarg );
		break;
		
	    default:
		errflg++;
		break ;
	}
    }

    if ( errflg || argc - optind != 1 ) {
	usage ();
    }

    database = argv[optind];

    if (opt_daemonize) {
	daemonize(database, port);
    } else {
	dbserve(database, stdin, stdout);
    }

    return 0;
}
