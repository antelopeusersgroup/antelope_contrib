#include <stdio.h>
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

void dbserve(char *database, FILE *fd) {

    Dbptr db;
    
    char *fmt ;
    Pf *pf = 0 ;
    Tbl *recipe, *keys, *values ;


    if ( dbopen(database, "r", &db ) != 0)
	die (0, "Couldn't open database %s\n", database ) ;
    
    if (pfin(fd, &pf) != 0)
	die(0, "Can't read parameter file\n");

    if (debug) printf("reading the recipe.\n");
    recipe = pfget_tbl (pf, "recipe" ) ;

    if (recipe == PFINVALID) 
	die(0, "Didn't find a 'recipe' entry in the parameter file." );

    if (recipe != PFTBL) 
        die(0, "The 'recipe' was not a &Tbl, as required." );

    if (debug) printf("performing dbprocess.\n");
    db = dbprocess ( db, recipe, 0 ) ;

    if (debug) printf("getting 'keys' and 'values'.\n");
    keys = pfget_tbl ( pf, "keys" ) ;
    values = pfget_tbl ( pf, "values" ) ;

    fmt = pfget_string ( pf, "format" ) ;

    if ( strcmp(fmt, "xml") == 0 ) {

	char *xml = 0;
	db2xml( db, 0, 0, keys, values, (void **) &xml, 0 );
	printf( "%s\n", xml );

    } else if ( strcmp(fmt, "ptolemy") == 0 ) {

	char *pt_expression = 0;
	db2ptolemy( db, keys, values, (void **) &pt_expression, 0 );
	printf( "%s", pt_expression);	

    } else {

	dbselect (db, values, stdout ) ;

    }
}


int main(int argc, char **argv) {

    extern char *optarg;
    extern int optind;
    int c, errflg = 0 ;
    char *database;    

    Program_Name = argv[0];

    if ( argc != 2 )
	die ( 0, "Usage: %s database\n", Program_Name ) ;

    database = argv[1];

    dbserve(database, stdin);

    return 0;
}
