#include <stdio.h>
#include <math.h>
#include "coords.h"
#include "db.h"
#include "stock.h"

/* This code is taken directly from the dbprocess(3) manpage's
   'try.c' example program.
*/

int main(int argc, char **argv) {

    extern char *optarg;
    extern int optind;
    int c, errflg = 0 ;
    Dbptr db;
    char *database ;
    int verbose = 0 ;
    Pf *pf ;
    Tbl *input, *list ;

    Program_Name = argv[0];

    if (pfread(Program_Name, &pf) != 0)
	die(0, "Can't read parameter file\n");

    if ( argc != 2 )
	die ( 0, "Usage: %s database\n", Program_Name ) ;

    database = argv[1] ;
    if ( dbopen(database, "r", &db ) < 0)
	die (0, "Couldn't open database %s\n", database ) ;

    input = pfget_tbl (pf, "input" ) ;
    db = dbprocess ( db, input, 0 ) ;
    list = pfget_tbl ( pf, "fields" ) ;
    dbselect (db, list, stdout ) ;
    return 0;
}
