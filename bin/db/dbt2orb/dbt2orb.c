/* Tobin Fricke  		1999-07-29  22:40:48:318 UTC Thursday */
/* Geophysical Institute, University of Alaska, Fairbanks             */
/* tobin@giseis.alaska.edu | tobin@uclink4.berkeley.edu               */

/* dbt2orb sends a database table to an orb.	*/

/* ROW_MAX_LENGTH is the size of the buffer that will be used to hold
   individual database rows. */

#define ROW_MAX_LENGTH	2048

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include "db.h"
#include "stock.h"
#include "orb.h"

int loop;

void done(int signal)
 { loop = 0;
   printf("\nCaught SIGINT, no more iterations. \n",signal);
 }

int main(int argc, char **argv)
 { Pf 		*pf;
   char		*orbname,
		*dbname,
		*expr,
		*table,
		*rowtemp,
                verbose;
   int  	orb,
		totalrecords,
		records,
 		record;
   Dbptr  	db,
		dbinput,
		dbscratch;

  elog_init(argc,argv);		

  /* initialize variables */

  verbose = 0;
  expr = NULL;
  loop = 0;
  record = 0;
  totalrecords = 0;

  rowtemp = malloc(ROW_MAX_LENGTH);
  
  if (rowtemp == NULL) elog_die(1,"malloc() error.\n");

  /* read in command line options */

  { int c;
    unsigned char errflg = 0;

    while (( c = getopt( argc, argv, "vl:s:")) != -1)
        switch (c) {
          case 'l': loop = atoi(optarg);
                    sigset(SIGINT,done);
                    break;
          case 's': expr = optarg;
                    break;
          case 'v': verbose = 1;
                    break;
          case '?': errflg++;
        }

    if ( (argc - optind) != 3 ) errflg++;

    if (errflg) 
       { elog_die(0,"usage: %s [-v] [-s subset] [-l delay] db table orb\n",argv[0]); 
       }
    
    dbname = argv[optind++];
    table  = argv[optind++];
    orbname= argv[optind++];
  }
    
  /* start our work */

  if ( dbopen ( dbname,  "r+",  &dbinput ) == dbINVALID )
     { elog_die(1,"Couldn't open input database, \"%s\".\n",dbname); }

  dbinput   = dblookup( dbinput, 0, table, 0, 0);
  dbscratch = dblookup( dbinput, 0, 0, 0, "dbSCRATCH");

  if ( dbinput.table == dbINVALID )
   { elog_die(1,"Couldn't lookup the table \"%s\" in the database \"%s\".\n",
		table,dbname); }
  if ( dbscratch.record == dbINVALID )
   { elog_die(1,"Couldn't lookup the scratch record in the database \"%s\".\n",
		dbname); }
  
  if ( dbquery( dbinput, dbRECORD_COUNT, &records) < 0 )
   { elog_die(1,"dbquery dbRECORD_COUNT failed.\n"); }

  orb      = orbopen( orbname, "w&" );

  if ( orb == -1 )
   { elog_die(1,"Couldn't open the orb, \"%s\".\n",orbname); }

  if (loop == 0) loop = -1;

  while (loop)
   { db = dbinput;
     
     if ( expr != NULL) db = dbsubset( db, expr, 0 );
     
     db.record = record;

     if ( dbquery( db, dbRECORD_COUNT, &records) < 0 )
   	 elog_die(1,"dbquery dbRECORD_COUNT failed.\n");  
     
     for (;db.record<records;db.record++)
       { if (verbose) printf("writing record %d to orb...\n",db.record); 

         /* copy the row from what could be a view into the scratch record of the
            input database, so as to avoid putting a view on the ORB */
         if ( dbget( db,        rowtemp) == dbINVALID ) elog_die(1,"dbget error.\n");
         if ( dbput( dbscratch, rowtemp) == dbINVALID ) elog_die(1,"dbput error.\n");

         if ( db2orbpkt( dbscratch, orb ) < 0 )
            { elog_complain( 0, "Couldn't write record #%d to %s.\n",
              	   db.record, orbname); } 
         totalrecords++;
       }

     record = db.record; 

     /* if a subset expression was supplied, then we're working with
        a view, which should be freed. */

     if ( expr != NULL) dbfree(db);
     if ( loop == -1 ) loop = 0;
     sleep(loop);
   } 

  printf("posted %d records from database %s to orb %s.\n",totalrecords,dbname,orbname);
  dbclose(dbinput);
  orbclose(orb); 
  free(rowtemp);
 }

