/******************************************************************
 *
 *  dbutil.c 
 * 
 *  Utilities to manipulate database tables.
 * 
 *  Author: Marina Harkins-Glushko
 *          UCSD, IGPP
 *          glushko@ucsd.edu
 *
 *******************************************************************/
#include "dbms.h"

extern char *Net;

int origin ( int evid, 
             int jrec,
	     int *orid, 
	     double *mb, 
	     double *ms, 
             double *otime, 
             char *auth )

{
	int id;

	if ( dball.record >= jrec ) return (0);

	dbgetv (dball, 0, 
                "orid", orid, 
		"evid", &id,
		"time", otime,
		"auth", auth,
		"mb", mb,
		"ms", ms, 0);
/*
fprintf( stderr, " evid %d (%d) orid %d (%d)\n", evid, id, *orid, prev_orid );
*/

	if ( id < 0) id = *orid;
	if ( id == evid && *orid != prev_orid) {
		prev_orid = *orid;
		Crnt_record = dball.record;
		return (1);
	}
        if( prev_orid < 0 ) prev_orid = *orid;
        if( id > evid ) return 0;
	
        return (1);
}

int assoc ( int jrec, 
	    int *arec,
            char *net, 
            char *sta, 
            char *phase, 
            int *arid, 
            double *delta, 
            double *otime )

{
	int orid;

        if( dball.record > jrec ) return 0;

	if (dbgetv ( dball, 0,	
                     "orid", &orid,
	             "arid", arid,
	             "delta", delta,
	             "phase", phase,
	             "sta", sta, 0) == dbINVALID) return (0);
        
        if( Net == 0 ) { 
            if( dbgetv ( dball, 0, "net", net, 0) == dbINVALID) return (0);
        } else strcpy( net, Net);

/*
fprintf( stderr, " orid %d (%d)\n", orid, prev_orid );
*/	
        if (orid != prev_orid) return (0);
	*arec =  dball.record;
	dball.record++;
	return (1);
}

int join_db ( Dbptr db, char *want_orig )
{
        Dbptr dbaf, dbas, dbarr, dbev, dbor;
        Tbl *keys;
        int orid, evid;
        int jrec, num;
        EvRec *evkey;
        char auth[64], orig_str[16], key[64];
 
        dbor = dblookup (db, 0, "origin", 0, 0);
        if( dbor.table < 0)  {
           elog_die(0, "Can't open origin table.\n" );
        }
 
        dbquery ( dbor, dbRECORD_COUNT, &num );
        if ( num < 1) elog_die( 0, "No origins.\n");
      
        dbaf = dblookup (db, 0, "affiliation", 0, 0);
        if( dbaf.table < 0)  {
           elog_die(0, "Database is not complete. Can't open affiliation table.\n" );
        }
        dbquery ( dbaf, dbRECORD_COUNT, &num );
        if ( num < 1) elog_die( 0, "Can't open affiliation table.\nBuild affiliation table or use -n option.\n");
        
        dbarr  = dblookup (db, 0, "arrival", 0, 0);
        dbas  = dblookup (db, 0, "assoc", 0, 0);
        dbev  = dblookup (db, 0, "event", 0, 0);
          
        dball = dbjoin (dbor, dbev, 0, 0, 1, 0, 0);
        dball = dbjoin (dball, dbas, 0, 0, 0, 0, 0);
        dball = dbjoin (dball, dbarr, 0, 0, 0, 0, 0);
        if( Net == 0 ) dball = dbjoin (dball, dbaf, 0, 0, 0, 0, 0);

        /* Select only vertical channels */ 
          
        sprintf( key,"(chan =~ /.*Z/&&iphase =~ /P/)\0" );     
        dball = dbsubset( dball, key, 0 );
        dbquery (dball, dbRECORD_COUNT, &jrec);
        if( jrec <= 0 )
            elog_die( 0, " no record with chan=*Z\n") ;

        /* Sort joined table by event&origin&arrival IDs  */                 
 
        keys = strtbl("evid", "orid", "arid", 0 );
        dball = dbsort (dball,  keys, 0, 0);
        dbquery (dball, dbRECORD_COUNT, &jrec);
        freetbl( keys, 0 );

        /*  Select record with specified origin ID. Fill an EVENT array  */
  
        for ( dball.record=0; dball.record < jrec; dball.record++ ) {
                dbgetv (dball, 0, "orid", &orid, "evid", &evid, "auth", auth, 0);
                sprintf( orig_str, "%d\0", orid);
                if( regexec( &orig_match, orig_str, (size_t) 0, NULL, 0 ) )
                   continue;
                if( regexec( &auth_match, auth, (size_t) 0, NULL, 0 ) )
                   continue;

                if( evid < 0 )
                    elog_die(0, "found NULL evid.\n");
                        
                sprintf( key, "%d\0", evid );
 
                if( ( evkey = (EvRec *) getarr( EvArr, key )) == 0 )  {
                      evkey = new_id( dball.record, evid );
                    setarr( EvArr, key, (char *) evkey );
                }
        }

        dball.record = 0;
        return jrec;
}
