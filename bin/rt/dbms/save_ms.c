/******************************************************************
 *
 *  dbms.c 
 *  
 *  Calculate a surface-wave magnitude   
 *  
 *
 *  Author: Marina Harkins-Glushko
 *          UCSD, IGPP
 *          glushko@ucsd.edu
 *
 *******************************************************************/
#include "dbms.h"

extern char *Net;

save_ms ( Dbptr dborig ) {

     Dbptr db;
     Event *event;
     Tbl  *AllMs;
     double sum, unc;
     char auth[64], net[16], sta[16];
     char magtype[8];
     char *str, orig_str[16];
     int numms, total;
     int smags, nmags;
     int i;
     int arid, orid, evid, msid, magid;



     AllMs = keysarr( AllEv );
     numms = maxtbl( AllMs );
     if( numms <= 0 )  {
         elog_complain(0, "Ther are no MS found for current orid.\n");
         return 0 ;
     }

     unc = 0.0; sum=0.0; total=0;

     for( i = 0; i < numms; i++ )  {
           str = ( char *) gettbl( AllMs, i );
           event = ( Event *) getarr( AllEv, str );
           if( event->ms < 0 )  continue;
           sum += event->ms;
           total ++;
     }
     sum /= total;
     if( total > 1 )  {
        for( i = 0; i < numms; i++ )  {
            str = ( char *) gettbl( AllMs, i );
            event = ( Event *) getarr( AllEv, str );
            if( event->ms < 0 )  continue;
            unc += (event->ms-sum) * (event->ms-sum);
        }
        unc /= (total-1);
        unc = sqrt(unc);
     } else unc = -1.0;


     /*  Fill netmag table  */

     db = dblookup (dborig, 0, "netmag", 0, 0);
     dbquery (db, dbRECORD_COUNT, &nmags);
	  
     magid = -1;
     for ( db.record = 0; db.record < nmags; db.record++) {
	dbgetv (db, 0, 
                "net", net, 
		"orid", &orid, 
                "magtype", magtype, 0);

        sprintf( orig_str, "%d\0", orid);
        if( !regexec( &orig_match, orig_str, (size_t) 0, NULL, 0 ) &&
            /*!regexec( &net_match, net, (size_t) 0, NULL, 0 ) &&  */
            orid == event->orid &&
            !strcmp(magtype, "ms")) break;
    }
   
    if ( db.record < nmags) {
	dbgetv (db, 0, "magid", &magid, 0);
	dbputv (db, 0, 
                "magid", magid, 
		"evid", event->evid,
		"magtype", "ms",
		"nsta", total,
		"magnitude", sum,
		"uncertainty", unc,
		"auth", "dbms", 0);
   } else  if ( sum > 0) {
		magid = dbnextid (db, "magid");
		db.field = dbALL;
		db.record = dbSCRATCH;
		dbputv (db, 0, 
			"net", event->net,
			"orid", event->orid,
			"evid", event->evid,
			"magid", magid, 
			"magtype", "ms",
			"nsta", total,
			"magnitude", sum,
			"uncertainty", unc,
			"auth", "dbms", 0);
		dbadd (db, 0);
   }

   msid = magid;

   /*	Fill origin table with MS  */                           
 
   if( !regexec( &orig_match, orig_str, (size_t) 0, NULL, 0 ) &&
       !regexec( &auth_match, event->auth, (size_t) 0, NULL, 0 ) ) {
	db = dball;
        db.record = Crnt_record;
	dbputv (db, 0, "ms", sum, "msid", msid, 0);
    }

   
    /*	Fill MS into stamag table  */
     

   db = dball;
   db = dblookup (db, 0, "stamag", 0, 0);
   dbquery (db, dbRECORD_COUNT, &smags);

   for( i = 0; i < numms; i ++ )  {
       str = ( char *) gettbl( AllMs, i );
       event = ( Event *) getarr( AllEv, str );
       if( event->ms < 0 )  continue;
       for (db.record=0; db.record<smags; db.record++) {
           dbgetv (db, 0, "sta", sta, "magid", &magid, 0);
           if (magid == msid && !strcmp(event->sta, sta)) break;
       }
       if (db.record<smags) {
 	   dbputv (db, 0, 
		"arid", event->arid,
		"orid", event->orid,
		"evid", event->evid,
		"phase", event->phase,
		"magtype", "ms",
		"magnitude", event->ms,
		"auth", "dbms", 0);
       } else if (event->ms > 0) {
		db.field = dbALL;
		db.record = dbSCRATCH;
		dbputv (db, 0, 
			"sta", event->sta,
			"arid", event->arid,
			"orid", event->orid,
			"evid", event->evid,
			"phase", event->phase,
			"magid", msid, 
			"magtype", "ms",
			"magnitude", event->ms,
			"auth", "dbms", 0);
		dbadd (db, 0);
       }	
       free(event);
       delarr( AllEv, str);
   }
 

    freetbl( AllMs, 0 );
    return (1);
}
