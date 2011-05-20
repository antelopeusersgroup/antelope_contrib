/******************************************************************
 *
 *  dbms.c 
 *  
 *  Calculate a surface-wave magnitude - MS .
 *  
 *
 *  Author: Marina Harkins-Glushko
 *          UCSD, IGPP
 *          glushko@ucsd.edu
 *
 *******************************************************************/
#include "dbms.h"

char *Net = 0;

regex_t orig_match;
regex_t auth_match;
regex_t net_match;
Arr *AllEv;
Arr *EvArr;
Dbptr dball;
int Crnt_record;
int Log;
int prev_orid;
Filter flt;


int
main ( int argc, char **argv)
{
      Dbptr 	db, dba, dbw, dbwa;
      Event	*event;
      EvRec     *evrec;
      double    delta, mb, ms, otime;
      int 	gotone, nrec, i, j, jrec;
      int	code, arec, evnum;
      int 	evid, orid, arid, id;
      char	*str, key[32], 
                stanet[12], sta[8], phase[16], 
           	orig_str[16],
                auth[16];
      Tbl  	*sort_sta_ch_tm;
      Tbl	*EvTbl=0;
      char	*want_orig=".*";
      int  	twin=1200;
      char 	*dbin=0,
           	*oauth=".*",
           	*net=".*";
      char 	*fil_str=0, 
		*pfile=0;
   
      elog_init (argc, argv) ;
      elog_notify (0, "$Revision$ $Date$") ;
      Program_Name = argv[0];

      prev_orid = -1;
      Crnt_record = -1;
      Log = 0;

      flt.lf = FL;
      flt.lo = FLO;
      flt.hf = FH;
      flt.ho = FHO;

      while ( ( i = getopt (argc, argv, "va:f:n:o:p:t:")) != -1)
           switch (i) {
 
           case 'v':
               Log = 1;
               break;
           case 'a':
               oauth = strdup(optarg);
               break;
           case 'f':
               fil_str = strdup(optarg);
               sscanf(fil_str, FLT_SCS, FLT_RVL(&flt) );
               break;
           case 'n':
               Net = strdup(optarg);
               net = strdup(optarg);
               break;
           case 'o':
               want_orig = strdup(optarg);
               break;
           case 'p':
               pfile = strdup(optarg);
               break;
           case 't':
               twin = atoi(optarg);
               break;
           default: 
               usage();
      }
      
      if ( argc - optind != 1 )
          usage ();

      dbin = argv[optind++];

      if (( code=regcomp(&orig_match, want_orig, REG_EXTENDED|REG_NOSUB)) != 0) 
            elog_die( 1, "regcomp error #%d for orig_match - %s\n", code, want_orig );
      if (( code=regcomp(&auth_match, oauth, REG_EXTENDED|REG_NOSUB)) != 0) 
            elog_die( 1, "regcomp error #%d for auth_match - %s\n", code, oauth );

      if (( code=regcomp(&net_match, net, REG_EXTENDED|REG_NOSUB)) != 0) 
            elog_die( 1, "regcomp error #%d for net_match - %s\n", code, net );

      EvArr=newarr(0);
      AllEv=newarr(0);

/*
      if( pfile ) read_pfile( pfile ) ;
*/

      /*  Open database.  */

      if (dbopen_database (dbin, "r+", &db) == dbINVALID )  
         elog_die(0, "Can't open database %s\n", dbin );

      dbw = dblookup (db, 0, "wfdisc", 0, 0);
      dba = dblookup (db, 0, "arrival", 0, 0);
      if ( dbw.table == dbINVALID || dba.table == dbINVALID )  
          elog_die(0, "Can't open '%s' wfdisc/arrival table.\n", dbin );

      dbwa = dbjoin( dbw, dba, 0, 0, 0, 0, 0);

      dbquery (dbwa, dbRECORD_COUNT, &nrec);
      if( nrec <= 0 ) 
         elog_die( 0, " no record after wfdisc/arrival join\n");

      /* Select vertical channels only  */

      sprintf( key,"(chan =~ /.*Z/)\0" );            
      dbwa = dbsubset( dbwa, key, 0 );
      sort_sta_ch_tm = strtbl("sta", "chan", "time", 0 ) ;
      dbwa = dbsort (dbwa, sort_sta_ch_tm, 0, 0 ) ; 
 
      dbquery (dbwa, dbRECORD_COUNT, &nrec);
      if( nrec <= 0 )
            elog_die( 0, " no record with Z component in wfdisc/arrival join\n") ;      
         
      /* Join assoc&origin&event&arrival tables  */

      if( ( jrec = join_db ( db, want_orig )) <= 0 )
          elog_die( 0, "There are no records in joined table.\n");

      EvTbl = keysarr( EvArr );
      evnum = maxtbl( EvTbl );
      if( evnum <= 0 ) elog_die(0, "event TBL is empty.\n");

      for ( i = 0; i < evnum; i++) {
          str = ( char *) gettbl( EvTbl, i );
 	  evrec = ( EvRec *) getarr( EvArr, str );

          if( Log ) fprintf (stderr, "Event #%d\n", evrec->evid );

          dball.record = evrec->record;

          /* For each event ID process all origin IDs   */

          while ( origin ( evrec->evid, jrec, &orid, &mb, &ms, &otime, auth )) { 
                gotone = 0; 
                sprintf( orig_str, "%d\0", orid);
 	        if( Log ) fprintf ( stderr, "Orid #%s\n", orig_str );
                
                if( regexec( &orig_match, orig_str, (size_t) 0, NULL, 0 ) || 
                    regexec( &auth_match, auth, (size_t) 0, NULL, 0 ) )  {
                    dball.record++;
                    continue;
                }

          /* For each origin ID process all arrival IDs   */

 		while ( assoc ( jrec, &arec, stanet, sta, phase, &arid, &delta, &otime)) {
                     sprintf( key, "%s_%s_%s\0", net, sta, phase );
                     if( ( event = getarr( AllEv, key ) ) == 0 )  
                         event = new_ev( );
                     
                     event->otime = otime ;
                     event->stime = event->otime + delta*DELC/SWVEL;
                     event->etime = event->stime + twin;
                     event->delta = delta ;
                     event->amp = -999.99 ;
                     event->mb = mb ;
                     event->ms = ms ;
                     event->arec = arec ;
                     event->arid = arid ;
                     event->orid = orid ;
                     event->evid = evrec->evid ;
                     strcpy(event->net, stanet); 
                     strcpy(event->sta, sta); 
                     strcpy(event->phase, phase); 
                     strcpy(event->auth, auth);

		  /* Calculate MS */		
	
		     if (!proc_sta( dbwa, nrec, event, fil_str )) 
                         continue;
                     
                     setarr ( AllEv, key, event );
                     if( Log )
                        fprintf( stderr, 
                        "sta: %s orid: %d evid: %d delta: %lf mb: %lf ms: %lf\n", 
                         event->sta, event->orid, event->evid, event->delta, 
                         event->mb, event->ms );

		     if( !gotone ) gotone = 1; 
 		}

                if( gotone ) 
		    if (!save_ms (db) ) 
			elog_complain( 0, "Can't save ms for orid=%d.\n", orid );
            }
        }
 	exit (0);
}
