/************************************************************************
  
  * 
  *
  *
************************************************************************/
#include "mrc.h"       

char *pfile = "pkt";

extern void init();
extern DAS *new_das();

Arr *Dases;
Arr *Dasid;
int Log;
int Ls;
struct sockaddr_in myadd_in;           
struct sockaddr_in peer_in;

void usage ()
{
    fprintf (stderr, "Usage: %s [ -v ] [-D dasid] [-R] [-V verbatim] [-d dasid] [-o mrcpar] [ -p pfile ] [-s staname] [-t timeout] iport \n", Program_Name);
    banner (Program_Name, "$Revision$ $Date$");
    exit (1);
}

main(argc, argv)
int argc;
char *argv[];
{
  extern char    *optarg;
  extern int      optind;
  FILE 		 *fplog;
  Pf		 *pf;
  int 		  cmd_timeout=1, 
                  dases_timeout=5*60,
                  das_timeout=0;
  int 		  nrepeat = 2, collect_time=1;
  int     	  i, len, nbytes;
  int	          send_err, htype=0;
  double        window = 0,
                  until=1.0e99;
  char            *staname = 0;     
/*  char            *pffile = "mrc"; */
  char            *pffile = "sendMRC";
  char            *logname= 0; 
  char            *wstr, *timstr = 0,
                  unit_ascii[5];
  char            buffer[IBUF_SIZE];
  char            echo_buf[IBUF_SIZE];
  int 		  timeout = 5;
  int             *val, num, dasid=0, dasnum;
  int 	          done = 0, err = 0;
  struct Prts     Ports;
  DAS		  *das=0;
  Tbl 		  *keys;
  char 		  *key;
  char *iport = 0;
  int resetDAS = 0,
      resetDC = 0,
      portnum;
  double rq_time = 0.0, 
         crnt_time, 
	 tmwait = 0.0 ;

   elog_init (argc, argv) ;
   elog_notify (0, "$Revision$ $Date$") ;
   Program_Name = argv[0];
   
  /* Set command line parameters default values  */
 
  while ( ( i = getopt (argc, argv, "V:D:Rvd:o:p:s:t:")) != -1)
        switch (i) {
        case 'V':
            logname = optarg; 
            if ( ( fplog = fopen ( logname, "a+" ) ) == NULL ) 
              elog_die( 1, "Can't open '%s' for logs \n", logname ) ; 
            break ;

        case 'R':
            resetDC = 1;
            break;
        case 'D':
            resetDAS = 1;
	    dasid = atoi(optarg);
            break;
        case 'v':
            Log = 1;
            break;
        case 'd':
            if( staname != 0 )  {
	      elog_complain( 0, "Staname was pecified already. Will ignore 'dasid'.\n");
	    } else
	      dasid = atoi(optarg);
            break;
        case 'o':
            pffile = optarg;
            break;
        case 'p':
            pfile = optarg;
            break;
        case 's':
            if( dasid > 0 )  {
	      elog_complain( 0, "'dasid' was pecified already. Will ignore 'staname'.\n");
	    } else
            staname = optarg;
            break;
        case 't':
            timeout = atoi(optarg);
            break;
        default: 
            usage();
        }
       if ( argc - optind < 1 || argc - optind > 3   )
          usage ();
        
       iport = argv[optind++];
       strcpy( Ports.ip_name, iport );
       Ports.ip_name[strlen(iport)] = '\0';



       if( pfread ( pffile, &pf) != 0 )
         elog_die(0, "Can't read parameter file\n");

       timstr = pfget_string( pf, "start_time");
       nrepeat = pfget_int( pf, "num_repeats");
       das_timeout = pfget_int( pf, "timeout_btw_onedas");
       dases_timeout = pfget_int( pf, "timeout_btw_dases");
       dases_timeout *= 60;
       cmd_timeout = pfget_int( pf, "timeout_btw_cmds");

       if( timstr ) rq_time = str2epoch( timstr );
       if (argc - optind >= 1) {
          timstr = argv[optind++] ;
          rq_time = str2epoch (timstr);
	  if ( argc - optind == 1 ) { 
	          wstr = argv[optind++] ; 
	          window = str2epoch ( wstr ) ; 
	          until = rq_time + window ;
	  }
       }
       crnt_time = now();
            
       if( ( tmwait = rq_time - crnt_time ) > 0 )
          sleep( tmwait);

       if( !dasid ) init();
       if( staname != 0 )  {
         val = ( int *) getarr(Dasid, staname) ;
	 if( val != 0 && *val > 0 )
	    dasid = *val;
	 else elog_die( 0, "Can't get dasid number for $s\n", staname );
       }
       while( !done || until >  crnt_time )  {

          portnum = 5001;
	  if( open_dc( &Ports, portnum ) <= 0 )
	     elog_die(1, "Can't open command DC port\n");
          if( !dasid )   {
             keys = ( Tbl *) keysarr( Dases );
             dasnum = maxtbl( keys );
	     if( dasnum <= 0 )  {
	       elog_complain( 0, "No DASes collected on RT system.\n");
	       sleep(10);
	       continue;
	     }
	  } else dasnum = 1;

          if( resetDC )  {

	    /* Send DAS RC command to specific DAS  */
		     
	     sprintf( &buffer[0], "XXXX\0");
             nbytes = write ( Ls, (char *) buffer, 4 );
 	     close( Ls ); 
             if ( nbytes != 4 ) 
	        elog_die( 0, "can't send RESET to DC\n");
	     else elog_die( 0, "RESET was sent to %s\n", iport );  

	  }
          if( resetDAS )  {

	    /* Send DAS RESET command to specific DAS  */
		     
	     sprintf( &buffer[0], "RS%4dRS\0", dasid);
             nbytes = write ( Ls, (char *) buffer, LEN );
             if ( nbytes != LEN )  {
 	        close( Ls ); 
	        elog_die( 0, "can't send %s to DAS %d(%d)\n", buffer, dasid, nbytes);
	     } else {
		
                nbytes = read( Ls, (char *) echo_buf, LEN);
		close( Ls );
	        if( nbytes != LEN )  {
	          send_err = 1;
	          elog_die(0, "can't get an echo of a DC command - %s\n", buffer );
                }
                if( strncmp( buffer, echo_buf, strlen( buffer ))!= 0 )  {
		    send_err = 1;
	            elog_complain(0, "echo != command (%s != %s)\n", buffer, echo_buf);
		    if( strncmp( echo_buf, "EE", 2) == 0 ) 
		       elog_complain( 0, "DC rejected command:%s\n", buffer);
		
	        } else elog_complain( 0, "send %s to %d\n", buffer, dasid );
	    } 
	    exit(0);
	  }
          for( i = 0; i < dasnum; i++ )  {
	      if( !dasid )  {
		  key = (char *) gettbl( keys, i );
	          das = ( DAS * ) getarr( Dases, key );
	      }
	      if( das == 0 && !dasid )
	         elog_complain( 0, "corrupted DAS array ( das = 0 for %s )\n",key);
	      else  {
		  if( dasid  && !das )  {
		     sprintf( unit_ascii, "%d\0", dasid);
		     das = ( DAS *) new_das( dasid, unit_ascii );
		  }

		  sprintf( unit_ascii, "%4d\0", das->unit);
                  num = nrepeat;
		    
		    /* Send DAS RC command to specific DAS  */
		     
		     sprintf( &buffer[0], "RC%4dRC\0", das->unit);
                     send_err = 0;
		     while( num ) {
                        nbytes = write ( Ls, (char *) buffer, LEN );
                        if ( nbytes == LEN ) {
                            if ( logname )
                               if (fwrite (buffer, LEN, 1, fplog ) != 1)  
	                          elog_die(1, "failed to copy DC command  to log file %s\n", logname );
                            nbytes = read( Ls, (char *) echo_buf, LEN);
	                    if( nbytes != LEN )  {
			       send_err = 1;
	                       elog_complain(0, "can't get an echo of a DC command - %s\n", buffer );
			       close( Ls );
			       if( open_dc( &Ports, portnum ) <= 0 )
			          elog_die( 1, "can't reopen DC port: %s:%d\n", Ports.ip_name, portnum );
                            } if( strncmp( buffer, echo_buf, strlen( buffer ))!= 0 )  {
				send_err = 1;
	                        elog_complain(0, 
				"echo != command (%s != %s)\n", buffer, echo_buf);
				if( strncmp( echo_buf, "EE", 2) == 0 ) {
				    elog_complain( 0, "DC rejected command:%s\n", buffer);
				    elog_complain(0, "re-sending...\n");
				} else {
				    close( Ls );
				    if( open_dc( &Ports, portnum ) <= 0 )
				      elog_die( 1, "can't reopen DC port: %s\n", Ports.ip_name);
				}
			    }
			    elog_complain( 0, "send %s to %d\n", buffer, das->unit );
   
                        } else { 
	                   send_err = 1;
			   elog_complain(0, "can't send a DC command - %s\n", buffer );
	    	           close( Ls );
			   if( open_dc( &Ports, portnum ) <= 0 )
			      elog_die( 1, "can't reopen DC port: %s\n", Ports.ip_name);
                        }
	                if( dasid ) exit(0);
			if( !send_err )  {
			    num--;
			    if( num > 0 ) sleep( das_timeout );
			} else send_err = 0;
		     }
	      }	      
	      sleep( dases_timeout);
          }
	  close(Ls);

	  if( cmd_timeout <= 0 ) exit(0);

          for( i = 0 ; i <  cmd_timeout; i++ ) 
	    sleep(ONEDAY); 

	  crnt_time = now();
       }

}


    
