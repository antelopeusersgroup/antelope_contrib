#include "dccmd.h"       

extern char *ucase();

char *pfile;
Arr *Dases;
Arr *Dasid;
Tbl *CmdArg;
Tbl *Dlist;
int Log;
regex_t argument;

struct sockaddr_in myadd_in;           
struct sockaddr_in peer_in;

main(argc, argv)
int argc;
char *argv[];
{
  extern char    *optarg;
  extern int      optind;
  int    dasid, i, code;
  int    cmdlen, err, num;
  int    echo, dcfp;
  char   *dcname, *cmd, *name ;
  char   buffer[IBUF_SIZE];
  char  *key;

   elog_init (argc, argv) ;
   elog_notify (0, "$Revision$ $Date$" );
   Program_Name = argv[0];
   
  /* Set command line parameters default values  */
 
  while ( ( i = getopt (argc, argv, "p:v")) != -1)
        switch (i) {

        case 'v':
            Log = 1;
            break;
        case 'p':
            pfile = optarg;
            break;
        default: 
            usage();
        }
       if ( argc - optind != 3 )
          usage ();

       cmd = ucase( strdup(argv[optind++]));
       name = ucase( strdup(argv[optind++]) );
       dcname = strdup(argv[optind++]);
        
       if ( (code = regcomp( &argument, name, REG_EXTENDED|REG_NOSUB)) != 0)
	          elog_die( 1, "\nregcomp error #%d for %s\n", code, name );
     
    
       if(!open_dc( dcname, &dcfp  ) )  
           elog_die(0, "\nCan open %s DC.\n", dcname );

       CmdArg=0; Dases=0; Dasid=0; Dlist=0;
       pfile = "pkt";

       if(  !strncmp( cmd, "ZD", 2 )  ||
            !strncmp( cmd, "XX", 2 )  )  {

          if( islegal( name, DCCMD) )  {
	     sprintf( &buffer[0], "%2s%2s\0", cmd, cmd );
             cmdlen = 4; echo = 0; 
             if( !sendcmd( dcfp, &buffer[0], cmdlen, echo )) {
	          close(dcfp);
                  elog_die( 0, "\nCan't send %s to %s DC.\n", buffer, dcname );
             }
	     elog_complain(0, "\n%s command was sent to %s.\n", cmd, dcname);

	  }  else elog_die( 0, "\n%s command has illegal argument - %s\n", cmd, name );

     }  else if ( strncmp( cmd, "IP", 2 ) == 0 )  {            

          if( islegal( name, DCCMD) )  {
	      key = (char *) gettbl( CmdArg, 0 );
              sprintf( &buffer[0], "%2s%s\0", cmd, key );
              cmdlen = strlen(buffer); echo = 0;
              if( !sendcmd( dcfp, &buffer[0], cmdlen, echo )) {
	          close(dcfp);
                  elog_die( 0, "\nCan't send %s to %s DC.\n", buffer, dcname );
              }
	     elog_complain(0, "\n%s was sent to %s.\n", buffer, dcname);

	  }  else elog_die( 0, "\n%s command has illegal argument - %s\n", cmd, name );


     }  else if ( !strncmp( cmd, "CF", 2 ) ||
               !strncmp( cmd, "CO", 2 )  )  {
               if( strncmp( name, "MAIN", 4 ) == 0 )
                   sprintf( &buffer[0], "%2s01%2s\0", cmd, cmd );
	       else if ( strncmp( name, "AUX", 3 ) == 0 )
                   sprintf( &buffer[0], "%2s02%2s\0", cmd, cmd );
	       else 
	       elog_die( 0, 
	       "\n%s command has illegal argument - %s\n. \nOnly \'MAIN\' or \'AUX\' must be specified.", cmd, name );
              
	    cmdlen = strlen(buffer); echo = 1;
            if( !sendcmd( dcfp, &buffer[0], cmdlen, echo )) {
	          close(dcfp);
                  elog_die( 0, "\nCan't send %s to %s DC.\n", buffer, dcname );
            }
	    elog_complain(0, "\n%s was sent to %s.\n", buffer, dcname);


     }  else if ( !strncmp( cmd, "RO", 2 ) ||
               !strncmp( cmd, "RF", 2 ) )  {

          if( (num = islegal( name, CRCMD)) )  {
              for(i = 0; i < num ; i++ ) {
		 key = gettbl( CmdArg, i );
		 dasid = atoi( key);
		 sprintf( &buffer[0], "%2s%02d%2s\0", cmd, dasid, cmd );
                 cmdlen = 6; echo = 1;
                 if( !sendcmd( dcfp, &buffer[0], cmdlen, echo )) {
	               close(dcfp);
                       elog_die( 0, "\nCan't send %s to %s DC.\n", buffer, dcname );
                 }
	         elog_complain(0, "\n%s was sent to %s.\n", buffer, dcname);
		 sleep(30);
	      }
	  }  else elog_die( 0, "\n%s command has illegal argument - %s\n", cmd, name );
           
     }  else if ( !strncmp( cmd, "RC", 2 ) ||           
               !strncmp( cmd, "RS", 2 ) ||
               !strncmp( cmd, "ZS", 2 ) ||
               !strncmp( cmd, "TO", 2 ) ||
               !strncmp( cmd, "TF", 2 ) ) {

          if( (num = islegal( name, DASCMD)) )  {
              for(i = 0; i < num ; i++ ) {
		 key = gettbl( CmdArg, i );
		 dasid = atoi( key);
                 sprintf( &buffer[0], "%2s%4d%2s\0", cmd, dasid, cmd );
                 cmdlen = 8; echo = 1;
                 if( !sendcmd( dcfp, &buffer[0], cmdlen, echo )) {
	               close(dcfp);
                       elog_die( 0, "\nCan't send %s to %s DC.\n", buffer, dcname );
                 }
	         elog_complain(0, "\n%s was sent to %s.\n", buffer, dcname);
		 sleep(30);
	      }
	  }  else elog_die( 0, "\n%s command has illegal argument - %s\n", cmd, name );
     } 


    exit(0);      
}

