#include "dccmd.h"       

int sendcmd( int dcfp, char *cmd, int len, int echo )
{
  extern char    *optarg;
  extern int      optind;
  int    nbytes;
  char   echo_buf[IBUF_SIZE];


        nbytes = write ( dcfp, (char *) cmd, len );
        if ( nbytes != len )  return 0;
		
        if( echo )  {
             nbytes = read( dcfp, (char *) echo_buf, len );
             if( nbytes != len  )  {
                  elog_complain(1, "\nCan't get an echo of %s.\n", cmd );
                  return 0;
             }
             if( strncmp( echo_buf, "EE", 2) == 0 )  { 
                    elog_complain( 0, "\nDC rejected %s command.\n", cmd );
	            return 0;	
             } 
             if( strncmp( cmd, echo_buf, len )!= 0 )  {
                 elog_complain( 0, "\necho != command (%s != %s).\n", cmd, echo_buf );
	         return 0;
	     }
        }
	
     return 1;
}

    
