/************************************************************************
  *
  *  sendmrc.c
  *
  *  send a mass re-centering 'RCRC' command to DAS trhough DC port
  *
  *
  *  Author: Marina Harkins-Glushko
  *  	    UCSD, IGPP
  *	    glushko@ucsd.edu
************************************************************************/
#include "mrc.h"       
#define LEN   8

extern Tbl *DC;

int sendmrc( int dasid, int timeout, int rep )

{

    int i, numdc, num;
    int nbytes;
    char buffer[64],
         echo_buf[64];
    char *iport, *s;


    /* Send DAS RC command to specific DAS  */
	     
  sprintf( &buffer[0], "RC%4dRC\0", dasid);
 
  numdc = maxtbl(DC);
  for( num = 0; num < numdc; num++ )  {

    iport = (char *) gettbl(DC, num );
    if( open_dc( iport) <= 0 )               
       elog_die(1, "Can't open command DC port\n");
		    
    for( i = 0; i < rep; i++ )  {
        nbytes = write ( Ls, (char *) buffer, LEN );
        if ( nbytes == LEN ) {
            if ( logname )
                if (fwrite (buffer, LEN, 1, fplog ) != 1)  
	              elog_die(1, "can't log a DC command  to log file %s\n", logname );
            nbytes = read( Ls, (char *) echo_buf, LEN);
            if( nbytes != LEN )  {
               elog_complain(0, "can't get an echo of a DC command - %s\n", buffer );
	       close( Ls );
	       if( open_dc( iport) <= 0 )               
	          elog_die( 1, "can't reopen DC port: %s\n", iport );
            } 
	    if( strncmp( buffer, echo_buf, strlen( buffer ))!= 0 )  {
                elog_complain(0, "echo != command (%s != %s)\n", buffer, echo_buf);
		if( strncmp( echo_buf, "EE", 2) == 0 ) {
		    elog_complain( 0, "DC rejected command:%s\n", buffer);
		    elog_complain(0, "re-sending...\n");
		} else {
		    close( Ls );
		    if( open_dc( iport) <= 0 )               
		         elog_die( 1, "can't reopen DC port: %s\n", iport );
		}
            }
            elog_complain( 0, "%s: send %s to %d on %s\n", 
	              s=strtime(now()), buffer, dasid, iport );
            free(s);

        } else { 
	   elog_complain(0, "can't send a DC command - %s on %s\n", buffer, iport );
           close( Ls );
           if( open_dc( iport) <= 0 )               
              elog_die( 1, "can't reopen DC port: %s\n", iport);
        }
        sleep(timeout); 

      }
      close(Ls);
  }
   return 1;
}


    
