#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/utsname.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include "_pkt2.h"
#include "diagp.h"

#define PORTNUM 5001
int Timeout;

static Arr *DID = 0;
static Arr *DcIP = 0;

typedef struct dcip {
   int ifp;
   char name[20];
} DCIP;

DCIP *new_dcip( char *name )
{
    DCIP *new;

    allot( DCIP *, new, 1);
    strcpy( new->name, name );
    new->ifp = -1;

    return new;
}


/*
 *--------------------------------------------------------------
 *
 * dccCmd --
 *
 *	This procedure is invoked to process the "dcc" Tcl command.
 *
 * Results:
 *	A standard Tcl result.
 *
 *--------------------------------------------------------------
 */

int dccCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window associated with
				 * interpreter. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    int *ptr=0, dasid, gain, value;
    char *das=0, *dases=0, radval[8];
    char *pfile = 0;
    char *parval, *cmd, *dcname, *gainval;
    char errmsg[512], msg[512];
    Pf   *pf;
    DCIP *dc;

 
    if (argc < 6) {
        Tcl_AppendResult(interp, "wrong # args:  should be \"",
              argv[0], " dcname cmd_name das_list pf\n", (char *) NULL);
        return TCL_ERROR;
    }


    dcname = strdup( argv[1]);
    if( !isip(dcname) )  {
	 Tcl_AppendResult(interp, "not valit IP address", dcname, (char *) NULL);
         return TCL_ERROR;
        
    }

    cmd = strdup( argv[2]);

    /* Get dcname; open dc */


    if( DcIP == 0 ) DcIP = newarr(0);
    if( ( dc = getarr( DcIP, dcname ) ) == 0 )  {
       dc = new_dcip( dcname );
       setarr( DcIP, dcname, dc );
    }

    if( dc->ifp < 0 )
        if(!open_dc( dc ) )  {
           Tcl_AppendResult(interp, "Can open ", dcname, " DC.", (char *) NULL);
	   free(cmd); free(dcname);
           return TCL_ERROR;
        } 
/*
fprintf(stderr, "%d %s %s %s %s %s %s %s\n", argc, argv[0], argv[1], argv[2], argv[3],
argv[4], argv[5], argv[6], argv[7] );
fflush(stderr);

*/
    if( strncmp(argv[3], "dc", 2) != 0 ) {

        dases = strdup(argv[3]);
        pfile = strdup(argv[4]);
	parval = strdup(argv[5]);
	gainval = strdup(argv[6]);
        Timeout = atoi( argv[7] );
        Timeout *= 1000;

        if( strncmp( cmd, "IP", 2) == 0 )  {
     
            if( !send_cmd( interp, dc, cmd, 0, dases,0,0 )) {
	            close(dc->ifp);
                    dc->ifp = -1;
                    setarr( DcIP, dc->name, dc );
	            free(parval);
                    free( dases); free(pfile);
	            free(cmd); free(dcname);
                    return TCL_ERROR;
            } else return TCL_OK;
        }

	if( ( value = isdasid(parval) ) == -1 )  {
             if( strncmp( cmd, "CN", 2 ) == 0 ) {
		 value = 14;
	     }  else  {
	   	 Tcl_AppendResult(interp, "parameter value is not a digit", parval, (char *) NULL);
	         free(cmd); free(dcname);
	         free(dases); free(pfile);
	         free(parval);
	         return TCL_ERROR;
	     }
	}

	if( ( gain = isdasid(gainval) ) == -1 )  {
             Tcl_AppendResult(interp, "gain value is not a digit", gainval, (char *) NULL);
	       free(cmd); free(dcname);
	       free(dases); free(pfile);
	       free(parval); free(gainval);
	       return TCL_ERROR;
	}

        if( DID == 0 )   {
           if( pfread ( pfile, &pf) != 0 )  {
               Tcl_AppendResult(interp, "Can read  ", pfile, " parameter file.", (char *) NULL);
	       free(parval); free(gainval);
	       free(cmd); free(dcname);
	       free(dases); free(pfile);
	       return TCL_ERROR;
           }  else init( interp, pf );
        }

        if( (das = strtok( dases, "," )) != 0 )  {
            if( strncmp( das, "MMM1", 4) == 0 ||
                    strncmp( das, "MMM2", 4)  == 0 ||
                    strncmp( das, "MMM3", 4) == 0 )
                sprintf( radval, "%s\0", das);

	    else  {
	    
	        if( (ptr = (int *) getarr( DID, das )) == 0 ) {
	
                    if( (dasid = isdasid(das) ) == -1 )  {
                        Tcl_AppendResult(interp, "Can get dasid for ", das, (char *) NULL);
	                free(parval); free(gainval);
	                free(cmd); free(dcname);
	                free(dases); free(pfile);
	                return TCL_ERROR;
                     }
                }  else {
		    dasid = *ptr;
		}
		sprintf( radval, "%04d\0", dasid);
	    }
	
            if( !send_cmd( interp, dc, cmd, dasid, radval, value, gain )) {
	            close(dc->ifp);
                    dc->ifp = -1;
                    setarr( DcIP, dc->name, dc );
	            free(parval);
                    free( dases); free(pfile);
	            free(cmd); free(dcname);
                    return TCL_ERROR;
            }
        }

        while( ( das = strtok( NULL, "," )) != 0 )  {
            if( strncmp( das, "MMM1", 4) == 0 ||
                strncmp( das, "MMM2", 4) == 0 ||
                strncmp( das, "MMM3", 4) == 0 )
                sprintf( radval, "%s\0", das);

	    else  {
	    
                if( (ptr = (int *) getarr( DID, das )) == 0 ) {
                     if( (dasid = isdasid(das) ) == -1 )  {
                        Tcl_AppendResult(interp, "Can get dasid for ", das, (char *) NULL);
	                free(parval); free(gainval);
	                free(cmd); free(dcname);
	                free(dases); free(pfile);
	                return TCL_ERROR;
                      }
                }  else dasid = *ptr;
	           
		sprintf( radval, "%04d\0", dasid);
           }
           if( !send_cmd( interp, dc, cmd, dasid, radval, value, gain )) {
	             close(dc->ifp);
                     dc->ifp = -1;
                     setarr( DcIP, dc->name, dc );
	             free(parval); free(gainval);
                     free( dases); free(pfile);
	             free(cmd); free(dcname);
	             return TCL_ERROR;
           }
        }

    }  else  if( !send_cmd( interp, dc, cmd, 0, 0 , 0, 0)) {
	  close(dc->ifp);
          dc->ifp = -1;
          setarr( DcIP, dc->name, dc );
	  free(parval); free(gainval);
	  free(cmd); free(dcname);
	  free(dases); free(pfile);
          return TCL_ERROR;
    }

    return TCL_OK;
}

int isdasid( char *das) 
{
	int i, dasid;
    
        for( i = 0; i < strlen(das); i++ )  
          if( !isdigit(das[i]) ) return -1; 

        dasid = atoi(das);
    
        return  dasid;

}


int send_cmd( Tcl_Interp *interp, 
              DCIP *dc, 
	      char *cmd, 
	      int dasid,
	      char *radval, 
	      int parval ,
	      int gain )

{
  double crnt_time;
  int   i, len, nbytes;
  char  buffer[64];
  char  echo_buf[64];
  char 	errmsg[512], msg[512];
  int   timeout = Timeout;
  
     crnt_time = now();
 

     if( strncmp( cmd, "AO", strlen("AO")) == 0  ||
         strncmp( cmd, "AF", strlen("AF")) == 0  ||
         strncmp( cmd, "BF", strlen("BF")) == 0  ||
         strncmp( cmd, "DO", strlen("DO")) == 0  ||
         strncmp( cmd, "DF", strlen("DF")) == 0  ||
         strncmp( cmd, "ST", strlen("ST")) == 0  ||
         strncmp( cmd, "ZD", strlen("ZD")) == 0  ||
         strncmp( cmd, "XO", strlen("XO")) == 0  ||
         strncmp( cmd, "XF", strlen("XF")) == 0  ||
         strncmp( cmd, "XX", strlen("XX")) == 0  )  {

          sprintf( &buffer[0], "%2s%2s\0", cmd, cmd );
          len = strlen(buffer);

     }  else if ( strncmp( cmd, "NR", 2) == 0  ||
                  strncmp( cmd, "RT", 2) == 0  ||
                  strncmp( cmd, "PL", 2) == 0  ||
                  strncmp( cmd, "PR", 2) == 0  )  {

          sprintf( &buffer[0], "%2s%s%02d%2s\0", cmd, radval, parval, cmd );
          len = strlen(buffer);
     }  else if ( strncmp( cmd, "CN", 2) == 0  )  {
	  if ( parval == 14 ) 
             sprintf( &buffer[0], "%2s%s%0e%2s\0", cmd, radval, cmd );
	  else 
             sprintf( &buffer[0], "%2s%s%02d%2s\0", cmd, radval, parval, cmd );

     }  else if ( strncmp( cmd, "PG", 2) == 0  ) {

          sprintf( &buffer[0], "%2s%04d%02d%02d%2s\0", cmd, dasid, parval, gain, cmd );
          len = strlen(buffer);

     }  else if ( strncmp( cmd, "CF", strlen("CF")) == 0  ||
               strncmp( cmd, "CO", strlen("CO")) == 0  ||
               strncmp( cmd, "RO", strlen("CO")) == 0  ||
               strncmp( cmd, "RF", strlen("CO")) == 0  )  {

          sprintf( &buffer[0], "%2s%02d%2s\0", cmd, dasid, cmd );
          len = strlen(buffer);
           
     }  else if ( strncmp( cmd, "RC", strlen("RC")) == 0  ||           
               strncmp( cmd, "RS", strlen("RS")) == 0  ||
               strncmp( cmd, "ZS", strlen("ZS")) == 0  ||
               strncmp( cmd, "TO", strlen("TO")) == 0  ||
               strncmp( cmd, "TF", strlen("TF")) == 0  ) {

          sprintf( &buffer[0], "%2s%4d%2s\0", cmd, dasid, cmd );
          len = strlen(buffer);

     }  else if ( strncmp( cmd, "IP", strlen("IP")) == 0 )  {            
          sprintf( &buffer[0], "%2s%s\0", cmd, radval );
          len = strlen(buffer);

     } 
/*
fprintf(stderr, "COMMAND %s EXECUTED!", buffer);
fflush(stderr);
*/
/* FOR TEST PERPOSE ONLY  */
/*
sprintf(errmsg, "COMMAND %s EXECUTED!", buffer);
sprintf( &msg[0], "%s {%s} \0", MSG_PROC, errmsg);
Tcl_Eval ( interp, msg );
while ( Tk_DoOneEvent (TK_DONT_WAIT) )
              ;
return 1;
*/


              nbytes = write ( dc->ifp, (char *) buffer, len );
              if ( nbytes != len ) {
                   sprintf(errmsg, "Can't send %s.\nDC socket error.", buffer);
	           sprintf( &msg[0], "%s {%s} \0", ERR_PROC, errmsg);
                   Tcl_Eval ( interp, msg );
                   while ( Tk_DoOneEvent (TK_DONT_WAIT) )
                     ;
                   return 1;
              }  else { 

 /* 
fprintf(stderr, "send %s \n", buffer);
fflush(stderr);
*/

                 if( !strncmp( cmd, "XX", 2) || 
                     !strncmp( cmd, "IP", 2) ) {

                     sprintf(errmsg, "COMMAND %s EXECUTED!", buffer);
	             sprintf( &msg[0], "%s {%s} \0", MSG_PROC, errmsg);
                     Tcl_Eval ( interp, msg );
                     while ( Tk_DoOneEvent (TK_DONT_WAIT) )
                           ;
                     return 1;
                 }  else  {
		
                    nbytes = read( dc->ifp, (char *) echo_buf, len );
                    if( nbytes != len  )  {
/*
fprintf( stderr, "%s %s\n", buffer, echo_buf );
fflush(stderr);
*/

                      sprintf(errmsg, "Can't get an echo of %s.\n\tCOMMAND LOST!", buffer);
	              sprintf( &msg[0], "%s {%s} \0", ERR_PROC, errmsg);
                      Tcl_Eval ( interp, msg );
                      while ( Tk_DoOneEvent (TK_DONT_WAIT) )
                       ;
                      return 1;
                     }
                     if( strncmp( buffer, echo_buf, len )!= 0 )  {
                         sprintf(errmsg, "echo != command(%s!=%s).\n\tCOMMAND LOST!", 
	                   echo_buf, buffer);
	                 sprintf( &msg[0], "%s {%s} \0", ERR_PROC, errmsg);
                         Tcl_Eval ( interp, msg );
                         while ( Tk_DoOneEvent (TK_DONT_WAIT) )
                              ;
                         return 1;
                     }
                 }
              }
              sprintf(errmsg, "COMMAND %s EXECUTED!", buffer);
	      sprintf( &msg[0], "%s {%s} \0", MSG_PROC, errmsg);
              Tcl_Eval ( interp, msg );
              while ( Tk_DoOneEvent (TK_DONT_WAIT) )
                        ;
              return 1;
}

			       
int open_dc( DCIP *dc )

{
  	struct hostent *hp;                /* remote host info pointer */
	struct utsname hname;
  	u_long addr;
  	int s, addrlen, port_num;                    /* size of a structure */
  	struct sockaddr_in peer_in;
        char *hostname;                    /* machine which wants connection */
  	char *port_ascii;
	char *server_name;
        int done = 0, tried = 0;

/* ONLY for TEST PERPOSE  */
/*
return 1;
*/

    if (!strncmp (dc->name, "localhost", strlen ("localhost")) )  { 
			     
           uname (&hname);
           hostname = strdup (hname.nodename);
			     
           hp = gethostbyname (hostname);
           if (hp == NULL) {
              elog_complain(0, "openID(): Can't get info for HOST - %s.\n", hostname);
              return 0;
           }
    }
    server_name = strdup( dc->name );
    if( port_ascii = strchr( server_name, ':'))  {
	*port_ascii++ = 0;
	port_num = atoi( port_ascii );
    } else port_num = PORTNUM;

    while( !done )  {

        /* clear out structures */
 
  	memset((char *)&peer_in, 0, sizeof(struct sockaddr_in));
  	
	/* create a socket  */
 
       if( ( dc->ifp = socket(AF_INET, SOCK_STREAM, 0)) < 0 )  {
    	    elog_die( 1, "Can't open stream socket\n" ) ; 
       }

  /* Convert IP address from a.b.c.d to the hexadecimal number  */
	   
       if ((int)(addr = inet_addr(server_name)) == -1) {
          elog_complain(0, "IPD/open_socket():IP-address must be of the form a.b.c.d\n");
          return 0;
       }

       hp = gethostbyaddr((char *)&addr, sizeof (addr), AF_INET);
       if(hp == NULL)

          peer_in.sin_addr.s_addr = htonl(addr);  
       else 
           peer_in.sin_addr.s_addr = ((struct in_addr *)(hp->h_addr))->s_addr;
  
       peer_in.sin_family = AF_INET; 
       peer_in.sin_port = htons( port_num );

       addrlen = sizeof(struct sockaddr_in);

       if ( connect (dc->ifp, (struct sockaddr *) & peer_in, addrlen) == -1) {
           if( !tried )  {
	      tried = 1;
	      elog_complain( 1, "waiting for connection \n");
              sleep(1);
	      elog_complain( 1, "can't connect %s\n", dc->name );
	   }
	   close(dc->ifp);
        } else done = 1;	
    } 

    return 1;

}
int init( Tcl_Interp *interp, Pf *Param )
 
{
   Tbl          *Site;
   struct Site   site;
   char         *istr;
   char         dasid[6];
   int          pkttype;
   int          ntrec, i, j;
   int          nsite;
   int          *num = 0;
 
        /* Get Input & Network tables  */
 
   Site = pfget_tbl(Param, "Site");
   nsite = maxtbl(Site);
 
   if( nsite <= 0  )  {
        Tcl_AppendResult(interp, "Parameter file is not complete.", (char *) NULL);
	return TCL_ERROR;
   } 
 
  DID = newarr( 0 );
 
  for( i = 0; i < nsite; i++ )  {
 
        istr = (char *) gettbl(Site, i);
        sscanf(istr, STE_SCS,  STE_RVL(&site));
           sprintf( dasid, "%d\0", site.sid );
 
           if( (num = ( int *) getarr( DID, site.name ) ) == NULL )  {
              allot( int *, num, 1 );
              *num = site.sid;
              setarr(DID, (char *) site.name, (char *) num );
           }
   }
}
 

int isip( char *name) 
{
	int i, pnts_num;

        for( i = 0, pnts_num=0; i < strlen(name); i++ )  
          if( !isdigit(name[i]) ) {
	      if( i > 0 && name[i] == '.' ) pnts_num++;
	      else return 0;
	  }

        if(pnts_num != 3 ) return 0;
    
        return  1;
}



	   

