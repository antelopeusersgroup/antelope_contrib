/*********************************************************************
 *
 *  collect_dases.c 
 *
 *
 *
 ********************************************************************/
#include "dccmd.h"

extern void initdas();

void usage ()
{
     fprintf (stderr,
    "usage: %s [ -v ] [ -p pfile ] cmdname cmd_argument dcname\n",
     Program_Name);
     fprintf (stderr,"The folowing DC commands are supported:\n");
     fprintf (stderr,"\tIP => Change_DC_IP\n");
     fprintf (stderr,"\tCF => Clock_OFF \n");
     fprintf (stderr,"\tCO => Clock_ON \n");
     fprintf (stderr,"\tRF => Master_Radio_OFF \n");
     fprintf (stderr,"\tRO => Master_Radio_ON \n");
     fprintf (stderr,"\tRC => Mass_Recenter \n");
     fprintf (stderr,"\tRS => DAS_Reset\n");
     fprintf (stderr,"\tTO => DAS_XMit_ON\n");
     fprintf (stderr,"\tTF => DAS_XMit_OFF\n");
     fprintf (stderr,"\tXX => Reset_DC\n");
     fprintf (stderr,"\tZS => Zero_DAS_Counters\n");
     fprintf (stderr,"\tZD => Zero_DC_Counters\n");
     exit (1);
}

char *newname( char *name )
{

    char *new;
    allot(char *, new, 16);
    strcpy( new, name);
    return new;
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

int isdasid( char *name) 
{
	int i, dasid;

        for( i = 0; i < strlen(name); i++ )  
          if( !isdigit(name[i]) ) return -1; 

        dasid = atoi(name);
    
        return  dasid;
}


int islegal( char *name, int cmdtype )
{
     
    int i, num=0;
    int dasnum;
    char *one, *arg;

    CmdArg = newtbl(0);

    switch( cmdtype)  {
	case DCCMD:

            if( (one = strtok( name, "|" )) != 0 )  {
                  if( isip(one) )  {
	             arg = newname( one );
		     pushtbl( CmdArg, arg);
	          } else elog_die(0, "\nillegal IP was specified - %s.\n", one);
	          num++;
	    }
            while( (one = strtok( NULL, "|" )) != 0 )  {
                  if( isip(one) )  {
	             arg = newname( one );
		     pushtbl( CmdArg, arg);
	          } else elog_die(0, "\nillegal IP was specified - %s.\n", one);
	          num++;
	    }
	    if( num > 1 )  {
	        elog_complain(0, 
		"\nMore than one IP addresses was specified. Will use the first one.\n");
	        num = 1;
	    }
	    break;

	case CRCMD:

            if( (one = strtok( name, "|" )) != 0 )  {
                  if( isdasid(one) >= 0 )  {
	             arg = newname( one );
		     pushtbl( CmdArg, arg);
	          } else elog_die(0, "\nillegal Radio/Clock number was specified - %s.\n", one);
	          num++;
	    }
            while( (one = strtok( NULL, "|" )) != 0 )  {
                  if( isdasid(one) >= 0 )  {
	             arg = newname( one );
		     pushtbl( CmdArg, arg);
	          } else elog_die(0, "\nillegal Radio/Clock number was specified - %s.\n", one);
	          num++;
	    }
	    break;

	case DASCMD:

	    initdas();
            dasnum = maxtbl( Dlist );
            for(i = 0; i < dasnum ; i++ )  {
		one = gettbl(Dlist, i );
		if( regexec( &argument, one, (size_t) 0, NULL, 0 ) == 0 )  {
		    if( (arg = (char *) getarr( Dases, one )) == 0 )
		       arg = (char *) getarr( Dasid, one);
		    pushtbl( CmdArg, arg);
		    num++;
		}
	     }
	    break;
    }

    return num;

}

int open_dc( char *dcname, int *dcfp )

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


    if (!strncmp (dcname, "localhost", strlen ("localhost")) )  { 
			     
           uname (&hname);
           hostname = strdup (hname.nodename);
			     
           hp = gethostbyname (hostname);
           if (hp == NULL) {
              elog_complain(0, "\nopenID(): Can't get info for HOST - %s.\n", hostname);
              return 0;
           }
    }
    server_name = strdup( dcname );
    if( port_ascii = strchr( server_name, ':'))  {
	*port_ascii++ = 0;
	port_num = atoi( port_ascii );
    } else port_num = CMDDC_PORT;

    while( !done )  {

        /* clear out structures */
 
  	memset((char *)&peer_in, 0, sizeof(struct sockaddr_in));
  	
	/* create a socket  */
 
       if( ( *dcfp = socket(AF_INET, SOCK_STREAM, 0)) < 0 )  {
    	    elog_die( 1, "\nCan't open stream socket\n" ) ; 
       }

  /* Convert IP address from a.b.c.d to the hexadecimal number  */
	   
       if ((int)(addr = inet_addr(server_name)) == -1) {
          elog_complain(0, "\nIP-address must be of the form a.b.c.d\n");
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

       if ( connect ( *dcfp, (struct sockaddr *) & peer_in, addrlen) == -1) {
           if( !tried )  {
	      tried = 1;
	      elog_complain( 1, "\nwaiting for connection \n");
              sleep(1);
	      elog_complain( 1, "\ncan't connect %s\n", dcname );
	   }
	   close( *dcfp);
        } else done = 1;	
    } 

     return 1;

}

void initdas( )

{
   Pf  		*Param;
   Tbl 		*Site;
   char		*das, *sta, prev_key[16], key[16];
   struct Site 	 site;
   char 	*istr;
   int 		i;
   int 		nsite;

   if(pfread( pfile, &Param) != 0)
       elog_die(0, "\nCan't read parameter file\n");
 
	/* Get Input & Network tables  */
	 
   Site = pfget_tbl(Param, "Site");
   nsite = maxtbl(Site);
    
   if( nsite <= 0  )
     elog_die( 0, "\nparameter file is not complete.\n");
 
  Dlist = newtbl(0);
  Dases = newarr( 0 );
  Dasid = newarr( 0 );
	   
  sprintf( &prev_key[0], "null\0");
  
  for( i = 0; i < nsite; i++ )  {

	istr = (char *) gettbl(Site, i);
	sscanf(istr, STE_SCS,  STE_RVL(&site));
	sprintf( key, "%d\0", site.sid);
	if( strncmp( &key[0], &prev_key[0], strlen(&key[0])) != 0 )  {
	    das = (char *) newname( key );
	    sta = (char *) newname( site.name );
	    pushtbl( Dlist, das );
	    pushtbl( Dlist, sta );
	    setarr(Dases, site.name, das );
	    setarr(Dasid, das, das );
	}
   }
}           


	   

