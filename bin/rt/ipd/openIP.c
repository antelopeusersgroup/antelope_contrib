/*************************************************************************
 *
 *  openIP.c
 *
 *  Open input port(s) - SCSI, serial, HSI, socket.                          
 *
 *
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 *************************************************************************/
#include "ipd.h"
#include <sys/dkio.h>
#include <sys/dklabel.h>
#include <sys/vtoc.h>
#include <termio.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
			       
extern int Psize;

int 
open_IN_ports( inport )
struct Prts *inport;

{
 
	struct stat buf;

/* What type of port we have?  */


	if((stat(inport->ip_name, &buf)) != 0)  {
  	   if(ENOENT)  {
		if( ( isdigit( inport->ip_name[0] ) && inport->ip_name[3] == '.' ) ||
		      !strncmp( inport->ip_name, "local", strlen("local") ) )  {
	              return open_socket( inport );
	        } 
                elog_complain(0, "open_IN_ports():Port:%s doesn't exist!\n", inport->ip_name );
                return 0; 
           } else {
                elog_complain(1, "open_IN_ports():can't stat %s.", inport->ip_name );
                return 0; 
           }
       }  else if( S_ISCHR(buf.st_mode) )  {
	  
	   if( !strncmp( inport->ip_name , "/dev/rsd", strlen("/dev/rsd") ) )  
#ifndef __i386
               return open_disk( inport );
#else 
	       return 0;
#endif
   
               else return open_chr( inport );

      }  else if( S_ISDIR(buf.st_mode) || S_ISREG(buf.st_mode) ||
               S_ISBLK(buf.st_mode) || S_ISLNK(buf.st_mode) )  {
 
               elog_complain(0, "open_IN_ports():%s can't be Input Port!", inport->ip_name);
               return 0; 
      }
}

#ifndef __i386
/* Input Port is Raw Disk.  */

int open_disk( inport )
struct Prts *inport;

{
 
	struct dk_geom geom;
	struct dk_allmap part;
	struct dk_label label;
	int val, nbytes;
	char buffer[1024];


	if (( inport->ifp = open( inport->ip_name, O_RDONLY ) ) < 0)  {
	  elog_complain(1,"open_IN_ports():Can't open %s disk.\n", inport->ip_name );
	  return 0; 
        }
   
           /* get disk geometry  */

   	if( ioctl(inport->ifp, DKIOCGGEOM, &geom) )  {
       	  elog_complain( 1, "openIP: Can't get disk geometry:" );
          return 0; 
        }

        /* get partition table of the disk */

	  if( ioctl( inport->ifp, DKIOCGAPART, &part) )  { 
	     elog_complain( 1, " openIP: Can't get disk partition table:" );
             return 0; 
          }


        /* get disk label  */

        if( read( inport->ifp, (char *) &label, sizeof(struct dk_label)) != 
                                   sizeof( struct dk_label))
      	read( inport->ifp, (char *) &label, sizeof(struct dk_label));

  	if ( label.dkl_ncyl  != geom.dkg_ncyl  ||
       		label.dkl_nhead != geom.dkg_nhead ||
       		label.dkl_nsect != geom.dkg_nsect ) { 
                
	/* reset kerner table */

        	 geom.dkg_ncyl = label.dkl_ncyl;
        	 geom.dkg_pcyl = label.dkl_pcyl;
        	 geom.dkg_nhead = label.dkl_nhead;
        	 geom.dkg_nsect = label.dkl_nsect;
        	 geom.dkg_apc = label.dkl_apc;
        	 geom.dkg_rpm = label.dkl_rpm;
        	 geom.dkg_intrlv = label.dkl_intrlv;
        
		if (ioctl( inport->ifp, DKIOCSGEOM, &geom) == -1) {
              	   elog_complain( 1, "openIP: Can't reset disk geometry");
         	   return 0; 
		}
 
	/* reset kernel's disk partition table */

        	if (ioctl( inport->ifp, DKIOCSAPART, &label.dkl_map) == -1) {
                  elog_complain(  1, "openIP: Can't reset disk  partitoon" );
                  return 0; 
                }
  	}

  	close( inport->ifp );

  	if (( inport->ifp = open( inport->ip_name, O_RDONLY ) ) < 0)  {
            elog_complain( 1,"open_IN_ports():Can't open %s disk.\n", inport->ip_name);
            return 0; 
   	}
   
  	if( (nbytes = read( inport->ifp, (char *) &buffer[0], 1024)) != 1024)  {
     	   elog_complain(1,"open_IN_ports():Can't read PASCAL label %s \n", buffer);
     	   return 0; 
  	}
    
  	if( (nbytes = read( inport->ifp, (char *) &buffer[0], 1024)) != 1024)  {
     	   elog_complain(1,"open_IN_ports():Can't read PASCAL label %s \n", buffer);
     	   return 0; 
  	}

/* Find the MAX disk block index  */

  	memcpy( (char *) &val, (char *) &buffer[0], sizeof(int) );
  	val = ( val - 4 ) / 2;
  	PsclDk.maxblk = val;

/* Fine the Logical End of Disk  */
   
  	memcpy( (char *) &val, (char *) &buffer[32], sizeof(int) );
  	val = ( val - 4 ) / 2;
  	PsclDk.leod = val;

  	if( PsclDk.maxblk > PsclDk.leod ) PsclDk.maxblk = PsclDk.leod;
    
  
   	return IN_DISK;
}
#endif


/* Input Port is Character Special device.  */

int open_chr( inport )
struct Prts *inport;

{
  
struct scc_mode mode;
struct strioctl strioctl;
short *val;
int max_pkt_siz;
int status;


	if (( inport->ifp = open( inport->ip_name, O_RDONLY ) ) < 0)  {
           elog_complain(1,"open_IN_ports():Can't open %s data stream.\n", inport->ip_name);
           return 0; 
        }
   	if(strncmp( inport->ip_name, "/dev/hih", strlen("/dev/hih")) == 0) {

        	strioctl.ic_cmd = S_IOCGETMODE;
        	strioctl.ic_timout = 0;
        	strioctl.ic_len = sizeof(struct scc_mode);
        	strioctl.ic_dp = (char *) &mode;
        	elog_complain( 0, "Retrieve the current transmission %s parameters ...",
                	inport->ip_name);
        	status = ioctl( inport->ifp, I_STR, &strioctl);
        	elog_complain( 0, "IOCTL=%d\n", status); 
        	if(status < 0)  {
            	elog_complain(0,"open_IN_ports():ioctl error");
                	return 0; 
                }  
        	elog_complain(  0, "txc  = %d\n", mode.sm_txclock);
        	elog_complain(  0, "rxc  = %d\n", mode.sm_rxclock);
        	elog_complain(  0, "iflg = %d\n", mode.sm_iflags);
        	elog_complain(  0, "cfg  = %d\n", mode.sm_config);
        	elog_complain(  0, "baud = %d\n", mode.sm_baudrate);
        	elog_complain(  0, "retv = %d\n", mode.sm_retval);

/*
        	elog_complain( 0, "Retrieve the MAX value for MRU&MTU");
        	strioctl.ic_cmd = S_IOCGETMRU;
        	strioctl.ic_timout = 0;
        	strioctl.ic_len = sizeof(struct scc_mode);
        	status = ioctl(inport->ifp, I_STR, &strioctl);
        	val = (short *) strioctl.ic_dp; 
        	max_pkt_siz = *val; 
        	elog_complain( 0, "MRU %d\n", max_pkt_siz); 
        	if(status < 0)  {
            		elog_complain(0,"open_IN_ports():ioctl error");
            		return 0; 
        	} 
*/

                max_pkt_siz = 1220; 
        	strioctl.ic_cmd = S_IOCSETMRU;
        	strioctl.ic_timout = 0;
        	strioctl.ic_len = sizeof(struct scc_mode);
        	strioctl.ic_dp = (char *) &max_pkt_siz;
           
        	elog_complain( 0, "Set the new tarnsmission %s parameters ...\n", inport->ip_name);
        	status = ioctl(inport->ifp, I_STR, &strioctl);
        	elog_complain( 0, "S_IOCSETMRU IOCTL=%d\n", status); 
        	if(status < 0)  {
            		elog_complain(0,"open_IN_ports():ioctl error");
            		return 0; 
        	} 
   
        	strioctl.ic_cmd = S_IOCSETMTU;
        	status = ioctl(inport->ifp, I_STR, &strioctl);
        	elog_complain( 0, "S_IOCSETMTU IOCTL=%d\n", status); 
        	if(status < 0)  {
            		elog_complain(0,"open_IN_ports():ioctl error");
            		return 0; 
        	} 
        	mode.sm_config = 0x10;
        	mode.sm_iflags = 0;
        	mode.sm_rxclock = 0;
        	mode.sm_txclock = 0;
        	mode.sm_baudrate = 5000000;
	
        	strioctl.ic_timout = 0;
        	strioctl.ic_len = sizeof(struct scc_mode);
        	strioctl.ic_dp = (char *) &mode;
	 
        	strioctl.ic_cmd = S_IOCSETMODE;
        	status = ioctl(inport->ifp, I_STR, &strioctl);
        	elog_complain( 0, "S_IOCSETMODE IOCTL=%d\n", status); 
        	if(status < 0)  {
            	        elog_complain(0,"open_IN_ports():ioctl error");
            	 	return 0; 
        	} 
        	strioctl.ic_cmd = S_IOCGETMODE;
        	elog_complain( 0, "Retrieve the new tarnsmission %s parameters ...", inport->ip_name);
        	status = ioctl(inport->ifp, I_STR, &strioctl);
        	elog_complain( 0, "IOCTL=%d\n", status); 
        	if(status < 0)  {
            		elog_complain(0,"open_IN_ports():ioctl error");
            		return 0; 
        	} 
        	elog_complain(  0, "txc  = %d\n", mode.sm_txclock);
        	elog_complain(  0, "rxc  = %d\n", mode.sm_rxclock);
        	elog_complain(  0, "iflg = %d\n", mode.sm_iflags);
        	elog_complain(  0, "cfg  = %d\n", mode.sm_config);
        	elog_complain(  0, "baud = %d\n", mode.sm_baudrate);
        	elog_complain(  0, "retv = %d\n", mode.sm_retval);
		return IN_HSI;
	
	}  else return IN_CHR;

}

/* Is Input Port socket?  */

#define DELIMITERS  "."

int open_socket( inport )
struct Prts *inport; 

{
  	long b_size;
  	struct hostent *hp;                /* remote host info pointer */
  	struct sockaddr_in Myaddr_in;
	struct utsname hname;
  	u_long addr;
  	int s, addrlen;                    /* size of a structure */
  	char *hostname, *str;              /* machine which wants connection */
  	char *port_str, *token[4];
  	char *port_ascii;
	char *server_name;
	int port_num = RTDAS_PORT ;
	char buffer[1024];
  	int i, ntoken, string, nonblk, digit;
  	int true = 1;
        int done = 0, tried = 0;
	struct linger linger = {1, 1};  /* allow a lingering, graceful close; */

	string = digit = 0;


    if (!strncmp (inport->ip_name, "local", strlen ("local")) ) {
			     
           uname (&hname);
           hostname = strdup (hname.nodename);
			     
           hp = gethostbyname (hostname);
           if (hp == NULL) {
              elog_complain(0, "openID(): Can't get info for HOST - %s.\n", hostname);
              return 0;
           }
    }
    server_name = strdup( inport->ip_name );
    if( port_ascii = strchr( server_name, ':'))  {
	*port_ascii++ = 0;
	port_num = atoi( port_ascii );
    }
    while( !done )  {

        /* clear out structures */
 
  	memset((char *)&peer_in, 0, sizeof(struct sockaddr_in));
  	memset((char *)&myadd_in, 0, sizeof(struct sockaddr_in));
	/* create a socket  */
 
       if( (Ls = socket(AF_INET, SOCK_STREAM, 0)) < 0 )  {
    	    elog_die( 1, "Can't open stream socket\n" ) ; 
       }

  /* Convert IP address from a.b.c.d to the hexadecimal number  */
	   
       if ((int)(addr = inet_addr(server_name)) == -1) {
          elog_complain(0, "IPD/open_socket():IP-address must be of the form a.b.c.d\n");
          return 0;
       }
/*
printf( "%s => %x \n", server_name, addr );
fflush(stdout);
*/

       hp = gethostbyaddr((char *)&addr, sizeof (addr), AF_INET);
       if(hp == NULL)

          peer_in.sin_addr.s_addr = htonl(addr); 
       else 
           peer_in.sin_addr.s_addr = ((struct in_addr *)(hp->h_addr))->s_addr;
  
       peer_in.sin_family = AF_INET; 
       peer_in.sin_port = htons( port_num );


/* Change size of the input/output buffer. 
   Size by Default is 8K which is not enough.   */

 
      b_size = Psize;  

      if( setsockopt( Ls, SOL_SOCKET, SO_SNDBUF, (char *)&b_size, sizeof(int)) != 0)  {
    	   elog_die( 1, "Unable to set size of send buffer.\n");
      }
      if( setsockopt(Ls, SOL_SOCKET, SO_RCVBUF, (char *)&b_size, sizeof(int)) != 0)  {
     	     elog_die( 1, "Unable to set size of send buffer.\n");
       }


       addrlen = sizeof(struct sockaddr_in);

       if ( connect (Ls, (struct sockaddr *) & peer_in, addrlen) == -1) {
           if( !tried )  {
	      tried = 1;
	      elog_complain( 1, "waiting for connection with %s \n", inport->ip_name );
              sleep(1);
	   }
	   close(Ls);
        } else done = 1;	
    } 

    if ( fcntl(Ls, F_SETFL, O_NONBLOCK) == -1 ) { 
        elog_die( 1, "Can't set non-blocking on accept socket\n" ) ; 
    }

     inport->ifp = Ls;
     return IN_SOCKET;

}
 
