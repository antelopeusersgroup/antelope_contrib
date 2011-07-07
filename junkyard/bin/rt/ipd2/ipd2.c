/************************************************************************
  *
  *  ipd2.c 
  *
  *  Read data from an input port; 
  *  recognize data packet type; 
  *  add specified header; 
  *  pass to ORB; 
  *
  *
  *
  *  Author: Marina Harkins-Glushko
  *  	    UCSD, IGPP
  *	    glushko@ucsd.edu
************************************************************************/
#include <signal.h>
#include "ipd2.h"       

extern void sig_hdlr();
extern char *Network_Name;
extern char *DB_Name;
int chrate = 1800;
int NoSP = 0;
char *pffile = "pkt";
int Psize = 4096;
int Dump = 0;
extern int PktLog;
  
int Log;
int Ls;
struct sockaddr_in myadd_in;           
struct sockaddr_in peer_in;

/*
void usage ()
{
   fprintf (stderr, 
             "Usage: %s [-v][-c check_rate] [-i] [-n netname] [-p pfile] [-s pkt_size] [-t timeout] [-u] iport orb\n", 
             Program_Name);
    banner (Program_Name, "$Revision$ $Date$");
    exit (1);
}
*/

main(argc, argv)
int argc;
char *argv[];
{
  extern char    *optarg;
  extern int     optind;
  struct Prts    Ports;
  int     	 i, timeout=30;
  int	         pktsize, htype=0;
  char           *iport = 0;
  char           *hdrtype = 0;
  char           *orbname = "localhost";
  char           *tmpname=0;
  char           *netname=0;
  char *usage_str="ipd2 [-v][-c check_rate] [-i] [-n netname] [-p pfile] [-s pkt_size] [-t timeout] [-u] iport orb";
  char *author="Marina Harkins";
  char *location="IGPP UCSD";
  char *email="fvernon@ucsd.edu";
  Pf *pf;

   elog_init (argc, argv) ;
   elog_notify (0, "$Revision$ $Date$") ;
   Program_Name = argv[0];
   

  /* Set command line parameters default values  */
 
  while ( ( i = getopt (argc, argv, "hc:in:p:s:t:v")) != -1)
        switch (i) {

        case 'c':
            chrate = atoi(optarg);
            break;
        case 'i':
            NoSP = 1;
            break;
        case 'n':
            netname = optarg;
            break;
        case 'p':
            pffile = strdup(optarg);
            break;
        case 's':
            pktsize = atoi(optarg);
            Psize = pktsize>Psize?pktsize:Psize;
            break;
        case 'v':
            Log = 1;
            break;
        case 'h':
            PktLog=1;
	    /* Dump = 1;  */
            break;
        case 't':
            timeout = atoi(optarg);
            break;
        default: 
           /* usage(); */
            cbanner("$Revision$", usage_str, author, location, email);
            exit (1);
        }
       if ( argc - optind != 2 )  {
          /* usage (); */
          cbanner("$Revision$", usage_str, author, location, email);
          exit (1);
       } 
       
/* Open input port and ORB  */
  
       iport = argv[optind++];
       orbname = argv[optind++] ; 

       signal(SIGUSR1, sig_hdlr );
       initpf( pffile );

       if (pfread (pffile, &pf) != 0)
        elog_die(0, "Can't read parameter file\n");
      
       if((tmpname = pfget_string(pf, "Network_Name")) != 0)
	  Network_Name = strdup(tmpname);
      
       if(netname)
	  Network_Name = strdup(netname); 
       
       if((tmpname = pfget_string(pf, "DB_Name")) != 0)  {
	  /* ok, now if we have a DB specified then we can read all
	     neccessary parameters (like calib, segtype, etc.) directly
             from DB. Otherwise use some default values from a Pfile
           */
           DB_Name = strdup(tmpname);
       }
       
       if( hdrtype != 0 )
         if( (htype = ( int ) decode( hdrtype )) < 0 ) 
	    elog_die( 0, "Can't recognize hdrtype - %s\n", hdrtype );

       strcpy( Ports.ip_name, iport );
       Ports.ip_name[strlen(iport)] = '\0';
       strcpy(Ports.orbname, orbname );
       Ports.orbname[strlen(orbname)] = '\0';

/* Read input port  */

       read_in_ports ( &Ports, htype, timeout );

}

