/************************************************************************
  *
  *  ipd.c 
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
#include "ipd.h"       

extern void sig_hdlr();
int chrate = 1800;
int NoSP = 0;
char *pffile = 0;
int Psize = 4096;

void usage ()
{
    fprintf (stderr, 
             "Usage: %s [-v][-c check_rate] [-i] [-p pfile] [-s pkt_size] [-t timeout] [-u] iport orb\n", 
             Program_Name);
    banner (Program_Name, "$Revision$ $Date$");
    exit (1);
}

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

   elog_init (argc, argv) ;
   elog_notify (0, "$Revision$ $Date$") ;
   Program_Name = argv[0];
   

  /* Set command line parameters default values  */
 
  while ( ( i = getopt (argc, argv, "c:ip:s:t:uv")) != -1)
        switch (i) {

        case 'c':
            chrate = atoi(optarg);
            break;
        case 'i':
            NoSP = 1;
            break;
        case 'p':
            /* pffile = argv[optind++]; */
            pffile = argv[optind-1];
            break;
        case 's':
            pktsize = atoi(optarg);
            Psize = pktsize>Psize?pktsize:Psize;
            break;
        case 'v':
            Log = 1;
            break;
        case 't':
            timeout = atoi(optarg);
            break;
        case 'U':
        case 'u':
            Raw_decomp = 1;
            break;
        default: 
            usage();
        }
       if ( argc - optind != 2 )
          usage ();
       
/* Open input port and ORB  */
  
       iport = argv[optind++];
       orbname = argv[optind++] ; 

       signal(SIGUSR1, sig_hdlr );
       initpf( pffile );

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

