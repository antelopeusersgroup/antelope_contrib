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
int NoSP = 0;
char *pffile = 0;

void usage ()
{
    fprintf (stderr, "Usage: %s [ -v ] [-V verbatim] [-h hdrtype] [ -u ] [-t timeout] [ -p pfile ] [-s] iport orbname \n", Program_Name);
    fprintf (stderr, "Where: \n");
    fprintf (stderr, "	-V       - print packet information on stdout.\n");
    fprintf (stderr, "	pfile    - parameter file name.\n");
    fprintf (stderr, "	-u       - uncompress data before sending to the RB .\n");
    fprintf (stderr, "	iport    - input port name.\n");
    fprintf (stderr, "	orbname  - orbserver name.\n");
    exit (1);
}

main(argc, argv)
int argc;
char *argv[];
{
  extern char    *optarg;
  extern int      optind;
  int     	  i, timeout=30;
  int	  htype=0;
  char            *iport = NULL;
  char            *version = "1.1 (03/22/97)";
  char            *hdrtype = 0;
  char            *orbname = "localhost";
  char            *verbatim_file; 
  struct Prts     Ports;

   elog_init (argc, argv) ;
   elog_notify (0, "%s version %s\n", argv[0], version) ;
   Program_Name = argv[0];
   
   Ports.verbatim = NULL;

  /* Set command line parameters default values  */
 
  while ( ( i = getopt (argc, argv, "V:v:h:p:t:us")) != -1)
        switch (i) {
        case 'V':
            verbatim_file = optarg; 
            if ( ( Ports.verbatim = fopen ( verbatim_file, "a+" ) ) == NULL ) 
              die ( 1, "Can't open '%s' for verbatim file\n", verbatim_file ) ; 
            break ;

        case 's':
            NoSP = 1;
            break;
        case 'v':
            Log = 1;
            break;
        case 'U':
        case 'u':
            Raw_decomp = 1;
            break;
        case 'h':
            hdrtype = argv[optind++];
            break;
        case 't':
            timeout = atoi(optarg);
            break;
        case 'p':
            pffile = argv[optind++];
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
	    die( 0, "Can't recognize hdrtype - %s\n", hdrtype );

       strcpy( Ports.ip_name, iport );
       Ports.ip_name[strlen(iport)] = '\0';
       strcpy(Ports.orbname, orbname );
       Ports.orbname[strlen(orbname)] = '\0';

/* Read input port  */

       read_in_ports ( &Ports, htype, timeout );

}

