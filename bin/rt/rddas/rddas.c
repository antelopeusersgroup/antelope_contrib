/************************************************************************
  *
  *   rddas
  *   Retrieve a PASSCAL packets from a DAS through a serial port.
  *
  *
************************************************************************/
#include "rddas.h"       

void usage ()
{
    fprintf (stderr, "Usage: %s [-V verbatim] [ -p pfile ] [-r] [-s baudrate] [-t timeout] [-u] [-v] iport orbname \n", Program_Name);
    fprintf (stderr, "Where: \n");
    fprintf (stderr, "	iport    - input port name.\n");
    fprintf (stderr, "	orbname  - orbserver name.\n");
    exit (1);
}

void null_port( struct Prts *iport )
{
   iport->verbatim = NULL;
   iport->ifp = -1;
   iport->orb = -1;
   iport->brate = DASRATE;
   iport->reset = 0;
   iport->uncompress = 0;
   iport->timeout = INFTIM;

}

main(argc, argv)
int argc;
char *argv[];
{
  extern char    *optarg;
  extern int      optind;
  int     	  i;
  char            *iport = 0;
  char            *pffile = "pscl";
  char            *version = "1.1 (03/22/97)";
  char            *orbname = "localhost";
  char            *verbatim_file; 
  struct Prts     Ports;

   elog_init (argc, argv) ;
   elog_notify (0, "%s version %s\n", argv[0], version) ;
   Program_Name = argv[0];
   
   null_port( &Ports );

  /* Set command line parameters default values  */
 
  while ( ( i = getopt (argc, argv, "V:p:rs:t:uv")) != -1)
        switch (i) {
        case 'V':
            verbatim_file = optarg; 
            if ( ( Ports.verbatim = fopen ( verbatim_file, "a+" ) ) == NULL ) 
              die ( 1, "Can't open '%s' for verbatim file\n", verbatim_file ) ; 
            break ;

        case 'v':
            Log = 1;
            break;
        case 'r':
            Ports.reset = 1;
            break;
        case 'U':
        case 'u':
            Ports.uncompress = 1;
            break;
        case 's':
            Ports.brate = atoi(optarg);           
            break;
        case 't':
            Ports.timeout = atoi(optarg);           
            break;
        case 'p':
            pffile = optarg;          
            break;
        default: 
            usage();
        }
       if ( argc - optind != 2 )
          usage ();
         
       iport = argv[optind++];
       orbname = argv[optind++] ; 

       initpf( pffile );

       strcpy( Ports.ip_name, iport );
       Ports.ip_name[strlen(iport)] = '\0';
       strcpy(Ports.orbname, orbname );
       Ports.orbname[strlen(orbname)] = '\0';
       read_in_ports ( &Ports );

}

