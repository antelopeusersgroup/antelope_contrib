/************************************************************************
  *
  *   rddas
  *   Retrieve a PASSCAL packets from a DAS through a serial port.
  *
  *
************************************************************************/
#include "rddas.h"       

extern int PsclLog;

void usage ()
{
    fprintf (stderr, "Usage: %s [-l] [ -p pfile ] [-R] [-s B,P,D,S] [-u] [-v] iport orbname \n", Program_Name);
    fprintf (stderr, "Where: \n");
    fprintf (stderr, "	iport    - input port name.\n");
    fprintf (stderr, "	orbname  - orbserver name.\n");
    banner (Program_Name, "$Revision$ $Date$");
    exit (1);
}

void null_port( struct Prts *iport )
{
   iport->ifp = -1;
   iport->orb = -1;
   iport->brate = 57600;
   iport->parity = 'n';
   iport->data_bits = 8;
   iport->stop_bits = 1;
   iport->reset = 0;
   iport->uncompress = 0;

}
void parse_port_par( struct Prts *port, char *pstring)
{
    int ipar;
    char *tmp, cpar;

    tmp = strtok( pstring, ",");
    if( tmp != 0 )
       port->brate = atoi( tmp ); 
    
    tmp = strtok( NULL, ",");
    if( tmp != 0 )
      port->parity = *tmp;

    tmp = strtok( NULL, ",");
    if( tmp != 0 )
      port->data_bits = atoi(tmp);
    
    tmp = strtok( NULL, ",");
    if( tmp != 0 )
      port->stop_bits = atoi(tmp);
    
}

main(argc, argv)
int argc;
char *argv[];
{
  extern char    *optarg;
  extern int      optind;
  int     	  i;
  char            *port_par = 0;
  char            *iport = 0;
  char            *pffile = "pscl";
  char            *orbname = "localhost";
  struct Prts     Ports;

   elog_init (argc, argv) ;
   elog_notify (0, "$Revision$ $Date$") ;
   Program_Name = argv[0];
   
   null_port( &Ports );

  /* Set command line parameters default values  */
 
  while ( ( i = getopt (argc, argv, "p:Rs:luv")) != -1)
        switch (i) {

        case 'l':
	    PsclLog = 1;
            break;
        case 'p':
            pffile = optarg;          
            break;
        case 'R':
            Ports.reset = 1;
            break;
        case 'u':
            Ports.uncompress = 1;
            break;
        case 's':
            port_par = strdup(optarg);           
            break;
        case 'v':
            Log = 1;
            break;
        default: 
            usage();
        }
       if ( argc - optind != 2 )
          usage ();
         
       iport = argv[optind++];
       orbname = argv[optind++] ; 
       parse_port_par( &Ports, port_par );

       initpf( pffile );
       if( !PsclLog )  {
          elog_notify( 0, "rddas will work in a silent mode.\n");
          elog_notify( 0, "Restart rddas with the '-l' option to get a PASSCAL log file.\n");
       }

       strcpy( Ports.ip_name, iport );
       Ports.ip_name[strlen(iport)] = '\0';
       strcpy(Ports.orbname, orbname );
       Ports.orbname[strlen(orbname)] = '\0';
       read_in_ports ( &Ports );

}

