/************************************************************************
  *
  *  liss2orb.c 
  *
  *  Read data from a LISS server and put in orb
  *
  *  Author: Marina Harkins-Glushko
  *  	    UCSD, IGPP
  *	    glushko@ucsd.edu
************************************************************************/
#include "liss2orb.h"       

extern void set_byte_order();

void usage ()
{
    fprintf (stderr, "Usage: %s [ -v ] [-s pkt_size] [-t timeout] [-m net_sta_chan] iport orbname \n", Program_Name);
    exit (1);
}

regex_t srcmatch;

main(argc, argv)
int argc;
char *argv[];
{
  extern char    *optarg;
  extern int      optind;
  int     	  i, timeout=30;
  int		  ifp = 1,
                  orb = -1;
  char            *match = 0;
  char            *iport = 0;
  char            *version = "1.1 (09/01/98)";
  char            *orbname = "localhost";
  int 		  code;


   elog_init (argc, argv) ;
   elog_notify (0, "%s version %s\n", argv[0], version) ;
   Program_Name = argv[0];
 
   PSize = Def_Seed_Size;

  /* Set command line parameters default values  */
 
  while ( ( i = getopt (argc, argv, "s:t:m:v")) != -1)
        switch (i) {
        case 'm':
            match = strdup(optarg);
	    if ( (code = regcomp( &srcmatch, match, REG_EXTENDED|REG_NOSUB)) != 0) 
	          die( 1, "regcomp error #%d for %s\n", code, match );
            break;
        case 's':
            PSize = atoi(optarg);
            break;
        case 't':
            timeout = atoi(optarg);
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
       complain( 0, "read %s write %s\n", iport, orbname);
           
      /*  Open A Ring Buffer server and LISS server  */
    
       if( (ifp = open_socket( iport )) < 0 )
          die( 0, "can't open %s server.\n", iport );
 
       if( ( orb = orbopen( orbname, "w" )) < 0)  
          die(0," Can't open %s orb.!\n", orbname );   
  
       set_byte_order();
       read_server ( ifp, orb, timeout, match );

}

