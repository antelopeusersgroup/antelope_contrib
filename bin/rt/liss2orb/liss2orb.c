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
    fprintf (stderr, 
    "Usage: %s [-m net_sta_chan] [-p pfile] [-s pkt_size] [-t timeout] [-v] iport orb \n",
    Program_Name);
    exit (1);
}


regex_t srcmatch;

main(argc, argv)
int argc;
char *argv[];
{
  extern char    *optarg;
  extern int      optind;
  Pf             *pf;
  Tbl 		 *sites;
  Newchan	 nch;
  int     	  i, num, timeout=30;
  int		  ifp = 1,
                  orb = -1;
  char            *istr, *newch, key[32];
  char            *pfile = 0;
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
 
  while ( ( i = getopt (argc, argv, "p:s:t:m:v")) != -1)
        switch (i) {
        case 'm':
            match = strdup(optarg);
	    if ( (code = regcomp( &srcmatch, match, REG_EXTENDED|REG_NOSUB)) != 0) 
	          die( 1, "regcomp error #%d for %s\n", code, match );
            break;
        case 'p':
            pfile = strdup(optarg);
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
   
       if( pfile != 0 )  {

	  if( pfread ( pfile, &pf) != 0 )
	      die (0, "Can't read %s parameter file\n", pfile );

	  sites = pfget_tbl( pf, "StaChan");
	  num = maxtbl( sites );
	  if(num == 0 )
	      die(0, "Can't get StaChan Tbl from %s parameter file.\n", pfile);
          else NewCh = newarr(0);

	  for(i = 0; i < num; i++)  {
              istr = (char *) gettbl(sites, i);
              sscanf(istr, NCH_SCS,  NCH_RVL(&nch));
              newch = strdup( nch.newkey);
	      setarr(NewCh, nch.oldkey, newch );  
	  }

       } else NewCh = 0;

       read_server ( ifp, orb, timeout, match );

}

