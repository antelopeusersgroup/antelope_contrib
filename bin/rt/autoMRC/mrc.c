/************************************************************************
  *
  *  mrc.c
  *
  *  Read data from ORD; calculate LTA; 
  *  if LTA >= MAX value send mass re-centering command to coresponding DAS.
  * 
  *
  *
  *  Author: Marina Harkins-Glushko
  *  	    UCSD, IGPP
  *	    glushko@ucsd.edu
  *
************************************************************************/
#include "mrc.h"       

Tbl *DC;

char *
new_name( char *name )  {

    char *new;

    new=strdup( name );
    return new;

}


void usage ()
{
    fprintf (stderr, "Usage: %s [-V verbatim] [-m srcmatch] [-p pfile ] [-s sleep] [-t tperiod] [-v max_lta] orb dcname1[,dcname2,dcname3...] \n", Program_Name);
    exit (1);
}

main(argc, argv)
int argc;
char *argv[];
{
  extern char   *optarg;
  extern int    optind;
  Pf	 	*pf;
  int		nselect, nbytes, bsize;
  int 		err_in;
  int 		dasid, i, id, orb, ndc;
  int 		tperiod = 3600,
  		asleep = 30;     /* 30 seconds  */
  double 	save_time=0, 
  		pkttime;
  char          *packet = 0,
  		srcid[ORBSRCNAME_SIZE] ;
  char 	        *dces = 0,
                *newname, *dcname,
  		*orbname = "localhost";
  char          *version = "1.1 (03/22/97)";
  char 		*s, *pfile = "pkt";
  char          *match = ".*/CBB1S";
  static Packet *unstuffed=0 ;
  Arr 		*Dases;
  Das		*das;

   elog_init (argc, argv) ;
   elog_notify (0, "%s version %s\n", argv[0], version) ;
   Program_Name = argv[0];
  		
  logname= 0; MaxOff = MAX_OFF; 
   
  /* Set command line parameters default values  */
 
  while ( ( i = getopt (argc, argv, "V:vm:p:t:")) != -1)
        switch (i) {
        case 'V':
            logname = optarg; 
            if ( ( fplog = fopen ( logname, "a+" ) ) == NULL ) 
              die ( 1, "Can't open '%s' for logs \n", logname ) ; 
            break ;

        case 'v':
            MaxOff = atoi(optarg);
            break;

        case 'm':
            match = optarg;
            break;

        case 'p':
            pfile = optarg;
            break;

        case 's':
            asleep = atoi(optarg);
	    break;

        case 't':
            tperiod = atoi(optarg);
            break;
        default: 
            usage();
        }
      
  if ( argc - optind != 2 )
       usage ();
 
/* open ORB and select specified packet type  */
 
  orbname = argv[optind++];
  
  dces = argv[optind++];
  
  if( (orb = orbopen( orbname, "r")) < 0)
      die( 0, "Can't open ORB\n");
     
 
  if ((nselect = orbselect ( orb, match)) < 1 )
      die (1, "orbselect '%s' failed\n", match);
  

  if (orbget (orb, ORBCURRENT, &id, srcid, &pkttime, &packet, &nbytes, &bsize)) 
      die(0,"fails to get ORBCURRENT time.\n") ; 

  DC = newtbl(0);
  Dases = newarr(0);
  PChan = newarr(0);
  collect_dases( pfile );        

  if( (dcname = strtok( dces, "," )) != 0 )  {
    newname = (char *) new_name(dcname);
    pushtbl( DC, newname );
  }
  while( (dcname = strtok( NULL, "," )) != 0 )  {
    newname = (char *) new_name(dcname);
    pushtbl( DC, newname );
  }
  ndc = maxtbl(DC);
  if( ndc <= 0 ) 
     die( 0, "can't get a DC names\n");

  /* Print current settings  */

  fprintf( stderr, "\nmrc was started at %s with the following parameters:\n\n",
           s=strtime(now()) );
	   free(s);

  fprintf( stderr, "    ORBNAME:\t\t %s\n", orbname );
  fprintf( stderr, "    DCNAMES:\t\t ");
  for( i = 0; i < ndc; i++ )  {
     dcname = (char *) gettbl(DC, i );
     fprintf( stderr, "%s  ", dcname );
  }
  fprintf( stderr, "\n");
  fprintf( stderr, "    DATA TYPE:\t\t %s\n", match );
  fprintf( stderr, "    LTA time period:\t %d\n", tperiod );
  fprintf( stderr, "    Max LTA allowed:\t %d\n", MaxOff );
  fprintf( stderr, "    'RCRC' command will be send twice to the problem DAS at %d second intervals\n\n", asleep );

/* Loop through RB; aclculate LTA  */

  while(1)  {   
    if( !orbreap( orb, &id, srcid, &pkttime, &packet, &nbytes, &bsize)  ) {
         err_in = 0;
         save_time = pkttime;

	 if( strncmp( srcid, "/", 1 ) == 0 ) continue;

         if (( das = ( Das *) getarr ( Dases, srcid )) == 0) {
                 das = ( Das *) new_das ( srcid, pkttime );
                 setarr ( Dases, srcid, das );
         }
         switch( orbsort(das->apipe, &id, &pkttime, srcid, &packet, &nbytes, &bsize))  {

	      case 0: 
		break;
	      case 2:
		break;
	      default:

                switch ( unstuffpkt( pkttime, srcid, packet, &unstuffed ) )  {
		     
                    case 1:
                    
		       if( offscale(unstuffed, pkttime, srcid, tperiod, &dasid ) )  {
		           sendmrc(dasid, asleep );
		       }
		       break;
		       
	            case 2:
		       break;
	            default:
		       complain( 0, "unknown packet type %s\n", srcid );
		       break;
	         }
	 } 
    }  else {
          err_in++;
	  complain( 0, "Can't get packet after %lf.\n", save_time );
	  if( err_in > 900 )  {
	       orbclose( orb );
	       sleep(10);
               if( (orb = orbopen( orbname, "r")) < 0)
                   die( 0, "Can't open ORB\n");

               if ((nselect = orbselect ( orb, match)) < 1 )
                   die (1, "orbselect '%s' failed\n", match);

               if ( save_time > 0 ) {
                   if ( orbafter ( orb, save_time ) < 0) {
                        complain ( 1, "orbafter to %lf failed\n", save_time )  ;
                        if (orbseek (orb, ORBCURRENT ) < 0 ) 
                             die(0,"fails to get ORBCURRENT time.\n") ; 
                   } 
               }  else  {
                   if (orbseek (orb, ORBCURRENT ) < 0 ) 
                        die(0,"fails to get ORBCURRENT time.\n") ; 
              }
	  }

    }

  }

}


