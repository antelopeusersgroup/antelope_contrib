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
#include "pf.h"

int Log = 0;
Tbl *DC;

Tbl *MailAdd;
Arr *MailSent;

Arr *Dasid;
Arr *PChan;

int MaxOff;
int Ls;
char *logname;
FILE *fplog;

struct sockaddr_in myadd_in;           
struct sockaddr_in peer_in;

main(argc, argv)
int argc;
char *argv[];
{
  extern char   *optarg;
  extern int    optind;
  Pf	 	*lpf;
  Arr 		*Dases;
  Das		*das;
  double 	save_time, pkttime;
  int		nselect, nbytes, bsize;
  int 		err_in;
  int 		dasid, i, id, orb, ndc;
  int 		tperiod, asleep;     
  int 		max_mrc, mrcnum, repeats;
  char          *packet = 0,
  		srcid[ORBSRCNAME_SIZE] ;
  char          *newname, *name;
  char 		*s, *mypf, *pfile ;
  char          *madd, *receipient, *match;
  static Packet *unstuffed=0 ;
  char 		*reciepient=0;
  char 	        *dces = 0,
  		*orbname = 0 ;

   elog_init (argc, argv) ;
   elog_notify (0, "$Revision$ $Date$") ;
   Program_Name = argv[0];
  
   /* Set default parameters  */

  mypf="automrc"; pfile = "pkt";
  
  match = ".*/CBB1S";	/* use those packets for LTA calculation */
  MaxOff = 100000;	/* MAX allowed LTA value  */ 
  tperiod = 3600;  	/* one hour for LTA  */
  asleep = 30;	   	/* 30 seconds interval between commands */
  repeats=2;		/* # MRC will be sent in one session  */
  max_mrc=4;		/* max # of unsuccessfull MRC before 
  			   "alarm" email will be sent  */  

  MailSent = newarr(0);

  /* Set command line parameters default values  */
 
  while ( ( i = getopt (argc, argv, "p:")) != -1)
        switch (i) {

        case 'p':
            pfile = optarg;
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
      elog_die( 0, "Can't open ORB\n");
     
  if( pfread ( mypf, &lpf) == 0 )  {
       elog_notify (0, "Will read parameters from %s parameter file\n", mypf);
       MaxOff = pfget_int( lpf, "max_lta");
       tperiod = pfget_int( lpf, "lta_window");
       asleep = pfget_int( lpf, "timeout_btw_cmds");
       repeats = pfget_int( lpf, "num_repeats");
       max_mrc = pfget_int( lpf, "max_mrc");
       match = pfget_string( lpf, "srcname");
       receipient = pfget_string( lpf, "receipient");
  }
  if( receipient != 0 ) {
      MailAdd = newtbl(0);
      if( ( madd = strtok( receipient, "," )) != 0 )  {
          newname = (char *) new_name(madd);
          pushtbl( MailAdd, newname );
      }
      while( ( madd = strtok( NULL, "," )) != 0 )  {
          newname = (char *) new_name( madd);
          pushtbl( MailAdd, newname );
      }
  }
  if ((nselect = orbselect ( orb, match)) < 1 )
      elog_die(1, "orbselect '%s' failed\n", match);
  

  if (orbget (orb, ORBCURRENT, &id, srcid, &pkttime, &packet, &nbytes, &bsize)) 
      elog_die(0,"fails to get ORBCURRENT time.\n") ; 

  DC = newtbl(0);
  Dases = newarr(0);
  PChan = newarr(0);
  collect_dases( pfile );        

  if( ( name = strtok( dces, "," )) != 0 )  {
    newname = (char *) new_name( name);
    pushtbl( DC, newname );
  }
  while( ( name = strtok( NULL, "," )) != 0 )  {
    newname = (char *) new_name( name);
    pushtbl( DC, newname );
  }
  ndc = maxtbl(DC);
  if( ndc <= 0 ) 
     elog_die( 0, "can't get a DC names\n");

  /* Print current settings  */

  fprintf( stderr, "\nmrc was started at %s with the following parameters:\n\n",
           s=strtime(now()) );
	   free(s);

  fprintf( stderr, "    ORBNAME:\t\t %s\n", orbname );
  fprintf( stderr, "    DCNAMES:\t\t ");
  for( i = 0; i < ndc; i++ )  {
      name = (char *) gettbl(DC, i );
     fprintf( stderr, "%s  ",  name );
  }
  fprintf( stderr, "\n");
  fprintf( stderr, "    DATA TYPE:\t\t %s\n", match );
  fprintf( stderr, "    LTA time period:\t %d\n", tperiod );
  fprintf( stderr, "    Max LTA allowed:\t %d\n", MaxOff );
  fprintf( stderr, "    'RCRC' command will be send twice to the DAS at %d second intervals\n\n", asleep );
  fprintf( stderr, "    in case of failed MRCs email will be sent to:\t"  );
    
  ndc = maxtbl(MailAdd);
  for( i = 0; i < ndc; i++ )  {
      name = (char *) gettbl(MailAdd, i );
      fprintf( stderr, "%s  ",  name );
  }

  fprintf( stderr, "\n    data starts at %s \n", s=strtime(pkttime) );
  free(s);
  fprintf( stderr, "\n\n"); fflush(stderr);

/* Loop through RB; aclculate LTA  */

  for(;;)  {   
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

                switch ( unstuffPkt( srcid, pkttime, packet, nbytes, &unstuffed ) )  {
		     
                    case Pkt_wf :
                    
		       if( (mrcnum = offscale(unstuffed, pkttime, srcid, tperiod, &dasid )) )  {
		           if( mrcnum >= max_mrc && receipient != 0 ) 
			       send_alarm( mrcnum, dasid, receipient );
			     /*  sendmrc(dasid, repeats, asleep );  */
		       }
		       break;
		       
	            default: /* ignore other packets */
		       break;

	            case -1 :
		       elog_complain( 0, "unknown packet type %s\n", srcid );
		       break;
	         }
	 } 
    }  else {
          err_in++;
	  elog_complain( 0, "Can't get packet after %lf.\n", save_time );
	  if( err_in > 900 )  {
	       orbclose( orb );
	       sleep(10);
               if( (orb = orbopen( orbname, "r")) < 0)
                   elog_die( 0, "Can't open ORB\n");

               if ((nselect = orbselect ( orb, match)) < 1 )
                   elog_die(1, "orbselect '%s' failed\n", match);

               if ( save_time > 0 ) {
                   if ( orbafter ( orb, save_time ) < 0) {
                        elog_complain( 1, "orbafter to %lf failed\n", save_time )  ;
                        if (orbseek (orb, ORBCURRENT ) < 0 ) 
                             elog_die(0,"fails to get ORBCURRENT time.\n") ; 
                   } 
               }  else  {
                   if (orbseek (orb, ORBCURRENT ) < 0 ) 
                        elog_die(0,"fails to get ORBCURRENT time.\n") ; 
              }
	  }

    }

  }

    return 0 ;
}


