/* %w% %g%  */
/************************************************************************
  *
  *  
  *
***********************************************************************/
#include <errno.h>
#include <malloc.h>
#include <math.h>
#include <regex.h>
#include "db.h"
#include "coords.h"
#include "stock.h"
#include "orb.h"
#include "Pkt.h"

#define DCCLOCK 28

Arr *MailSent;
Tbl *MailAdd;
regex_t dcmatch;

/* Client Packet headers . Do NOT move around structure fields! */
struct PreHdr {
    short           hdrsiz;	       /* header size */
    short           pktsiz;	       /* raw packet size */
    ushort_t        hdrtype;	       /* header type  */
    ushort_t        pkttype;	       /* packet type tag  */
};

static void usage( )
{

      fprintf (stderr, "usage: %s [-d] [-m srcmatch ] orbin mail_add [stime]\n" , Program_Name);
      banner (Program_Name, "$Revision$ $Date$");

      exit (1);

} 

char * new_name( char *name )  {
 
    char *new;
 
    new=strdup( name );
    return new;
 
}
 

int
main(int argc, char **argv)
{
  double 	after, save_time, 
  		pkttime;
  int 		err_in = 0,
  		id, rorb;
  int		nbytes, bsize=0;
  int 		dump=0; 
  int           val, nadd, i;
  char          net[16], sta[16];

  struct PreHdr *hdr;
  short		*sval, hdrsize;
  uchar_t       *dcstat;
  char          *after_str, *dc, *packet = 0,
  		srcid[ORBSRCNAME_SIZE] ;
  char 		*inorbname = "localhost";
  char          alarm_str[512], *who_str, mail_str[512], *madd, *s ;
  char  	*match = ".*BSP";
  Srcname        parts ;

  elog_init (argc, argv) ;
  elog_notify (0, "$Revision$ $Date$") ;
  Program_Name = argv[0];
  
  while( ( id = getopt( argc, argv, "dm:") ) != -1 )
     switch ( id )  {

        case 'd':
            dump = 1;
            break;
 
        case 'm':
            match = optarg;
            break;
 
       case '?' :
         err_in++;

     }
  if( err_in || argc - optind < 2  || argc - optind > 3 )
     usage();

  inorbname = strdup( argv[optind++]);
  who_str = strdup( argv[optind++]);


  MailSent = newarr(0);
  MailAdd = newtbl(0);

  if( ( madd = strtok( who_str, "," )) != 0 )  {
    s = (char *) new_name(madd);
    pushtbl( MailAdd, s );
  }
  while( ( madd = strtok( NULL, "," )) != 0 )  {
    s = (char *) new_name( madd);
    pushtbl( MailAdd, s );
  }
  nadd = maxtbl(MailAdd);
  if( nadd <= 0 ) 
     elog_die( 0, "can't get a mail recipients addresses\n");
 

  /* Set signal to handle USER interupt  */
   
  if( (rorb = orbopen( inorbname, "r")) < 0)
     elog_die( 0, "Can't open ORB\n");
     
  if ( match ) {
     if ((orbselect ( rorb, match)) < 1 )
        elog_die(1, "orbselect '%s' failed\n", match);
  }

  if ( regcomp(&dcmatch, match, REG_EXTENDED|REG_NOSUB) != 0)   {
            elog_die( 1, "regcomp error for %s\n", match );
  }

  if ( (argc - optind) == 1) {
        after_str = argv[optind++] ; 
        after = str2epoch (after_str);
        if ( orbafter (rorb, after-0.001) < 0) 
            elog_die(1, "orbafter to %s failed\n", strtime (after));
  }  else if (orbget (rorb, ORBCURRENT, &id, srcid, &after, &packet, &nbytes, &bsize)) 
        elog_die(0,"fails to get ORBCURRENT time.\n") ; 

/* Loop through RB; runnin triggering algorithm  */

  save_time = 0.0;
  for(;;) { 
    if( !orbreap( rorb, &id, srcid, &pkttime, &packet, &nbytes, &bsize)  ) {
         if ( regexec( &dcmatch, srcid, (size_t) 0, NULL, 0 ) ) continue;

         if( dump )  {
              sval = (short *) packet;
              hdrsize = *sval;
              hexdump( stderr, packet+hdrsize, nbytes-hdrsize );
         }

	 split_srcname ( srcid, &parts ) ;
	 strcpy(net, parts.src_net ) ; 
	 strcpy(sta, parts.src_sta ) ; 

         hdr = ( struct PreHdr *) packet;
       
         dcstat = (uchar_t *) ( packet + hdr->hdrsiz + DCCLOCK );
         
         val = (dcstat[2]&0x80) ? 1:0;
         if( val != 0 )  {
             if( (dc = (char *) getarr( MailSent, sta)) == 0 )  {
                 dc = new_name( sta );
                 setarr( MailSent, dc, dc);
           
                 sprintf( alarm_str, 
                      "WARNING from DC %s!\n\nGENERATOR FAILED TO START\nat %s\n",
                      sta, s=strtime(pkttime) );
                 free(s);
                 for( i = 0; i < nadd; i++ )  {
                    madd = (char *) gettbl( MailAdd, i );
                    sprintf( mail_str, "echo %s | mailx -v -s \"DC ALARM\" %s\0", alarm_str, madd );
                    system( mail_str );
                 }
              }
         }  else {

             if( (dc = (char *) getarr( MailSent, sta)) != 0 ) { 
                 delarr( MailSent, dc);
                 sprintf( alarm_str, 
                      "Message from DC %s!\n\nGENERATOR STARTED\nat %s\n",
                       sta, s=strtime(pkttime) );
                 free(s);
                 for( i = 0; i < nadd; i++ )  {
                    madd = (char *) gettbl( MailAdd, i );
                    sprintf( mail_str, "echo %s | mailx -v -s \"DC BACK to NORMAL\" %s\0", alarm_str, madd );
                    system( mail_str );
                 }
             }
        }
   
         save_time = pkttime; 
      }  else {
	  elog_complain( 0, "Can't get packet after %lf.\n", save_time );

     }

  } 

  return 0 ;
}

