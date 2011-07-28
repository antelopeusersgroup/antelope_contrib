/* %W% %G%  */
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

#define DCBATT 26
#define DC_MINBATT  2400
#define MAX_FOR_AVR  360

/* Client Packet headers . Do NOT move around structure fields! */
struct PreHdr {
    short           hdrsiz;	       /* header size */
    short           pktsiz;	       /* raw packet size */
    ushort_t        hdrtype;	       /* header type  */
    ushort_t        pkttype;	       /* packet type tag  */
};

Arr *MailSent;
Tbl *MailAdd;
Arr *BatRec;

regex_t dcmatch;

typedef struct batrecord {
   Tbl *vpipe;
   int avr;
   char *key;
} Batrecord;
 
static void usage( )
{

      fprintf (stderr, "usage: %s [-d] [-m srcmatch ] orbin mail_add \n" , Program_Name);
      banner (Program_Name, "$Revision$ $Date$");
      exit (1);

} 

char * new_name( char *name )  {
 
    char *new;
 
    new=strdup( name );
    return new;
 
}

Batrecord *new_batrec( char *key )
{
        Batrecord *new;
        allot( Batrecord *, new, 1) ;
        new->avr = 0.0;
        new->key = strdup( key );
        new->vpipe = inittbl( 0, MAX_FOR_AVR+1, 1, 0, sizeof(int) ); 
        return new;
}
 
int check_dcbatt( char *key, int sval, int *avr )
{
      Batrecord *acmp;
      char *tmp;
      int old_batt = 0;
      int maxnum=1, alert=0;
 
      if( BatRec == 0 )  BatRec = newarr(0);
          
      acmp = ( Batrecord *) getarr( BatRec, key );
      if( acmp == 0 )  
         acmp = ( Batrecord *) new_batrec( key );
          
      maxnum = maxtbl( acmp->vpipe ); 
      if( maxnum > MAX_FOR_AVR )  {
         if( (tmp = shifttbl( acmp->vpipe)) != 0 )  {
             memcpy( (char *) &old_batt, tmp, sizeof(int) );
         }
      }
      pushtbl( acmp->vpipe, (char *) &sval );
      maxnum = maxtbl( acmp->vpipe ); 
      
/*fprintf( stderr, "%d + %d -  %d \n", acmp->avr, sval, old_batt );  */
 
      acmp->avr = acmp->avr + sval - old_batt;
 
/* fprintf( stderr, "%d dev %d \n", acmp->avr, maxnum );  */
 
      *avr = acmp->avr / maxnum; 
/*
fprintf( stderr, "%s %d %d %d %d %d\n", key, maxnum, sval, old_batt, acmp->avr, *avr )
;
fflush(stderr);
*/
      setarr( BatRec, key, (char *) acmp);
 
      if( maxnum >= MAX_FOR_AVR && *avr < DC_MINBATT ) alert = 1;
    
      return alert;
 
}
 

int
main(int argc, char **argv)
{
  double 	after, save_time, 
  		pkttime;
  float		fval;
  int 		err_in = 0,
  		id, rorb;
  int		nbytes, bsize=0;
  int 		dump=0; 
  int           avr, val, nadd, i;
  short         *sptr,sval;
  char          net[12], sta[12];

  struct PreHdr *hdr;
  short		hdrsize;
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
  while(1)  {   
    if( !orbreap( rorb, &id, srcid, &pkttime, &packet, &nbytes, &bsize)  ) {
         if ( regexec( &dcmatch, srcid, (size_t) 0, NULL, 0 ) ) continue;

         if( dump )  {
              sptr = (short *) packet;
              hdrsize = *sptr;
              hexdump( stderr, packet+hdrsize, nbytes-hdrsize );
         }


	 split_srcname ( srcid, &parts ) ;
	 strcpy(net, parts.src_net ) ; 
	 strcpy(sta, parts.src_sta ) ; 

         hdr = ( struct PreHdr *) packet;
      
	 memcpy( (char *) &sval, packet + hdr->hdrsiz + DCBATT, 2 ); 
         val = check_dcbatt( &sta[0], sval, &avr) ;
         fval = avr / 100.0;

         if( val != 0 )  {
             if( (dc = (char *) getarr( MailSent, sta)) == 0 )  {
                 dc = new_name( sta );
                 setarr( MailSent, dc, dc);
           
                 sprintf( alarm_str, 
                      "WARNING from DC %s!\nLOW BATTERY VOLATAGE: %.2f\nat %s\n",
                      sta, fval, s=strtime(pkttime) );
                 free(s);
                 for( i = 0; i < nadd; i++ )  {
                    madd = (char *) gettbl( MailAdd, i );
                    sprintf( mail_str, "echo \"%s\" | mailx -v -s \"%s:LOW BATTERY\" %s\0", alarm_str, sta, madd );

                    system( mail_str );
                 }
              }
         }  else {

             if( (dc = (char *) getarr( MailSent, sta)) != 0 ) { 
                 delarr( MailSent, dc);
                 sprintf( alarm_str, 
                      "Message from DC %s!\n\nBATTERY OK: %.2f\nat %s\n",
                       sta, fval, s=strtime(pkttime) );
                 free(s);
                 for( i = 0; i < nadd; i++ )  {
                    madd = (char *) gettbl( MailAdd, i );
                    sprintf( mail_str, "echo \"%s\" | mailx -v -s \"%s:BATTERY OK\" %s\0", alarm_str, sta, madd );
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

