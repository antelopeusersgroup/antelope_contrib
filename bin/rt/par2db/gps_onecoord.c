/******************************************************************
 *
 *  bba2_par.c
 *
 ********************************************************************/
#include "header.h"

#define NCOORD 11

static int GPS[] = {LATDD_BBA2, LATMM_BBA2,LATSS_BBA2,LATHH_BBA2,
                    NS_BBA2,LONDD_BBA2,LONMM_BBA2,LONSS_BBA2,
		    LONHH_BBA2,EW_BBA2,ALTD_BBA2 };

static char *GPS_NAME[] = { "LATDD", "LATMM","LATSS","LATHH",
                           "NS","LONDD","LONMM","LONSS",
		           "LONHH","EW","ALTD" };

static char *GPS_UNITS[] = { "DEGREES", "MIN","SEC","MSEC",
                           "GPS_POSITION","DEGREE","MIN","SEC",
		           "MSEC","GPS_POSITION","GPS_ALTITUDE" };

Arr *Gps=0;

typedef struct StaFp {
    char fname[32];
    int fp;
} StaFp;

int gps_coord( packet , srcname )
uchar_t *packet;
char *srcname;
{

    StaFp *stafp;
    int  i;
    struct BBAHdr *hdr;
    int off;
    short sval;
    short buf[NCOORD];
    char *done;
    char net[PKT_NAMESIZE], sta[PKT_NAMESIZE], key[64];
    
    hdr = ( struct BBAHdrr *) packet;
    parse_srcname( srcname, &net[0], &sta[0], 0, 0 );

    if( Gps == 0 ) Gps = newarr( 0 );
    sprintf( &key[0], "%s.coord\0", sta );
    memcpy( done, &key[0], strlen(key));

    if ( ( stafp = ( StaFp *) getarr( Gps, key ) ) == 0 )  {
         allot( StaFp *, stafp, sizeof(StaFp) );
          strcpy( stafp->fname, key );

          if( ( stafp->fp = open( key, O_CREAT | O_WRONLY, 0664) ) < 0 )  
                die( 1, "Can't open %s file\n", key );
          setarr( Gps, key, (char *) stafp );
    }

    for( i = 0; i < NCOORD; i ++ )  { 
         sprintf( key, "%s_%s\0", sta, GPS_NAME[i] );
         off = GPS[i]+hdr->prehdr.hdrsiz;
         if( i == 5 || i == 10 )  {
            memcpy( (char *) &sval, &packet[off], 2 );
         }  else
            sval = packet[off];
         write( stafp->fp, (char *) &sval, sizeof( short) );
   } 
    return 2; 
}


