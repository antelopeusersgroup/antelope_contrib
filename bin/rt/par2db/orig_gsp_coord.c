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
int Gps_cnt = 1;
int Reject_cnt = 0;

int gps_coord( packet , srcname )
uchar_t *packet;
char *srcname;
{

    FILE *fp;
    int  i;
    struct BBAHdr *hdr;
    int off, val;
    short sval;
    char *done;
    char net[PKT_NAMESIZE], sta[PKT_NAMESIZE], key[64];
    
    hdr = ( struct BBAHdrr *) packet;
    parse_srcname( srcname, &net[0], &sta[0], 0, 0 );
/*  
    if( Gps_cnt >= 33 || Reject_cnt >= 300 ) 
      die(0, "Got all possible coordinates\n");
*/
    if( Gps == 0 ) Gps = newarr( 0 );
    if( ( done = ( char *) getarr( Gps, sta ) ) == 0 )  { 
    
       sprintf( &key[0], "%s.coord\0", sta );
       allot( char *, done, strlen(key) );
       memcpy( done, &key[0], strlen(key));
       setarr( Gps, sta, done );

       if( ( fp = fopen( key, "w" ) ) == NULL )  {
          die( 1, "Can't open %s file\n", key );
       }

       for( i = 0; i < NCOORD; i ++ )  { 
             sprintf( key, "%s_%s\0", sta, GPS_NAME[i] );
             off = GPS[i]+hdr->prehdr.hdrsiz;
             if( i == 5 || i == 10 )  {
	        memcpy( (char *) &sval, &packet[off], 2 );
	        val = sval;
	     }  else
	          sval = packet[off];
                /* val = atoi( &packet[off] );  */
             fprintf( fp, "%s:\t %d\t %s\n", GPS_NAME[i], sval, GPS_UNITS[i] );
             fflush( fp );
   
       }
       fclose(fp);
       Gps_cnt++;
    }  else Reject_cnt++;

    return 2; 
}


