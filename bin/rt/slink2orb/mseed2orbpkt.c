/*
 * mseed2orbpkt.c
 * Wrap a Mini-SEED record to make an Antelope ORB packet of type 'SEED'
 *
 * Written by Chad Trabant, ORFEUS/EC-Project MEREDIAN
 *
 * Partially based on the liss2orb source code written by Dan Quinlan
 * (BRTT) and Marina Glushko (while at UCSD/IGPP) as found in the 
 * Antelope User's Group source code distribution.
 *
 * version 2002.008
 */

#include <stdlib.h>
#include <unistd.h>

#include <Pkt.h>
#include <stock.h>
#include <coords.h>
#include <xtra.h>
#include <tr.h>
#include <db.h>
#include <elog.h>
#include <msd.h>
#include <swapbytes.h>

int UNSEEDmod ( char *seed, int size, Steim *confp, double *time,
		double *samprate, int *nsamp, int **outp, int *datasz );

int
mseed2orbpkt ( char *seed, int size, char *dbase, int remap, char *srcname,
	       double *time, char **packet, int *nbytes, int *bufsize )
{
  static int *data, datasz=0;
  int needed, nsamp;
  int version = 2;
  int retcode = 0;
  double samprate, calib, calper;
  char sta[16], chan[16], segtype[6];
  char *cp;
  Srcname parts;
  Steim *conf;

  /* Initialize a new "Steim" structure and set record pointer */
  conf = newsteim();
  conf->record = seed;

  retcode = UNSEEDmod ( seed, size, conf, time, &samprate, &nsamp,
			&data, &datasz );

  switch ( retcode ) {
  case 0:
    needed = 20+size;  /* add extra for the Antelope header stuff */
    SIZE_BUFFER ( char *, *packet, *bufsize, needed );
    cp = *packet;

    strcpy ( parts.src_net, conf->sdh.net );
    strcpy ( parts.src_sta, conf->sdh.sta );
    strcpy ( parts.src_chan, conf->sdh.chan );
    strcpy ( parts.src_loc, conf->sdh.loc );
    strcpy ( parts.src_suffix, "SEED" );
    *parts.src_subcode = 0;

    if (map_seed_netsta ( conf->sdh.net, conf->sdh.sta, sta )  < 0) {
      retcode = -1;
      elog_complain(0, "mseed2orbpkt: map_seed_netsta() error [%s_%s]\n",
                    conf->sdh.net, conf->sdh.sta);
      break;
    }
    if (map_seed_chanloc ( sta, conf->sdh.chan, conf->sdh.loc, chan )  < 0) {
      retcode = -1;
      elog_complain(0, "mseed2orbpkt: map_seed_chanloc() error [%s_%s]\n",
                    conf->sdh.chan, conf->sdh.loc);
      break;
    }

    if ( remap ) {
      strcpy ( parts.src_sta, sta );
      strcpy ( parts.src_chan, chan );
      strcpy ( parts.src_loc, "" );
    }

    join_srcname ( &parts, srcname);

    /* get calibration information */
    dbget_calib( &sta[0], &chan[0], *time, dbase, &calib, &calper, segtype );

    /* start building the ORB packet */
    HI2NC(cp, &version, 1);
    cp += 1 * 1;
    memcpy (cp, &segtype, 1);
    cp += 1 * 1;
    HD2NF (cp, &samprate, 1);
    cp += 4 * 1;
    HD2NF (cp, &calib, 1);
    cp += 4 * 1;
    HD2NF (cp, &calper, 1);
    cp += 4 * 1;

    /* stick on the Mini-SEED record */
    memcpy (cp, seed, size);
    cp += size;
    *nbytes = cp-*packet;
    break;

  case -2:  /* got garbage */
    if (   conf->sdh.samprate_factor != 0
           || conf->sdh.samprate_multiplier != 0) {
      elog_complain ( 0, "Can't decode this packet:"  );
      hexdump ( stderr, seed, size );
    }
    break;

  default:
  case -1:
    elog_complain ( 0, "failed to decode packet." );
    hexdump ( stderr, seed, size );
    break;

  case -4: /* ASCII log data */
    break;

  }
  /* free the "Steim" struct */
  freesteim(conf);

  return retcode ;
}


/* UNSEEDmod():
   Modified from the version found in the liss2orb source code in
   the Antelope contributed code collection.

   This version does not do anything with the actual samples, just
   parse the record header and fill in the time, samprate and
   nsamp variables.
*/
int
UNSEEDmod ( char *seed, int size, Steim *confp, double *time,
            double *samprate, int *nsamp, int **outp, int *datasz ) 
{
    int retcode = 0 ;

    if ( parse_seed_data_header(confp) ) {

	if ( confp->has_s1000 && confp->s1000.dataformat == 0 ) {
	    elog_clear ();
	    retcode = -4 ;
	} else {
	    elog_notify ( 0,
		"Problems parsing SEED header or 1000 blockette.\n" ) ;
	    retcode = -2 ;
	}
	confp->record = 0 ;

    } else {

	*time = confp->sdh.epoch ;
	*samprate = confp->sdh.samprate ;
	*nsamp = confp->sdh.nsamp ;

	if ( confp->has_s1000 ) { 

	    switch (confp->s1000.dataformat) { 
		case 0:	/* ASCII text !? */
		    elog_notify ( 0, 
			"ASCII data format not supported" );
		    retcode = -1 ;
		    break ;

		case 1:	/* 16 bit integers */
		    break ;

		case 2:	/* 24 bit integers */
		    elog_notify ( 0, 
			"24 bit integers not supported" );
		    retcode = -1 ;
		    break ;

		case 3:	/* 32 bit integers */
		    break ;

		case 4:	/* 32 bit float */
		    elog_notify ( 0, 
			"32 bit floats not supported" );
		    retcode = -1 ;
		    break ;

		case 5:	/* 64 bit float */
		    elog_notify ( 0, 
			"64 bit floats not supported" );
		    retcode = -1 ;
		    break ;

		case 10:	/* Steim(1) compression */
		case 11:	/* Steim(2) compression */
		case 20:	/* Liss code for Steim(2) compression !? */
		    break ;

		case 12:	/* GEOSCOPE Multiplexed */
		case 13:
		case 14:
		    elog_notify ( 0, 
			"GEOSCOPE multiplexed data not supported" );
		    retcode = -1 ;
		    break; 

		case 15:    	/* US National Network Compression */
		    elog_notify ( 0, 
			"US National Network Compression not supported" );
		    retcode = -1 ;
		    break ;

		case 16:	/* CDSN 16 bit gain ranged */
		case 33:	/* RTSN 16 bit gain ranged Format */
		    break ;

		case 17:	/* Graefenberg 16 bit gain ranged */
		    elog_notify ( 0, 
			"Graefenberg 16 bit gain ranged not supported" );
		    retcode = -1 ;
		    break ;

		case 18:	/* IPG - Strasbourg 16 bit gain ranged */
		    elog_notify ( 0, 
			"IPG - Strasbourg 16 bit gain ranged not supported" );
		    retcode = -1 ;
		    break; 

		case 19:	/* Steim (3) compression */
		    elog_notify ( 0, 
			"Steim (3) compression not supported" );
		    retcode = -1 ;
		    break;

		case 30:	/* SRO Format */
		    break ;

		case 31:	/* HGLP Format */
		    elog_notify ( 0, 
			"HGLP Format not supported" );
		    retcode = -1 ;
		    break ;

		case 32:	/* DWWSSN Gain Range Format */
		    elog_notify ( 0, 
			"DWWSSN Gain Range Format not supported" );
		    retcode = -1 ;
		    break ;

		default:
		    elog_notify ( 0,
			"unrecognized SEED 1000 encoding format %d", 
			confp->s1000.dataformat );
		    retcode = -1 ;
		    break; 

	    }
	}
        else { /* no 1000 data blockette */
	  elog_notify( 0,
	      "no 1000 blockette found in Mini-SEED record for %s",
              confp->sdh.sta);
	}
    }
    confp->record = 0 ;
    confp->data = 0 ;

    return retcode ;
}
