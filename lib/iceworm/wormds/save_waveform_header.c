#include <stdlib.h>
#include "coords.h"
#include "earthworm.h"
#include "ak_ahheader.h"
#include "sac.h"
#include "iceworm_extensions.h"

#define STREQ(a, b) (strcmp((a), (b)) == 0)

/************************************************************************
 * save_header() save file header of the type requested in the 		*
 *   configuration file and return the byte offset at which to start    *
 *   saving data.                                                       *
 ***********************************************************************/
int
save_waveform_header( char *format, FILE *fp, STACHAN *stachan,
			double ts, int nsamp, double samprate, char *datatype )
{
	int	foff;
	ak_ahhed akahheader;
	sac_t	sach;              /* SAC header structure             */
	char	segtype[2];
	char	datestring[STRSZ];
	char	character[STRSZ];
	char	*s;

	if( STREQ( format, "Raw" ) )
	{
		foff = 0;
	}
	else if( STREQ( format, "AlaskanAH" ) )
	{
		get_null_ak_ahhead( &akahheader );

                strcpy( akahheader.station.code, stachan->sta );
                strcpy( akahheader.station.chan, stachan->chan );
                akahheader.station.slat = stachan->lat;
                akahheader.station.slon = stachan->lon;
                akahheader.station.elev = stachan->elev * 1000;
                akahheader.record.ndata = (long) nsamp;
                akahheader.record.delta = 1 / samprate;
                if(STREQ(datatype,"s2")) {
                        akahheader.record.type = 7;
                } else if(STREQ(datatype,"t4")) {
                        akahheader.record.type = 1;
                } else if(STREQ(datatype,"s4")) {
			akahheader.record.type = 8;
		}

                strcpy(datestring, s = epoch2str(ts, "%Y %m %d %H %M %S.%s"));
		free( s );
                sscanf( datestring, "%hd %hd %hd %hd %hd %f",
                        &akahheader.record.abstime.yr,
                        &akahheader.record.abstime.mo,
                        &akahheader.record.abstime.day,
                        &akahheader.record.abstime.hr,
                        &akahheader.record.abstime.mn,
                        &akahheader.record.abstime.sec);

		akahheader.extra[9] = stachan->commdelay;
 
		write_ak_ah( fileno( fp ), &akahheader );
		foff = AK_AHHEADSIZE;
	}
	else if( STREQ( format, "SAC" ) )
	{
		sach = sac_def;

		strcpy( sach.kstnm, stachan->sta );

		sach.scale = stachan->calib;

		/* npts - number of points in the data segment */
		sach.npts = nsamp;

		/* b - beginning value of independent variable */
		sach.b = (float) 0.0;

		/* e - ending value of independent variable */
		sach.e = (float) ( ( nsamp - 1 ) / samprate );

		/* iftype - type of file  (known a priori) */
		sach.iftype = ITIME;

		/* leven - true if data is evenly spaced  (known a priori) */
		sach.leven = TRUE;

		/* delta - time increment between samples */
		sach.delta = 1 / samprate;

		units_to_segtype( stachan->units, segtype );
		switch( segtype[0] )
		{
		case ( 'A' ):
			sach.idep = IACC;
			break;
		case ( 'V' ):
			sach.idep = IVEL;
			break;
		case ( 'D' ):
			sach.idep = IDISP;
			break;
		default:
			sach.idep = IVEL;
			break;
		}

                strcpy(datestring, s = epoch2str(ts, "%Y %j %H %M %S %s"));
		free( s );
                sscanf( datestring, "%ld %ld %ld %ld %ld %ld",
				&sach.nzyear,
				&sach.nzjday,
				&sach.nzhour,
				&sach.nzmin,
				&sach.nzsec,
				&sach.nzmsec );

		sach.stla = stachan->lat;
		sach.stlo = stachan->lon;
		sach.stel = stachan->elev;

		sprintf( character, "%-8.8s", stachan->chan );
		strncpy( sach.kcmpnm, character, 8 );

		/* lpspol - true if station components have positive polarity */
		sach.lpspol = FALSE;

		/* lcalca - true if event info is to be calculated from position */
		sach.lcalda = TRUE;

		/* isynth - synthetic data flag */
		sach.isynth = IRLDTA;

		/* lovrok - true if the file can be overwritten */
		sach.lovrok = FALSE;

		foff = write_sac_header( fp, &sach );
	}
	else
	{
		logit( "t", "Requested output format %s not understood\n",
			format );
		foff = 0;
	}

	return( foff );
}

