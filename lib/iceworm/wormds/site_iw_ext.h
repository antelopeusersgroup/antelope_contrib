/*
 * site_iw_ext.h 
 * 
 * Header file for utility routines added to Earthworm to provide 
 *  datascope interface
 * Kent Lindquist
 * Geophysical Institute 
 * University of Alaska, Fairbanks
 * May, 1996
 */

#ifndef SITE_IW_EXT_H
#define SITE_IW_EXT_H

#include <stdio.h>
#include "stock.h"

typedef struct {
        char    sta[7];
        char    chan[9];
        int     chanid;
	int	pinno;
        double  lat;
        double  lon;
        double  elev;
	double	calib;
	double	calper;
	double	commdelay;
	char	units[13];
	char	savechan[2];
	char	respfile[FILENAME_MAX];
} STACHAN;
Tbl *Stachans;

int read_site_db( char * );
STACHAN *lookup_stachan( char *, char * );
int lookup_network( char *, char * );
int station_in_network( char * );
int subset_for_network();
void units_to_segtype( char *, char * );

#endif
