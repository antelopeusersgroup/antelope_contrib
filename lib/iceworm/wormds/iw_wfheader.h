/*
 * iw_wfheader.h
 *
 * Iceworm Header file for output of various headers for waveform data
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * February, 1997
 */

#ifndef IW_WFHEADER_H
#define IW_WFHEADER_H

#include "iceworm_extensions.h"

int save_waveform_header( char *, FILE *, STACHAN *, double, int, double, char * );
int save_waveform_data( char *, FILE *, int, char *, DataPtr );

#endif
