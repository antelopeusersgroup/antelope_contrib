/*  Copyright (c) 1997 Boulder Real Time Technologies, Inc.           */
/*                                                                    */
/*  This software module is wholly owned by Boulder Real Time         */
/*  Technologies, Inc. Any use of this software module without        */
/*  express written permission from Boulder Real Time Technologies,   */
/*  Inc. is prohibited.                                               */


#include "tr.h"
#include "coords.h"

int 
UNSEED ( char *seed, int size, Steim **confp, double *time, double *samprate, int *nsamp, 
	    int **outp, int *datasz ) 
{
    int i, asamp, *out, npts, swap ; 
    Steim *conf ;
    char *indata ;
    int retcode = 0 ;

    conf = newsteim() ; 
    *confp = conf ;
    conf->record = seed ;

    if ( parse_seed_data_header(conf) ) {

	if ( conf->has_s1000 && conf->s1000.dataformat == 0 ) {
	    elog_clear ();
	    retcode = -4 ;
	} else {
	    register_error ( 0, 
		"Problems parsing SEED header or 1000 blockette.\n" ) ;
	    retcode = -2 ;
	}
	conf->record = 0 ; 
	freesteim(conf) ;

    } else {

	*time = conf->sdh.epoch ;
	*samprate = conf->sdh.samprate ;
	*nsamp = conf->sdh.nsamp ;

	SIZE_BUFFER(int *, *outp, *datasz, *nsamp ) ;
	out = *outp ;
	if ( conf->has_s1000 ) { 

	    indata = conf->record + conf->sdh.data_offset ;

#ifdef WORDS_BIGENDIAN
	    swap = (conf->s1000.sparc_order == 0 ) ;
#else
	    swap = (conf->s1000.sparc_order != 0 ) ;
#endif

	    switch (conf->s1000.dataformat) { 
		case 0:	/* ASCII text !? */
		    register_error ( 0, 
			"ASCII data format not supported" );
		    retcode = -1 ;
		    break ;

		case 1:	/* 16 bit integers */ {
		    short *shorts = (short *) out ;
		    if ( swap ) { 
			swap2((char *) indata, (char *)out, *nsamp) ; 
		    } else { 
			memcpy(out, indata, (*nsamp)*2) ; 
		    }
		    for (i=(*nsamp)-1 ; i>= 0 ; i-- ) {
			out[i] = shorts[i] ; 
		    }
		    }
		    break ;

		case 2:	/* 24 bit integers */
		    register_error ( 0, 
			"24 bit integers not supported" );
		    retcode = -1 ;
		    break ;

		case 3:	/* 32 bit integers */
		    if ( swap ) { 
			swap4((char *)indata, (char *)out, *nsamp) ; 
		    } else { 
			memcpy(out, indata, (*nsamp)*4) ; 
		    }
		    break ;

		case 4:	/* 32 bit float */
		    register_error ( 0, 
			"32 bit floats not supported" );
		    retcode = -1 ;
		    break ;

		case 5:	/* 64 bit float */
		    register_error ( 0, 
			"64 bit floats not supported" );
		    retcode = -1 ;
		    break ;

		case 10:	/* Steim(1) compression */
		case 11:	/* Steim(2) compression */
		case 20:	/* Liss code for Steim(2) compression !? */
		    conf->data = out ;
		    retcode = usteim (conf, outp, &npts ) ;
		    break ;

		case 12:	/* GEOSCOPE Multiplexed */
		case 13:
		case 14:
		    register_error ( 0, 
			"GEOSCOPE multiplexed data not supported" );
		    retcode = -1 ;
		    break; 

		case 15:    	/* US National Network Compression */
		    register_error ( 0, 
			"US National Network Compression not supported" );
		    retcode = -1 ;
		    break ;

		case 16:	/* CDSN 16 bit gain ranged */
		case 33:	/* RTSN 16 bit gain ranged Format */
		    for (i=0; i<((*nsamp)*2); i+=2) {
			short ashort;
			int gr;
			ashort  = ( indata[i] & 0x3F) << 8;
			ashort |= ( indata[i+1] & 0xFF);
			ashort -= 8191;
			asamp = (int) ashort;
			gr = ( indata[i] & 0xC0) >> 6;
			switch(gr) {
			case 1:
			  asamp <<= 2;  
			  break;
			case 2:
			  asamp <<= 4;
			  break;
			case 3:
			  asamp <<= 7;
			  break;

			}
			*out++ = asamp; 
		      }
		      break ;

		case 17:	/* Graefenberg 16 bit gain ranged */
		    register_error ( 0, 
			"Graefenberg 16 bit gain ranged not supported" );
		    retcode = -1 ;
		    break ;

		case 18:	/* IPG - Strasbourg 16 bit gain ranged */
		    register_error ( 0, 
			"IPG - Strasbourg 16 bit gain ranged not supported" );
		    retcode = -1 ;
		    break; 

		case 19:	/* Steim (3) compression */
		    register_error ( 0, 
			"Steim (3) compression not supported" );
		    retcode = -1 ;
		    break; 

		case 30:	/* SRO Format */
		      for (i=0; i<((*nsamp)*2); i+=2) {
			  int gr;
			  asamp  = (indata[i] & 0x0F) << 8;
			  asamp |= (indata[i+1] & 0xFF);
			  if (asamp >= 0x800) asamp -= 4096;
			  gr = (indata[i] & 0xF0) >> 4;
			  asamp <<= (10-gr);
			  *out++ = asamp;
		     }
		    break ;

		case 31:	/* HGLP Format */
		    register_error ( 0, 
			"HGLP Format not supported" );
		    retcode = -1 ;
		    break ;

		case 32:	/* DWWSSN Gain Range Format */
		    register_error ( 0, 
			"DWWSSN Gain Range Format not supported" );
		    retcode = -1 ;
		    break ;

		default:
		    register_error ( 0, 
			"unrecognized SEED 1000 encoding format %d", 
			conf->s1000.dataformat );
		    retcode = -1 ;
		    break; 

	    }
	}
    }
    conf->record = 0 ; 
    conf->data = 0 ;
    return retcode ;
}

/* $Id$ */
