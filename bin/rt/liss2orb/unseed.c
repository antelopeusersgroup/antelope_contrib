
#include "liss2orb.h"

int 
UNSEED ( char *seed, int size, Steim **confp, double *time, double *samprate, int *nsamp, 
	    int **outp, int *datasz ) 
{
    int i, asamp, *out, swap ; 
    long npts ;
    Steim *conf ;
    char *indata ;
    int retcode = 0 ;
    static int 
	notified_ASCII=0, 
	notified_DWWSSN=0,
	notified_24bit=0,
	notified_32bit=0,
	notified_64bit=0,
	notified_GEOSCOPE=0,
	notified_USNN=0,
	notified_Graefenberg=0,
	notified_Strasbourg=0,
	notified_Steim3=0,
	notified_HGLP=0 ;

    conf = newsteim() ; 
    *confp = conf ;
    conf->record = seed ;

    if ( parse_seed_data_header(conf) ) {

	if ( conf->has_s1000 && conf->s1000.dataformat == 0 ) {
	    elog_clear ();
	    retcode = -4 ;
	} else {
	    elog_log( 0, 
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
		    if ( ! notified_ASCII ) { 
			notified_ASCII = 1 ;
			elog_complain( 0, 
			    "ASCII data format not supported" );
			hexdump ( stderr, seed, size ) ;
			elog_complain( 0, "ignoring these packets hereafter" ) ; 
		    }
		    retcode = -3 ;
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
		    if ( ! notified_24bit ) { 
			notified_24bit = 1 ; 
			elog_complain( 0, "24 bit integers not supported" );
			hexdump ( stderr, seed, size ) ;
			elog_complain( 0, "ignoring these packets hereafter" ) ; 
		    }
		    retcode = -3 ;
		    break ;

		case 3:	/* 32 bit integers */
		    if ( swap ) { 
			swap4((char *)indata, (char *)out, *nsamp) ; 
		    } else { 
			memcpy(out, indata, (*nsamp)*4) ; 
		    }
		    break ;

		case 4:	/* 32 bit float */
		    if ( ! notified_32bit ) { 
			notified_32bit = 1 ; 
			elog_complain( 0, "32 bit floats not supported" );
			hexdump ( stderr, seed, size ) ;
			elog_complain( 0, "ignoring these packets hereafter" ) ; 
		    }
		    retcode = -3 ;
		    break ;

		case 5:	/* 64 bit float */
		    if ( ! notified_64bit ) { 
			notified_64bit = 1 ; 
			elog_complain( 0, "64 bit floats not supported" );
			hexdump ( stderr, seed, size ) ;
			elog_complain( 0, "ignoring these packets hereafter" ) ; 
		    }
		    retcode = -3 ;
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
		    if ( ! notified_GEOSCOPE ) { 
			notified_GEOSCOPE = 1 ;
			elog_complain( 0, "GEOSCOPE multiplexed data not supported" );
			hexdump ( stderr, seed, size ) ;
			elog_complain( 0, "ignoring these packets hereafter" ) ; 
		    }
		    retcode = -3 ;
		    break; 

		case 15:    	/* US National Network Compression */
		    if ( ! notified_USNN ) { 
			notified_USNN = 1 ;
			elog_complain( 0, "US National Network Compression not supported" );
			hexdump ( stderr, seed, size ) ;
			elog_complain( 0, "ignoring these packets hereafter" ) ; 
		    }
		    retcode = -3 ;
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
		    if ( ! notified_Graefenberg ) { 
			notified_Graefenberg = 1 ;
			elog_complain( 0, "Graefenberg 16 bit gain ranged not supported" );
			hexdump ( stderr, seed, size ) ;
			elog_complain( 0, "ignoring these packets hereafter" ) ; 
		    }
		    retcode = -3 ;
		    break ;

		case 18:	/* IPG - Strasbourg 16 bit gain ranged */
		    if ( ! notified_Strasbourg ) { 
			notified_Strasbourg = 1 ;
			elog_complain( 0, "IPG - Strasbourg 16 bit gain ranged not supported" );
			hexdump ( stderr, seed, size ) ;
			elog_complain( 0, "ignoring these packets hereafter" ) ; 
		    }
		    retcode = -3 ;
		    break; 

		case 19:	/* Steim (3) compression */
		    if ( ! notified_Steim3 ) { 
			notified_Steim3 = 1 ;
			elog_complain( 0, "Steim (3) compression not supported" );
			hexdump ( stderr, seed, size ) ;
			elog_complain( 0, "ignoring these packets hereafter" ) ; 
		    }
		    retcode = -3 ;
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
		    if ( ! notified_HGLP ) { 
			notified_Steim3 = 1 ;
			elog_complain( 0, "HGLP Format not supported" );
			hexdump ( stderr, seed, size ) ;
			elog_complain( 0, "ignoring these packets hereafter" ) ; 
		    }
		    retcode = -3 ;
		    break ;

		case 32:	/* DWWSSN Gain Range Format */
		    if ( ! notified_DWWSSN ) { 
			notified_DWWSSN = 1 ;
			elog_complain( 0, "DWWSSN Gain Range Format not supported" );
			hexdump ( stderr, seed, size ) ;
			elog_complain( 0, "ignoring these packets hereafter" ) ; 
		    }
		    retcode = -3 ;
		    break ;

		default:
		    elog_log( 0, 
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
