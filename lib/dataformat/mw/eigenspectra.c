
#include "stock.h"
#include "db.h"
#include "tr.h"

typedef struct complex
{
    float           r,
                    i;
}               complex;

int 
eigenspectra(mw)
Dbptr           mw;
{
    int             nsamp,
                    nfft,
                    ntapers;
    float          *data;
    double         *tapers;
    double         *eigen,
                   *eigen_delta;
    complex        *exponentials, *eigenspectra;
    double          time_bandwidth;
    int             rs,
                    re;

    dbget_range(mw, &rs, &re);
    for (mw.record = rs; mw.record < re; mw.record++)
      {
	dbgetv(mw, 0,
	       "nsamp", &nsamp,
	       "nfft", &nfft,
	       "ntapers", &ntapers,
	       "tbw", &time_bandwidth,
	       "data", &data,
	       0);
	allot(complex *, exponentials, 2*nfft+8 ) ;
	allot(complex *, eigenspectra, ntapers * nfft);
	get_mwtapers(nsamp, ntapers, time_bandwidth,
		     &eigen, &eigen_delta, &tapers);
	eigenft_(&nsamp, &nfft, &ntapers, data, &nsamp, tapers,
		 exponentials, &nfft, eigenspectra);
	dbputv(mw, 0,
	       "exponentials", exponentials,
	       "eigenspectra", eigenspectra,
	       0);
      }
}

/* $Id$ */
