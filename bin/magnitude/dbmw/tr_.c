
#include <stdio.h>
#include "tr.h"
#include "db.h"
#include "stock.h"

void trgetwf2_ ( 
    FDbptr *f_db, 
    Arr **f_maparr, 
    Trsample *fdata, 
    int *f_nmax, 
    double *f_reqt0, 
    double *f_reqt1, 
    double *t0p, 
    double *t1p, 
    int *nptsp, 
    int (*fill)(), 
    void **pvt )
{
    static Trsample *data = 0 ; 
    static long datasz = 0 ;
    long i, n, npts ; 
    int result ;
    Dbptr db ;
    
    _fdb2db (f_db, &db ) ;
    if ( *fill != 0 ) {
	result = trgetwf(db, 0, &data, &datasz, *f_reqt0, *f_reqt1, t0p, t1p, &npts, fill, pvt );
    } else {
	result = trgetwf(db, 0, &data, &datasz, *f_reqt0, *f_reqt1, t0p, t1p, &npts, 0, pvt );
    }
    if ( result != 0 ) {
	trgetwf_error ( db, result );
    }
    n = MIN(npts, *f_nmax) ;
    for(i=0;i<n; i++ ) {
	fdata[i] = data[i] ;
    }
    *nptsp = n ;
}

int trputwf_ ( f_db, data )
Dbptr *f_db ;
float *data ;
{
    return trputwf(*f_db, data );
}

