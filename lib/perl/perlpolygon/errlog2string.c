#ifdef lint
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include <stdio.h>
#include "db.h"
#include "coords.h"
#endif
#include "elog.h"

SV *
errlog2string(printflag)
int             printflag;
{
    int             i, n ;
    Elog_msg *elog_msg ;
    char s[STRSZ] ;
    SV	*sv; 
    Tbl *tbl ;
 
    sv = sv_2mortal(newSVpv("", 0));
    elog_query ( ELOG_TBL, 0, (void **) &tbl ) ;
    if (printflag) {
	n = maxtbl ( tbl ) ;
	for ( i=0 ; i<n ; i++ ) {
	    elog_msg = (Elog_msg *) gettbl ( tbl, i ) ; 
	    sv_catpv(sv, elog_msg->tag) ; 
	    sv_catpv(sv, ": " ) ; 
	    sv_catpv(sv, elog_msg->msg) ;
	    if ( elog_msg->sys ) { 
		sv_catpv ( sv, "\t" ) ; 
		sv_catpv ( sv, elog_msg->sys ) ; 
		sv_catpv ( sv, "\n" ) ; 
	    }
	    if ( elog_msg->n > 1 ) { 
		sprintf ( s, "\t** repeated %d times\n", elog_msg->n ) ;
		sv_catpv ( sv, s ) ;
	    }
        }
    }
 
    elog_clear() ;
    return sv ;
}

/* $Id$ */
