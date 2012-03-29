#ifndef PFORBSTAT
#define PFORBSTAT
#include "orb.h"
#include "pf.h"
#include "swapbytes.h"

#ifdef	__cplusplus
extern "C" {
#endif

#define PFORBSTAT_SERVER 1
#define PFORBSTAT_SOURCES 2
#define PFORBSTAT_CLIENTS 4
#define PFORBSTAT_CONNECTIONS 8
#define PFORBSTAT_DATABASES 16

#define PFORBSTAT_VERSION 2.0

extern Pf *orbstat2pf( Orbstat *orbstat, int orbversion );
extern Pf *orbsources2pf( double atime, Orbsrc *sources, int nsources );
extern Pf *orbclients2pf( double atime, Orbclient *clients, int nclients );

extern Pf *pforbstat( int orbfd, int flags );

#ifdef	__cplusplus
}
#endif

#endif
