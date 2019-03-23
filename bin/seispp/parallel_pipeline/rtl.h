#ifndef __rtl__
#define __rtl__

#include <unistd.h>
#include <stdlib.h>
#include <stdarg.h>

#include "stock.h"
#include "xheap.h"
#include "elog.h"

#define RTLOG_UNDEF	2.71828183e-10
/* #define RTLOG_INIT_SIZE	(81920) */
#define RTLOG_INTERVAL_MIN 0 
#define RTLOG_INTERVAL_MAX (86400*10) 
#define RTLOG_INITIAL_XHEAP_SIZE 	1000000

#define RT_VERYLARGENUMBER	(2.718e39)

#define RTLOG_INSTANCE  11
#define RTLOG_SCALAR    12
#define RTLOG_VECTOR    13

#define ACTIVE   	0   /* accumulating during current period */
//#define SHORT		1   /* previous short period stats */
//#define LONG 		2   /* accumulated since midnight */
#define DECAYED		3   /* decayed average of short */

typedef struct Rtl_instance { /* an instance has no associated value, just a message  */
    long count ; 		/* number of observations */
    double lastt ;		/* last observation time */
} Rtl_instance ; 

typedef struct Rtl_scalar {  /* one associated value */
    long count ; 		/* number of observations */
    double lastt ;		/* last observation time */
    double min ; 		/* minimum of all observed values */
    double max ;		/* maximum of all observed values */
    double M ; 			/* Welford method for calculating mean and variance */
    double S ;
} Rtl_scalar ; 

/* The old, prev, latest, and decayed should be handled with indexes into an array, to  */
/* allow the report routine to be the same for short/long/decayed/current reports. */
typedef struct Rtl_instance_track { 
    long kind ;			/* sanity check = RTLOG_INSTANCE */
    int severity ;		/* severity level  */
    Rtl_instance r[4]; 		/* ACTIVE, SHORT, LONG, DECAYED */
    char *msg ;			/* last associated message  */
} Rtl_instance_track ;	

typedef struct Rtl_scalar_track { 
    long kind ;			/* sanity check = RTLOG_SCALAR */
    int severity ;		/* severity level  */
    Rtl_scalar r[4]; 		/* ACTIVE, SHORT, LONG, DECAYED */
    char *msg ;			/* last associated message  */
} Rtl_scalar_track ;

typedef struct Rtlflags { 
	unsigned int short_print : 1 ; 
	unsigned int long_print  : 1 ; 
	unsigned int decayed_print  : 1 ; 
	unsigned int short_stat : 1 ; 
	unsigned int long_stat  : 1 ; 
	unsigned int decayed_stat  : 1 ; 
        unsigned int verbose : 4 ; 
} Rtlflags ;

typedef struct Rtlog_config { 
    Arr *a ;			/* records are saved in either a or x */
    Xheap *x ; 
    char *xfilename ;		/* filename when x is used */
    int long_interval ;         /* reports are generated at short and/or long intervals. */
    long short_interval ;     
    long last_long ; 		/* time when last long report was generated. */
    long last_short ; 		/* time when last short report was generated. */
    double alpha ; 		/* decay value */
    double start ; 		/* start time  */
    int init ;			/* set after this structure has been initialized ;  */
    pthread_mutex_t lock ; 	/* lock for all statistics */
    double curtime ; 		/* current time at start of update thread */
    Rtlflags flags ;		/* global flags */
} Rtlog_config ;

extern Rtlog_config Rtc ;
extern int rtl_noisy_fini ; 

#ifdef  __cplusplus
extern "C" {
#endif

/* extern void rtlog (int severity, char *type, char *id, double val, char *fmt, ...) ; */
extern void rtlog_scalar (int severity, char *type, char *id, double val, char *fmt, ...) ;
extern void rtlog_instance (int severity, char *type, char *id, char *fmt, ...) ;
extern void rtreport() ;
extern void rtltypeid(char *key, char **type, char **id) ;
extern char * rtlkey(char *type, char *id) ;
extern void * rtlookup(char *key, int code) ;
extern void rtlog_bad_type(char *key, int found, int wanted) ;
extern void rtlreport(int rtltype) ;

void rtl_instance_add(Rtl_instance *src, Rtl_instance *dst) ;
void rtl_instance_copy(Rtl_instance *src, Rtl_instance *dst) ;
void rtl_instance_decay(Rtl_instance *src, Rtl_instance *dst, double alpha) ;
void init_instance(Rtl_instance *instance) ;
Rtl_instance_track * new_instance_track() ;

void rtl_scalar_add(Rtl_scalar *src, Rtl_scalar *dst) ;
void rtl_scalar_copy(Rtl_scalar *src, Rtl_scalar *dst) ;
void rtl_scalar_decay(Rtl_scalar *src, Rtl_scalar *dst, double alpha) ;
void init_scalar(Rtl_scalar *scalar) ;
Rtl_scalar_track * new_scalar_track() ;

void rtl2orb(int which) ;

#ifdef  __cplusplus
}
#endif

#endif

