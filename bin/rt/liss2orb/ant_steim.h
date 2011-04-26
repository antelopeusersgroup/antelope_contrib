#ifndef __steim__
#define __steim__

/****  Seed Data Header blockette info ****/
#define  NET_LEN   2
#define  STA_LEN   5
#define  LOC_LEN   2
#define  CHAN_LEN  3 

/* Information for Seed Data Header */ 
typedef struct SDH
{
    int             seq ; 
    char            net[NET_LEN+1];
    char            sta[STA_LEN+1];
    char            loc[LOC_LEN+1];
    char            chan[CHAN_LEN+1];
    int             year, doy, hour, minutes, sec, msec_x_10 ;
    int             nsamp;
    int             samprate_factor, samprate_multiplier ;
    int             activity_flags ;
    int             io_flags;
    int             quality_flags;
    int             blks_following;
    int             time_correction;
    int             data_offset ;
    int             next_blockette_offset ;
    char *data ;
    double          epoch, samprate ; /* not actually in the blockette,
                                        but converted to a useful form */
}               SDH ;
        
#define STEIM_BYTES_PER_FRAME 64
    
/* Information for Seed Data Blockette 1000 */
typedef struct S1000 {
    int         type ;
    int         next_blockette_offset ;
    int         dataformat ;
    int         sparc_order ;
    int         log2_record_length ;
}       S1000 ;


typedef struct SeedDataHeader {
    int sequence ;
    double time, samprate ;
    char *data ;
    FILE *infile ;
    FILE *outfile ;
} SeedDataHeader ;


/* Information for Seed Data Blockette 100 */
typedef struct S100 {
    int         type ;
    int         next_blockette_offset ;
    double      samprate ;
    int         flags ;
    int         reserved ;
}       S100 ;

typedef struct Sample {
    int x, dx, aclass ;
} Sample ;



typedef struct Steimdef {
    int bits, cnt, code, dnib, mn, mx, mask, sign, extend  ;
} Steimdef ;
    
typedef struct Steim {
    int frames_per_record,      /* frames per record */
        firstframe,             /* first frame (of fpr) to fill */
        level,                  /* steim compression level = 1 or 2 */
        byteswap,               /* byteswap flag */
        data_offset ;           /* Steim frames start at this offset in record */
    long nrecords,              /* count of records (= calls to save_record) */
         n0, n1 ;                 /* index of first and last sample in current record */
    int  record_size;            /* size of a record in bytes */
    char *record ;              /* record buffer */
    int  nerr ;                 /* number of errors encountered during (Steim 2) compression */
    int  *ffp ;                 /* pointer to first frame of record */
    int  *x0, *xn ;             /* pointer to first, last value of record */
    int  *fp ;                  /* pointer to current frame (and therefore w0) */
    int  new_record_flag ;      /* flag which is set to save the first sample in a record */
    int  fcnt ;                 /* count of frames in current record */
    int  *wp ;                  /* pointer to current w to fill */
    int  wcnt ;                 /* count of w's filled in current frame */
    int *data ;                 /* buffer for returned data from usteim */

    int last_x ;                /* previous sample value */ 
    Tbl *cbuf ;                 /* buffer for samples submitted to csteim, but
                                        not yet packed into a word or frame. */
    int maxclass ;              /* maximum "class" for the samples in cbuf */
    int code ;                  /* accumulating code for current frame */
    Steimdef *steimdef ;        /* static table which defines parameters for
                                        compression algorithm */
    int nsteimdef ;             /* number of entries in steimdef */
        
    SDH sdh ;
    S1000 s1000 ;
    S100 s100 ;
    int has_s100, has_s1000 ;   /* flags set if s100 and s1000 are present */
    void *pvt ;             /* user data passed to save_record */
} Steim ;

#ifdef	__cplusplus
extern "C" {
#endif

extern int csteim ( Steim *conf, int (*save_record) (), int *data, long npts );
extern Steim *newsteim ( void );
extern void freesteim ( Steim *conf );
extern int usteim ( Steim *conf, int **data, long *npts );

extern int _usteim ( Steim *conf, int *data, long *npts );
extern int unseed ( char *seed, long size, double *time, double *samprate, long *nsamp, int **outp, long *datasz );
extern int unpackSEEDblock ( Steim *conf, int *data, long *npts );

#ifdef	__cplusplus
}
#endif

#endif
