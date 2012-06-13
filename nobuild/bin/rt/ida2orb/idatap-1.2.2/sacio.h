/* @(#)sacio.h	1.2 12/28/93 */
/*======================================================================
 *
 *  sacio.h
 *
 *  Include file for sacio library.  Everything before the prototypes
 *  was lifted from Dennis O'Neil's SEED reader... thanks!
 *
 *====================================================================*/
#ifndef sac_h_included
#define sac_h_included

#include <stdio.h>

#define SAC_HDRLEN 632

struct sac_header {
    float   delta;      /* RF time increment, sec    */
    float   depmin;     /*    minimum amplitude      */
    float   depmax;     /*    maximum amplitude      */
    float   scale;      /*    amplitude scale factor */
    float   odelta;     /*    observed time inc      */
    float   b;          /* RD initial value, ampl.   */
    float   e;          /* RD final value, amplitude */
    float   o;          /*    event start, sec > 0   */
    float   a;          /*    1st arrival time       */
    float   internal1;  /*    internal use           */
    float   t0;         /*    user-defined time pick */
    float   t1;         /*    user-defined time pick */
    float   t2;         /*    user-defined time pick */
    float   t3;         /*    user-defined time pick */
    float   t4;         /*    user-defined time pick */
    float   t5;         /*    user-defined time pick */
    float   t6;         /*    user-defined time pick */
    float   t7;         /*    user-defined time pick */
    float   t8;         /*    user-defined time pick */
    float   t9;         /*    user-defined time pick */
    float   f;          /*    event end, sec > 0     */
    float   resp0;      /*    instrument respnse parm*/
    float   resp1;      /*    instrument respnse parm*/
    float   resp2;      /*    instrument respnse parm*/
    float   resp3;      /*    instrument respnse parm*/
    float   resp4;      /*    instrument respnse parm*/
    float   resp5;      /*    instrument respnse parm*/
    float   resp6;      /*    instrument respnse parm*/
    float   resp7;      /*    instrument respnse parm*/
    float   resp8;      /*    instrument respnse parm*/
    float   resp9;      /*    instrument respnse parm*/
    float   stla;       /*  T station latititude     */
    float   stlo;       /*  T station longitude      */
    float   stel;       /*  T station elevation, m   */
    float   stdp;       /*  T station depth, m       */
    float   evla;       /*    event latitude         */
    float   evlo;       /*    event longitude        */
    float   evel;       /*    event elevation        */
    float   evdp;       /*    event depth            */
    float   unused1;    /*    reserved for future use*/
    float   user0;      /*    available to user      */
    float   user1;      /*    available to user      */
    float   user2;      /*    available to user      */
    float   user3;      /*    available to user      */
    float   user4;      /*    available to user      */
    float   user5;      /*    available to user      */
    float   user6;      /*    available to user      */
    float   user7;      /*    available to user      */
    float   user8;      /*    available to user      */
    float   user9;      /*    available to user      */
    float   dist;       /*    stn-event distance, km */
    float   az;         /*    event-stn azimuth      */
    float   baz;        /*    stn-event azimuth      */
    float   gcarc;      /*    stn-event dist, degrees*/
    float   internal2;  /*    internal use           */
    float   internal3;  /*    internal use           */
    float   depmen;     /*    mean value, amplitude  */
    float   cmpaz;      /*  T component azimuth      */
    float   cmpinc;     /*  T component inclination  */
    float   unused2;    /*    reserved for future use*/
    float   unused3;    /*    reserved for future use*/
    float   unused4;    /*    reserved for future use*/
    float   unused5;    /*    reserved for future use*/
    float   unused6;    /*    reserved for future use*/
    float   unused7;    /*    reserved for future use*/
    float   unused8;    /*    reserved for future use*/
    float   unused9;    /*    reserved for future use*/
    float   unused10;   /*    reserved for future use*/
    float   unused11;   /*    reserved for future use*/
    float   unused12;   /*    reserved for future use*/
    long    nzyear;     /*  F zero time of file, yr  */
    long    nzjday;     /*  F zero time of file, day */
    long    nzhour;     /*  F zero time of file, hr  */
    long    nzmin;      /*  F zero time of file, min */
    long    nzsec;      /*  F zero time of file, sec */
    long    nzmsec;     /*  F zero time of file, msec*/
    long    internal4;  /*    internal use           */
    long    internal5;  /*    internal use           */
    long    internal6;  /*    internal use           */
    long    npts;       /* RF number of samples      */
    long    internal7;  /*    internal use           */
    long    internal8;  /*    internal use           */
    long    unused13;   /*    reserved for future use*/
    long    unused14;   /*    reserved for future use*/
    long    unused15;   /*    reserved for future use*/
    long    iftype;     /* RA type of file           */
    long    idep;       /*    type of amplitude      */
    long    iztype;     /*    zero time equivalence  */
    long    unused16;   /*    reserved for future use*/
    long    iinst;      /*    recording instrument   */
    long    istreg;     /*    stn geographic region  */
    long    ievreg;     /*    event geographic region*/
    long    ievtyp;     /*    event type             */
    long    iqual;      /*    quality of data        */
    long    isynth;     /*    synthetic data flag    */
    long    unused17;   /*    reserved for future use*/
    long    unused18;   /*    reserved for future use*/
    long    unused19;   /*    reserved for future use*/
    long    unused20;   /*    reserved for future use*/
    long    unused21;   /*    reserved for future use*/
    long    unused22;   /*    reserved for future use*/
    long    unused23;   /*    reserved for future use*/
    long    unused24;   /*    reserved for future use*/
    long    unused25;   /*    reserved for future use*/
    long    unused26;   /*    reserved for future use*/
    long    leven;      /* RA data-evenly-spaced flag*/
    long    lpspol;     /*    station polarity flag  */
    long    lovrok;     /*    overwrite permission   */
    long    lcalda;     /*    calc distance, azimuth */
    long    unused27;   /*    reserved for future use*/
    char    kstnm[8+1]; /*  F station name           */
    char    kevnm[16+1];/*    event name             */
    char    khole[8+1]; /*    man-made event name    */
    char    ko[8+1];    /*    event origin time id   */
    char    ka[8+1];    /*    1st arrival time ident */
    char    kt0[8+1];   /*    time pick 0 ident      */
    char    kt1[8+1];   /*    time pick 1 ident      */
    char    kt2[8+1];   /*    time pick 2 ident      */
    char    kt3[8+1];   /*    time pick 3 ident      */
    char    kt4[8+1];   /*    time pick 4 ident      */
    char    kt5[8+1];   /*    time pick 5 ident      */
    char    kt6[8+1];   /*    time pick 6 ident      */
    char    kt7[8+1];   /*    time pick 7 ident      */
    char    kt8[8+1];   /*    time pick 8 ident      */
    char    kt9[8+1];   /*    time pick 9 ident      */
    char    kf[8+1];    /*    end of event ident     */
    char    kuser0[8+1];/*    available to user      */
    char    kuser1[8+1];/*    available to user      */
    char    kuser2[8+1];/*    available to user      */
    char    kcmpnm[8+1];/*  F component name         */
    char    knetwk[8+1];/*    network name           */
    char    kdatrd[8+1];/*    date data read         */
    char    kinst[8+1]; /*    instrument name        */
};

/* definitions of constants for SAC enumerated data values */

#define IREAL   0       /* undocumented              */
#define ITIME   1       /* file: time series data    */
#define IRLIM   2       /* file: real&imag spectrum  */
#define IAMPH   3       /* file: ampl&phas spectrum  */
#define IXY     4       /* file: gen'l x vs y data   */
#define IUNKN   5       /* x data: unknown type      */
                        /* zero time: unknown        */
                        /* event type: unknown       */
#define IDISP   6       /* x data: displacement (nm) */
#define IVEL    7       /* x data: velocity (nm/sec) */
#define IACC    8       /* x data: accel (cm/sec/sec)*/
#define IB      9       /* zero time: start of file  */
#define IDAY   10       /* zero time: 0000 of GMT day*/
#define IO     11       /* zero time: event origin   */
#define IA     12       /* zero time: 1st arrival    */
#define IT0    13       /* zero time: user timepick 0*/
#define IT1    14       /* zero time: user timepick 1*/
#define IT2    15       /* zero time: user timepick 2*/
#define IT3    16       /* zero time: user timepick 3*/
#define IT4    17       /* zero time: user timepick 4*/
#define IT5    18       /* zero time: user timepick 5*/
#define IT6    19       /* zero time: user timepick 6*/
#define IT7    20       /* zero time: user timepick 7*/
#define IT8    21       /* zero time: user timepick 8*/
#define IT9    22       /* zero time: user timepick 9*/
#define IRADNV 23       /* undocumented              */
#define ITANNV 24       /* undocumented              */
#define IRADEV 25       /* undocumented              */
#define ITANEV 26       /* undocumented              */
#define INORTH 27       /* undocumented              */
#define IEAST  28       /* undocumented              */
#define IHORZA 29       /* undocumented              */
#define IDOWN  30       /* undocumented              */
#define IUP    31       /* undocumented              */
#define ILLLBB 32       /* undocumented              */
#define IWWSN1 33       /* undocumented              */
#define IWWSN2 34       /* undocumented              */
#define IHGLP  35       /* undocumented              */
#define ISRO   36       /* undocumented              */
#define INUCL  37       /* event type: nuclear shot  */
#define IPREN  38       /* event type: nuke pre-shot */
#define IPOSTN 39       /* event type: nuke post-shot*/
#define IQUAKE 40       /* event type: earthquake    */
#define IPREQ  41       /* event type: foreshock     */
#define IPOSTQ 42       /* event type: aftershock    */
#define ICHEM  43       /* event type: chemical expl */
#define IOTHER 44       /* event type: other source  */
                        /* data quality: other problm*/
#define IGOOD  45       /* data quality: good        */
#define IGLCH  46       /* data quality: has glitches*/
#define IDROP  47       /* data quality: has dropouts*/
#define ILOWSN 48       /* data quality: low s/n     */
#define IRLDTA 49       /* data is real data         */
#define IVOLTS 50       /* file: velocity (volts)    */
#define INIV51 51       /* undocumented              */
#define INIV52 52       /* undocumented              */
#define INIV53 53       /* undocumented              */
#define INIV54 54       /* undocumented              */
#define INIV55 55       /* undocumented              */
#define INIV56 56       /* undocumented              */
#define INIV57 57       /* undocumented              */
#define INIV58 58       /* undocumented              */
#define INIV59 59       /* undocumented              */
#define INIV60 60       /* undocumented              */

/* Format strings for writing headers of SAC ASCII files */

#define SACWFCS "%15.7f%15.7f%15.7f%15.7f%15.7f\n"  /* for floats  */
#define SACWICS "%10ld%10ld%10ld%10ld%10ld\n"       /* for longs   */
#define SACWCCS1 "%-8.8s%-8.8s%-8.8s\n"             /* for strings */
#define SACWCCS2 "%-8.8s%-16.16s\n"                 /* for strings */

/* Format strings for reading headers of SAC ASCII files */

#define SACRFCS "%f %f %f %f %f"        /* for floats  */
#define SACRICS "%ld %ld %ld %ld %ld"   /* for longs   */
#define SACRCCS1 "%8s%8s%8s"            /* for strings */
#define SACRCCS2 "%8s%16s"              /* for strings */

/*  sacio library function prototypes  */

#ifdef __STDC__

double sacio_sttodt(struct sac_header *);
int sacio_rah(FILE *, struct sac_header *);
int sacio_rbh(FILE *, struct sac_header *);
int sacio_wad(FILE *, float *, long, long *);
int sacio_wah(FILE *, struct sac_header *);
int sacio_wbh(FILE *, struct sac_header *);

#else

double sacio_sttodt();
int sacio_rah();
int sacio_rbh();
int sacio_wad();
int sacio_wah();
int sacio_wbh();

#endif /* ifdef __STDC__ */

#endif
