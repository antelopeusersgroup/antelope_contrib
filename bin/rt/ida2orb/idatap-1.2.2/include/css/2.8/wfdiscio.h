/* @(#)wfdiscio.h	1.1 4/4/96 */
#ifndef CSS_WFDISC28IO_H
#define CSS_WFDISC28IO_H

#ifndef TRIM
#define TRIM(s,l) {int i;for(i=l-1;i>0,s[i-1]==' ';i--);s[i]='\0';}
#endif
#ifndef FPAD
#define FPAD(s,l) {int i;for(i=strlen(s);i<l;s[i++]=' ');}
#endif
#ifndef BFIL
#define BFIL(s,l) {int i;for(i=0;i<l;s[i++]=' ');}
#endif

/*  structure for waveform index info  */

struct wfdisc28 {
/*  1 */   long   date;       /* 3.0 jdate    */
/*  2 */   double time;       /* 3.0 time     */
/*  3 */   char   sta[8];     /* 3.0 sta      */
/*  4 */   char   chan[9];    /* 3.0 chan     */
/*  5 */   long   nsamp;      /* 3.0 nsamp    */
/*  6 */   float  smprat;     /* 3.0 smprate  */
/*  7 */   float  calib;      /* 3.0 calib    */
/*  8 */   float  calper;     /* 3.0 calper   */
/*  9 */   char   instyp[8];  /* 3.0 instype  */
/* 10 */   char   segtyp;     /* 3.0 segtype  */
/* 11 */   char   dattyp[4];  /* 3.0 datatype */
/* 12 */   char   clip;       /* 3.0 clip     */
/* 13 */   long   chid;       /* 3.0 chanid   */
/* 14 */   long   wfid;       /* 3.0 wfid     */
/* 15 */   char   dir[32];    /* 3.0 dir      */
/* 16 */   char   file[24];   /* 3.0 dfile    */
/* 17 */   long   foff;       /* 3.0 foff     */
/* 18 */   long   adate;      /* N/A in 3.0   */
/* 19 */   char   remark[32]; /* N/A in 3.0   */
}; 

#define WFDISC28_NFIELD 19

static struct wfdisc28 wfdisc28_null = {
/*  1 date    */ -1,
/*  2 time    */ -9999999999.999,
/*  3 sta     */ "_",
/*  4 chan    */ "_",
/*  5 nsamp   */ -1,
/*  6 smprat  */ -1.,
/*  7 calib   */ 0.,
/*  8 calper  */ -1.,
/*  9 instyp  */ "_",
/* 10 segtyp  */ '_',
/* 11 dattyp  */ "_",
/* 12 clip    */ '_',
/* 13 chid    */ -1,
/* 14 wfid    */ -1,
/* 15 dir     */ "_",
/* 16 file    */ "_",
/* 17 foff    */ 0,
/* 18 adate   */ -1,
/* 19 remark  */ "_"
};

#define WFDISC28_RCS \
"%ld%lf%*c%6c%*c%2c%ld%f%f%f%*c%6c%*c%c%*c%2c%*c%c%ld%ld%*c/**/%30c%*c%20c%ld%ld%*c%30c"
/* following extra read string is a gift from Keith */
/* NF means Number of Fields, (just like in awk) */
#define WFDISC28_NF    19
/* SCS means Scanf Control String, used by archive tape processing programs */
#define WFDISC28_SCS \
"%ld %lf %s %s %ld %f %f %f %s %c %s %c %ld %ld %s %s %ld %ld%*c%30[^\n] \n"
/* xxx_SIZE is external file record size in bytes including newline */
#define WFDISC28_SIZE 209

#define WFDISC28_RVL(SP) \
&(SP)->date,&(SP)->time,(SP)->sta,(SP)->chan,&(SP)->nsamp,\
&(SP)->smprat,&(SP)->calib,&(SP)->calper,(SP)->instyp,&(SP)->segtyp,\
(SP)->dattyp,&(SP)->clip,&(SP)->chid,&(SP)->wfid,(SP)->dir,\
(SP)->file,&(SP)->foff,&(SP)->adate,(SP)->remark

#define WFDISC28_WCS \
"%8ld %15.3f %-6.6s %-2.2s %8ld %11.7f %9.6f %7.4f %-6.6s %c %-2.2s %c %8ld %8ld %-30.30s %-20.20s %10ld %8ld %-30.30s\n"

#define WFDISC28_WVL(SP) \
(SP)->date,(SP)->time,(SP)->sta,(SP)->chan,\
(SP)->nsamp,(SP)->smprat,(SP)->calib,(SP)->calper,\
(SP)->instyp,(SP)->dattyp,(SP)->chid,\
(SP)->wfid,(SP)->dir,(SP)->file,(SP)->foff,\
(SP)->adate,(SP)->remark

#define WFDISC28_TRM(SP) \
TRIM((SP)->sta,7); TRIM((SP)->chan,3); TRIM((SP)->instyp,7); \
TRIM((SP)->dattyp,3); \
TRIM((SP)->dir,31); TRIM((SP)->file,21); TRIM((SP)->remark,31)

#define WFDISC28_FPD(SP) \
FPAD((SP)->sta,8); FPAD((SP)->chan,4); FPAD((SP)->instyp,8); \
FPAD((SP)->dattyp,4); \
FPAD((SP)->dir,32); FPAD((SP)->file,24); FPAD((SP)->remark,32)

#define WFDISC28_BFL(SP) \
BFIL((SP)->sta,8); BFIL((SP)->chan,4); BFIL((SP)->instyp,8); \
BFIL((SP)->dattyp,4); \
BFIL((SP)->dir,32); BFIL((SP)->file,24); BFIL((SP)->remark,32)

#endif /* CSS_WFDISC28IO_H */
