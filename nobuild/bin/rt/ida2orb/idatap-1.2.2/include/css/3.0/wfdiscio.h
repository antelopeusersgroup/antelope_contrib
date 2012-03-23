/* @(#)wfdiscio.h	1.1 4/4/96 */
#ifndef CSS_WFDISCIO_H
#define CSS_WFDISCIO_H

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

struct wfdisc {
/*  1 */   char   sta[7];
/*  2 */   char   chan[9];
/*  3 */   double time;
/*  4 */   long   wfid;
/*  5 */   long   chanid;
/*  6 */   long   jdate;
/*  7 */   double endtime;
/*  8 */   long   nsamp;
/*  9 */   float  smprate;
/* 10 */   float  calib;
/* 11 */   float  calper;
/* 12 */   char   instype[7];
/* 13 */   char   segtype;
/* 14 */   char   datatype[3];
/* 15 */   char   clip;
/* 16 */   char   dir[65];
/* 17 */   char   dfile[33];
/* 18 */   long   foff;
/* 19 */   long   commid;
/* 20 */   char   lddate[18];
}; 

#define WFDISC_NFIELD 20

static struct wfdisc wfdisc_null = {
/*  1  sta      */ "-",
/*  2  chan     */ "-",
/*  3  time     */ -9999999999.999,
/*  4  wfid     */ -1,
/*  5  chanid   */ -1,
/*  6  jdate    */ -1,
/*  7  endtime  */ -9999999999.999,
/*  8  nsamp    */ -1,
/*  9  smprate  */ -1.0,
/* 10  calib    */  0.0,
/* 11  calper   */ -1.0,
/* 12  instype  */ "-",
/* 13  segtype  */ '-',
/* 14  datatype */ "-",
/* 15  clip     */ '-',
/* 16  dir      */ "-",
/* 17  dfile    */ "-",
/* 18  foff     */ -1,
/* 19  commid   */ -1,
/* 20  lddate   */ "-"
};

#define WFDISC_RCS \
"%-6.6s %-8.8s %17.5lf %8ld %8ld %8ld %17.5lf %8ld %11.7f %16.6f %16.6f %-6.6s %c %-2.2s %c %-64.64s %-32.32s %10ld %8ld %-17.17s"

#define WFDISC_SCS \
"%s %s %lf %ld %ld %ld %lf %ld %f %f %f %s %c %s %c %s %s %ld %ld %s"

#define WFDISC_SIZE 284

#define WFDISC_RVL(SP) \
(SP)->sta, (SP)->chan, &(SP)->time, &(SP)->wfid, &(SP)->chanid, \
&(SP)->jdate, &(SP)->endtime, &(SP)->nsamp, &(SP)->smprate, \
&(SP)->calib, &(SP)->calper, (SP)->instype, &(SP)->segtype, (SP)->datatype, \
&(SP)->clip, (SP)->dir, (SP)->dfile, &(SP)->foff, &(SP)->commid, \
(SP)->lddate

#define WFDISC_WCS \
"%-6.6s %-8.8s %17.5lf %8ld %8ld %8ld %17.5lf %8ld %11.7f %16.6f %16.6f %-6.6s %c %-2.2s %c %-64.64s %-32.32s %10ld %8ld %-17.17s\n"

#define WFDISC_WVL(SP) \
(SP)->sta, (SP)->chan, (SP)->time, (SP)->wfid, (SP)->chanid, \
(SP)->jdate, (SP)->endtime, (SP)->nsamp, (SP)->smprate, \
(SP)->calib, (SP)->calper, (SP)->instype, (SP)->segtype, (SP)->datatype, \
(SP)->clip, (SP)->dir, (SP)->dfile, (SP)->foff, (SP)->commid, \
(SP)->lddate

#define WFDISC_TRM(SP) \
TRIM((SP)->sta,7); TRIM((SP)->chan,9); TRIM((SP)->instype,7); \
TRIM((SP)->datatype,3); TRIM((SP)->dir,65); TRIM((SP)->dfile,33); \

#define WFDISC_FPD(SP) \
FPAD((SP)->sta,7); FPAD((SP)->chan,9); FPAD((SP)->instype,7); \
FPAD((SP)->datatype,3); FPAD((SP)->dir,65); FPAD((SP)->dfile,33); \

#define WFDISC_BFL(SP) \
BFIL((SP)->sta,7); BFIL((SP)->chan,9); BFIL((SP)->instype,7); \
BFIL((SP)->datatype,3); BFIL((SP)->dir,65); BFIL((SP)->dfile,33); \
BFIL((SP)->lddate,18);

#endif /* CSS_WFDISCIO_H */
