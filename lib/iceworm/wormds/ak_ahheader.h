/*   structure for data file header for Lamont ah or ad hoc system
     witte, 11 June 85 
     headers and data for events are stored in same file typically
     named ah.YRMODYHRMN
*/
/* Alaskan version uses the non-XDR form here, and adds the two 
codes for SHORT and LONG integer data, 7 and 8.
'ak_' prefixes added 2/97 by KGL to avoid collisions in systems that 
support both flavors of AH */

#ifndef AK_AHHEADER_H
#define AK_AHHEADER_H

#define AK_AHHEADSIZE 1024         /* Size of an Alaskan AH file header */

#define TYPEMIN	  1
#define TYPEMAX	  8
#define LOGSIZE	202
#define LOGENT 	 10
#define NEXTRAS  21
#define NOCALPTS 30

#define ak_ahFLOAT    1
#define ak_ahCOMPLEX  2
#define ak_ahVECTOR   3
#define ak_ahTENSOR   4
#define ak_ahDOUBLE   6
#define ak_ahSHORT    7	/* cds 09/18/89 */
#define ak_ahLONG     8	/* cds 09/18/89 */

typedef   struct   {
   float   x;
   float   y;
   } vector;

typedef   struct   {
   float   r;
   float   i;
   } complex;

typedef struct  {
   double r;
   double i;
   } d_complex;

typedef   struct   {
   float   xx;
   float   yy;
   float   xy;
   } tensor;

struct   calib   {
   complex   pole;        /* pole    */
   complex   zero;        /* zero    */
   };

struct   ak_station_info   {
   char      code[6];     /* station code      */
   char      chan[6];     /* lpz,spn, etc.     */
   char      stype[8];    /* wwssn,hglp,etc.   */
   float     slat;        /* station latitude  */
   float     slon;        /*    "    longitude */
   float     elev;        /*    "    elevation */
   float     DS;          /* maximum gain at peak of calibration curve */
   float     A0;          /* normalization, factor to make
			     calibration curve at peak equal 1 for
			     a specific frequency   */
   struct    calib  cal[NOCALPTS]; /* calibration info   */
   };
/* stores the time when an event occurs */
struct time {
	short   yr;                       /*year */
	short   mo;                       /* mounth */
	short   day;                      /* day */
	short   hr;                       /* hour */
	short   mn;                       /* minute */
	float   sec;                      /* second */
};
struct   ak_event_info   {
   float    lat;          /* event latitude    */
   float    lon;          /*   "   longitude   */
   float    dep;          /*   "   depth       */
   struct   time   ot;    /*   "   origin time */
   char     ecomment[80]; /*   comment line    */
   };

struct   ak_record_info   {
   short   type;           /* data type ([0],1 = float
				7 = short)		   */
   long    ndata;          /* number of samples            */
   float   delta;          /* sampling interval            */
   float   maxamp;         /* maximum amplitude of record  */
   struct  time  abstime;  /* start time of record section */
   float   rmin;           /* minimum value of abscissa    */
   char    rcomment[80];   /* comment line                 */
   char    log[LOGSIZE];   /* log of data manipulations    */
   };

 typedef struct ak_ahhed {
   struct   ak_station_info   station;  /* station info */
   struct   ak_event_info   event;      /* event info   */
   struct   ak_record_info   record;    /* record info  */
   float    extra[NEXTRAS];          /* freebies: 
					[0] = pin (channel) number - (cds 09/18/89)
					[1] = removed bias - (cds 09/18/89)
					[2] = mv/unit - (cds 09/18/89)
					[9] = Telemetry time-delay that has already
					      been subtracted from start time (jl 4/97)
				     */
   } ak_ahhed;

#endif
