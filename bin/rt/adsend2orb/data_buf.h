#ifndef DATA_BUF_H
#define DATA_BUF_H
/*
File DATA_BUF.H

EARTHWORM PROJECT
2/9/93
5/95 modified to include pin numbers

Header file containing Waveform Data Buffer Format

Background:

In earthworm, there are global parameter files which all modules
will have access to.  The Data Source parameter file will contain the
                channel-station name/component association,
                skew,
                data sample format code,
                and other relevant parameters of the data source.


Waveform Data Buffer Header

Time Stamp                              Two 4 byte words - first=the number
                                        of seconds since some specified date.
                                        second=the no. of microseconds past
                                        the last second.
Scan Count of first scan of buffer      4 bytes
Series                                  2 bytes -  incremented each time
                                        a data source is started.
Digitization Rate                       Two 2 byte integers -
                                        first integer = length of standard
                                        interval in seconds
                                        second integer = the number of samples
                                        per interval.
Data Source Module ID                   byte
Error Byte                              byte
No of Channels                          2 bytes
No of scans per buffer                  2 bytes
Network Identifier                      2 bytes


Header is followed by nchan pin numbers, expressed as two-byte integers.
The waveform data, in multiplexed format, follows the pin numbers.
*/

/* This defines the message type
  ******************************/
#define MSG_TYPE_TRACE_DATA 1

typedef struct {
   long           tssec;       /* Time stamp - seconds */
   long           tsmic;       /* Time stamp - microsecs past the last sec */
   unsigned long  first_scan;  /* Number of first scan in buffer */
   short          series;      /* Incremented each time data source starts */
   short          sample_dt;   /* Length of standard interval in seconds */
   short          nsample;     /* Number of samples per this interval */
   char           mod_id;      /* Data source module id */
   char           padding;
   unsigned short errors;      /* Error word */
   short          nchan;       /* Number of channels */
   short          nscan;       /* Number of scans per buffer */
   short          netid;       /* Network identifier */
} WF_HEADER;

/* Below are bits to be set in the error word
   ******************************************/
#define AD_TIME_BAD    1
#define AD_GUIDE1_BAD  2
#define AD_GUIDE2_BAD  4
#define AD_GUIDE3_BAD  8
#define AD_GUIDE4_BAD  16
#define AD_DKWRERR     32
#define AD_NI_ERR      64
#define AD_NEW_YEAR    128

#endif
