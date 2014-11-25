#include <stdint.h>

/* Strict SEG-Y uses a 16-bit signed int for the number of samples in a trace
 * The Pavlis/IRIS-PASSCAL extensions use a custom field to extend this to
 * 32-bits */
#define SEGY_MAX_NSAMP 32767

/* This is the header for the segy trace header.  Reading bytes
  directly into this header will allow access to all of the fields.
*/
typedef struct SEGYTraceHeader {
  int32_t lineSeq;          /* 1-4 Trace sequence number within line - Numbers
                               continue to increase if the same line continues
                               across multiple SEG-Y files.
                               *Highly recommended for all types of data.* */
  int32_t reelSeq;          /* 5-8 Trace sequence number within SEG Y file or
                               reel.  Each file starts with trace sequence
                               one. */
  int32_t event_number;     /* 9-12 Original field record number */
  int32_t channel_number;   /* 13-16 Trace number within the original field
                               record.*/
  int32_t energySourcePt;   /* 17-20 Energy source point number */
  int32_t cdpEns;           /* 21-24 ensemble number (i.e. CDP,CMP,CRP,etc) */
  int32_t traceInEnsemble;  /* 25-28 Trace number within CDP ensemble */
  int16_t traceID;          /* 29-30 Trace identification code:
                               1 = seismic data
                               2 = dead
                               3 = dummy
                               4 = time break
                               5 = uphole
                               6 = sweep
                               7 = timing
                               8 = water break
                               9...N = optional use (rev 0)
                               rev 1 defines these additional codes:
                               9 = Near-field gun signature
                              10 = far-field gun signature
                              11 = Seismic pressure sensor
                              12 = Multicomponent seismic sensor - Vertical
                              13 = Multicomponent seismic sensor - Cross-line
                              14 = Multicomponent seismic sensor - In-line
                              15 = Rotated multicomponent seismic sensor - Vert
                              16 = Rotated multi. - Transverse
                              17 = Rotated multi. - Radial
                              18 = Vibrator reaction mass
                              19 = Vibrator baseplate
                              20 = Vibrator estimated ground force
                              21 = Vibrator reference
                              22 = Time-velocity pairs
                              23...N = optional use (maximum N = 32,767)
                              */
  int16_t vertSum;          /* 31-32 Number of vertically summed traces
                               this trace. (1 is one trace, 2 is two summed
                               traces, etc) */
  int16_t horSum;           /* 33-34 Number of horizontally stacked traces */
  int16_t dataUse;          /* 35-36 1 = production, 2 = test */
  int32_t sourceToRecDist;  /* 37-40 distance from center of source point to
                               the center of the receiver group */
  /* The scalar in Trace Header bytes 69-70 applies to the following values
   * from byte 41 to 68. The units are feet or meters as specified in Binary
   * File Header bytes 3255-3256. The vertical datum should be defined through
   * a Location Data stanza (for SEG-Y REV1). */
  int32_t recElevation;     /* 41-44 receiver group elevation */
  int32_t sourceSurfaceElevation; /* 45-48 Surface elevation at source. */
  int32_t sourceDepth;      /* 49-52 Source depth below surface (n>0) */
  int32_t datumElevRec;     /* 53-56 Datum elevation at receiver group. */
  int32_t datumElemSource;  /* 57-60 Datum elevation at source. */
  int32_t sourceWaterDepth; /* 61-64 Water depth at source */
  int32_t recWaterDepth;    /* 65-68 Water depth at group. */
  int16_t elevationScale;   /* 69-70 Scalar to be applied to all elevations and
                               depths specified in TH bytes 41-68. If positive,
                               scalar is a multiplier; if negative, scalar is a
                               divisor */
  int16_t coordScale;       /* 71-72 Scaler to be applied to all coordinates in
                               TH bytes 73-88. */
  int32_t sourceLongOrX;    /* 73-76 Source coordinate - X */
  int32_t sourceLatOrY;     /* 77-80 Source coordinate - Y */
  int32_t recLongOrX;       /* 81-84 Receiver group coordinate - X */
  int32_t recLatOrY;        /* 85-88 Receiver group coordinate - Y */
  int16_t coordUnits;       /* 89-90 Coordinate units:
                               1 = Length (meters or feet)
                               2 = Seconds of arc
                               3 = Decimal Degrees
                               4 = Degrees, Minutes, Seconds (DMS) */
  int16_t weatheringVelocity; /* 91-92 Weathering velocity */
  int16_t subWeatheringVelocity; /* 93-94 Subweathering velocity */
  int16_t sourceUpholeTime; /* 95-96 Uphole time at source in ms */
  int16_t recUpholeTime;    /* 97-98 Uphole time at group in ms */
  int16_t sourceStaticCor;  /* 99-100 Source static correction in ms */
  int16_t recStaticCor;     /* 101-102 Group static correction in ms */
  int16_t totalStatic;      /* 103-104 Total static applied in ms */
  int16_t lagTimeA;         /* 105-106 Lag Time A */
  int16_t lagTimeB;         /* 107-108 Lag Time B */
  int16_t delay;            /* 109-110 Delay recording time */
  int16_t muteStart;        /* 111-112 mute time start in ms */
  int16_t muteEnd;          /* 113-114 mute time end in ms */
  int16_t sampleLength;     /* 115-116 Number of samples in this trace
                               *Highly Recommended for all types of data* */
  int16_t deltaSample;      /* 117-118 Sampling interval in microseconds.
                               *Highly Recommended for all types of data*  */
  int16_t gainType;         /* 119-120 Gain type of field instruments
                                1 = fixed, 2 = binary, 3 = floating, 4... opt.*/
  int16_t gainConst;        /* 121-122 Instrument gain constant (dB) */
  int16_t initialGain;      /* 123-124 Insturment early or initial gain (dB) */
  int16_t correlated;       /* 125-126 Correlated 1 = no, 2 = yes */
  int16_t sweepStart;       /* 127-128 sweep freq at start (Hz) */
  int16_t sweepEnd;         /* 129-130 sweep freq at end (Hz) */
  int16_t sweepLength;      /* 131-132 sweep length in milliseconds */
  int16_t sweepType;        /* 133-134 Sweep type:
                               1 = linear, 2 = parabolic, 3 = exponential,
                               4... opt. */
  int16_t sweepTaperAtStart;/* 135-136 sweep trace taper length at start (ms) */
  int16_t sweepTaperAtEnd;  /* 137-138 sweep trace taper length at end (ms) */
  int16_t taperType;        /* 139-140 taper type: 1 = liner, 2 = cos^2,
                               3 = other */
  int16_t aliasFreq;        /* 141-142 Alias filter freq (Hz) */
  int16_t aliasSlope;       /* 143-144 Alias filter slop (dB/octave) */
  int16_t notchFreq;        /* 145-146 Notch filter freq (Hz) */
  int16_t notchSlope;       /* 147-148 Notch filter freq (dB/octave) */
  int16_t lowCutFreq;       /* 149-150 */
  int16_t hiCutFreq;        /* 151-152 */
  int16_t lowCutSlope;      /* 153-154 */
  int16_t hiCutSlope;       /* 155-156 */
  int16_t year;             /* 157-158 */
  int16_t day;              /* 159-160 */
  int16_t hour;             /* 161-162 */
  int16_t minute;           /* 163-164 */
  int16_t second;           /* 165-166 */
  int16_t timeBasisCode;    /* 167-168 Time basis code: 1 = Local; 2 = GMT;
                               3 = Other; 4 = UTC Note: "classic" SEGY defines
                               only 1,2, and 3. UTC support added in REV1 */
  int16_t traceWeightingFactor; /* 169-170 */
  int16_t phoneRollPos1;    /* 171-172 */
  int16_t phoneFirstTrace;  /* 173-174 */
  int16_t phoneLastTrace;   /* 175-176 */
  int16_t gapSize;          /* 177-178 */
  int16_t taperOvertravel;  /* 179-180 */
  /* Begin Pavlis/IRIS-PASSCAL non-standard extensions to SEG-Y Rev 0 */
  int16_t extrash[10];      /* 181-200 */
  int32_t samp_rate;        /* 201-204 Gary Pavlis non-standard extension */
  int16_t data_form;
  int16_t m_secs;
  int16_t trigyear;
  int16_t trigday;
  int16_t trighour;
  int16_t trigminute;
  int16_t trigsecond;
  int16_t trigmills;
  float scale_fac;
  int16_t inst_no;
  int16_t not_to_be_used;
  int32_t num_samps;
  char extra[8];
  /* End Pavlis/IRIS-PASSCAL non-standard extensions */
} SEGYTraceHeader; /* end of segy trace header */
#define SEGY_TRACE_HEADER_SIZE 240

/* Values for the traceID in the Trace Header Blocks (bytes 29-30)
 * Note: this is not an exhaustive list, it just covers some commonly used
 * types from the 1975 standard. Rev 1 defines additional values from 9-22. */
#define SEGY_TRACE_ID_OTHER htons(-1)
#define SEGY_TRACE_ID_UNKNOWN htons(0)
#define SEGY_TRACE_ID_SEISMIC htons(1)
#define SEGY_TRACE_ID_DEAD htons(2)
#define SEGY_TRACE_ID_DUMMY htons(3)
#define SEGY_TRACE_ID_TIME_BREAK htons(4)
#define SEGY_TRACE_ID_UPHOLE htons(5)
#define SEGY_TRACE_ID_SWEEP htons(6)
#define SEGY_TRACE_ID_TIMING htons(7)
#define SEGY_TRACE_ID_WATER_BREAK htons(8)

/* Values for the coordUnits in the Trace Header Blocks (bytes 89-90) */
#define SEGY_TRACE_COORDUNITS_LENGTH htons(1)
#define SEGY_TRACE_COORDUNITS_ARCSECONDS htons(2)
#define SEGY_TRACE_COORDUNITS_DECIMAL_DEGREES htons(3) /* rev1 or greater */
#define SEGY_TRACE_COORDUNITS_DMS htons(4)             /* rev1 or greater */

/* Values for the timeBasisCode in the Trace Header Blocks (bytes 167-168) */
#define SEGY_TRACE_TIMEBASIS_UNKNOWN htons(0)
#define SEGY_TRACE_TIMEBASIS_LOCAL htons(1)
#define SEGY_TRACE_TIMEBASIS_GMT htons(2)
#define SEGY_TRACE_TIMEBASIS_OTHER htons(3)
#define SEGY_TRACE_TIMEBASIS_UTC htons(4)

/* Values for the Gain type in the trace header (bytes 119-120) */
#define SEGY_TRACE_GAIN_UNKNOWN htons(0)
#define SEGY_TRACE_GAIN_FIXED htons(1)
#define SEGY_TRACE_GAIN_BINARY htons(2)
#define SEGY_TRACE_GAIN_FP htons(3)

/* The 400-byte Binary File Header from the SEG-Y rev 1 spec
 * This header starts at byte 3201 and continues to byte 3600
 *
 * Populating all of the fields should be OK in a SEG-Y rev 0 reader */
typedef struct SEGYBinaryFileHeader{
    int32_t kjob;   /* 3201-3204 Job identification number */
    int32_t kline;  /* 3205-3208 Line number. For 3-D poststack data,
                       this will typically contain the in-line number */
    int32_t kreel;  /* 3209-3212 Reel number */
    int16_t kntr;   /* 3213-3214 Number of traces per ensemble. *Mandatory for
                       prestack data* */
    int16_t knaux;  /* 3215-3216 Number of auxiliary traces per ensemble.
                       *Mandatory for prestack data* */
    int16_t sr;     /* 3217-3218 Sample interval in microseconds (mu-s).
                       *Mandatory for all data types */
    int16_t kfldsr; /* 3219-3220 Sample interval in microseconds of original
                       field recording. */
    int16_t knsamp; /* 3221-3222 Number of samples per data trace.
                       *Mandatory for all types of data.*
                       Note: the sample interval and number of samples in the
                       Binary File Header shuld be for the primary set of
                       seismic data traces in the file. */
    int16_t kfsamp; /* 3223-3224 Number of samples per data trace for the
                       original field recording */
    int16_t dsfc;   /* 3225-3226 Data sample format code.
                       *Mandatory for all data.*
                       1 = 4-byte IBM floating-point
                       2 = 4-byte, two's complement integer
                       3 = 2-byte, two's complement integer
                       4 = 4-byte fixed-point with gain (obsolete)
                       5 = 4-byte IEEE floating-point
                       6 = Not currently used
                       7 = Not currently used
                       8 = 1-byte, two's complement integer */
    int16_t kmfold; /* 3227-3228 Ensemble fold - The expected number of data
                       traces per trace ensemble (e.g. the CMP fold).
                       *Highly recommended for all types of data* */
    int16_t ksort;  /* 3229-3230 Trace sorting code (i.e. type of ensemble):
                       -1 = Other (shuld be explained in user Extended Textual
                            File Header stanza (rev 1)
                       0 = Unknown
                       1 = As recorded (no sorting)
                       2 = CDP ensemble
                       3 = Single fold continuous profile
                       4 = Horizontally stacked
                       5 = Common source point
                       6 = Common receiver point
                       7 = Common offset point
                       8 = Common mid-point
                       9 = Common conversion point

                       *Highly recommended for all types of data.*
                       */
    /*********
     * 3231-3254 rev0unused1 - 24 bytes were reserved for future use in SEG-Y
     * rev 0 */
    // unsigned char rev0unused1[24];

    /* 3231-3232 Vertical sum code:
     * 1: no sum,
     * 2: two sum,
     * ...,
     * N = M - 1 sum (M = 2 to 32,767 */
    int16_t vertical_sum_code;
    /* 3233-3234 Sweep frequency at start (Hz). */
    int16_t sweep_frequency_at_start;
    /* 3235-3236 Sweep frequency at end (Hz). */
    int16_t sweep_frequency_at_end;
    /* 3237-3238 Sweep length (ms). */
    int16_t sweep_length_in_ms;
    /* 3239-3240 Sweep type code:
     * 1 = linear
     * 2 = parabolic
     * 3 = exponential
     * 4 = other */
    int16_t sweep_type;
    /* 3241-3242 Trace number of sweep channel. */
    int16_t sweep_trace_num;
    /* 3243-3244 Sweep trace taper length in milliseconds at start if tapered
     * (the taper starts at zero time and is effective for this length). */
    int16_t sweep_trace_taper_length_at_start_in_ms;
    /* 3245-3246 Sweep trace taper length in milliseconds at end (the ending
     * taper starts at sweep length minus the taper length at end). */
    int16_t sweep_trace_taper_length_at_end_in_ms;
    /* 3247-3248 Taper type:
     * 1 = linear
     * 2 = cos^2
     * 3 = other */
    int16_t taper_type;
    /* 3249-3250 Correlated data traces:
     * 1 = no
     * 2 = yes */
    int16_t correlated;
    /* 3251-3252 Binary Gain Recovered:
     * 1 = yes
     * 2 = no */
    int16_t binary_gain_recovered;
    /* 3253-3254 Amplitude recovery method:
     * 1 = none
     * 2 = spherical divergence
     * 3 = AGC
     * 4 = other */
    int16_t amplitude_recovery_method;

    /* End SEG-Y rev0unused1
     **********/

    int16_t kunits; /* 3255-3256 Measurement system.
                       *Highly recommended for all types of data.* If Location
                       Data stanzas (rev 1) are included in the file, this entry
                       must agree with the Location Data stanza. If there is a
                       disagreement, the last Location Data stanza is the
                       controlling authority.
                       1 = Meters
                       2 = Feet */
    /********
     * 3257-3600 rev0unused2 - 344 bytes were reserved for future use in SEG-Y
     * rev 0 */
    // unsigned char rev0unused2[344];

    /* 3257-3258 Impulse signal polarity
     * 1 = increase in pressure or upward geophone case movment gives negative
     * number on tape
     * 2 = increate in pressure or upward geophone case movement gives positive
     * number on tape */
    int16_t impulse_signal_polarity;
    /* 3259-3260 Vibratory polarity code:
     * Seismic signal lags pilot signal by (ranges in degrees):
     * 1 = 337.5 to  22.5
     * 2 =  22.5 to  67.5
     * 3 =  67.5 to 112.5
     * 4 = 112.5 to 157.5
     * 5 = 157.5 to 202.5
     * 6 = 202.5 to 247.5
     * 7 = 247.5 to 292.5
     * 8 = 292.5 to 337.5 */
    int16_t vibratory_polarity;
    /* 3261-3500 SEG-Y rev 1 unassigned */
    unsigned char rev1unused1[240];
    /* 3501-3502 SEG-Y Format Revision Number. This is a 16-bit unsigned value
     * with a Q-point between the first and second bytes. Thus for SEG-Y
     * revision 1.0, this will be recorded as * 0100 (base 16).
     * *This field is mandatory for all versions of SEG Y, although a value of
     * zero indicates "traditional" SEG-Y conforming to the 1975 standard.* */
    int16_t segy_format;
    /* 3503-3504 Fixed trace length flag. A value of one indicates that all
     * traces in this SEG-Y file are guaranteed to have the same sample
     * interval and number of samples, as specified in Textual File Header
     * bytes 3217-3218 and 3221-3222. A value of zero indicates that the length
     * of the traces in the file may vary and the number of samples in bytes
     * 115-116 of the Trace Header must be examined to determine the actual
     * length of each trace.
     * *This field is mandatory for all versions of SEG-Y, although a value of
     * zero indicated "traditional" SEG Y conforoming to the 1975 standard* */
    int16_t fixed_length_trace_flag;
    /* 3505-3506 Number of 3200-byte Extended Textual File HEader records
     * following the Binary Header. A value of zero indicates there are no
     * Extended Textual File Header records (i.e. this file has no Extended
     * Textual File Header(s)). A value of -1 indicates that there are a
     * variable number of Extended Textual File Header records and the end of
     * the Extended Textual FIle Header is denoted by an ((SEG: EndText))
     * standza in the final record. A positive value indicates that there are
     * exactly that many Extended Textual FIle Header records. Note that,
     * although the exact number of Extended Textual File Header records may be
     * a useful piece of information, it will not always be known at the time
     * the Binary Header is written and it is not mandatory that a positive
     * value be recorded here.
     * *This field is mandatory for all versions of SEG Y, although a value of
     * zero indicates "traditional" SEG-Y conforming to the 1975 standard.* */
    int16_t etxt_count;
    /* 3507-3600 SEG-Y rev 1 unassigned */
    unsigned char rev1unused2[94];

    /* End SEG-Y rev0unused2
     ********/
} SEGYBinaryFileHeader;
#define SEGY_BINARY_HEADER_SIZE 400

#define SEGY_FORMAT_SU -1
#define SEGY_FORMAT_REV_0 htons(0x0000)
#define SEGY_FORMAT_REV_1_0 htons(0x0100)

/* Fixed length trace flag */
#define SEGY_TRLEN_FIXED htons(1)
#define SEGY_TRLEN_VARIABLE htons(0)


#define SEGY_TEXT_HEADER_RECORDS 40
#define SEGY_TEXT_HEADER_COLUMNS 80
#define SEGY_TEXT_HEADER_SIZE 3200 //RECORDS * COLUMNS
#define SEGY_TEXT_HEADER_USABLE_COLUMNS 76

static const unsigned char a2e[256] = {
      0,  1,  2,  3, 55, 45, 46, 47, 22,  5, 37, 11, 12, 13, 14, 15,
     16, 17, 18, 19, 60, 61, 50, 38, 24, 25, 63, 39, 28, 29, 30, 31,
     64, 79,127,123, 91,108, 80,125, 77, 93, 92, 78,107, 96, 75, 97,
    240,241,242,243,244,245,246,247,248,249,122, 94, 76,126,110,111,
    124,193,194,195,196,197,198,199,200,201,209,210,211,212,213,214,
    215,216,217,226,227,228,229,230,231,232,233, 74,224, 90, 95,109,
    121,129,130,131,132,133,134,135,136,137,145,146,147,148,149,150,
    151,152,153,162,163,164,165,166,167,168,169,192,106,208,161,  7,
     32, 33, 34, 35, 36, 21,  6, 23, 40, 41, 42, 43, 44,  9, 10, 27,
     48, 49, 26, 51, 52, 53, 54,  8, 56, 57, 58, 59,  4, 20, 62,225,
     65, 66, 67, 68, 69, 70, 71, 72, 73, 81, 82, 83, 84, 85, 86, 87,
     88, 89, 98, 99,100,101,102,103,104,105,112,113,114,115,116,117,
    118,119,120,128,138,139,140,141,142,143,144,154,155,156,157,158,
    159,160,170,171,172,173,174,175,176,177,178,179,180,181,182,183,
    184,185,186,187,188,189,190,191,202,203,204,205,206,207,218,219,
    220,221,222,223,234,235,236,237,238,239,250,251,252,253,254,255
};
static const unsigned char e2a[256] = {
      0,  1,  2,  3,156,  9,134,127,151,141,142, 11, 12, 13, 14, 15,
     16, 17, 18, 19,157,133,  8,135, 24, 25,146,143, 28, 29, 30, 31,
    128,129,130,131,132, 10, 23, 27,136,137,138,139,140,  5,  6,  7,
    144,145, 22,147,148,149,150,  4,152,153,154,155, 20, 21,158, 26,
     32,160,161,162,163,164,165,166,167,168, 91, 46, 60, 40, 43, 33,
     38,169,170,171,172,173,174,175,176,177, 93, 36, 42, 41, 59, 94,
     45, 47,178,179,180,181,182,183,184,185,124, 44, 37, 95, 62, 63,
    186,187,188,189,190,191,192,193,194, 96, 58, 35, 64, 39, 61, 34,
    195, 97, 98, 99,100,101,102,103,104,105,196,197,198,199,200,201,
    202,106,107,108,109,110,111,112,113,114,203,204,205,206,207,208,
    209,126,115,116,117,118,119,120,121,122,210,211,212,213,214,215,
    216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,
    123, 65, 66, 67, 68, 69, 70, 71, 72, 73,232,233,234,235,236,237,
    125, 74, 75, 76, 77, 78, 79, 80, 81, 82,238,239,240,241,242,243,
     92,159, 83, 84, 85, 86, 87, 88, 89, 90,244,245,246,247,248,249,
     48, 49, 50, 51, 52, 53, 54, 55, 56, 57,250,251,252,253,254,255
};
