/*  Field definitions used in SEED data record headers.		*/
/*	@(#)sdr.h	1.4 5/24/96 15:44:19	*/

#ifndef	__sdr_h
#define __sdr_h

#define	ACTIVITY_CALIB_PRESENT	0x1
#define	ACTIVITY_TIME_GAP	0x2
#define	ACTIVITY_BEGINNING_OF_EVENT	0x4
#define	ACTIVITY_END_OF_EVENT	0x8
#define	ACTIVITY_POS_LEAP_SECOND    0x10
#define	ACTIVITY_NEG_LEAP_SECOND    0x20
#define	ACTIVITY_EVENT_IN_PROGRESS  0x40

#define	IO_PARITY_ERROR		0x2
#define	IO_LONG_RECORD		0x2
#define	IO_SHORT_RECORD		0x4

#define	QUALITY_SATURATION	0x1
#define	QUALITY_CLIPPING	0x2
#define	QUALITY_SPIKES		0x4
#define	QUALITY_GLITHES		0x8
#define	QUALITY_MISSING		0x10
#define	QUALITY_TELEMETRY_ERROR	0x20
#define	QUALITY_FILTER_CHARGE	0x40
#define	QUALITY_QUESTIONABLE_TIMETAG	0x80

typedef	char		BYTE;	/* signed byte			*/
typedef	unsigned char	UBYTE;	/* unsigned byte		*/
typedef	short		WORD;	/* 16 bit signed		*/
typedef	unsigned short	UWORD;	/* 16 bit unsigned		*/
typedef	int		LONG;	/* 32 bit signed		*/
typedef	unsigned int	ULONG;	/* 32 bit unsigned		*/
typedef	char		CHAR;	/* 7 bit character, high bit 0	*/
typedef unsigned char	UCHAR;	/* 8 bit character, unsigned.	*/
typedef signed char	SCHAR;	/* 8 bit character, signed.	*/
typedef	float		FLOAT;	/* IEEE floating point		*/

typedef struct _sdr_time {
    UWORD	year;
    UWORD	day;
    UBYTE	hour;
    UBYTE	minute;
    UBYTE	second;
    UBYTE	pad;
    UWORD	ticks;
} SDR_TIME;

#define	SDR_SEQ_LEN		6
#define	SDR_STATION_LEN		5
#define	SDR_LOCATION_LEN	2
#define	SDR_CHANNEL_LEN		3
#define	SDR_NETWORK_LEN		2

/*  Fixed seed header	*/
typedef struct _sdr_hdr {			/* byte offset  */
    CHAR	seq_no[SDR_SEQ_LEN];		/*	0   */
    CHAR	data_hdr_ind;			/*	6   */
    CHAR	space_1;			/*	7   */
    CHAR	station_id[SDR_STATION_LEN];	/*	8   */
    CHAR	location_id[SDR_LOCATION_LEN];	/*	13  */
    CHAR	channel_id[SDR_CHANNEL_LEN];	/*	15  */
    CHAR	network_id[SDR_NETWORK_LEN];	/*	18  */
    SDR_TIME	time;				/*	20  */
    UWORD	num_samples;			/*	30  */
    WORD	sample_rate_factor;		/*	32  */
    WORD	sample_rate_mult;		/*	34  */
    UBYTE	activity_flags;			/*	36  */
    UBYTE	io_flags;			/*	37  */
    UBYTE	data_quality_flags;		/*	38  */
    UBYTE	num_blockettes;			/*	39  */
    LONG	num_ticks_correction;		/*	40  */
    UWORD	first_data;			/*	44  */
    UWORD	first_blockette;		/*	46  */
} SDR_HDR;

typedef struct _blockette_hdr {		/*  Common binary blockette hdr.*/
    UWORD	type;			/*  binary blockette number.	*/
    UWORD	next;			/*  byte offset from sdr to next*/
} BLOCKETTE_HDR;			/*  blockette for this record.	*/

typedef struct _blockette_100 {		/*  Sample Rate Blockette.	*/
    BLOCKETTE_HDR   hdr;		/*  binary blockette header.	*/
    FLOAT	actual_rate;
    CHAR	flags;
    CHAR	reserved[3];
} BLOCKETTE_100;

/*  Quanterra Threshold Detector. Comments refer to Quanterra usage.	*/
typedef struct _blockette_200 {		/*  Generic Event Detection blockette.*/
    BLOCKETTE_HDR   hdr;		/*  binary blockette header.	*/
    FLOAT	signal_amplitude;	/*  Amp that caused detection.	*/
    FLOAT	signal_period;		/*  Not used by Quanterra.	*/
    FLOAT	background_estimate;	/*  Limit that was exceeded.	*/
    UBYTE	detection_flags;	/*  Not used by Quanterra.	*/
    UBYTE	reserved;		/*  Not used.			*/
    SDR_TIME	time;			/*  Onset time of detector.	*/
    /* Quanterra additions to SEED version 2.3 blockette.		*/
    CHAR	detector_name[24];	/*  Quanterra detector name.	*/
} BLOCKETTE_200;
#define	BLOCKETTE_200_STD_SIZE	(sizeof(BLOCKETTE_200)-24)

typedef struct _blockette_201 {		/*  Murdock Event Detection blockette.*/
    BLOCKETTE_HDR   hdr;		/*  binary blockette header.	*/
    FLOAT	signal_amplitude;	/*  Amplitude of signal (counts)*/
    FLOAT	signal_period;		/*  Period of signal in seconds.*/
    FLOAT	background_estimate;	/*  Background estimates (counts)*/
    UBYTE	detection_flags;	/*  bit 0: 1=dilitational,0=compression */
    UBYTE	reserved;		/*  Unused - set to 0.		*/
    SDR_TIME	time;			/*  Signal onset time.		*/
    UBYTE	signal_to_noise[6];	/*  sn ratios - only use first 5*/
    UBYTE	loopback_value;		/*  Loopback value (0, 1, or 2).*/
    UBYTE	pick_algorithm;		/*  Pick algorithm - (0 or 1).	*/
    /* Quanterra additions to SEED version 2.3 blockette.		*/
    CHAR	detector_name[24];	/*  Quanterra detector name.	*/
} BLOCKETTE_201;
#define	BLOCKETTE_201_STD_SIZE	(sizeof(BLOCKETTE_201)-24)

typedef struct _blockette_300 {		/*  Step Calibration blockette.	*/
    BLOCKETTE_HDR   hdr;		/*  binary blockette header.	*/
    SDR_TIME	time;
    UBYTE	num_step_calibrations;
    UBYTE	calibration_flags;
    ULONG	step_duration;
    ULONG	interval_duration;
    FLOAT	calibration_amplitude;
    CHAR	calibration_channel[3];
    UBYTE	reserved;
    /* Quanterra additions to SEED version 2.3 blockette.		*/
    FLOAT	reference_amplitude;
    CHAR	coupling[12];
    CHAR	rolloff[12];
} BLOCKETTE_300;
#define	BLOCKETTE_300_STD_SIZE	(sizeof(BLOCKETTE_300)-8)

typedef struct _blockette_310 {		/*  Sine Calibration Blockette.	*/
    BLOCKETTE_HDR   hdr;		/*  binary blockette header.	*/
    SDR_TIME	time;    
    UBYTE	reserved_1;
    UBYTE	calibration_flags;
    ULONG	calibration_duration;
    FLOAT	calibration_period;
    FLOAT	calibration_amplitude;
    CHAR	calibration_channel[3];
    UBYTE	reserved;
    /* Quanterra additions to SEED version 2.3 blockette.		*/
    FLOAT	reference_amplitude;
    CHAR	coupling[12];
    CHAR	rolloff[12];
} BLOCKETTE_310;
#define	BLOCKETTE_310_STD_SIZE	(sizeof(BLOCKETTE_310)-8)

typedef struct _blockette_320 {		/*  Pseudo-random Calibration blockette.*/
    BLOCKETTE_HDR   hdr;		/*  binary blockette header.	*/
    SDR_TIME	time;    
    UBYTE	reserved_1;
    UBYTE	calibration_flags;
    FLOAT	calibration_duration;
    FLOAT	calibration_amplitude;
    CHAR	calibration_channel[3];
    UBYTE	reserved;
    /* Quanterra additions to SEED version 2.3 blockette.		*/
    FLOAT	reference_amplitude;
    CHAR	coupling[12];
    CHAR	rolloff[12];
    CHAR	noise_type[8];
} BLOCKETTE_320;
#define	BLOCKETTE_320_STD_SIZE	(sizeof(BLOCKETTE_320)-8)

typedef struct _blockette_390 {		/*  Generic Calibration blockette*/
    BLOCKETTE_HDR   hdr;		/*  binary blockette header.	*/
    SDR_TIME	time;    
    UBYTE	reserved_1;
    UBYTE	calibration_flags;
    FLOAT	calibration_duration;
    FLOAT	calibration_amplitude;
    CHAR	calibration_channel[3];
    UBYTE	reserved;
} BLOCKETTE_390;

typedef struct _blockette_395 {		/*  Calibration Abort blockette.*/
    BLOCKETTE_HDR   hdr;		/*  binary blockette header.	*/
    SDR_TIME	time;    
    UWORD	reserved;
} BLOCKETTE_395;

typedef struct _blockette_400 {		/*  Beam blockette.		*/
    BLOCKETTE_HDR   hdr;		/*  binary blockette header.	*/
    FLOAT	azimuth;
    FLOAT	slowness;
    UWORD	config;
    UWORD	reserved;
} BLOCKETTE_400;

typedef struct _blockette_405 {		/*  Beam Delay blockette.	*/
    BLOCKETTE_HDR   hdr;		/*  binary blockette header.	*/
    UWORD	delay;
} BLOCKETTE_405;

typedef struct _blockette_500 {		/*  Timing blockette (Quanterra). */
    BLOCKETTE_HDR   hdr;		/*  binary blockette header.	*/
    FLOAT	vco_correction;		/*  VCO correction.		*/
    SDR_TIME	time;			/*  Time of timing exception.	*/
    SCHAR	usec99;			/*  time extension to microsec.	*/
    SCHAR	reception_quality;	/*  Clock Reception quality.	*/
    LONG	count;			/*  Count (for exception type).	*/
    CHAR	exception_type[16];	/*  Type of timing exception.	*/
    CHAR	clock_model[32];	/*  Type of clock in use.	*/
    CHAR	clock_status[128];	/*  Clock status string.	*/
} BLOCKETTE_500;

typedef struct _blockette_1000 {	/*  Data format blockette.	*/
    BLOCKETTE_HDR   hdr;		/*  binary blockette header.	*/
    CHAR	format;			/*  data format.		*/
    CHAR	word_order;		/*  word order.			*/
    CHAR	data_rec_len;		/*  record length in 2**n.	*/
    CHAR	reserved;		/*  unused.			*/
} BLOCKETTE_1000;

typedef struct _blockette_1001 {	/*  Data extention blockette.	*/
    BLOCKETTE_HDR   hdr;		/*  binary blockette header.	*/
    SCHAR	clock_quality;		/*  clock quality.		*/
    SCHAR	usec99;			/*  time extension to microsec.	*/
    UCHAR	reserved;		/*  reserved.			*/
    SCHAR	frame_count;		/*  # of 64-byte steim frames.	*/
} BLOCKETTE_1001;

typedef struct _blockette_unknown {	/*  Unknown blockette.		*/
    BLOCKETTE_HDR   hdr;		/*  binary blockette header.	*/
    CHAR	body[128];		/*  body, suitably large.	*/
} BLOCKETTE_UNKNOWN;

#endif
