#

struct	net_time {
	ushort_t year;
	ushort_t day;
	uchar_t	hour;
	uchar_t	minute;
	uchar_t	seconds;
	uchar_t	dummy;
	ushort_t fracs;	
}; 

typedef struct lisspkt { 
	uchar_t		Seq_ID[6];		/* Sequence number of record */
	uchar_t		Record_Type;		/* Always a 'D' */
	uchar_t		Filler;			/* Always a space */

	uchar_t		Station_ID[5];		/* Station id (space filled) */
	uchar_t		Location_ID[2];		/* Array/Extended Station (filled) */
	uchar_t		Channel_ID[3];		/* Channel Id (space filled) */
	uchar_t		Network_ID[2];		/* Extended Network Type */

	struct net_time	Start_Time;		/* Start time of record */
	ushort_t 	Number_Samps;		/* Number of samples in record */

	short		Rate_Factor;		/* Sample rate factor */
	short		Rate_Mult;		/* Rate Multiplier */

	uchar_t		Activity_Flags;		/* Activity Information */

#define ACTFLAG_CALSIG 0x01		/* Calibration signals this record */
#define ACTFLAG_CLKFIX 0x02		/* Error caused by clock correction */
#define ACTFLAG_BEGEVT 0x04		/* Beginning of event */
#define ACTFLAG_ENDEVT 0x08		/* End of event */

	uchar_t		IO_Flags;		/* I/O Information */

#define IOFLAG_ORGPAR 0x01		/* Original tape had parity error */
#define IOFLAG_LONGRC 0x02		/* Original read long record */
#define IOFLAG_SHORTR 0x04		/* Original had short record */

	uchar_t		Qual_Flags;		/* Data Quality Information */

#define QULFLAG_AMPSAT 0x01		/* Amplifier saturation detected */
#define QULFLAG_SIGCLP 0x02		/* Signal clipping detected */
#define QULFLAG_SPIKES 0x04		/* Signal spiking detected */
#define QULFLAG_GLITCH 0x08		/* Signal glitch detected */
#define QULFLAG_PADDED 0x10		/* Data missing or padded */
#define QULFLAG_TLMSNC 0x20		/* Telemetry sync error/dropout */
#define QULFLAG_CHARGE 0x40		/* Digitial filter charging */
#define QULFLAG_TIMERR 0x80		/* Time tag questionable */

	uchar_t		Total_Blockettes;	/* Number blockettes to follow */
	int		Time_Correction;	/* Number of .0001 sec correction */
	ushort_t 	Data_Start;		/* Byte where data starts */
	ushort_t 	First_Blockette;	/* Byte of first blockette */
} LissPkt;


struct Data_only {
	ushort_t	Blockette_Type;		/* Blockette identifier */
	ushort_t	Next_Begin;		/* Byte where next blockette begins */

	uchar_t		Encoding;
	uchar_t   	Order;
	uchar_t   	Length;
	uchar_t   	Resv1;
};

