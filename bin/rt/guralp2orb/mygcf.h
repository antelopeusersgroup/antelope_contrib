#define MOTOROLA_BYTEORDER 1
#define INTEL_BYTEORDER 2

#define MAX24 0x007FFFFF
#define MIN24 ( - MAX24 )

#define MAX_NUM_RECORDS 250

typedef unsigned long DWORD;
typedef unsigned short WORD;
typedef unsigned char BYTE;

typedef struct {
	DWORD sysid;              /* 6 character system Identifier string.
				     Coded as a base 36 number.
				     Constant for each digitier */
	DWORD streamid;           /* 6 character stream Identifier string. 	
				     Coded as a base 36 number. */
	DWORD datecode;           /* Timestamp of start of data in this block.
				     Blocks contain whole seconds of data. */

	BYTE reserved1;
	BYTE samplerate;          /* Sample Rate of data in block */
	BYTE compressioncode;     /* Indicated size of differences in
				     block (1=32 bit, 2=16 bit, 4=8 bit) */
	BYTE numrecords;          /* Number of 4 byte 'records' of data
				     following (not counting fic and ric) */

	union {
		char chars[1008];       /* If a status block (samplerate=0),
					   this is text information */
		struct {
			long fic;             /* Forward integration Constant */
			union {
				long D[250];    /* 4 byte difference data
							if compressioncode=1 */
				short W[500];   /* 2 byte difference data
							if compressioncode=2 */
				char B[1000];   /* 1 byte difference data
							if compressioncode=4 */
	      		} data;
        		long ric;             /* Reverse intergration Constant. 
						FIC+all differences should
						equal this. */
		} wf;
	} c; /* Contents */

} TGCFblock;

typedef struct {
	TGCFblock block;                /* The standard GCF block */
	char ver;                       /* Version of SCREAM */
	char source[33];
	WORD blockseq;                  /* Added V1.8 */
	BYTE byteorder;                 /* Added V1.8: 1=Motorola, 2=Intel */
	BYTE reserved[253];
} TMessageBlock;
 
typedef struct {
        char system_id[8];      /* digitizer id */
        char stream_id[8];      /* stream id */
        double epoch;           /* epoch time */
        long nsamp;         	/* number of samples in data, chars if sps==0 */
        long nrec;       	/* number of compressed records */
        long compressioncode;   /* 1, 2, or 4 */
        long samprate;       	/* in sps, 0 indicates SOH block in data */
        long reserved;
        long data[1000];        /* data array in native byte order */
} TGCFpacket;

double 	datecode2epoch( long );
char 	*decodebase36( DWORD, char *, int );
void 	swapGCFblock( TGCFblock * );
long 	add24bit( long, long );
long 	difference( TGCFblock *, int );
int 	decompressGCF_UDPmessage( char *, TGCFpacket * );
