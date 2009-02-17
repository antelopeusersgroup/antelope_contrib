/******************************************************************************
 *
 * dcbba2orb/dcbba2orb.h
 *
 * INclude file of constatns, macors, function declarations, etc.
 *
 *
 * Author: Geoff Davis
 * UCSD, IGPP
 * gadavis@ucsd.edu
 ******************************************************************************/

#ifndef dcbba2orb_h_included
#define dcbba2orb_h_included

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>

#include "Pkt.h"
#include "orb.h"
#include "forb.h"
#include "stock.h"
/*
 * Constants
 */
#define CONNECT_SERIAL          1	/* Not supported here */
#define CONNECT_TCP_IP          2
#define CONNECT_FILE			3
#define MAX_CONNECT_ATTEMPTS    10  /* Max tries to connect via serial/TCP */
#define MAX_DC_READ_DELAY			10   /* number of seconds to wait for
										an expected response before failing.
										it the response is sooner, we will exit
										before this time expires. */
#define DEFAULT_REPEAT_INTERVAL 3600
#define DEFAULT_SERIAL_SPEED    "19200"
#define DEFAULT_DC_DATA_PORT 	"5000"
#define DEFAULT_DC_CONTROL_PORT "5001"
#define DEFAULT_PARAM_FILE		"dcbba2orb.pf"
#define RESULT_SUCCESS          0
#define RESULT_FAILURE          -1
#define INVALID_HANDLE          -1
#define min(a, b)               (a < b ? a : b)
#define strsame(s1, s2)          (!strcmp ((s1), (s2)))
#define FALSE                   0
#define TRUE                    1
#define MAX_BUFFER_SIZE         5000
#define DATA_RD_BUF_SZ			1024
#define DEFAULT_BBA_PKT_BUF_SZ	16384

/*
 * BBA Packet Sync Character
 */
#define BBA_SYNC 				0xDA

/*
 * BBA Packet Types
 */
#define BBA_DAS_DATA			0xAB
#define BBA_DAS_STATUS			0xBC
#define BBA_DC_STATUS			0xCD
#define BBA_RTX_STATUS			0xDE

/*
 * BBA Packet Offsets
 */
#define BBA_CTRL_COUNT			16
#define BBA_CHKSUM_OFF 			(2)
#define BBA_PSIZE_OFF  			(4)
#define BBA_HSIZE_OFF  			(6)
#define BBA_STAID_OFF  			(10)
#define BBA_TIME_OFF   			(14)
#define BBA_NSAMP_OFF  			(18)
#define BBA_SRATE_OFF  			(20)
#define BBA_DTYPE_OFF  			(22)
#define BBA_NCHAN_OFF  			(23)

#define BBA_CHBYTES_OFF 		(2)
#define BBA_CHHDR_SIZE  		(4)

/*
 * Holds configuration data from command line and parameter file.
 */
struct stConfigData {
	int bVerboseModeFlag;			/* Print debug information */
	int iConnectionType;			/* Connection type - either CONNECT_TCP_IP or CONNECT_FILE */
	char *sNetworkName;				/* Network code - AZ, TA, etc */
	char *sOrbName;					/* Orbserver to output packets to */
	char *sDCConnectionParams [3];	/* Array to hold connection parameters for the Data concentrator */
	char *sDCControlPort;			/* Port number for the data concentrator's control port */
	char *sParamFileName;			/* Parameter File Name */
	char *sStateFileName;			/* State file name - Unused currently */
	Pf *oConfigPf;					/* Pf struct holding the configuration as read from sParamFileName */
	Tbl *oSiteTbl;					/* Tbl 'Site' as read from sParamFileName */
	Arr *oStaName;					/* List of station names by DAS/Station Id */
	Arr *oStaCh;					/* List of Site structs by PktType, SID, and SensorID */
	Arr *oDasID;					/* Arr 'Das_Stat' as read from sParamFileName */
	Arr *oDcID;						/* Arr 'DC_Stat' as read from sParamFileName */
	Arr *oRTXID;					/* Arr 'RTX_Stat' as read from sParamFileName */
	int iBBAPktBufSz;				/* How big to set the string buffer for reading in BBA packets */
};

struct stBBAPacketInfo {
	unsigned short usRawPktType;
	Srcname oSrcname;	/* Struct containing Net/Sta/Chan info */
	char sSrcname[500];	/* String to hold generated source name derived from oSrcname */
	double dPktTime;	/* Packet Timestamp */
	int iPktSize;		/* Packet Size in Bytes */
	int iStaID;			/* Station ID - needs to be translated to a string via the parameter file */
	int iNSamp;			/* Packet Size in Bytes */
	int iNChan;			/* Number of Channels */
	float fSrate;		/* Sample Rate */
	int iHdrSize;		/* Size of the header before the data points */
    char sDataType[4];	/* Data type of packet in orb header format. One of: s2, s4, t4, c0 */
    char sPktType[12];  /* Pkt type - UCSDDP,UCSDSP, UCSDCP,etc*/
    char sChNames[128]; /* Names of the channels, formatted for the orb header */
};

#define TRIM(s,l) {int i;for(i=l-1;i>0,s[i-1]==' ';i--);s[i]='\0';}

/* A row from the Site Table in pkt.pf */
struct stSiteEntry {
	char sDTYPE[12];		/* Packet type - CBBHS, CBBLS, etc */
	int iSID;				/* DAS/Station ID (StaID) */
	char sNAME[8];			/* Site name */
	int iCOMP;				/* Sensor ID, referred to as sensid in old _pkt2.h */
	char sSENS[12];			/* Sensor name */
	double dCALIB;			/* calibration coef */
};

/* Definition for reading&trimming&writing the structure  */

#define STE_RVL(SP)  \
(SP)->sDTYPE,&(SP)->iSID,(SP)->sNAME,&(SP)->iCOMP,(SP)->sSENS, &(SP)->dCALIB

#define STE_TRIM(SP) \
TRIM((SP)->sDTYPE,11);TRIM((SP)->sNAME,7); TRIM((SP)->sSENS,11)

#define STE_SCS " %s %d %s %d %s %lf[^\n] \n"

/* Client Packet headers . Do NOT move around structure fields! */
struct stBBAPreHdr {
    int16_t   hdrsiz;	   /* header size */
    int16_t   pktsiz;	   /* raw packet size */
    uint16_t  hdrtype;	   /* header type  */
    uint16_t  pkttype;	   /* packet type tag  */
};

#endif
