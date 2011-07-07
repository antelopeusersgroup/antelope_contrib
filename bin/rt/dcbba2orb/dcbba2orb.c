/*
 * Copyright (c) 2003-2006 The Regents of the University of California
 * All Rights Reserved
 *
 * Permission to use, copy, modify and distribute any part of this software for
 * educational, research and non-profit purposes, without fee, and without a
 * written agreement is hereby granted, provided that the above copyright
 * notice, this paragraph and the following three paragraphs appear in all
 * copies.
 *
 * Those desiring to incorporate this software into commercial products or use
 * for commercial purposes should contact the Technology Transfer Office,
 * University of California, San Diego, 9500 Gilman Drive, La Jolla, CA
 * 92093-0910, Ph: (858) 534-5815.
 *
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING
 * LOST PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE, EVEN IF THE UNIVERSITY
 * OF CALIFORNIA HAS BEEN ADIVSED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE SOFTWARE PROVIDED HEREIN IS ON AN "AS IS" BASIS, AND THE UNIVERSITY OF
 * CALIFORNIA HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
 * ENHANCEMENTS, OR MODIFICATIONS.  THE UNIVERSITY OF CALIFORNIA MAKES NO
 * REPRESENTATIONS AND EXTENDS NO WARRANTIES OF ANY KIND, EITHER IMPLIED OR
 * EXPRESS, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, OR THAT THE USE OF THE
 * SOFTWARE WILL NOT INFRINGE ANY PATENT, TRADEMARK OR OTHER RIGHTS.
 *
 *  This program is designed to interface with the IGPP Data Concentrator, which
 *  acquires and redistributes data from a number of RefTek data loggers.
 *
 *  NAMING CONVENTIONS
 *
 *    The following variable naming conventions are used throughout this code:
 *    xVarName, where "x" is one of the following:
 *
 *      [i]  Integer variable
 *      [c]  Character variable
 *      [s]  String variable
 *      [a]  Array variable
 *      [o]  Object/struct variable
 *      [us] unsigned short variable
 *      [st] Struct definition
 *      [d]	 double variable
 *      [b]  Boolean (psuedo) variable (use FALSE and TRUE constants defined
 *           in "dcbba2orb.h")
 *      [p]  Pointer variable for use while traversing arrays
 */

/*
 * Constants
 */
#define VERSION "dcbba2orb 1.1.2"
/* State variables for use in readFromDC() */
#define ST_WAIT_FOR_SYNC 0
#define ST_READ_PKTTYPE 1
#define ST_READ_HEADER 4
#define ST_READ_BODY 5

/*
 * Includes
 */
#include "dcbba2orb.h"
#include <stddef.h>
#include <stdint.h>

/*
 * Globals
 */
static Bns *oDCDataBNS = NULL;
static int orbfd = -1;
static struct stConfigData oConfig;

/*
 * State Variables
 */

/*
 * Prototypes
 */
static void showCommandLineUsage(void);
static int parseCommandLineOptions(int iArgCount, char *aArgList[]);
static int paramFileRead(void);
static void dcbbaCleanup(int iExitCode);
void closeAndFreeHandle(int *iHandle);
static int getBBAStaFromSID(int iStaID, char *sStaName);
static int getBBADataTypeFromSRate(float fSampleRateIn, char *sDataTypeOut);
static int dcDataConnect(int iConnType, char *sConnectionParams[]);
static int dcDataConnectFile(char *sFileName);
static int dcDataConnectSocket(char *sHost, in_port_t iPort);
static void sig_hdlr(int iSignal);
static int validateBBAChecksum(uint8_t *aBBAPkt, int iBBAPktLength);
static int parseBBAPacket(uint8_t *aBBAPkt, struct stBBAPacketInfo* oPktInfo);
static int readFromDC(struct stBBAPacketInfo *oPktInfo, uint8_t *aBBAPkt);
static size_t stuffBBAPkt(struct stBBAPacketInfo *PktInfo, uint8_t *BBAPktIn,
		char **OrbPktOut);
static char *getBBAChNameFromId(uint8_t ucBBAPktType, char *sSubCode,
		int iStaId, int iChId);
static int initSiteLookupArrays(void);

/*
 * Main program loop
 */
int main(int iArgCount, char *aArgList[]) {

	uint8_t *aBBAPkt; /* Buffer to hold a bba packet read from the wire */
	struct stBBAPacketInfo oPktInfo; /* Struct to hold information from the packet header */
	double dPrevTime;
	int bOKToSend;
	char *sOutPkt; /* Packet to put onto the orb */
	int iOutPktLen; /* Length of sOutPkt */

	elog_init(iArgCount, aArgList);

	/* Parse out command line options */
	if (parseCommandLineOptions(iArgCount, aArgList) == RESULT_SUCCESS) {

		/* Read in the parameter file */
		if (paramFileRead() == RESULT_FAILURE) {
			elog_complain(1,
					"main(): Error encountered during paramFileRead() operation.");
			dcbbaCleanup(-1);
		}

		/* Exit if bPFValidateFlag is set */
		if (oConfig.bPFValidateFlag == TRUE) {
			elog_notify(
					0,
					"main(): Parameter File %s validated successfully. Exiting.",
					oConfig.sParamFileName);
			dcbbaCleanup(0);
		}

		/* Allocate memory for our packet */
		allot (uint8_t *, aBBAPkt, oConfig.iBBAPktBufSz);

		/* Set up a signal handler to re-read the parameter file on SIGUSR1*/
		signal(SIGUSR1, sig_hdlr);

		/* Connect to the ORB */
		if ((orbfd = orbopen(oConfig.sOrbName, "w&")) < 0) {
			elog_complain(1, "orbopen: unable to connect to ORB \"%s\".",
					oConfig.sOrbName);
			dcbbaCleanup(-1);
		}

		/* Connect to Data Concentrator's Data read port */
		if (dcDataConnect(oConfig.iConnectionType, oConfig.sDCConnectionParams)
				== RESULT_SUCCESS) {
			dPrevTime = now();

			/*** BEGIN MAIN LOOP ***/
			while (readFromDC(&oPktInfo, aBBAPkt) == RESULT_SUCCESS) {
				bOKToSend = TRUE;
				/* Check the packet age */
				/*if (fabs(oPktInfo.dPktTime - dPrevTime) > 86400.0) {
				 dPrevTime = now();
				 if (fabs(oPktInfo.dPktTime - dPrevTime) > 86400.0) {
				 elog_complain(
				 0,
				 "%s packet has bad time - %s (epoch:%lf). Will discard packet.\n",
				 oPktInfo.sSrcname, sTimeStamp = strtime(
				 oPktInfo.dPktTime), oPktInfo.dPktTime);
				 free(sTimeStamp);
				 bOKToSend = FALSE;
				 } else
				 dPrevTime = oPktInfo.dPktTime;
				 } else
				 dPrevTime = oPktInfo.dPktTime;*/

				if (bOKToSend == TRUE) {
					/* Add orb header to Packet */
					iOutPktLen = (int) stuffBBAPkt(&oPktInfo, aBBAPkt, &sOutPkt);
					if (iOutPktLen == 0) {
						/* There was an error stuffing the packet*/
						elog_complain(
								1,
								"An error occurred while adding the ORB header to the raw packet. Not submitting to the orb.");
					} else if (sOutPkt == 0) {
						elog_die(1,
								"Output packet length was non-zero but pointer to Output packet is null");
					} else {

						/* put it into the orb */
						if (oConfig.bVerboseModeFlag == TRUE) {
							showPkt(0, oPktInfo.sSrcname, oPktInfo.dPktTime,
									sOutPkt, iOutPktLen, stderr, PKT_UNSTUFF);
							showPkt(0, oPktInfo.sSrcname, oPktInfo.dPktTime,
									sOutPkt, iOutPktLen, stderr, PKT_DUMP);
						}

						if (orbput(orbfd, oPktInfo.sSrcname, oPktInfo.dPktTime,
								sOutPkt, iOutPktLen)) {
							elog_complain(0, "orbput() failed in main()\n");
							dcbbaCleanup(-1);
						}

						if (oConfig.bVerboseModeFlag == TRUE)
							elog_notify(0, "packet submitted under %s\n",
									oPktInfo.sSrcname);

						/* Free the packet */
						free(sOutPkt);
					}
				}
			}

			/*
			 * If we get here, it means readFromDC failed to get a packet from oDCDataBNS.
			 * This could be either that an EOF was reached if we were reading from a file,
			 * or that the socket died unexpectedly.
			 */
			dcbbaCleanup(-1);
		}

		/* Else unable to connect, cleanup with failure (-1) exit code */
		else
			dcbbaCleanup(-1);

	} else {
		elog_complain(1,
				"main(): Error encountered during parseCommandLineOptions() operation.");
		dcbbaCleanup(-1);
	}

	/* If we got this far, cleanup with success (0) exit code */
	dcbbaCleanup(0);
	return (0);
}

/*
 * Read a packet from the Data Concentrator
 *
 * This function loops continuously until either a full packet is read from the
 * data concentrator or an EOF is reached. The connection to the Data
 * Concentrator, oDCDataBNS, must be established. Once a packet is read,
 * various fields are then looked up from tables defined in the parameter file
 * to translate them from numeric IDs to station and channel codes.
 *
 * Parameters:
 * 	oPktInfo
 * 		An empty stBBAPacketInfo struct where parsed data about the packet is
 * 		placed. This should be allocated by the calling function.
 *  aBBAPkt
 *  	A character buffer where the packet read from the wire is placed. This
 *  	should be allocated by the calling function as well.
 *
 * Returns:
 *  The raw packet as read from the wire (converted to host byte order) is placed into aBBAPkt
 *  Data parsed from the DC packet is placed into oPktInfo
 *  A result of RESULT_SUCCESS is returned if a packet was successfully read
 *  A result of RESULT_FAILURE is returned if an error was encountered reading from oDCDataBNS
 *
 */
static int readFromDC(struct stBBAPacketInfo *oPktInfo, uint8_t *aBBAPkt) {
	/*
	 * Declarations
	 */
	int iBBAPktIndex; /* Index for current position in aBBAPkt */
	uint8_t cIn; /* holding variable for a character read in via bnsget() */
	uint8_t iReadState; /* Current read state, used in read loop */
	uint16_t usVal; /* Temp variable used for reading unsigned shorts in network byte order. */
	uint16_t iPktLength; /* The full length of the current packet as reported by the packet header */

	/*
	 * Initialization
	 */
	if (oDCDataBNS == NULL) { /* Make sure we're connected */
		elog_complain(1,
				"readFromDC called before connection to data concentrator was established");
		return RESULT_FAILURE;
	}
	iBBAPktIndex = 0;
	iReadState = ST_WAIT_FOR_SYNC;

	/*
	 * BEGIN READ LOOP
	 *
	 * Keep reading from the BNS one character at a time until we get an
         * entire packet or an EOF is encountered
	 */
	while (bnsget(oDCDataBNS, &cIn, BYTES, 1) >= 0) {

		switch (iReadState) {

		case ST_WAIT_FOR_SYNC:
			/* Waiting for sync byte */

			iBBAPktIndex = 0;
			if (cIn == BBA_SYNC) {
                          /* If we received a BBA_SYNC byte, 
                           * place the sync byte into the packet buffer and 
                           * prepare to read the packet type byte */
                          aBBAPkt[iBBAPktIndex++] = cIn;
                          iReadState = ST_READ_PKTTYPE;
                        } else if (cIn == 0xAB || cIn == 0xBB) {
                          /* ipd2 had handling routines for these sync bytes,
                           * but underlying libdefunctpkt2 no longer parsed them */
                          elog_complain(
                              0,
                              "readFromDC(): state=ST_WAIT_FOR_SYNC, Unsupported packet prefix encountered: %x\n",
                              cIn);
                        } else {
                          /* We have a completely invalid sync character
                           * so we discard it and move on to the next */
                          elog_complain(
                              0,
                              "readFromDC(): state=ST_WAIT_FOR_SYNC, discarding character '%c' = %x\n",
                              cIn, cIn);
                        }
			break;

		case ST_READ_PKTTYPE:

			/* Copy packet type into packet */
			aBBAPkt[iBBAPktIndex++] = cIn;

			switch (cIn) {
			case BBA_DAS_DATA:
			case BBA_DAS_STATUS:
			case BBA_DC_STATUS:
			case BBA_RTX_STATUS:
				/* We recognized the packet type */
				iReadState = ST_READ_HEADER;
				break;

			default:
				/* Garbage data, return to ST_WAIT_FOR_SYNC */
				iReadState = ST_WAIT_FOR_SYNC;
				elog_complain(
						0,
						"main: state=1, Unknown BBA Packet subtype - discarding character '%c' = %x\n",
						cIn, cIn);
				break;
			}
			break;

		case ST_READ_HEADER: /* Wait for packet length in the header */
			aBBAPkt[iBBAPktIndex++] = cIn;
			if (iBBAPktIndex == BBA_CTRL_COUNT) {
				usVal = 0;
				memcpy((char *) &usVal, &aBBAPkt[BBA_PSIZE_OFF], 2); /* packet size  */
				if (usVal == 0) {
					elog_complain(0,
							"Wrong header. Zero packet size detected.\n");
					hexdump(stderr, aBBAPkt, iBBAPktIndex);
					iReadState = ST_WAIT_FOR_SYNC;
				} else {
					iPktLength = ntohs(usVal);
					iReadState = ST_READ_BODY;
				}
			}
			break;

		case ST_READ_BODY:
			aBBAPkt[iBBAPktIndex++] = cIn; /* We have a packet length, so keep spooling packet from buffer */

			if (iBBAPktIndex >= iPktLength) { /* packet complete */

				if (validateBBAChecksum(aBBAPkt, iPktLength) == RESULT_FAILURE) {
					/* Checksum's don't match */
					elog_complain(0, "discarding packet with bad checksum\n");
					hexdump(stderr, aBBAPkt, iPktLength);
				} else {
					/* Checksum's match, grab additional data from the packet */
					if (oConfig.bVerboseModeFlag)
						hexdump(stderr, aBBAPkt, iPktLength);

					if (parseBBAPacket(aBBAPkt, oPktInfo) == RESULT_SUCCESS) {
						/*
						 * If the parsing was successful, return.
						 */
						return RESULT_SUCCESS;
					} else {
						/*
						 * Log an error and discard the packet
						 */
						elog_complain(0,
								"parseBBAPacket was unable to parse the packet. Skipping.\n");
					}
				}
				iReadState = ST_WAIT_FOR_SYNC;
			} else if (iBBAPktIndex >= oConfig.iBBAPktBufSz) {
				elog_complain(
						0,
						"attempted to accumulate %d byte packet: too large for internal buffer\n",
						iBBAPktIndex);
				iReadState = ST_WAIT_FOR_SYNC;
			}
			break;

		}

	}
	/*
	 * If we get here, it means we encountered an unrecoverable error while reading the file handle/socket
	 */
	elog_log(0, "readFromDC(): bnsget failed to read");
	return RESULT_FAILURE;
}

/*
 * Creates a packet ready to place into the orb from a BBA Packet read from the
 * DC using data in oPktInfo. Does not modify the raw BBA Packet.
 *
 * NOTE: While the Antelope orb libs include a stuff packet function for BBA
 * packets, it is neither well tested, nor suitable for our case where most of
 * the channel data is already present in it's final form.
 *
 * Parameters:
 * 	PktInfo
 * 		Struct containing information about the packet, as extracted by
 * 		parseBBAPacket
 * 	BBAPktIn
 * 		The raw packet read from the wire.
 * 	OrbPktOut
 *		Address of a variable which will point to the final orb packet. The
 *		caller will need to free the memory used by the new packet
 *
 * Returns:
 * 	The length of OrbPacketOut. This will be 0 if an error occurred, and
 *  OrbPktOut will be null.
 */
static size_t stuffBBAPkt(struct stBBAPacketInfo *PktInfo, uint8_t *BBAPktIn,
		char **OrbPktOut) {

	char *oNewPkt = 0; /* the new orb packet */
	double dCalib; /* Calibration value for the entire packet */
	float fCalib; /* Cal value converted to float */
	float fSampRate; /* Sample rate, converted to float */
	struct stBBAPreHdr oPreHdr; /* Orb Pre Header */
	char aOrbHdr[512]; /* The full Orb Header, ready to prepend to the packet */
	char aHdrPart[512];/* The variable length part of the header after the pre-header */
	char *pHdrPart; /* Pointer to a location in the variable length header */
	char sSta[PKT_NAMESIZE]; /* Station name as translated by getBBAStaFromSID */
	char sChNames[BBA_ORB_MAX_CHNAMES_SZ]; /* The channel names (in network byte order) */
	size_t iHdrPartSize;
	size_t iNewPktSize, iChNamesSize;
	int16_t iDatatype, iNSamp, iNChan, iCHSize, iBBAHdrSize, iOrbHdrSize;

	/*
	 * Initialization
	 */
	iHdrPartSize = 0; /* Initialize the byte counter */
	pHdrPart = &aHdrPart[0]; /* Initialize header pointer */
	memset(sChNames, 0, BBA_ORB_MAX_CHNAMES_SZ); /* Zero out the sChNames buffer */

	/*
	 * Populate the pre-header struct
	 * The fourth field, hdrsiz, is generated below after we assemble the main header
	 */
	oPreHdr.pkttype = (uint16_t) htons (PktInfo->usRawPktType);
	oPreHdr.hdrtype = (uint16_t) htons (0xBBA); /* Fixed to BBA format */
	oPreHdr.pktsiz = htons(PktInfo->iPktSize);

	/*
	 * Prepare to assemble the header
	 * Retrieve channel information, station name, and calibration info
	 */
	iNChan = PktInfo->iNChan;
	strcpy((char *)sChNames, PktInfo->sChNames);
	iChNamesSize = strlen((char *)sChNames);
	strcpy((char *)sSta, PktInfo->oSrcname.src_sta);
	dCalib = 0; /* Always null for BBA packets, each channel has a cal factor instead */

	/*
	 * Start assembling the main header
	 */

	/* Put the calibration value into hdr */
	fCalib = (float) dCalib;
	htonfp(&fCalib, &fCalib);
	memcpy(pHdrPart, &fCalib, sizeof(float));
	iHdrPartSize = (size_t) sizeof(float);
	pHdrPart += sizeof(float);

	/* Put the sample rate into the header */
	htonfp(&PktInfo->fSrate, &fSampRate);
	memcpy(pHdrPart, &fSampRate, sizeof(float));
	iHdrPartSize += (size_t) sizeof(float);
	pHdrPart += sizeof(float);

	/* Get the Trace code (from tr.h) for the data type */
	if (strncmp(PktInfo->sDataType, "s2", 2) == 0) {
		iDatatype = (int16_t) trSHORT;
	} else if (strncmp(PktInfo->sDataType, "s4", 2) == 0) {
		iDatatype = (int16_t) trINT;
	} else if (strncmp(PktInfo->sDataType, "c0", 2) == 0) {
		iDatatype = 0;
	}
	/* Put the data type into the header */
	iDatatype = htons(iDatatype);
	memcpy(pHdrPart, &iDatatype, sizeof(int16_t));
	iHdrPartSize += (size_t) sizeof(int16_t);
	pHdrPart += sizeof(int16_t);

	/* Put the number of samples into the header */
	iNSamp = (int16_t) htons(PktInfo->iNSamp);
	memcpy(pHdrPart, &iNSamp, sizeof(int16_t));
	iHdrPartSize += (size_t) sizeof(int16_t);
	pHdrPart += sizeof(int16_t);

	/* Put the number of channels into the header */
	iNChan = (int16_t) htons(PktInfo->iNChan);
	memcpy(pHdrPart, &iNChan, sizeof(int16_t));
	iHdrPartSize += (size_t) sizeof(int16_t);
	pHdrPart += sizeof(int16_t);

	/* Put the BBA header size into the header */
	iBBAHdrSize = (int16_t) htons(PktInfo->iHdrSize);
	memcpy(pHdrPart, &iBBAHdrSize, sizeof(int16_t));
	iHdrPartSize += (size_t) sizeof(int16_t);
	pHdrPart += sizeof(int16_t);

	/* Put the length of the channel names into the header */
	iCHSize = htons( (int16_t) iChNamesSize );
	memcpy(pHdrPart, &iCHSize, sizeof(int16_t));
	iHdrPartSize += (size_t) sizeof(int16_t);
	pHdrPart += sizeof(int16_t);

	/* Put the channel names into the header */
	memcpy(pHdrPart, sChNames, iChNamesSize);
	iHdrPartSize += iChNamesSize;
	pHdrPart += iChNamesSize;

	/* Finalize the Pre Header */
	iOrbHdrSize = iHdrPartSize + sizeof(struct stBBAPreHdr);
	iNewPktSize = iOrbHdrSize + ntohs(oPreHdr.pktsiz);
	oPreHdr.hdrsiz = htons (iOrbHdrSize);

	/* Copy the pre header and the generated hdrbuf into hdr */
	memcpy(&aOrbHdr[0], (char *) &oPreHdr, sizeof(struct stBBAPreHdr));
	memcpy(&aOrbHdr[sizeof(struct stBBAPreHdr)], aHdrPart, iHdrPartSize);

	/*
	 * Build the ORB data packet
	 */
	allot( char *, oNewPkt, iNewPktSize );
	memcpy(oNewPkt, aOrbHdr, iOrbHdrSize); /* Add the entire header, including the PreHdr */
	memcpy(oNewPkt + iOrbHdrSize, (signed char *) BBAPktIn, ntohs(oPreHdr.pktsiz)); /* Add data */

	/*
	 * If we get this far, the packet has been successfully generated
	 *
	 * Return the pointer to the newly generated packet in OrbPktOut
	 * Finally, return the size of the packet in psize
	 */
	*OrbPktOut = oNewPkt;
	return iNewPktSize;
}

/*
 * Displays the command line options
 */
static void showCommandLineUsage(void) {
	cbanner(
			VERSION,
			" [-V|--usage] [-r|--validatepf] [-v|--verbose] [--brief] "
			"-a DC_IP_ADDRESS|--dcaddress=DC_IP_ADDRESS|-t TEST_FILE_NAME|--testfile=TEST_FILENAME "
			"[-d DC_DATA_PORT|--dcdataport=DC_DATA_PORT] "
			"[-o DATA_ORB_NAME|--dataorb=DATA_ORB_NAME] "
			"[-g PARAM_FILENAME|--pf=PARAM_FILENAME]",
			"Geoff Davis", "IGPP, UCSD", "gadavis@ucsd.edu");
}

/*
 * Parses out the command line options. Returns RESULT_SUCCESS if all options
 * are good and within limits, RESULT_FAILURE if there was a problem or that
 * the program needn't continue running
 */
static int parseCommandLineOptions(int iArgCount, char *aArgList[]) {
	int iOption = '\0';
	int iLongOptIdx = 0;
	int bAddressSet = FALSE;
	int bDataFileSet = FALSE;
	static struct option oLongOpts[] = {
			/* These options set a flag */
			{ "verbose", no_argument, &oConfig.bVerboseModeFlag, TRUE},
			{ "brief",	no_argument, &oConfig.bVerboseModeFlag, FALSE},
			{ "validatepf", no_argument, &oConfig.bPFValidateFlag, TRUE},
			/* These options don't set a flag. We distinguish them by their indices. */
			{ "usage", no_argument, 0, 'V' },
			{ "dcaddress", required_argument, 0, 'a' },
			{ "dataport", required_argument, 0, 'd' },
			{ "controlport", required_argument, 0, 'c' },
			{ "dataorb", required_argument, 0, 'o' },
			{ "pf", required_argument, 0, 'g' },
			{ "statefile", required_argument, 0, 's' },
			{ "testfile", required_argument, 0, 's' },
			{ 0, 0, 0, 0 } };

	/* Initialize the CONFIG structure */
	oConfig.bVerboseModeFlag = FALSE;
	oConfig.bPFValidateFlag = FALSE;
	oConfig.iConnectionType = CONNECT_TCP_IP;
	oConfig.sDCConnectionParams[0] = "";
	oConfig.sDCConnectionParams[1] = DEFAULT_DC_DATA_PORT;
	oConfig.sDCConnectionParams[2] = DEFAULT_DC_CONTROL_PORT;
	oConfig.sOrbName = ":";
	oConfig.sParamFileName = DEFAULT_PARAM_FILE;
	oConfig.sStateFileName = NULL;
	oConfig.oSiteTbl = NULL;
	oConfig.oStaName = NULL;
	oConfig.iBBAPktBufSz = DEFAULT_BBA_PKT_BUF_SZ;

	/* Loop through all possible options */
	while ((iOption = getopt_long(iArgCount, aArgList, "vVra:d:c:o:g:s:t:",
			oLongOpts, &iLongOptIdx)) != -1) {
		if (iOption == 0) { 
		/* Parse long options */

			if (oLongOpts[iLongOptIdx].flag != 0) {
				/* If this option set a flag, do nothing else now */
				break;
			}
			/*
			 * Handle options that don't set a flag
			 */

			/*
			 * If there was a long option without a corresponding short option
			 * that didn't set a flag either, we would handle it here and break
			 */

			/*
			 * Handle long options with corresponding short option by passing
			 * them off to the switch statement below
			 */
			iOption = oLongOpts[iLongOptIdx].val;
		}
		switch (iOption) {
		case 0:
			break; /* This long option was handled above so don't do anything */
		case 'V':
			showCommandLineUsage();
			return RESULT_FAILURE;
		case 'v':
			oConfig.bVerboseModeFlag = TRUE;
			break;
		case 'a': /* Address of the DC */
			oConfig.sDCConnectionParams[0] = optarg;
			oConfig.iConnectionType = CONNECT_TCP_IP;
			bAddressSet = TRUE;
			break;
		case 'd': /* Port number of the data port on the DC */
			oConfig.sDCConnectionParams[1] = optarg;
			break;
		case 'c': /* Port number of the control port on the DC */
			oConfig.sDCConnectionParams[2] = optarg;
			break;
		case 'o': /* Data Orb Name */
			oConfig.sOrbName = optarg;
			break;
		case 'g':
			oConfig.sParamFileName = optarg;
			break;
		case 's':
			oConfig.sStateFileName = optarg;
			break;
		case 't': /* Filename of the Data port test file */
			oConfig.sDCConnectionParams[0] = optarg;
			bDataFileSet = TRUE;
			oConfig.iConnectionType = CONNECT_FILE;
			break;
		case 'r': /* Read the parameter file and exit */
			oConfig.bPFValidateFlag = TRUE;
			break;

			/* Handle invalid arguments */
		default:
			elog_complain(
					0,
					"parseCommandLineOptions(): Invalid command line argument: '-%c'\n\n",
					iOption);
			showCommandLineUsage();
			return RESULT_FAILURE;
		}
	}

	/* Output a log header for our program */
	elog_notify(0, "%s\n", VERSION);

	/* Verify valid command line options & combinations */
	if ((bAddressSet == FALSE) && (bDataFileSet == FALSE)
			&& (oConfig.bPFValidateFlag == FALSE)) {
		elog_complain(0,
				"parseCommandLineOptions(): No address for Data Concentrator specified.\n");
		showCommandLineUsage();
		return RESULT_FAILURE;
	}
	/* If we got this far, everything was fine! */
	return RESULT_SUCCESS;
}

/*
 * Performs "cleanup" tasks -- closes the connections to the data concentrator. Never returns.
 */
static void dcbbaCleanup(int iExitCode) {
	/* Log that we exited */
	elog_notify(0, "dcbbaCleanup(): Exiting with code %i...\n", iExitCode);

	/* Close the data connection handle */
	if (oDCDataBNS)
		bnsclose(oDCDataBNS);

	/* Close the orb connection */
	if (orbfd)
		orbclose(orbfd);
	/* Exit */
	exit(iExitCode);
}

/*
 * Initialize Station-Name-by-SID and Station-Channel Lookup Arrays
 * Helper function for paramFileRead
 *
 * Returns:
 * 	RESULT_FAILURE if an error occurs, RESULT_SUCCESS otherwise
 */
static int initSiteLookupArrays() {
	long iNumRows = 0, iRowIdx = 0; /* Used below while iterating through the Site Table */
	char *sRow = 0; /* Contains a row read from the Site Table */
	char sKey[32];
	struct stSiteEntry oSiteEntry, *oNewSiteEntry = 0;

	/* Initialize the Station Name lookup array */
	iNumRows = maxtbl(oConfig.oSiteTbl);

	if (iNumRows > 0) {
		if (oConfig.oStaName != NULL) {
			freearr(oConfig.oStaName, 0);
		}
		if (oConfig.oStaCh != NULL) {
			freearr(oConfig.oStaName, 0);
		}
		oConfig.oStaName = newarr(0);
		oConfig.oStaCh = newarr(0);
	} else {
		elog_log(0, "Can't get site parameters. Check parameter file!\n");
		return RESULT_FAILURE;
	}

	for (iRowIdx = 0; iRowIdx < iNumRows; iRowIdx++) {
		/* Read a row from the site table */
		sRow = gettbl(oConfig.oSiteTbl, iRowIdx);
		/* Parse it into it's component entries */
		sscanf(sRow, STE_SCS, STE_RVL(&oSiteEntry));
		free(sRow);

		/* Handle the StaName table */
		sprintf(sKey, "%d", oSiteEntry.iSID);

		if (getarr(oConfig.oStaName,sKey) == NULL) {
			/* If we haven't seen this key before, add an entry in the array */
			setarr(oConfig.oStaName, sKey, strdup(oSiteEntry.sNAME));
		}

		/* Handle the StaCh table */
		sprintf(sKey, "%s_%d_%d", oSiteEntry.sDTYPE, oSiteEntry.iSID, oSiteEntry.iCOMP);

		if ( (struct stSiteEntry *) getarr( oConfig.oStaCh, sKey ) == NULL ) {
			allot( struct stSiteEntry *, oNewSiteEntry, sizeof(struct stSiteEntry));
			memcpy( oNewSiteEntry, &oSiteEntry, sizeof (struct stSiteEntry));
			setarr(oConfig.oStaCh, sKey, oNewSiteEntry);
		}
	}
		return RESULT_SUCCESS;
	}

	/*
	 * Reads the parameter file and initializes several lookup tables.
	 */
static int paramFileRead(void) {
	int ret; /* Return value from pfupdate */
	static int iPFFirstRead = 1; /* Tracks whether or not this is the first read of the parameter file */
	char *pfver_read = NULL;

	/* Read the parameter file */
	if ((ret = pfupdate(oConfig.sParamFileName, &oConfig.oConfigPf)) < 0) {
		/* An error occurred reading the parameter file */
		elog_log(
				0,
				"pfupdate(\"%s\", oConfig.oConfigPf): failed to open config file.\n",
				oConfig.sParamFileName);
		return RESULT_FAILURE;
	} else if (ret == 1) {
		/* We were able to successfully read the parameter file */

		/* Notify that we've read our config file */
		if (iPFFirstRead)
			elog_notify(0, "config file loaded %s\n", oConfig.sParamFileName);
		else
			elog_notify(0, "updated config file loaded %s\n",
					oConfig.sParamFileName);

		/* Check to make sure the parameter file version matches #PFVER */
		pfver_read = pfget_string(oConfig.oConfigPf, "dcbba2orb_pf_ver");
		if (pfver_read == 0) {
                  /* pfget_string returns 0 (not NULL) if the key is not found */
			elog_log(0,
					"pfupdate(): no version number found in parameter file %s",
					oConfig.sParamFileName);
			free(pfver_read);
			return RESULT_FAILURE;
		} else if (strcmp(pfver_read, PFVER) != 0) {
			elog_log(
					0,
					"pfupdate(): dcbba2orb_pf_ver \"%s\" in \"%s\" does not match the required version \"%s\". Please update the parameter file. See the manpage for formatting details.",
					pfver_read, oConfig.sParamFileName, PFVER);
			free(pfver_read);
			return RESULT_FAILURE;
		}
                /* If we get here, pfver_read should not be null or zero*/
		free(pfver_read);

		/* Read in the Network Name from the parameter file */
		oConfig.sNetworkName = pfget_string(oConfig.oConfigPf, "Network_Name");
		if (oConfig.bVerboseModeFlag == TRUE)
			elog_notify(0, "Network Name set to %s", oConfig.sNetworkName);

		/* Read in the Site Table from the parameter file */
		if (oConfig.oSiteTbl)
			freetbl(oConfig.oSiteTbl, 0);
		oConfig.oSiteTbl = pfget_tbl(oConfig.oConfigPf, "Site");

		/* Initialize the Site lookup arrays */
		if (initSiteLookupArrays() == RESULT_FAILURE)
			return RESULT_FAILURE;

		/* Read in the Das_Stat array */
		if (oConfig.oDasID)
			freearr(oConfig.oDasID, 0);
		oConfig.oDasID = pfget_arr(oConfig.oConfigPf, "Das_Stat");

		/* Read in the DC_Stat array */
		if (oConfig.oDcID)
			freearr(oConfig.oDcID, 0);
		oConfig.oDcID = pfget_arr(oConfig.oConfigPf, "DC_Stat");

		/* Read in the RTX_Stat array */
		if (oConfig.oRTXID)
			freearr(oConfig.oRTXID, 0);
		oConfig.oRTXID = pfget_arr(oConfig.oConfigPf, "RTX_Stat");

		/* Next read will no longer be the first */
		iPFFirstRead = 0;
	} /* pfudate returns 0 if nothing changed */

	/* Return results */
	return RESULT_SUCCESS;
}

/*
 * Helper function to get a BBA Station Name from the SID encoded in the packet.
 * Uses oConfig.oSites pulled from the Sites array defined in the parameter file
 */
static int getBBAStaFromSID(int iStaID, char *StaName) {
	char sKey[5];
	void *result; /* Pointer to an entry in the station name lookup array */
	/* Convert iStaID to a string so we can do the pf lookup */
	sprintf(sKey, "%i", iStaID);
	result = getarr(oConfig.oStaName, sKey);
	if (result == NULL) {
		elog_log(0, "getBBAStaFromSID: unable to find StaName for StaID %i",
				iStaID);
		return RESULT_FAILURE;
	}
	strcpy(StaName, result);
	return RESULT_SUCCESS;
}

/*
 * Get the data packet type
 *
 * BBA packets can be HS, BS, or LS in type, defined from their sample rate.
 * LS: <10 Hz
 * BS: 10-99 Hz
 * HS: >=100 Hz
 *
 * This information was taken from the parse_newbba function in libdefuntpkt2 pkttype.c
 */
static int getBBADataTypeFromSRate(float fSampleRateIn, char *sDataTypeOut) {

	if (fSampleRateIn < 10) {
		strcpy(sDataTypeOut, "LS");
		return RESULT_SUCCESS;
	} else if ((fSampleRateIn >= 10) && (fSampleRateIn < 100)) {
		strcpy(sDataTypeOut, "BS");
		return RESULT_SUCCESS;
	} else if (fSampleRateIn >= 100) {
		strcpy(sDataTypeOut, "HS");
		return RESULT_SUCCESS;
	}

	/* We shouldn't ever get here unless iSampleRate is way out of range or NULL*/
	strcpy(sDataTypeOut, "");
	elog_log(
			0,
			"getBBADataTypeFromSRate: Unable to determine DataType from given sample rate of %f",
			fSampleRateIn);
	return RESULT_FAILURE;
}

/*
 *  Connect to the Data Concentrator's Data port
 */
static int dcDataConnect(int iConnType, char *sConnectionParams[]) {
	/* Initialize */
	int iAttemptsMade = 0;
	int iResult = RESULT_SUCCESS;

	/* Check for invalid params */
	if ((sConnectionParams[0] == NULL) || ((iConnType == CONNECT_TCP_IP)
			&& sConnectionParams[1] == NULL)) {
		elog_log(0, "dcDataConnect(): Bad connection parameters.   "
			"Expected '<host>,<port>' or "
			"'<filename>'.\n");
		return RESULT_FAILURE;
	}

	/* Main loop */
	if (oConfig.bVerboseModeFlag == TRUE) {
		elog_notify(0,
				"dcDataConnect(): Connecting to the Data Concentrator's data port...\n");
	}

	while (iAttemptsMade < MAX_CONNECT_ATTEMPTS) {

		/* Handle TCP/IP connections */
		if (iConnType == CONNECT_TCP_IP) {
			iResult = dcDataConnectSocket(sConnectionParams[0],
					(in_port_t) atol(sConnectionParams[1]));
		}

		/* Handle File Simulation */
		else if (iConnType == CONNECT_FILE) {
			iResult = dcDataConnectFile(sConnectionParams[0]);
		}

		/* If successful, break out of loop */
		if (iResult == RESULT_SUCCESS) {
			break;
		}

		/* Increment attempt counter */
		iAttemptsMade++;
		sleep(3);
	}

	/* If we failed too many times, log error */
	if (iAttemptsMade >= MAX_CONNECT_ATTEMPTS) {
		elog_log(
				0,
				"dcDataConnect(): Aborting after %i failed connection attempts.\n",
				iAttemptsMade);
		return RESULT_FAILURE;
	}

	/* Return results */
	if ((iResult == RESULT_SUCCESS) && (oConfig.bVerboseModeFlag == TRUE)) {
		elog_notify(0,
				"dcDataConnect(): Connected to the Data Concentrator's data port.\n");
	}
	return iResult;
}

/*
 * Connect to the Data Concentrator's Data Port via TCP/IP
 */
static int dcDataConnectSocket(char *sHost, in_port_t iPort) {

	/* Initialize */
	in_addr_t iNetAddress;
	int iConnectionResult = 1;
	int iFH; /*Temporary filehandle that gets used to create the Bns structure */
	struct hostent *oHostEnt;
	struct sockaddr_in oAddress;

	/* Check for host already being a numeric IP address */
	if ((iNetAddress = inet_addr(sHost)) != INADDR_NONE)
		memcpy(&oAddress.sin_addr, &iNetAddress, min (sizeof (iNetAddress), sizeof(oAddress.sin_addr)));

	/* Else resolve name to IP address */
	else {
		oHostEnt = gethostbyname(sHost);
		if (oHostEnt == NULL) {
			elog_log(1,
					"dcDataConnectSocket(): Could not resolve address '%s'.",
					sHost);
			return RESULT_FAILURE;
		}
		memcpy(&oAddress.sin_addr, oHostEnt -> h_addr, min (oHostEnt -> h_length, (size_t) sizeof (oAddress.sin_addr)));
		free(oHostEnt);
	}

	/* Create socket */
	if ((iFH = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
		elog_log(1, "dcDataConnectSocket(): Could not create socket.");
		return RESULT_FAILURE;
	}

	/* Extract address from host entry */
	oAddress.sin_family = AF_INET;
	oAddress.sin_port = htons (iPort);
	if (oConfig.bVerboseModeFlag == TRUE)
		elog_notify(0, "connectSocket(): Connecting to '%s' on port %i.\n",
				sHost, iPort);

	/* Try connecting */
	iConnectionResult = connect(iFH, (struct sockaddr *) &oAddress,
			sizeof(oAddress));
	if (iConnectionResult) {
		elog_complain(1, "dcDataConnectSocket(): Could not connect to socket");
		close(iFH);
		return RESULT_FAILURE;
	}

	/* Now that we've got our socket handle, make a Bns out of it */
	oDCDataBNS = bnsnew(iFH, DATA_RD_BUF_SZ);
	/* Since iFH is a socket, set up the BNS to use Socket read routines */
	bnsuse_sockio(oDCDataBNS);

	/* If we got this far, everything was successful */
	return RESULT_SUCCESS;

}

/*
 * Read a packet dump file as a simulation of the DC Data Port
 *
 * Packet dump is simply a file containing only the data fields from the TCP/IP packet stream as extracted by Wireshark
 */
static int dcDataConnectFile(char *sFileName) {
	int iFH;
	/*  Attempt to open the file in Read-Only mode */
	iFH = open(sFileName, O_RDONLY);
	if (iFH < 0) {
		elog_log(1,
				"dcDataConnectFile(): Unable to open dummy data file '%s'.",
				sFileName);
		return RESULT_FAILURE;
	}

	oDCDataBNS = bnsnew(iFH, DATA_RD_BUF_SZ);
	/* If we got here, return RESULT_SUCCESS */
	return RESULT_SUCCESS;
}

static void sig_hdlr(int signo) {

	elog_notify(0, "sig_hdlr(): Got signal %d.\n", signo);
	elog_notify(0, "Re-reading parameter file to get new settings...");

	if (paramFileRead() == RESULT_FAILURE) {
		elog_complain(1,
				"sig_hdlr(): Error encountered during paramFileRead() operation:");
		dcbbaCleanup(-1);
	}

	elog_notify(0, "Done.\n");
	signal(SIGUSR1, sig_hdlr);
	return;
}

/*
 * Calculate a packet checksum and compare it to what is in the packet.
 * In BBA packets generated by the data concentrator, the checksum is the third and fourth bytes
 *
 * Parameters:
 * 	aBBAPkt
 * 		The packet as read from the Data Concentrator (in network byte order)
 * 	iBBAPktLength
 * 		The length of the packet
 *
 * Returns:
 *  RESULT_TRUE if the checksums match, RESULT_FALSE if they don't
 */
static int validateBBAChecksum(uint8_t *aBBAPkt, int iBBAPktLength) {
	unsigned short usPktChksum, usCalcChksum; /* Holds the checksum values */
	uint8_t *pBBAPktChkSum; /* Pointer for calculating the checksum */

	/* Go through the data portion of the packet one unsigned short at a time.
	 * The checksum is calculated on the entire packet assuming
	 * that the checksum byte fields are 0. Thus, we include the packet sync bytes from the beginning
	 * of the packet (0xDAAB or similar) but skip over the checksum bytes*/

	/* Initialize variables for checksum loop */
	usCalcChksum = 0;
	pBBAPktChkSum = &aBBAPkt[0];

	/* Include the packet header bytes in usCalcChksum */
	usCalcChksum ^= ntohs(*pBBAPktChkSum++);

	/* Extract the checksum from the packet, and skip it in the calculated checksum */
	usPktChksum = ntohs(*pBBAPktChkSum++);

	/*
	 * We have now skipped forward enough that usChksumPtr should be pointing at the offset
	 * for the packet size, so we need to iterate through the remainder of the packet
	 */
	for (; pBBAPktChkSum < (&aBBAPkt[0] + (uintptr_t)iBBAPktLength); pBBAPktChkSum++) {
		usCalcChksum ^= ntohs(*pBBAPktChkSum);
	}

	if (usCalcChksum != usPktChksum) {
		elog_complain(0, "bad checksum  PCHK:%04X!=CHK:%04X\n", usPktChksum,
				usCalcChksum);
		return RESULT_FAILURE;
	}

	return RESULT_SUCCESS;
}

/*
 * Parse a new format BBA packet and fill out a stBBAPacketInfo structure
 *
 * Parameters:
 * 	aBBAPkt
 * 		The raw packet as read from the data concentrator, in network byte order
 * 	oPktInfo
 * 		A pointer to a pre-allocated struct stBBAPacketInfo which will hold the extracted packet info
 *
 * Returns:
 * 	RESULT_FAILURE if there's an error
 */
static int parseBBAPacket(uint8_t *aBBAPkt, struct stBBAPacketInfo* oPktInfo) {
	/* Variables */
	uint16_t usVal;
	double dPTime, dYTime, dSec;
	int32_t iYear, iDay, iHour, iMin;
	uint32_t ysec;
	char sTMPNameCmpt[PKT_TYPESIZE];
	char *sChName, aChan[128];
	uint32_t iChId, iChBytes, iChIdx, iDataOffset, i;
	uint8_t ucBBAPktType;

	/*
	 * Start extracting portions of the packet and putting the results into oPktInfo fields
	 */

	ucBBAPktType = aBBAPkt[1];

	usVal = 0;
	memcpy(&usVal, &aBBAPkt[BBA_PSIZE_OFF], 2); /* packet size */
	if (usVal == 0) {
		elog_log(
		    0,
		    "parseBBAPacket(): Wrong header. Zero packet size detected.\n"
		    );
		return RESULT_FAILURE;
	} else
		oPktInfo->iPktSize = ntohs(usVal);

	usVal = 0;
	memcpy(&usVal, &aBBAPkt[BBA_STAID_OFF], 2); /* sta ID */
	if (oPktInfo->iPktSize == 0) {
		elog_log(0,
                    "parseBBAPacket(): Wrong header. Zero packet size detected.\n");
		return RESULT_FAILURE;
	} else
		oPktInfo->iStaID = ntohs(usVal);
	usVal = 0;
	memcpy((char *) &usVal, &aBBAPkt[BBA_NSAMP_OFF], 2); /* # of samples */
	if (usVal == 0) {
		elog_log(0,
				"parseBBAPacket(): Wrong header. Zero number of samples detected.\n");
		return RESULT_FAILURE;
	} else
		oPktInfo->iNSamp = ntohs(usVal);

	usVal = 0;
	memcpy((char *) &usVal, &aBBAPkt[BBA_SRATE_OFF], 2); /* Sample rate */
	if (usVal == 0) {
		elog_log(0,
				"parseBBAPacket(): Wrong header. Zero sample rate detected.\n");
		return RESULT_FAILURE;
	} else
		oPktInfo->fSrate = ntohs(usVal);

	usVal = 0;
	memcpy((char *) &usVal, &aBBAPkt[BBA_HSIZE_OFF], 2); /* header size */
	if (usVal == 0) {
		elog_log(0,
				"parseBBAPacket(): Wrong header. Zero header size detected.\n");
		return RESULT_FAILURE;
	} else
		oPktInfo->iHdrSize = ntohs(usVal);

	oPktInfo->iNChan = aBBAPkt[BBA_NCHAN_OFF]; /* Number of channels */
	if (oPktInfo->iNChan == 0) {
		elog_log(0,
				"parseBBAPacket(): Wrong header. Zero number of channels detected.\n");
		return RESULT_FAILURE;
	}

	/* Put raw packet type (DAAB, DABC, etc) into oPktInfo */
	oPktInfo->usRawPktType = (aBBAPkt[0] * 256) + aBBAPkt[1];

	/* get data type in orb header format */
	switch (aBBAPkt[BBA_DTYPE_OFF]) {
	case 0x0: /* 16 bit */
		strcpy(oPktInfo->sDataType, "s2");
		break;
	case 0x01: /* 32 bit */
		strcpy(oPktInfo->sDataType, "s4");
		break;
	case 0x02: /* 64 bit */
		strcpy(oPktInfo->sDataType, "t4");
		break;
	case 0x10: /* 16 bit "UCSD" compressed */
	case 0x11: /* 32 bit "UCSD" compressed */
	case 0x12: /* 64 bit "UCSD" compressed */
		strcpy(oPktInfo->sDataType, "c0");
		break;
	default:
		elog_log(0, "parseBBAPacket(): Can't recognize a data type %d(%02x)\n",
				aBBAPkt[BBA_DTYPE_OFF], aBBAPkt[BBA_DTYPE_OFF]);
		return RESULT_FAILURE;
	}

	/* Get packet time */
	dYTime = now();
	e2h(dYTime, &iYear, &iDay, &iHour, &iMin, &dSec);
	dPTime = epoch(iYear * 1000);

	ysec = 0;
	memcpy((char *) &ysec, &aBBAPkt[BBA_TIME_OFF], 4);
	oPktInfo->dPktTime = dPTime + ntohl(ysec);

	/*
	 * Fill in the SrcName fields
	 */

	/* Subcode and Station Name */
	switch (aBBAPkt[1]) {

	case BBA_DAS_DATA: /* Data Packets */

		/* Get the subcode from the sample rate */
		if (getBBADataTypeFromSRate(oPktInfo->fSrate, sTMPNameCmpt)
				== RESULT_FAILURE) {
			elog_log(0,
					"parseBBAPacket(): Lookup of Subcode from Sample Rate failed.\n");
			return RESULT_FAILURE;
		}
		strcpy(oPktInfo->oSrcname.src_subcode, sTMPNameCmpt);

		/* Get the station name, reusing sTMPNameCmpt */
		if (getBBAStaFromSID(oPktInfo->iStaID, sTMPNameCmpt) == RESULT_FAILURE) {
			elog_log(0, "parseBBAPacket(): Lookup of Station Name Failed.\n");
			return RESULT_FAILURE;
		}
		break;

	case BBA_DAS_STATUS: /* DAS Status Packets */

		strcpy(oPktInfo->oSrcname.src_subcode, "DAS");

		/* Get the station name */
		if (getBBAStaFromSID(oPktInfo->iStaID, sTMPNameCmpt) == RESULT_FAILURE) {
			elog_log(0, "parseBBAPacket(): Lookup of Station Name Failed.\n");
			return RESULT_FAILURE;
		}
		break;

	case BBA_DC_STATUS: /* DC Status Packets */

		strcpy(oPktInfo->oSrcname.src_subcode, "DC");

		/*
		 * The unitid field in the DC status packet is actually the serial
		 * number of the Data concentrator itself, not a RefTek UnitID. Thus
		 * we do not translate it via the station name lookup table.
		 */
		sprintf(sTMPNameCmpt, "%d", oPktInfo->iStaID);
		break;

	case BBA_RTX_STATUS: /* RTX Status Packets */

		/* Get the station name */
		strcpy(oPktInfo->oSrcname.src_subcode, "RTX");
		if (getBBAStaFromSID(oPktInfo->iStaID, sTMPNameCmpt) == RESULT_FAILURE) {
			elog_log(0, "Lookup of Station Name Failed.\n");
			return RESULT_FAILURE;
		}
		break;
	default:
		elog_log(0, "Can't recognize a data packet type - %d(%02x)\n",
				aBBAPkt[1], aBBAPkt[1]);
		return RESULT_FAILURE;
	}

	strcpy(oPktInfo->oSrcname.src_net, oConfig.sNetworkName); /* Net */
	strcpy(oPktInfo->oSrcname.src_sta, sTMPNameCmpt); /* Sta */
	strcpy(oPktInfo->oSrcname.src_chan, ""); /* Chan (always null for these packets) */
	strcpy(oPktInfo->oSrcname.src_loc, ""); /* Loc (always null for these packets) */
	strcpy(oPktInfo->oSrcname.src_suffix, "BBA"); /* Suffix */

	join_srcname(&oPktInfo->oSrcname, oPktInfo->sSrcname);
	if (oConfig.bVerboseModeFlag)
		elog_debug(1, "parseBBAPacket(): Sourcename evaluates to %s\n",
				oPktInfo->sSrcname);

	/*
	 * Extract channel ids, translate them to channel names, and put them into a format suitable for an orb header
	 */
	memset(aChan, 0, 128);
	memcpy(aChan, "_", strlen("_"));

	iDataOffset = oPktInfo->iHdrSize;
	for (i = 0, iChIdx = 0; i < oPktInfo->iNChan; i++) {
		//usVal = 0;
		//memcpy((char *) &usVal, &aBBAPkt[iDataOffset], 1); /* header size */
		//iChId = ntohs(usVal);
		iChId = aBBAPkt[iDataOffset];

		usVal = 0;
		memcpy((char *) &usVal, &aBBAPkt[iDataOffset + BBA_CHBYTES_OFF], 2);
		iChBytes = ntohs(usVal);

		sChName = getBBAChNameFromId(ucBBAPktType,
				oPktInfo->oSrcname.src_subcode, oPktInfo->iStaID, iChId);
		if (sChName == NULL) {
			elog_complain(0,
					"parseBBAPacket(): an error occured retrieving the channel name");
			return RESULT_FAILURE;
		}
		strcat(aChan, sChName);
		iChIdx += strlen(sChName);
		free(sChName);
		strcat(aChan, "_");
		iChIdx++;
		iDataOffset += BBA_CHHDR_SIZE + iChBytes;
	}
	aChan[iChIdx] = '\0';
	strcpy(oPktInfo->sChNames, aChan);

	/* If we got this far, everything parsed and looked up ok */
	return RESULT_SUCCESS;
}

/*
 * Look up channel names based on the SubCode, Station Id, and Channel Id
 *
 * Parameters:
 * 	ucBBAPktType
 * 		one of: BBA_DAS_DATA, BBA_DAS_STATUS, BBA_DC_STATUS, BBA_RTX_STATUS
 * 	sSubCode
 * 		the translation of the BBAPktType to a string, usually the str_subcode
 * 		part of a Srcname structure
 * 	iStaId
 * 		the station Id (SID) field from the Site table
 * 	iChId
 * 		the Channel Id (COMP) field from the Site table
 *
 * Returns:
 * A pointer to a string containing the channel name. If ucBBAPktType is
 * BBA_DAS_DATA and their is no corresponding entry in the Site table for the
 * combination of sSubCode, iStaId, and iChId, a dummy channel name is
 * generated based on sSubCode and iChId. For all other packet types, a null
 * pointer is returned if the lookup fails.
 */
static char *getBBAChNameFromId(uint8_t ucBBAPktType, char *sSubCode,
		int iStaId, int iChId) {
	char key[64], *name = NULL, *cherr = NULL;
	struct stSiteEntry *oSiteEntry;
	static Arr *oErrMsg;

	switch (ucBBAPktType) {

	case BBA_DAS_DATA:

		sprintf(key, "BBA/%s_%d_%d", sSubCode, iStaId, iChId);
		if ((oSiteEntry = (struct stSiteEntry *) getarr(oConfig.oStaCh, key))
				!= NULL) {
			name = oSiteEntry->sSENS;
		} else {
			/* We can't find an entry so we'll make up a fake channel Id */
			sprintf(&key[0], "%s_%d", sSubCode, iChId);
			name = key;

			/* To prevent excessive noise in the logs, only complain
			 * if we haven't tried looking this up before */
			if (oErrMsg == NULL)
				oErrMsg = newarr(0);
			cherr = (char *) getarr(oErrMsg, key);
			if (cherr == NULL) {
				elog_complain(
						0,
						"getBBAChNameFromId: can't get channel name (SubCode=%s StaID=%d, ChId=%d)\n",
						sSubCode, iStaId, iChId);
				cherr = strdup(key);
				setarr(oErrMsg, key, cherr);
			}
		}
		break;

	case BBA_DAS_STATUS:

		sprintf(key, "%d", iChId);
		if ((name = getarr(oConfig.oDasID, key)) == NULL) {
			elog_log(
					0,
					"getBBAChNameFromId: can't get DAS parameter name (StaID=%d, ChId=%d)\n",
					iStaId, iChId);
		}
		break;

	case BBA_DC_STATUS:

		sprintf(key, "%d", iChId);
		if ((name = getarr(oConfig.oDcID, key)) == NULL) {
			elog_log(
					0,
					"getBBAChNameFromId: can't get DC parameter name (StaID=%d, ChId=%d)\n",
					iStaId, iChId);
		}
		break;

	case BBA_RTX_STATUS:

		sprintf(key, "%d", iChId);
		if ((name = getarr(oConfig.oRTXID, key)) == NULL) {
			elog_log(
					0,
					"getBBAChNameFromId: can't get RTX parameter name (StaID=%d, ChId=%d)\n",
					iStaId, iChId);
		}
		break;
	}

	if (name)
		return (strdup(name));
	else
		return (NULL);
}
