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
*   This code was created as part of the ROADNet project.
*   See http://roadnet.ucsd.edu/
* 
*   This code is designed to interface with the Davis Instruments
*   Vantage Pro 2 Weather Station.
*
*    Based on Code By : Todd Hansen    18-Dec-2003
*    This Code By     : Todd Hansen & Jason Johnson  17-Mar-2006 
*                                                    (St. Patricks Day)
*    Last Updated By  : Todd Hansen    18-July-2006
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
*      [st] Struct definition
*      [b]  Boolean (psuedo) variable (use FALSE and TRUE constants below)
*/


/*
**  Constants
*/
#define CONNECT_SERIAL          1
#define CONNECT_TCP_IP          2
#define MAX_CONNECT_ATTEMPTS    10  /* Max tries to connect via serial/TCP */
#define MAX_DAVIS_WAKE_ATTEMPTS 3   /* Max tries to wake up the Davis */
#define MAX_DAVIS_PAGE_ATTEMPTS 10  /* Max tries to read a page of Davis data*/
#define DEFAULT_REPEAT_INTERVAL 3600
#define DEFAULT_SERIAL_SPEED    "19200"
#define FLUSH_WAIT              3   /* number of seconds to wait for data
				       before flushing data file descriptor. 
				       This will cause the program to wait 
				       this many seconds even if no data 
				       arrives. */

#define MAXDAVISDELAY             10 /* number of seconds to wait for
					an expected response before failing.
				        if the response is sooner, we will exit
					before this time expires. */
#define RESULT_SUCCESS          0
#define RESULT_FAILURE          -1
#define INVALID_HANDLE          -1
#define min(a, b)               (a < b ? a : b)
#define strsame(s1, s2)          (!strcmp ((s1), (s2)))
#define FALSE                   0
#define TRUE                    1
#define MAX_SERIAL_TIME_MS      255
#define MAX_BUFFER_SIZE         5000

/* Davis special character codes */
char DAVIS_CR                 = 0x0d;
char DAVIS_LF                 = 0x0a;
char DAVIS_ACK                = 0x06;
char DAVIS_NAK                = 0x21;
char DAVIS_CANCEL             = 0x18;

/* Davis EEPROM memory locations */
char DAVIS_EE_GMT_OFFSET      = 0x14;
char DAVIS_EE_GMT_OR_ZONE     = 0x16;

/* Davis command types */
#define COMMAND_OK              1
#define COMMAND_LF              2
#define COMMAND_ACK             3
#define COMMAND_NORESP          4
#define COMMAND_OK_NOEXTRA      5

#define ORB_GAP_FILL            2147483647
#define MAXRECORDCOUNTPKT       2565

#ifndef INADDR_NONE
#define INADDR_NONE             ((unsigned long int) 0xffffffff)
#endif
/*
**  TypeDefs
*/
typedef unsigned char ubyte;
typedef char sbyte;

/* Holds configuration data from commandline & paramfile. */
struct stConfigData {
  int   bVerboseModeFlag;
  int   iRepeatInterval;
  int   iDavisSampleInterval;
  int   iDavisSampleInterval_toset;
  int   iConnectionType; 
  int   iBaudRateTermios;
  char  *sConnectionParams [2];
  char  *sBaseSrcName;        
  char  *sOrbName;            
  char  *sParamFileName;      
  char  *sStateFileName;      
  char  *sTimeZone;           
  int   bMeasureTimeSkewFlag; 
  int   bAutoAdjustSleepFlag; 
  int   bSkipDavisRateSetFlag;
  int   bForceIgnoreTiming;
  int   bKickStateFile;
  int   bRXCheckFlag;
  int   bSetDavisClock;             
  int   bConfigScreenMode;             
  int   bInitalizeDavis;      
  int   bAutoProgramDavis;
};

/* Holds a single archive record (used with DMP and DMPAFT commands) */
struct stArchiveData {
  unsigned short   iDateStamp;
  unsigned short   iTimeStamp;
  short            iOutsideTemp;
  short            iHighOutsideTemp;
  short            iLowOutsideTemp;
  unsigned short   iRainLevel;
  unsigned short   iHighRainRate;
  unsigned short   iBarometer;
  short            iSolarRadiation;
  unsigned short   iWindSampleCount;
  short            iInsideTemp;
  ubyte            iInsideHumidity;
  ubyte            iOutsideHumidity;
  ubyte            iAvgWindSpeed;
  ubyte            iHighWindSpeed;
  ubyte            iHighWindSpeedDir;
  ubyte            iPrevailingWindDir;
  ubyte            iAvgUVIndex;
  ubyte            iET;                 /* EvapoTranspiration */
  short            iHighSolarRadiation;
  ubyte            iHighUVIndex;
  ubyte            iForecastRule;
  ubyte            aLeafTemps [2];
  ubyte            aLeafMoistures [2];
  ubyte            aSoilTemps [4];
  ubyte            iDownloadRecType;
  ubyte            aExtraHumidities [2];
  ubyte            aExtraTemps [3];
  ubyte            aSoilMoistures [4];  
};

/* Holds a "page" of Davis archive data */
struct stArchivePage {
  unsigned char         iSequenceNumber;
  struct stArchiveData  aArchiveData [5];
  char                  aUnusedBytes [4];
  unsigned short        iCRC;
};

/* Holds a Davis date/time stamp (4 bytes + 2 byte CRC) */
struct stDavisDateTime {
  unsigned short   iDateStamp;
  unsigned short   iTimeStamp;
  unsigned short   iCRC;
};

/* Holds a Davis DMPAFT request response block */
struct stDavisDMPAFTResponse {
  unsigned short   iRecordCount;
  unsigned short   iFirstValidRecord;
  unsigned short   iCRC;
};

/* Holds a Davis "GETTIME/SETTING" data block */
struct stDavisGetSetTime {
  ubyte            iSecond;
  ubyte            iMinute;
  ubyte            iHour;
  ubyte            iDay;
  ubyte            iMonth;
  ubyte            iYearMinus1900;
  unsigned short   iCRC;
};

/* Holds RXCheck data. */
struct stDavisRXCheckData {
  unsigned int   iPacketsReceived;
  unsigned int   iPacketsMissed;
  unsigned int   iResyncCount;
  unsigned int   iLargestPacketBlock;
  unsigned int   iCRCErrorCount;
};


/*
** 16-bit CRC lookup table.  Note, this is NOT a CRC-16 table, but rather a
** CRC-CCITT table.
*/
unsigned short aCRCTable [] = {
  0x0000, 0x1021, 0x2042, 0x3063, 0x4084, 0x50a5, 0x60c6, 0x70e7, 
  0x8108, 0x9129, 0xa14a, 0xb16b, 0xc18c, 0xd1ad, 0xe1ce, 0xf1ef,   
  0x1231, 0x0210, 0x3273, 0x2252, 0x52b5, 0x4294, 0x72f7, 0x62d6, 
  0x9339, 0x8318, 0xb37b, 0xa35a, 0xd3bd, 0xc39c, 0xf3ff, 0xe3de, 
  0x2462, 0x3443, 0x0420, 0x1401, 0x64e6, 0x74c7, 0x44a4, 0x5485, 
  0xa56a, 0xb54b, 0x8528, 0x9509, 0xe5ee, 0xf5cf, 0xc5ac, 0xd58d, 
  0x3653, 0x2672, 0x1611, 0x0630, 0x76d7, 0x66f6, 0x5695, 0x46b4, 
  0xb75b, 0xa77a, 0x9719, 0x8738, 0xf7df, 0xe7fe, 0xd79d, 0xc7bc, 
  0x48c4, 0x58e5, 0x6886, 0x78a7, 0x0840, 0x1861, 0x2802, 0x3823, 
  0xc9cc, 0xd9ed, 0xe98e, 0xf9af, 0x8948, 0x9969, 0xa90a, 0xb92b, 
  0x5af5, 0x4ad4, 0x7ab7, 0x6a96, 0x1a71, 0x0a50, 0x3a33, 0x2a12, 
  0xdbfd, 0xcbdc, 0xfbbf, 0xeb9e, 0x9b79, 0x8b58, 0xbb3b, 0xab1a, 
  0x6ca6, 0x7c87, 0x4ce4, 0x5cc5, 0x2c22, 0x3c03, 0x0c60, 0x1c41, 
  0xedae, 0xfd8f, 0xcdec, 0xddcd, 0xad2a, 0xbd0b, 0x8d68, 0x9d49, 
  0x7e97, 0x6eb6, 0x5ed5, 0x4ef4, 0x3e13, 0x2e32, 0x1e51, 0x0e70, 
  0xff9f, 0xefbe, 0xdfdd, 0xcffc, 0xbf1b, 0xaf3a, 0x9f59, 0x8f78, 
  0x9188, 0x81a9, 0xb1ca, 0xa1eb, 0xd10c, 0xc12d, 0xf14e, 0xe16f, 
  0x1080, 0x00a1, 0x30c2, 0x20e3, 0x5004, 0x4025, 0x7046, 0x6067, 
  0x83b9, 0x9398, 0xa3fb, 0xb3da, 0xc33d, 0xd31c, 0xe37f, 0xf35e, 
  0x02b1, 0x1290, 0x22f3, 0x32d2, 0x4235, 0x5214, 0x6277, 0x7256, 
  0xb5ea, 0xa5cb, 0x95a8, 0x8589, 0xf56e, 0xe54f, 0xd52c, 0xc50d, 
  0x34e2, 0x24c3, 0x14a0, 0x0481, 0x7466, 0x6447, 0x5424, 0x4405, 
  0xa7db, 0xb7fa, 0x8799, 0x97b8, 0xe75f, 0xf77e, 0xc71d, 0xd73c, 
  0x26d3, 0x36f2, 0x0691, 0x16b0, 0x6657, 0x7676, 0x4615, 0x5634, 
  0xd94c, 0xc96d, 0xf90e, 0xe92f, 0x99c8, 0x89e9, 0xb98a, 0xa9ab, 
  0x5844, 0x4865, 0x7806, 0x6827, 0x18c0, 0x08e1, 0x3882, 0x28a3, 
  0xcb7d, 0xdb5c, 0xeb3f, 0xfb1e, 0x8bf9, 0x9bd8, 0xabbb, 0xbb9a, 
  0x4a75, 0x5a54, 0x6a37, 0x7a16, 0x0af1, 0x1ad0, 0x2ab3, 0x3a92, 
  0xfd2e, 0xed0f, 0xdd6c, 0xcd4d, 0xbdaa, 0xad8b, 0x9de8, 0x8dc9, 
  0x7c26, 0x6c07, 0x5c64, 0x4c45, 0x3ca2, 0x2c83, 0x1ce0, 0x0cc1, 
  0xef1f, 0xff3e, 0xcf5d, 0xdf7c, 0xaf9b, 0xbfba, 0x8fd9, 0x9ff8, 
  0x6e17, 0x7e36, 0x4e55, 0x5e74, 0x2e93, 0x3eb2, 0x0ed1, 0x1ef0 
};

