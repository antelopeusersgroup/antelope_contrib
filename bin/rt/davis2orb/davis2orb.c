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
*  This code was created as part of the ROADNet project.
*  See http://roadnet.ucsd.edu/
* 
*  This code is designed to interface with the Davis Instruments
*  Vantage Pro 2 Weather Station.
* 
*    Based on Code By : Todd Hansen    18-Dec-2003
*    This Code By     : Todd Hansen & Jason Johnson  18-Apr-2006 
*                                                    (Anniversary of 1906 Eq)
*    Last Updated By  : Todd Hansen    18-July-2006
*    Moved to Contrib : Todd Hansen     9-Sep-2006
*
*
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
*      [b]  Boolean (psuedo) variable (use FALSE and TRUE constants defined
*           in "davis2orb.h")
*/


/*
**  Constants
*/
#define VERSION  "davis2orb $Revision$"


/*
**  Includes
*/
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <time.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <errno.h>
#include <strings.h>
#include <termios.h>
#include <time.h>
#include <math.h>
#include <orb.h>
#include <coords.h>
#include <stock.h>
#include <Pkt.h>
#include "davis2orb.h"


/*
**  Globals
*/
int                  iConnectionType = 0;
int                  iConnectionHandle = INVALID_HANDLE;
int                  orbfd=-1;
int                  skewlog;
int                  skewlogvalid=0;
Pf                   *configpf=NULL;
struct termios       oSavedTermios;
struct stConfigData  oConfig;

/*
** State Variables
*/
double               lastdownloadtimestamp=0;


/*
**  Prototypes
*/
unsigned short htorns (unsigned short iValue);
ubyte getHighLowByte (int bReturnLow, short iValue);
unsigned short calcCRC_CCITTBuffer (unsigned char aData [], int iBufSize);
int resolveNumericBaudRate (long iIntBaudRate);
void closeAndFreeHandle (int *iHandle);
int setFileBlocking (int *iHandle, int iBlocking);
int flushOutput (int *iHandle);
int doReadBytes (int *iHandle, char *sBuffer, int iByteCount, int bBlocking);
int doWriteBytes (int *iHandle, char *sBuffer, int iByteCount);
int readLine (int *iHandle, char *sBuffer, char cEOLNMarker);
int connectSocket (char *sHost, int iPort);
int connectSerial (char *sDeviceName, int iBaudRateTermios);
int davisConnect (int iConnType, char *sConnectionParams []);
int davisWakeUp (void);
int davisExecCommand (char sCommand [], char *sResponse, int iCommandType);
void davisCleanup (int iExitCode);
void davisMakeDateTime (unsigned char *dtRecord,
                        int iYear, int iMonth, int iDay,
                        int iHour, int iMinute);
double davisDateTimeToEpoch(unsigned short iDateStamp, unsigned short iTimeStamp);
int davisExecDMPAFT (int iYear, int iMonth, int iDay, int iHour, int iMinute, int bAllData);
int getDavisBatt(int *iHandle, int *batt, int *tranbat, int *bartrend);
double davisGetTime ();
int davisSetTime ();
int davisSetTimeZone (float fGMTOffset);
int davisSetScreenMode ();
int davisGetRXCheck(struct stDavisRXCheckData *oDavisRXCheck);
void showCommandlineUsage (void);
int parseCommandLineOptions (int iArgCount, char *aArgList []);
int paramFileRead();
int StatPacket(int *iHandle);
int davisSetSampleRate (int iSampleRate);
int sendPkt(struct stArchiveData *aArchiveData,int iRecordCount, int firstvalidrec, double aftertime);
void unpackArchivePage(struct stArchivePage *ap, unsigned char *buf);
void unpackDMPAFTResp(struct stDavisDMPAFTResponse *resp, unsigned char *buf);
void unpackstDavisGetSetTime(struct stDavisGetSetTime *t, unsigned char *buf);
void packstDavisGetSetTime(struct stDavisGetSetTime *t, unsigned char *buf);

PktChannel* buildChannel(char *sChan_Name, int *data, int numsamp, double samprate, double firstsampletime, double calib, Srcname srcparts);


/*
**  Utility functions
*/

/*
**  Takes an unsigned short value and converts it to reverse network byte order (little endian)
**  Note: network byte order: big endian (all data to/from davis2orb is little endian except CRC.
**
*/
unsigned short htorns (unsigned short iValue)
{

  ubyte  iLowByte;
  ubyte  iHighByte;
  unsigned short iResult;
  
  iResult=ntohs(iValue);

  /* Extract the high byte */
  iHighByte = (iResult >> 8);
  
  /* Extract the low byte */
  iLowByte = (iResult & 0x00FF);

  /* Return combined results */
  iResult = (iLowByte << 8) + iHighByte;
  return iResult;
}


/*
**  Returns the high or low byte of the specified value.
**
**  if bReturnLow is "TRUE", then the LOW byte is returned, else the HIGH
**  byte is returned.
*/
ubyte getHighLowByte (int bReturnLow, short iValue) {

  ubyte iReturnValue = 0;

  /* Extract the low byte */
  if (bReturnLow == TRUE) 
    iReturnValue = (iValue & 0x00FF);

  /* Extract the high byte */
  else
    iReturnValue = (iValue >> 8);

  return iReturnValue;
}


/*
**  Calculates a 16-bit CRC of a buffer with starting CRC of 0x0000.
*/
unsigned short calcCRC_CCITTBuffer (unsigned char aData [], int iBufSize) {

  /* Initialize */
  unsigned short iCRC;
  int            lcv;

  /* Build the CRC value */
  iCRC = 0;
  for (lcv = 0; lcv < iBufSize; lcv ++)
    {
      iCRC = aCRCTable[(iCRC >> 8) ^ aData[lcv]] ^ iCRC << 8;
    }

  /* Return results */
  return iCRC; 

}


/*
**  Takes an integer serial speed and finds the appropriate "termios" constant.
**
**  Parameters:
**
**    [iIntBaudRate]  Longint value specifying the baud rate.
**                         
**  Returns:
**
**    Bxxxxxx Termios constant on success, RESULT_FAILURE otherwise.
*/
int resolveNumericBaudRate (long iIntBaudRate) {

  /* Initialize */
  long iTermiosBaud = RESULT_FAILURE; 

  /* Try to match it up */
  switch (iIntBaudRate) {
    case 19200: /* default: */
      iTermiosBaud = B19200;
      break;
  /*case 14400:  Not Supported by OS, supported by Davis
      iTermiosBaud = B14400;
      break; */
    case 9600:
      iTermiosBaud = B9600;
      break;
    case 4800:
      iTermiosBaud = B4800;
      break;
    case 2400:
      iTermiosBaud = B2400;
      break;
    case 1200:
      iTermiosBaud = B1200;
      break;
  }

  /* Return results */
  return iTermiosBaud;
}


/*
**  Closes the specified file handle and sets it to INVALID_HANDLE to prevent
**  further read/write ops on the handle.
*/
void closeAndFreeHandle (int *iHandle) {

  /* Only close it if it's not already closed */
  if (*iHandle != INVALID_HANDLE) {
    close (*iHandle);
    *iHandle = INVALID_HANDLE;
  }
}


/*
**  Sets the BLOCKING state of the specified file descriptor.
**
**  Parameters:
**
**    [*iHandle]   The file descriptor to operate on
**    [iBlocking]  Integer value representing the state of BLOCKING
**                 (0 = Disable; 1 = Enable) 
**
**  Returns:
**
**    RESULT_SUCCESS on success, RESULT_FAILURE otherwise.
*/
int setFileBlocking (int *iHandle, int iBlocking) {

  int iTempVal = 0;

  /* Initialize */
  iTempVal = fcntl (*iHandle, F_GETFL, 0);
  if (iTempVal == -1)
    {
      elog_complain(1,"setFileBlocking: fcntl(*fd,F_GETFL,0)");
      return RESULT_FAILURE;
    }

  /* Set the BLOCKING mode */
  if (iBlocking == 1)
    iTempVal &= ~O_NONBLOCK;
  else  
    iTempVal |= O_NONBLOCK;

  /* Set the state */
  iTempVal = fcntl (*iHandle, F_SETFL, iTempVal);
  if (iTempVal == -1)
    {
      elog_complain(1,"setFileBlocking: fcntl(*fd,F_SETFL,iTempVal)");
      return RESULT_FAILURE;
    }

  /* Return results */
  return RESULT_SUCCESS;
}


/*
**  Flushes the output stream of the specified file descriptor.
**
**  Returns:
**
**    RESULT_SUCCESS on success, RESULT_FAILURE otherwise.
*/
int flushOutput (int *iHandle) {

  int ret;

  /* Initialize */
  char cBuffer = '\0';

  if (*iHandle<0)
    {
      elog_complain(0,"davis connection closed, yet flushOutput() was called\n");
      return RESULT_FAILURE;
    }

  /* Turn off BLOCKING on the connection */
  if (setFileBlocking (iHandle, FALSE) == RESULT_FAILURE)
    {
      elog_complain(0,"flushOutput: failed to set device to non-blocking\n");
      return RESULT_FAILURE;
    }
  else
    sleep (FLUSH_WAIT);

  /* Read until there is no more data */
  while ((ret=read(*iHandle, &cBuffer, 1))>0) 
    {
      /* Do nothing */
    }

  if (ret<0 && errno!=EAGAIN)
    {
      elog_complain(0,"flushOutput: failed\n");
      return RESULT_FAILURE;
    }

  /* Restore BLOCKING on the connection */
  return setFileBlocking (iHandle, TRUE);
}


/*
**  Wrapper for read() command; handles errors and checks the file descriptor 
**  for validity before attempting to read.
**
**  Returns the number of bytes read (>=0), or RESULT_FAILURE if there was
**  a problem.
*/
int doReadBytes (int *iHandle, char *sBuffer, int iByteCount, int bBlocking) {

  int iReturnVal = 0;
  int iBytesRead = 0;
  fd_set readfd;
  fd_set except;
  struct timeval timeout;
  int selret;

  /* Check for valid handle */
  if (*iHandle == INVALID_HANDLE)
    return RESULT_FAILURE;

  if (setFileBlocking (iHandle, FALSE) == RESULT_FAILURE)
    {
      elog_complain(0,"doReadBytes: failed to set device to non-blocking\n");
      close(*iHandle);
      *iHandle=INVALID_HANDLE;
      return RESULT_FAILURE;
    }

  while (1) {
        timeout.tv_sec=MAXDAVISDELAY;
        timeout.tv_usec=0;

        FD_ZERO(&readfd);
        FD_SET(*iHandle,&readfd);

        FD_ZERO(&except);
        FD_SET(*iHandle,&except);

        selret=select(*iHandle+1,&readfd,NULL,&except,&timeout);

        if (selret<0)
        {
	  elog_complain(1,"doReadBytes: select() on read failed");
	  close(*iHandle);
	  *iHandle=INVALID_HANDLE;
	  return RESULT_FAILURE;
        }
        else if (!selret)
        {
	  if (bBlocking == TRUE)
	    {
	      elog_complain(0,"doReadBytes: timed out (%d seconds) in select()\n",MAXDAVISDELAY);
	      close(*iHandle);
	      *iHandle=INVALID_HANDLE;
	      return RESULT_FAILURE;
	    }
	  else
	    return iBytesRead;
        }
	else
	{

	  iReturnVal = read (*iHandle, &(sBuffer[iBytesRead]), iByteCount);
	  
	  /* See if we need to error out */
	  if ((iReturnVal < 0) && (errno != EAGAIN)) {
	    elog_complain (1, "doReadBytes: Error encountered during read from Davis:");
	    close(*iHandle);
	    *iHandle=INVALID_HANDLE;
	    return RESULT_FAILURE;
	  }
	  else if (errno == EAGAIN && bBlocking == FALSE)
	    {
	      return 0;
	    }
	  else if (iReturnVal > 0) {
	    iBytesRead += iReturnVal;
	    if (iBytesRead == iByteCount) {
	      /* Restore BLOCKING on the connection */
	      if (setFileBlocking (iHandle, TRUE) ==  RESULT_FAILURE)
		{
		  close(*iHandle);
		  *iHandle=INVALID_HANDLE;
		  return RESULT_FAILURE;
		}
	      
	      return iBytesRead;
	    }
	  }
	}
  }
}
  

/*
**  Wrapper for read() command; handles errors and checks the file descriptor 
**  for validity before attempting to write.
**
**  Returns the number of bytes written (>=0), or RESULT_FAILURE if there was
**  a problem.
*/
int doWriteBytes (int *iHandle, char *sBuffer, int iByteCount) {

  /* Initialize */
  int iResult = RESULT_FAILURE;

  /* Check for valid handle */
  if (*iHandle == INVALID_HANDLE)
    elog_complain (0, "doWriteBytes: called with invalid file descriptor.\n");

  /* Else it's valid, try writing */
  else {
    iResult = write (*iHandle, sBuffer, iByteCount);
    if (iResult < iByteCount) {
      elog_complain (1, "doWriteBytes: Error calling 'write' (error code=%u, %d bytes written):", errno,iResult);
      close(*iHandle);
      *iHandle=INVALID_HANDLE;
      return RESULT_FAILURE;
    }
  }

  /* Return results */
  return iResult;
}


/*
**  Reads from the specified file descriptor until 'cEOLNMarker' encountered,
**  checking for overflow.
*/
int readLine (int *iHandle, char *sBuffer, char cEOLNMarker) {

  /* Initialize */
  int  lcv   = 0;

  /* Read a line */
  while (lcv ++ < MAX_BUFFER_SIZE) {

    /* Attempt to read a byte */
    if (doReadBytes (iHandle, &sBuffer[lcv - 1], 1, TRUE) == RESULT_FAILURE) {
      elog_complain (1, "readLine(): Error encountered in doReadBytes() inside read line loop.");
      closeAndFreeHandle (iHandle);
      return RESULT_FAILURE;
    }

    /* Stop at <CR> */
    if (sBuffer[lcv - 1] == cEOLNMarker)	{
	if (cEOLNMarker == DAVIS_CR)
	     sBuffer [lcv - 2] = '\0';
	  else
	     sBuffer [lcv - 1] = '\0';

      return RESULT_SUCCESS;
    }

    if (sBuffer[lcv - 1] == DAVIS_CANCEL && cEOLNMarker == DAVIS_ACK)
      {
	elog_complain(0,"readLine: Opps, the davis responded with a cancel. We must have messed with it.\n");
	return RESULT_FAILURE;
      }
  }

  /* 
  **  If we got this far, and there was no length limit imposed,
  **  there were too many characters.
  */
  if (setFileBlocking (iHandle, FALSE) == RESULT_FAILURE)
    elog_complain (1, "readLine(): Failure calling setFileBlocking() at end of function.");
  elog_complain (0, "readLine(): Buffer overflow (c = %c)\n", sBuffer [lcv - 1]);
  closeAndFreeHandle (iHandle);
  return RESULT_FAILURE;
}


/*
**  Attempts to open a TCP/IP connection to the specified host & port with keep
**  alives enabled.  If the connection is successful, the global 
**  "iConnectionHandle" is set to the handle of the connection.
**
**  Parameters:
**
**    [sHost]  String value specifying the host IP address or name. 
**    [iPort]  Integer value specifying the host port.
**                         
**  Returns:
**
**    RESULT_SUCCESS on success, RESULT_FAILURE otherwise.
*/
int connectSocket (char *sHost, int iPort) {

  /* Initialize */
  unsigned long       iNetAddress;
  int                 iConnectionResult = 1;
  struct hostent      *oHostEnt;
  struct sockaddr_in  oAddress;
  int                 iSocketOptVal = 1;


  /* Check for host already being a numeric IP address */ 
  if ((iNetAddress = inet_addr (sHost)) != INADDR_NONE)
    memcpy (&oAddress.sin_addr, &iNetAddress,
            min (sizeof (iNetAddress), sizeof (oAddress.sin_addr)));

  /* Else resolve name to IP address */
  else {
    oHostEnt = gethostbyname (sHost);
    if (oHostEnt == NULL)	{  
      elog_complain (1, "connectSocket(): Could not resolve address '%s'.",
                     sHost);
      return RESULT_FAILURE;
    }
    memcpy (&oAddress.sin_addr, oHostEnt -> h_addr,
            min (oHostEnt -> h_length, sizeof (oAddress.sin_addr)));
  }

  /* Create socket */
  if ((iConnectionHandle = socket (AF_INET, SOCK_STREAM, 0)) == -1) {
    elog_complain (1, "connectSocket(): Could not create socket.");
    return RESULT_FAILURE;
  }
  
  /* Extract address from host entry */
  oAddress.sin_family = AF_INET;
  oAddress.sin_port = htons (iPort);
  if (oConfig.bVerboseModeFlag == TRUE)
    elog_notify (0, "connectSocket(): Connecting to '%s' on port %i.\n", sHost, iPort);
  
  /* Try connecting */
  iConnectionResult = connect (iConnectionHandle, (struct sockaddr *)&oAddress,
                               sizeof (oAddress));
  if (iConnectionResult) {
    elog_complain (1, "connectSocket(): Could not connect socket:");
    close (iConnectionHandle);
    return RESULT_FAILURE;
  }

  /* If connection is successful, try enabling TCP/IP KEEPALIVE mode */
  if (setsockopt (iConnectionHandle, SOL_SOCKET, SO_KEEPALIVE, &iSocketOptVal,
                  sizeof (int))) {
    elog_complain (0, "connectSocket(): Failure calling setsockopt (SO_KEEPALIVE)\n");
    return RESULT_FAILURE;
  }
  
  /* If we got this far, everything was successful */
  return RESULT_SUCCESS;
}


/*
**  Attempts to open a serial connection to the specified device at the specified
**  baud rate.  If the connection is successful, the global "iConnectionHandle"
**  is set to the handle (file descriptor) of the connection.
**
**  Parameters:
**
**    [sDeviceName]       String value specifying the device. 
**    [iBaudRateTermios]  Integer value specifying the baud rate to connect to;
**                        it is a "termios" constant.  Supported values are:
**                        B50, B75, B110, B134, B150, B200, B300, B600, B1200,
**                        B1800, B2400, B4800, B9600, and B19200
**                         
**  Returns:
**
**    RESULT_SUCCESS on success, RESULT_FAILURE otherwise.
*/
int connectSerial (char *sDeviceName, int iBaudRateTermios) {

  /* Initialize */
  struct termios oTempTermios;

  /* Attempt to open the device in READ/WRITE mode */
  iConnectionHandle = open (sDeviceName, O_RDWR);
  if (iConnectionHandle < 0) {
    elog_complain (1, "connectSerial(): Unable to open serial port device '%s'.",
                   sDeviceName);
    return RESULT_FAILURE;
  }

  /* Save the old Termios settings */
  if (tcgetattr (iConnectionHandle, &oTempTermios) < 0) {
    elog_complain (1, "connectSerial(): Unable to get serial attributes (device=%s)",
                   sDeviceName);
    return RESULT_FAILURE;
  }
  oSavedTermios = oTempTermios;

  /* Set baud rate and other serial options */
  cfsetispeed (&oTempTermios, iBaudRateTermios);
  cfsetospeed (&oTempTermios, iBaudRateTermios);
  oTempTermios.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  oTempTermios.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  oTempTermios.c_cflag &= ~(CSIZE | PARENB);
  oTempTermios.c_cflag |= CS8;
  oTempTermios.c_oflag &= ~OPOST;
  oTempTermios.c_cc [VMIN] = 0;
  oTempTermios.c_cc [VTIME] = MAX_SERIAL_TIME_MS;
  if (tcsetattr (iConnectionHandle, TCSANOW, &oTempTermios) < 0) {
    elog_complain (1, "connectSerial(): Unable to set serial attributes (device=%s)",
                   sDeviceName);
    return RESULT_FAILURE;
  }

  /* If we got here, return RESULT_SUCCESS */
  return RESULT_SUCCESS;
}


/*
**  Davis interface functions
*/

/*
**  Connects to the Davis weather station via serial or TCP/IP network.  This
**  function will try up to MAX_CONNECT_ATTEMPTS times before erroring out.
**  
**  Parameters:
**
**    [iConnType]         Integer value specifying connection type;
**                         CONNECT_SERIAL = Serial,
**                         CONNECT_TCP_IP = Network TCP/IP
**
**    [aConnectionParams] Array of the connection parameters;
**                         TCP/IP = <host>,<port>
**                                  (e.g. "192.168.1.2,4001" or "networkname,4001")
**                         Serial = <device name>,<baud rate>
**                                  (e.g. "/dev/ttyS0,19200")
**                         
**  Returns:
**
**    RESULT_SUCCESS on success, RESULT_FAILURE otherwise.
*/
int davisConnect (int iConnType, char *sConnectionParams []) {

  /* Initialize */
  int iAttemptsMade = 0;
  int iResult = RESULT_SUCCESS;

  /* Check for invalid params */
  if ((sConnectionParams [0] == NULL) || (sConnectionParams [1] == NULL)) {
    elog_complain (0, "davisConnect(): Bad connection parameters.  "
                      "Expected '<host>,<port>' or "
                      "'<device name>,<baud rate>'.\n");
    return RESULT_FAILURE;
  }

  /* Main loop */
  if (oConfig.bVerboseModeFlag == TRUE)
    elog_notify (0, "davisConnect(): Connecting to the Davis...\n");
  while (iAttemptsMade < MAX_CONNECT_ATTEMPTS) {

    /* Handle TCP/IP connections */
    if (iConnType == CONNECT_TCP_IP)
      iResult = connectSocket (sConnectionParams [0], atoi (sConnectionParams [1]));

    /* Handle Serial connections */
    else if (iConnType == CONNECT_SERIAL)
      iResult = connectSerial (sConnectionParams [0], oConfig.iBaudRateTermios);

    /* If successful, break out of loop */
    if (iResult == RESULT_SUCCESS)
      break;

    /* Increment attempt counter */
    iAttemptsMade ++;
    sleep(3);
  }

  /* If we failed too many times, log error */
  if (iAttemptsMade >= MAX_CONNECT_ATTEMPTS) {
    elog_complain (0, "davisConnect(): Aborting after %i failed connection attempts.\n",
                   iAttemptsMade);
    return RESULT_FAILURE;
  }

  /* Return results */
  iConnectionType = iConnType;
  if ((iResult == RESULT_SUCCESS) && (oConfig.bVerboseModeFlag == TRUE))
    elog_notify (0, "davisConnect(): Connected to the Davis.\n");
  return iResult;
}


/*
**  Wakes up the Davis.  Tries up to MAX_DAVIS_WAKE_ATTEMPTS times.
**  Returns RESULT_SUCCESS if Davis woke up, RESULT_FAILURE otherwise.
*/
int davisWakeUp (void) {

  /* Initialize */
  char cBuffer       = '\0';
  int  iResult       = RESULT_FAILURE;
  int  iAttemptsMade = 0;
  int  iReturnVal    = 0;
  int  iQuitFlag     = 0;
  fd_set readfd;
  fd_set except;
  struct timeval timeout;
  int selret;

  /* Flush the serial port output so we can start fresh */
  if (flushOutput (&iConnectionHandle) == RESULT_FAILURE) { 
    elog_complain (1, "davisWakeUp(): Failure calling flushOutput().");
    return RESULT_FAILURE;
  }

  /* Try MAX_DAVIS_WAKE_ATTEMPTS times */
  if (oConfig.bVerboseModeFlag == TRUE)
    elog_notify (0, "davisWakeUp(): Attempting to wake up the Davis...\n");
  while ((iAttemptsMade < MAX_DAVIS_WAKE_ATTEMPTS) && (iQuitFlag == 0)) {

    /* Send <LF> to wake up the davis and wait */
    if (doWriteBytes (&iConnectionHandle, "\n", 1) == RESULT_FAILURE) {
      iQuitFlag = 1;
      iResult = RESULT_FAILURE;
      break;
    }

    timeout.tv_sec=3;
    timeout.tv_usec=0;
    
    FD_ZERO(&readfd);
    FD_SET(iConnectionHandle,&readfd);
    
    FD_ZERO(&except);
    FD_SET(iConnectionHandle,&except); 

    selret=select(iConnectionHandle+1,&readfd,NULL,&except,&timeout);
    if (selret<0)
    {
	elog_complain(1,"davisWakeUp: select() on read failed");
	close(iConnectionHandle);
	iConnectionHandle=INVALID_HANDLE;
	return RESULT_FAILURE;
    }
    else if (selret)
    {
	if (setFileBlocking (&iConnectionHandle, FALSE) == RESULT_FAILURE)
	{
	    elog_complain(0,"davisWakeUp: failed to set device to non-blocking\n");
	    close(iConnectionHandle);
	    iConnectionHandle=INVALID_HANDLE;
	    return RESULT_FAILURE;
	}

	/* Try reading the response (we are looking for another <CR> back) */
	while (iReturnVal = read(iConnectionHandle, &cBuffer, 1)>0) 
	{
	    
	    /* If we found the <LF>, set success and break out */ 
	    if (cBuffer == '\n') 
	    {
		if (oConfig.bVerboseModeFlag == TRUE)
		    elog_notify (0, "davisWakeUp(): Successfully woke up the Davis.\n");
		iQuitFlag = 1;
		iResult = RESULT_SUCCESS;
		break;
	    }
	}
	
	/* Else see if we need to error out */
	if ((iReturnVal < 0) && (errno != EAGAIN)) {
	    elog_complain (1, "davisWakeUp(): Unable to read from Davis.");
	    closeAndFreeHandle (&iConnectionHandle);
	    iQuitFlag = 1;
	    break;
	}

	/* Restore BLOCKING on the connection */
	if (setFileBlocking (&iConnectionHandle, TRUE) ==  RESULT_FAILURE)
	{
	    close(iConnectionHandle);
	    iConnectionHandle=INVALID_HANDLE;
	    return RESULT_FAILURE;
	}
    }

    
    /* Increment attempt counter and try again, if needed */
    iAttemptsMade ++;
    if (iResult == RESULT_FAILURE) {
	elog_notify (0, "davisWakeUp(): Unable to wake up the Davis (attempt %i of %i). Retrying...\n", iAttemptsMade, MAX_DAVIS_WAKE_ATTEMPTS);
    }
  }

  /* If failed to wake up the Davis, log such */
  if (iResult == RESULT_FAILURE)
      elog_complain (0, "davisWakeUp(): Unable to wake up the Davis after %i attempts.\n", MAX_DAVIS_WAKE_ATTEMPTS);
  
  /* Flush the serial port output so we can start fresh */
  if (flushOutput (&iConnectionHandle) == RESULT_FAILURE) { 
      elog_complain (0, "davisWakeUp(): Failure calling flushOutput after waking up the davis.\n");
      return RESULT_FAILURE;
  }

  /* Return results */
  return iResult;
}


/*
**  Sends a command to the Davis and reads back the response.  Waits for the
**  Davis to respond and then reads & returns the response.
**
**  Returns RESULT_SUCCESS if command is successful, RESULT_FAILURE otherwise.
*/
int davisExecCommand (char sCommand [], char *sResponse, int iCommandType) {

  int iResult = RESULT_SUCCESS;

  /* Send the command */
  if (oConfig.bVerboseModeFlag == TRUE)
    elog_notify (0, "davisExecCommand(): Sending command '%s' to Davis...\n",
		 sCommand);
  if (doWriteBytes (&iConnectionHandle, sCommand, strlen (sCommand)) == RESULT_FAILURE)
    return RESULT_FAILURE;
  if (doWriteBytes (&iConnectionHandle, "\n", 1) == RESULT_FAILURE)
    return RESULT_FAILURE;
  if (oConfig.bVerboseModeFlag == TRUE)
    elog_notify (0, "davisExecCommand(): Command sent, waiting for response...\n",sCommand);
  
  /* Handle COMMAND_OK command type */
  if (iCommandType == COMMAND_OK) {
    
    /* Read the response until 'OK' hit */
    while (iResult == RESULT_SUCCESS) {
      iResult = readLine (&iConnectionHandle, sResponse, DAVIS_CR);
      if (strsame (sResponse, "OK"))
	break;
    }
    
    /* Only get the response if we found "OK" without errors */
    if (iResult == RESULT_SUCCESS)
      iResult = readLine (&iConnectionHandle, sResponse, DAVIS_CR);
    if (iResult == RESULT_SUCCESS) {
      if (oConfig.bVerboseModeFlag == TRUE)
	elog_notify (0, "davisExecCommand(): Response = '%s'\n", sResponse);
    }
  }
  else if (iCommandType == COMMAND_OK_NOEXTRA) {
    /* Read the response until 'OK' hit */
    sResponse[0]='\0';
    while (iResult == RESULT_SUCCESS && strncmp(sResponse, "OK",2)) {
      iResult = readLine (&iConnectionHandle, sResponse, DAVIS_CR);
    }
    return iResult;
  }    
  /* Handle COMMAND_LF command type */
  else if (iCommandType == COMMAND_LF) {
  /* this command is broke since most command return a LF from the original command. Hence it doesn't prove the command was successful!*/  
    /* Read the response until 'LF' hit */
    iResult = readLine (&iConnectionHandle, sResponse, DAVIS_LF);
  }
  
  /* Handle COMMAND_ACK command type */
  else if (iCommandType == COMMAND_ACK) {
    
    /* Read the response until 'ACK' hit */
    iResult = readLine (&iConnectionHandle, sResponse, DAVIS_ACK);
  }
  
  /* Return results */
  return iResult;
} 


/*
**  Performs "cleanup" tasks -- closes serial connection or TCP/IP socket.
*/
void davisCleanup (int iExitCode) {

  /* Log that we exited */
  elog_notify (0, "davisCleanup(): Exiting with code %i...\n", iExitCode);
  
  /* Close the connection handle */
  closeAndFreeHandle (&iConnectionHandle);

  /* Exit */
  exit (iExitCode);
}


/*
**  Takes a month, day, year, hour, and minute (24-hour clock) and
**  converts it to a Davis timestamp value.
*/
void davisMakeDateTime (unsigned char *dtRecord,
                        int iYear, int iMonth, int iDay,
                        int iHour, int iMinute) {

  /* Calculate date/time value and store them in a buffer */
  ((unsigned short*)dtRecord)[0] = htorns(iDay + (iMonth * 32) + ((iYear - 2000) * 512)); 
  ((unsigned short*)dtRecord)[1] = htorns((100 * iHour) + iMinute);


  /* Calculate the CRC checksum and place it in the record */
  ((unsigned short*)dtRecord)[2] = htons(calcCRC_CCITTBuffer (dtRecord, 4));
}


/*
**  Executes the Davis "DMPAFT" command and fills record(s) with data.
**  Returns the number of records read, or RESULT_FAILURE if an error occurred.
**
**  NOTE: setting bAllData = TRUE may not work, use a really early date instead.
*/
int davisExecDMPAFT (int iYear, int iMonth, int iDay, int iHour, int iMinute, int bAllData) 
{

  /* Initialize */
  int                          iResult = 0;
  unsigned short               iRecordCount = 0;
  unsigned short               iRecordCount_curpkt = 0;
  unsigned short               iCalcCRC = 0;
  int                          lcv = 0;
  char                         sResponse[5000];
  struct stDavisDMPAFTResponse oDMPAFTResponse; 
  struct stArchivePage         oArchivePage; 
  int                          iPageNum = 0;

  int                          iPageReadAttempt = 1;
  int                          bDataGood = FALSE;
  char                         sDavisReturnCode = DAVIS_ACK;
  struct stArchiveData         *aArchiveData=NULL;
  double                       aftertime;
  char                         sDateTime[10];

  if (oConfig.sTimeZone)
    sprintf(sResponse,"%d-%02d-%02d %d:%02d %s",iYear,iMonth,iDay,iHour,iMinute,oConfig.sTimeZone);
  else
    sprintf(sResponse,"%d-%02d-%02d %d:%02d",iYear,iMonth,iDay,iHour,iMinute);

  if (zstr2epoch(sResponse,&aftertime))
    {
      elog_complain(0,"davisExecDMPAFT Failed to convert time to epoch (%s)\n",sResponse);
      exit(-1);
    }

  aArchiveData=malloc(sizeof(*aArchiveData)*MAXRECORDCOUNTPKT);
  if (aArchiveData == NULL)
    {
      elog_complain(1,"Error while attempting to allocated memory for %d samples:");
      elog_complain(0,"You may need to adjust the MaxSamplesPerPacket variable in %s\n",oConfig.sParamFileName);
      davisCleanup(-1);
    }


  /* Build a Davis date/time object */
  davisMakeDateTime ((unsigned char*)sDateTime, iYear, iMonth, iDay, iHour, iMinute);

  if (bAllData == FALSE)
    {
      /* Send the DMPAFT command */
      if (oConfig.bVerboseModeFlag == TRUE)
	elog_notify (0, "davisExecDMPAFT(): Sending 'DMPAFT' command to Davis...\n");

      if (doWriteBytes (&iConnectionHandle, "DMPAFT\n", strlen ("DMPAFT\n")) == RESULT_FAILURE) {
	elog_complain (0, "davisExecDMPAFT(): Error calling doWriteBytes (). DMPAFT\n");
	  return RESULT_FAILURE;
      }
      
      if (oConfig.bVerboseModeFlag == TRUE)
	elog_notify (0, "davisExecDMPAFT(): Command sent, waiting for response...\n");
    }
  else
    {
      /* Send the DMP command */
      if (oConfig.bVerboseModeFlag == TRUE)
	elog_notify (0, "davisExecDMPAFT(): Sending 'DMP' command to Davis...\n");
      if (doWriteBytes (&iConnectionHandle, "DMP\n", strlen ("DMP\n")) == RESULT_FAILURE) {
	elog_complain (0, "davisExecDMPAFT(): Error calling doWriteBytes (). DMP\n");
	return RESULT_FAILURE;
      }
      
      if (oConfig.bVerboseModeFlag == TRUE)
	elog_notify (0, "davisExecDMPAFT(): Command sent, waiting for response...\n");	
    }
  
  /* Wait for ACK */
  iResult = readLine (&iConnectionHandle, sResponse, DAVIS_ACK);
  if (iResult == RESULT_FAILURE) {
    elog_complain (0, "davisExecDMPAFT(): Error calling readLine ().\n");
    return RESULT_FAILURE;
  }
  if (oConfig.bVerboseModeFlag == TRUE)
    elog_notify (0, "davisExecDMPAFT(): ACK received.\n");

  if (bAllData == FALSE)
    {
      /* Send date/time block to Davis */
      if (oConfig.bVerboseModeFlag == TRUE)
	elog_notify (0, "davisExecDMPAFT(): Sending date/time...(%d/%02d/%02d %d:%02d)\n",iMonth,iDay,iYear, iHour, iMinute);

      if (doWriteBytes (&iConnectionHandle, sDateTime, 6) == RESULT_FAILURE) {
	elog_complain (0, "davisExecDMPAFT(): Error calling doWriteBytes (). Send date/time.\n");
	return RESULT_FAILURE;
      }
      
      /* Wait for ACK */
      iResult = readLine (&iConnectionHandle, sResponse, DAVIS_ACK);
      if (iResult == RESULT_FAILURE) {
	elog_complain (0, "davisExecDMPAFT(): Error calling readLine ().\n");
	return RESULT_FAILURE;
      }

      if (oConfig.bVerboseModeFlag == TRUE)
	elog_notify (0, "davisExecDMPAFT(): ACK received.\n");
      
      /* Read the response block from Davis
	 (2 bytes of page count + 2 bytes of first valid record + 2 byte CRC */
      iResult = doReadBytes (&iConnectionHandle, sResponse, 6, TRUE);
      if (iResult == RESULT_FAILURE) {
	elog_complain (0, "davisExecDMPAFT(): Error calling doReadBytes ().\n");
	return RESULT_FAILURE;
      }
      
      /* Calculate CRC of the block, check for validity */
      iCalcCRC = calcCRC_CCITTBuffer ((unsigned char*)&sResponse, 6);

      if (iCalcCRC != 0) { /* If calculated CRC != 0, it's invalid */
	elog_complain (0, "davisExecDMPAFT(): Calculated CRC doesn't match reported CRC.\n");
	return RESULT_FAILURE;
      }
      
      unpackDMPAFTResp(&oDMPAFTResponse, (unsigned char*)sResponse);

      if (oConfig.bVerboseModeFlag == TRUE) {   
	elog_notify (0, "davisExecDMPAFT(): Page count received: %u\n", oDMPAFTResponse.iRecordCount);
	elog_notify (0, "davisExecDMPAFT(): First valid record: %u\n", oDMPAFTResponse.iFirstValidRecord);
      }
      
      /* 
	 YO, I'M NOT SURE WHY THIS HAS TO BE HERE, APPARENTLY THERE IS SOME RESIDUAL PROTOCOL GIBERISH TAHT IS NOT DESCRIBED IN THE PROTOCOL SPEC (THE FIRST CHAR IS USUALLY '(' OF THIS RESIDUAL DATA. THIS REMOVES ALL RESIDUAL DATA AND DRAWS PRETTY DIAGRAMS OF WATER SKIS 
      */

      /* Flush the serial port output so we can start fresh */
      if (flushOutput (&iConnectionHandle) == RESULT_FAILURE) { 
	elog_complain (1, "davisWakeUp(): Failure calling flushOutput after waking up the davis.");
	return RESULT_FAILURE;
      }
      
      /* At this point, the CRC is good, so sent <ACK> back to Davis */
      if (oConfig.bVerboseModeFlag == TRUE)
	elog_notify (0, "davisExecDMPAFT(): Starting Davis data dump...\n");

      if (doWriteBytes (&iConnectionHandle, &DAVIS_ACK, sizeof (DAVIS_ACK)) == RESULT_FAILURE) 
	{
	  elog_complain (0, "davisExecDMPAFT(): Error calling doWriteBytes ().\n");
	  return RESULT_FAILURE;
	}

      if (oDMPAFTResponse.iRecordCount==0)
	{  
	  if (oConfig.bVerboseModeFlag == TRUE)
	    elog_notify (0, "davisExecDMPAFT(): No data to download.\n");

	  free(aArchiveData);
	  return(0);
	}
    }
  else
    oDMPAFTResponse.iRecordCount=513;
  
  /* Read a page of data at a time */
  for (iPageNum = 0; iPageNum < oDMPAFTResponse.iRecordCount; iPageNum ++) {

    /* Loop for retrying if CRC fails */
    iPageReadAttempt = 1;
    do {
      
      /* Output debug info */
      if (oConfig.bVerboseModeFlag == TRUE)
	elog_notify (0, "davisExecDMPAFT(): Davis data dump: Reading page %u of %u.\n", (iPageNum + 1), oDMPAFTResponse.iRecordCount);
      
      /* Read a page */
      iResult = doReadBytes (&iConnectionHandle, sResponse, 267, TRUE);
      if (iResult == RESULT_FAILURE) {
	elog_complain (0, "davisExecDMPAFT(): Error calling doReadBytes ().  Data Pages\n");
	return RESULT_FAILURE;
      }
      
      /* Check the page's CRC */
      iCalcCRC = calcCRC_CCITTBuffer ((unsigned char*)sResponse, 267);

      unpackArchivePage(&oArchivePage, (unsigned char*)sResponse);
      
      
      bDataGood = (iCalcCRC == 0);
      if (!bDataGood) {
	elog_notify (0, "davisExecDMPAFT(): Sequence received: %u\n", oArchivePage.iSequenceNumber);
	elog_notify (0, "davisExecDMPAFT(): Date Stamp: %u\n", oArchivePage.aArchiveData [0].iDateStamp);
	elog_notify (0, "davisExecDMPAFT(): Time Stamp: %u\n", oArchivePage.aArchiveData [0].iTimeStamp);

	elog_complain (0, "davisExecDMPAFT(): Error reading page %u (CRC Failed).  Attempt %u of %u.\n",
		       (iPageNum + 1), iPageReadAttempt, MAX_DAVIS_PAGE_ATTEMPTS);
	sDavisReturnCode = DAVIS_NAK;
	iPageReadAttempt ++;
      }
      /* Copy it to the return structure, incrementing the record counter */
      else {
	if (oConfig.bVerboseModeFlag == TRUE)
	  {
	    elog_notify (0, "davisExecDMPAFT(): Sequence received: %u\n", oArchivePage.iSequenceNumber);
	    elog_notify (0, "davisExecDMPAFT(): Date Stamp: %u\n", oArchivePage.aArchiveData [0].iDateStamp);
	    elog_notify (0, "davisExecDMPAFT(): Time Stamp: %u\n", oArchivePage.aArchiveData [0].iTimeStamp);
	  }

	for (lcv = 0; lcv < 5; lcv ++) {
	  memcpy (&(aArchiveData [iRecordCount_curpkt]), &(oArchivePage.aArchiveData [lcv]),sizeof (oArchivePage.aArchiveData [lcv]));
	  iRecordCount ++;
	  iRecordCount_curpkt++;
	  
	  if (iRecordCount_curpkt == MAXRECORDCOUNTPKT)
	    {
	      elog_notify(0,"sending pkt with %d samples\n",iRecordCount_curpkt);
	      sendPkt(aArchiveData,iRecordCount_curpkt,oDMPAFTResponse.iFirstValidRecord,aftertime);
	      oDMPAFTResponse.iFirstValidRecord=0;
	      iRecordCount_curpkt=0;
	    }
	}
	sDavisReturnCode = DAVIS_ACK;
      }
      
      /* Send either ACK or NAK */
      if (doWriteBytes (&iConnectionHandle, &sDavisReturnCode, sizeof (sDavisReturnCode)) == RESULT_FAILURE) {
	elog_complain (0, "davisExecDMPAFT(): Error calling doWriteBytes ().\n");
	return RESULT_FAILURE;
      }
    } while ((!bDataGood) && (iPageReadAttempt <= MAX_DAVIS_PAGE_ATTEMPTS));

    if (!bDataGood)
      {
	elog_complain(0,"Failed to download page num #%d\n",iPageNum);
	davisCleanup(-1);
      }
  }

  if (oConfig.bVerboseModeFlag == TRUE)
    elog_notify(0,"sending pkt with %d samples\n",iRecordCount_curpkt);

  sendPkt(aArchiveData,iRecordCount_curpkt,oDMPAFTResponse.iFirstValidRecord,aftertime);
  iRecordCount_curpkt=0;
  
  if (oConfig.bVerboseModeFlag == TRUE)
    elog_notify (0, "davisExecDMPAFT(): Davis data dump complete.\n");
  
  free(aArchiveData);

  /* Return results */
  return iRecordCount;
}


/*
**  Reads the current date/time from the Davis.  The date/time value returned
**  is double value containing the Davis' UTC time.
**
**  Returns RESULT_FAILURE if an error occurred.
*/
double davisGetTime () {

  /* Initialize */
  int                          iResult = 0;
  unsigned short               iCalcCRC = 0;
  char                         sResponse[5000];
  char                         sTime [255];
  struct stDavisGetSetTime     oDavisDateTime; 
  double                       timestamp;

  /* Send the GETTIME command */
  if (oConfig.bVerboseModeFlag == TRUE)
    elog_notify (0, "davisGetTime(): Sending 'GETTIME' command to Davis...\n");
  if (doWriteBytes (&iConnectionHandle, "GETTIME\n", strlen ("GETTIME\n")) == RESULT_FAILURE) {
    elog_complain (0, "davisGetTime(): Error calling doWriteBytes (). GETTIME\n");
    return RESULT_FAILURE;
  }
  if (oConfig.bVerboseModeFlag == TRUE)
    elog_notify (0, "davisGetTime(): Command sent, waiting for response...\n");
  
  /* Wait for ACK */
  iResult = readLine (&iConnectionHandle, sResponse, DAVIS_ACK);
  if (iResult == RESULT_FAILURE) {
    elog_complain (0, "davisGetTime(): Error calling readLine () during wait for ACK.\n");
    return RESULT_FAILURE;
  }
  if (oConfig.bVerboseModeFlag == TRUE)
    elog_notify (0, "davisGetTime(): ACK received.\n");
  
  /* Read the response block from Davis */
  iResult = doReadBytes (&iConnectionHandle, sResponse, 8,TRUE);
  if (iResult == RESULT_FAILURE) {
    elog_complain (0, "davisGetTime(): Error calling doReadBytes() during read response block.\n");
    return RESULT_FAILURE;
  }

  /* Verify the CRC */
  iCalcCRC = calcCRC_CCITTBuffer ((unsigned char*)sResponse, 8);

  if (iCalcCRC != 0) {
    elog_complain (0, "davisGetTime(): Error reading time (CRC Failed).\n");
    return RESULT_FAILURE;
  }

  unpackstDavisGetSetTime(&oDavisDateTime, (unsigned char*)sResponse);

  /* All is good if we made it here -- convert and return UNIX timestamp */
  if (oConfig.sTimeZone)
    sprintf (sTime, "%u-%02u-%02u %u:%02u:%02u %s", (oDavisDateTime.iYearMinus1900 + 1900), oDavisDateTime.iMonth, oDavisDateTime.iDay, oDavisDateTime.iHour, oDavisDateTime.iMinute, oDavisDateTime.iSecond,oConfig.sTimeZone);
  else
    sprintf (sTime, "%u-%02u-%02u %u:%02u:%02u", (oDavisDateTime.iYearMinus1900 + 1900), oDavisDateTime.iMonth, oDavisDateTime.iDay, oDavisDateTime.iHour, oDavisDateTime.iMinute, oDavisDateTime.iSecond);

  if (zstr2epoch(sTime,&timestamp))
    {
      elog_complain(0,"davisGetTime: failed to parse davis time (%s). Found %d errors\n",sTime,-zstr2epoch(sTime,&timestamp));
      close(iConnectionHandle);
      iConnectionHandle = INVALID_HANDLE;
      return RESULT_FAILURE;
    }
  if (oConfig.bVerboseModeFlag == TRUE)   
    elog_notify (0, "Davis Date/Time: %s %f\n", sTime, timestamp);
  return timestamp;
}



/*
**  Sets the current date/time on the Davis.  The source date/time value 
**  is double value containing the UTC time.
**
**  Returns RESULT_FAILURE if an error occurred.
*/
int davisSetTime () {

  /* Initialize */
  int                          iResult = 0;
  char                         sResponse[MAX_BUFFER_SIZE];
  unsigned char                         sTimeToSet[MAX_BUFFER_SIZE];
  double iUTCTime;

  /* Wake up the Davis and continue */
  if (davisWakeUp () == RESULT_SUCCESS) {

    /* Send the SETTIME command */
    if (oConfig.bVerboseModeFlag == TRUE)
      elog_notify (0, "davisSetTime(): Sending 'SETTIME' command to Davis...\n");
    if (doWriteBytes (&iConnectionHandle, "SETTIME\r", strlen ("SETTIME\r")) == RESULT_FAILURE) {
      elog_complain (0, "davisSetTime(): Error calling doWriteBytes (). SETTIME\n");
      return RESULT_FAILURE;
    }
    if (oConfig.bVerboseModeFlag == TRUE)
      elog_notify (0, "davisSetTime(): Command sent, waiting for response...\n");

    /* Wait for ACK */
    iResult = readLine (&iConnectionHandle, sResponse, DAVIS_ACK);
    if (iResult == RESULT_FAILURE) {
      elog_complain (0, "davisSetTime(): Error calling readLine () during wait for ACK.\n");
      return RESULT_FAILURE;
    }
    if (oConfig.bVerboseModeFlag == TRUE)
      elog_notify (0, "davisSetTime(): ACK received (after sending SETTIME command).\n");

    flushOutput(&iConnectionHandle);

    iUTCTime=now();

    /* Build the "stDavisGetSetTime" structure */
    if (oConfig.sTimeZone)
      {
	sTimeToSet[0] = atoi (zepoch2str (iUTCTime, "%S", oConfig.sTimeZone));
	sTimeToSet[1] = atoi (zepoch2str (iUTCTime, "%M", oConfig.sTimeZone));
	sTimeToSet[2] = atoi (zepoch2str (iUTCTime, "%H", oConfig.sTimeZone));
        sTimeToSet[3] = atoi (zepoch2str (iUTCTime, "%e", oConfig.sTimeZone));
	sTimeToSet[4] = atoi (zepoch2str (iUTCTime, "%m", oConfig.sTimeZone));
	sTimeToSet[5] = atoi (zepoch2str (iUTCTime, "%Y", oConfig.sTimeZone)) - 1900;
      }
    else
      {
	sTimeToSet[0] = atoi (epoch2str (iUTCTime, "%S"));
	sTimeToSet[1] = atoi (epoch2str (iUTCTime, "%M"));
	sTimeToSet[2] = atoi (epoch2str (iUTCTime, "%H"));
	sTimeToSet[3] = atoi (epoch2str (iUTCTime, "%e"));
	sTimeToSet[4] = atoi (epoch2str (iUTCTime, "%m"));
	sTimeToSet[5] = atoi (epoch2str (iUTCTime, "%Y")) - 1900;
      }

    ((unsigned short*)sTimeToSet)[3]  = htons(calcCRC_CCITTBuffer ((unsigned char*)sTimeToSet, 6));


    /* Send the time data */
    if (oConfig.bVerboseModeFlag == TRUE)
      elog_notify (0, "davisSetTime(): Sending date/time...\n");
    if (doWriteBytes (&iConnectionHandle, (char*)sTimeToSet, 8) == RESULT_FAILURE) {
      elog_complain (0, "davisSetTime(): Error calling doWriteBytes (). Send date/time.\n");
      return RESULT_FAILURE;
    }

    /* Wait for ACK */
    iResult = readLine (&iConnectionHandle, sResponse, DAVIS_ACK);
    if (iResult == RESULT_FAILURE) {
      elog_complain (0, "davisSetTime(): Error calling readLine () during wait for ACK.\n");
      return RESULT_FAILURE;
    }
    if (oConfig.bVerboseModeFlag == TRUE)
      elog_notify (0, "davisSetTime(): ACK received (after sending date/time to Davis.\n");

    /* All is good if we made it here */
    return RESULT_SUCCESS;
  }

  return RESULT_FAILURE;
}


/*
**  Sets the current timezone in the DAVIS to the specified GMT offset.  Note
**  that the offset must be in 15-minute increment.
**
**  Returns RESULT_SUCCESS if the setting was successful, or 
**  RESULT_FAILURE if an error occurred.
*/
int davisSetTimeZone (float fGMTOffset) {

  /* Initialize */
  char   sCmdResponse [MAX_BUFFER_SIZE];
  char   sCmd [MAX_BUFFER_SIZE];
  short  iGMTOffset = 0;  

  /* Wake up the Davis and continue */
  if (davisWakeUp () == RESULT_SUCCESS) {

    /* Build the timezone string */
    iGMTOffset = floor(fGMTOffset * 100);

    /* Send the EEWR command to set the timezone (Byte 1) */
    sprintf (sCmd, "EEWR %02X %02X",
             DAVIS_EE_GMT_OFFSET,
             getHighLowByte (TRUE, iGMTOffset));
    if (davisExecCommand (sCmd, sCmdResponse, COMMAND_OK) != RESULT_SUCCESS) {
      elog_complain (0, "davisSetTimeZone(): Error executing 'EEWR' command (Timezone Byte 1).\n");
      return RESULT_FAILURE;
    }

    /* Send the EEWR command to set the timezone (Byte 2) */
    sprintf (sCmd, "EEWR %02X %02X",
             DAVIS_EE_GMT_OFFSET + 1,
             getHighLowByte (FALSE, iGMTOffset));
    if (davisExecCommand (sCmd, sCmdResponse, COMMAND_OK) != RESULT_SUCCESS) {
      elog_complain (0, "davisSetTimeZone(): Error executing 'EEWR' command (Timezone Byte 2).\n");
      return RESULT_FAILURE;
    }

    /* Send the EEWR command to set the GMT offset use flag */
    sprintf (sCmd, "EEWR %02X %02X",
             DAVIS_EE_GMT_OR_ZONE,
             0x01);
    if (davisExecCommand (sCmd, sCmdResponse, COMMAND_OK) != RESULT_SUCCESS) {
      elog_complain (0, "davisSetTimeZone(): Error executing 'EEWR' command (Timezone Byte 2).\n");
      return RESULT_FAILURE;
    }

    /* Finally, send the NEWSETUP command to force the Davis to re-initialize */
    if (davisExecCommand ("NEWSETUP", sCmdResponse, COMMAND_ACK) != RESULT_SUCCESS) {
      elog_complain (0, "davisSetTimeZone(): Error executing 'NEWSETUP' command.\n");
      return RESULT_FAILURE;
    }

    /* Return SUCCESS if we got here */
    return RESULT_SUCCESS;
  }

  return RESULT_FAILURE;
}


/*
**  Reads the current RXCHECK data from the Davis.  Populates the specified
**  "stDavisRXCheckData" structure with the results.
**
**  Returns RESULT_FAILURE if an error occurred.
*/
int davisGetRXCheck (struct stDavisRXCheckData *oDavisRXCheck) {

  /* Initialize */
  char  sCmdResponse [MAX_BUFFER_SIZE];

  /* Send the RXCHECK command */
  if (davisExecCommand ("RXCHECK", sCmdResponse, COMMAND_OK) == RESULT_SUCCESS) 
    {
      sscanf (sCmdResponse, "%u %u %u %u %u",
	      &oDavisRXCheck->iPacketsReceived,
	      &oDavisRXCheck->iPacketsMissed,
	      &oDavisRXCheck->iResyncCount,
	      &oDavisRXCheck->iLargestPacketBlock,
	      &oDavisRXCheck->iCRCErrorCount);
      if (oConfig.bVerboseModeFlag == TRUE)
	elog_notify (0, "davisGetRXCheck(): Pkts Recd=%u, Pkts Missed=%u, Resync Count=%u, Largest Pkt Block=%u, CRC Error Count=%u\n",
		     oDavisRXCheck -> iPacketsReceived,
		     oDavisRXCheck -> iPacketsMissed,
		     oDavisRXCheck -> iResyncCount,
		     oDavisRXCheck -> iLargestPacketBlock,
		     oDavisRXCheck -> iCRCErrorCount);
    }
  
  /* Report error if it failed */
  else 
    {
      elog_complain (0, "davisGetRXCheck(): Error executing 'RXCHECK' command.\n");
      return RESULT_FAILURE;
    }

  return RESULT_SUCCESS;
}


/*
**  Displays the commandline options.
*/
void showCommandlineUsage (void) {
  cbanner (VERSION,
           " [-V] [-v] [-d] [-e] [-f] [-j] [-k] [-m] [-T] [-x] [-1] [-r interval] {[-p serialport] [-b serialspeed] | [-a hostaddr] [-n hostport]} [-c srcname] [-o orbname] [-g paramfile] [-s statefile] [-z timezone] [-t starttime] [-i davisinterval_toset]",
           "Todd Hansen", "UCSD ROADNet Project", "tshansen@ucsd.edu");
}


/*
**  Parses out the commandline options.  Returns RESULT_SUCCESS if all options
**  are good and within limits, RESULT_FAILURE if there was a problem or that
**  the program needn't continue running.
*/
int parseCommandLineOptions (int iArgCount, char *aArgList []) {
         
  int  iOption          = '\0';
  int  bAddressSet      = FALSE;
  int  bPortSet         = FALSE;
  int  bSerialPortSet   = FALSE;
  int  bBaudRateSet     = FALSE;
  char buf[500];
  double epochtest;

  /* Initialize the CONFIG structure */
  oConfig.bVerboseModeFlag = FALSE;
  oConfig.iRepeatInterval = DEFAULT_REPEAT_INTERVAL;
  oConfig.iConnectionType = CONNECT_SERIAL;
  oConfig.sConnectionParams [0] = "";
  oConfig.sConnectionParams [1] = DEFAULT_SERIAL_SPEED;
  oConfig.sBaseSrcName = "NONE_NONE";        
  oConfig.sOrbName = ":";            
  oConfig.sParamFileName = "davis2orb.pf";      
  oConfig.sStateFileName = NULL;      
  oConfig.bMeasureTimeSkewFlag = FALSE; 
  oConfig.bAutoAdjustSleepFlag = FALSE;
  oConfig.bSkipDavisRateSetFlag = TRUE;
  oConfig.bKickStateFile = FALSE;
  oConfig.sTimeZone = NULL;            
  oConfig.bRXCheckFlag = FALSE;
  oConfig.bSetDavisClock = FALSE;             
  oConfig.bInitalizeDavis = FALSE;             
  oConfig.bForceIgnoreTiming = FALSE;
  oConfig.bConfigScreenMode = FALSE;
  oConfig.iDavisSampleInterval_toset = 0;
  oConfig.iBaudRateTermios = RESULT_FAILURE;
  oConfig.bAutoProgramDavis = FALSE;
	
  /* Loop through all possible options */
  while ((iOption = getopt (iArgCount, aArgList,"1VvdkjxfmeTr:p:b:t:i:a:n:c:o:g:s:z:")) != -1) 
    {     
      switch (iOption) {
      case 'V':
        showCommandlineUsage();
        return RESULT_FAILURE;
        break;	
      case 'v':
        oConfig.bVerboseModeFlag = TRUE;
        break;
      case 'f':  
	oConfig.bForceIgnoreTiming = TRUE;
	break;
      case 'd':
        oConfig.bRXCheckFlag = TRUE;
        break;
      case 'i':
        oConfig.bSkipDavisRateSetFlag = FALSE;
	oConfig.iDavisSampleInterval_toset=atoi(optarg);
        break;
      case '1':
	oConfig.bInitalizeDavis = TRUE;  
	oConfig.bSkipDavisRateSetFlag = FALSE;
	oConfig.bSetDavisClock = TRUE;
	oConfig.bConfigScreenMode = TRUE;
	elog_notify(0,"Initializing davis for first use. I will delete the old data instead of downloading it.\n");
	break;
      case 'e': 
 	oConfig.bAutoProgramDavis = TRUE;
	break;
      case 'j':
        oConfig.bAutoAdjustSleepFlag = TRUE;
        break;
      case 'k':
	oConfig.bKickStateFile = TRUE;
	break;
      case 't':
	oConfig.bKickStateFile = TRUE;
	if (zstr2epoch(optarg,&lastdownloadtimestamp))
	  {
	    /* Output a log header for our program */
	    elog_notify (0, "%s\n", VERSION);
  
	    elog_complain(0,"Failed to parse start time (%s), it contained %d errors",optarg,-zstr2epoch(optarg,&lastdownloadtimestamp));
	    exit(-1);
	  }
	break;
      case 'x':
        oConfig.bMeasureTimeSkewFlag = TRUE;
        break;
      case 'T':
        oConfig.bSetDavisClock = TRUE;
        break;
      case 'm':
	  oConfig.bConfigScreenMode = TRUE;
        break;
      case 'r':
        oConfig.iRepeatInterval = atoi (optarg);
        break;
      case 'p':
        oConfig.sConnectionParams [0] = optarg;
        oConfig.iConnectionType = CONNECT_SERIAL;
        bSerialPortSet = TRUE;
        break;
      case 'b':
        oConfig.sConnectionParams [1] = optarg;
        bBaudRateSet = TRUE;
        break;
      case 'a':
        oConfig.sConnectionParams [0] = optarg;
        oConfig.iConnectionType = CONNECT_TCP_IP;
        bAddressSet = TRUE;
        break;
      case 'n':
        oConfig.sConnectionParams [1] = optarg;
        bPortSet = TRUE;
        break;
      case 'c':
        oConfig.sBaseSrcName = optarg;
        break;
      case 'o':
        oConfig.sOrbName = optarg;
        break;
      case 'g':
        oConfig.sParamFileName = optarg;
        break;
      case 's':
        oConfig.sStateFileName = optarg;
        break;
      case 'z':
        oConfig.sTimeZone = optarg;
        break;

      /* Handle invalid arguments */
      default:
        elog_complain (0, "parseCommandLineOptions(): Invalid commandline argument: '-%c'\n\n", iOption);
        showCommandlineUsage ();
        return RESULT_FAILURE;
      }
    }

  /* Output a log header for our program */
  elog_notify (0, "%s\n", VERSION);  

  /* Verify valid commandline options & combinations */
  if (((bAddressSet == FALSE) && (bSerialPortSet == FALSE)) ||
      ((bAddressSet == TRUE) && (bSerialPortSet == TRUE))) {   
    elog_complain (0, "parseCommandLineOptions(): Missing/vague connection arguments; specify either -a or -p but not both.\n");
    showCommandlineUsage ();
    return RESULT_FAILURE;
  }

  if (oConfig.bAutoProgramDavis == TRUE && oConfig.iRepeatInterval > 4*3600)
    {
      elog_complain(0,"parseCommandLineOptions(): You can not set the -e flag when the repeat interval is greater than 4 hours.\n");
      return RESULT_FAILURE;
    }

  if (bSerialPortSet == TRUE) {

    /* Resolve the baud rate */
    oConfig.iBaudRateTermios = resolveNumericBaudRate (atoi (oConfig.sConnectionParams [1]));
    if (oConfig.iBaudRateTermios == RESULT_FAILURE) {
      elog_complain (0, "parseCommandLineOptions(): Invalid baud rate '%i' specified.\n", oConfig.sConnectionParams [1]);
      return RESULT_FAILURE;
    }
  }

  if (oConfig.bAutoAdjustSleepFlag==TRUE && oConfig.bMeasureTimeSkewFlag==FALSE)
    {
      elog_complain(0,"You did not set the flag to measure timeskew (-t). It must be set for automatic sleep adjust to work (-j). I will set it for you this time.\n");
      oConfig.bMeasureTimeSkewFlag=TRUE;
    }


  if (oConfig.sTimeZone)
    {
      sprintf(buf,"2006-001 12:21:21 %s",oConfig.sTimeZone);
      if (zstr2epoch(buf,&epochtest))
	{
	  elog_complain(0,"parseCommandLineOptions: Timezone improper (%s)\n",oConfig.sTimeZone);
	  exit(-1);
	}
    }

  if (bSerialPortSet == TRUE && oConfig.bVerboseModeFlag == TRUE)
    elog_notify(0,"baud rate set to: %s\n",oConfig.sConnectionParams [1]);
 
  
  /* If we got this far, everything was fine! */
  return RESULT_SUCCESS;
}


/*
**  Reads the parameter file.
*/
int paramFileRead () 
{
  char buf[5000];
  int ret;
  static int first=1;

  if ((ret=pfupdate(oConfig.sParamFileName,&configpf))<0)
    {
      elog_complain(0,"pfupdate(\"%s\",configpf): failed to open config file.\n",oConfig.sParamFileName);
      exit(-1);
    } 
  else if (ret==1)
  {
    if (first)
      elog_notify(0,"config file loaded %s\n",oConfig.sParamFileName);
    else
      elog_notify(0,"updated config file loaded %s\n",oConfig.sParamFileName);

    first=0;

    sprintf(buf,"%s{DavisLoggerSampleRate}",oConfig.sBaseSrcName);
    oConfig.iDavisSampleInterval=pfget_int(configpf,buf);

    if (oConfig.bInitalizeDavis == TRUE || oConfig.bAutoProgramDavis == TRUE)
    {
	if (oConfig.iDavisSampleInterval_toset != 0)
	    elog_notify(0,"Since an initalize davis command line option was selected (-1 or -e), I am ignoring the -i parameter and using the value from the parameter file\n");

	oConfig.iDavisSampleInterval_toset=oConfig.iDavisSampleInterval;
	elog_notify(0,"davis2orb will set the Davis internal sample interval to %d min (value from parameter file %s)\n",oConfig.iDavisSampleInterval_toset,oConfig.sParamFileName);
    }

    
    /* Check for valid sample rate */
    if ((oConfig.iDavisSampleInterval != 1) &&
	(oConfig.iDavisSampleInterval != 5) &&
	(oConfig.iDavisSampleInterval != 10) &&
	(oConfig.iDavisSampleInterval != 15) &&
	(oConfig.iDavisSampleInterval != 30) &&
	(oConfig.iDavisSampleInterval != 60) &&
	(oConfig.iDavisSampleInterval != 120)) 
      {
	elog_complain (0, "paramFileRead: Invalid sample rate (%i) specified in %s using the variable %s.  Must be 1, 5, 10, 15, 30, 60, or 120 minutes.", oConfig.iDavisSampleInterval,oConfig.sParamFileName,buf);
      
	return RESULT_FAILURE;
      }
    
    if (oConfig.bAutoAdjustSleepFlag==TRUE && oConfig.iRepeatInterval>0)
      {
	oConfig.iRepeatInterval=oConfig.iDavisSampleInterval*60;
	if (oConfig.bVerboseModeFlag==TRUE)
	  elog_notify(0,"adjusting repeat interval to ~%d seconds",oConfig.iRepeatInterval);
      }
  }
  
  /* Return results */
  return RESULT_SUCCESS;
}

int StatPacket(int *iHandle)
{
  Packet *orbpkt;
  PktChannel *pktchan;
  Srcname srcparts;
  struct stDavisRXCheckData      oRXCheckData;
  double rxchecktime, battchecktime;
  double timeskewtime;
  double davistime;
  char generatedSourceName[500];
  static char *packet=NULL;
  static int packetsz=0;
  int nbytes;
  char buf[5000], *tmp;
  int batt, tranbat, bartrend;

  if (*iHandle<0)
    {
      elog_complain(0,"StatPacket() called with invalid davis file descriptor\n");
      return RESULT_FAILURE;
    }

  orbpkt=newPkt();
  if (orbpkt==NULL)
    {
      elog_complain(1,"creating newPkt in StatPacket:");
      davisCleanup(-1);
    }

  orbpkt->nchannels=0;
  orbpkt->pkttype=suffix2pkttype("MGENC");

  split_srcname(oConfig.sBaseSrcName,&srcparts);

  if (oConfig.bMeasureTimeSkewFlag == TRUE)
    {
      if ((davistime=davisGetTime())==RESULT_FAILURE)
	{
	  freePkt(orbpkt);
	  close(*iHandle);
	  *iHandle=-1;
	  return RESULT_FAILURE;
	}

      timeskewtime=now();
      skewlog=(int)davistime-timeskewtime;
      skewlogvalid=1;
      
      pktchan=newPktChannel();
      if (pktchan==NULL)
	{
	  elog_complain(1,"creating newPktChannel in StatPacket:");
	  davisCleanup(-1);
	}
      pktchan->datasz=1;
      pktchan->nsamp=1;

      pktchan->data=malloc(4);
      if (pktchan->data==NULL)
	{
	  elog_complain(1,"malloc failed in StatPacket:");
	  davisCleanup(-1);
	}

      pktchan->data[0]=skewlog;
      pktchan->time=timeskewtime;
      
      strncpy(pktchan->net,srcparts.src_net,PKT_TYPESIZE);
      strncpy(pktchan->sta,srcparts.src_sta,PKT_TYPESIZE);
      sprintf(buf,"%s{Channels}{TimeSkew}",oConfig.sBaseSrcName);
      tmp=pfget_string(configpf,buf);
      if (tmp == NULL)
	{
	  elog_complain(0,"Parameter file (%s) missing channel name for variable \'%s\'.\n",oConfig.sParamFileName,buf);
	  davisCleanup(-1);
	}
      if (strlen(tmp)>8)
	{
	  elog_complain(0,"Parameter file (%s): channel name for variable \'%s\' longer than 8 characters (%d chars).\n",oConfig.sParamFileName,buf,strlen(tmp));
	  davisCleanup(-1);
	}
      strncpy(pktchan->chan,tmp,PKT_TYPESIZE);
      *(pktchan->loc)='\0';
      strncpy(pktchan->segtype,"T",2);
      pktchan->calib=1.0;
      pktchan->calper=-1;

      if (oConfig.iRepeatInterval)
	pktchan->samprate=1.0/oConfig.iRepeatInterval;
      else
	pktchan->samprate=1;

      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
      orbpkt->time=timeskewtime;
    }
  
  /* Check for sending RXCHECK packets */
  if (oConfig.bRXCheckFlag == TRUE)
    {
      if (getDavisBatt(iHandle,&batt,&tranbat,&bartrend)==RESULT_FAILURE)
      {
	  elog_complain(0,"getDavisBatt failed when called from StatPacket()\n");
	  return RESULT_FAILURE;
      
      }

      battchecktime=now();

      if (davisWakeUp()==RESULT_FAILURE)
      {
	  elog_complain(0,"failed to wake up davis in StatPacket()\n");
	  return RESULT_FAILURE;
      }

      /* Get the RXCheck data from the Davis */
      if (davisGetRXCheck(&oRXCheckData) == RESULT_SUCCESS) 
      {
	  rxchecktime=now();
	  orbpkt->time=rxchecktime;
	  
	  /* Send the data to the ORB */
	  pktchan=newPktChannel();
	  if (pktchan==NULL)
	    {
	      elog_complain(1,"creating newPktChannel in StatPacket:");
	      davisCleanup(-1);
	    }
	  pktchan->datasz=1;
	  pktchan->nsamp=1;
	  
	  pktchan->data=malloc(4);
	  if (pktchan->data==NULL)
	    {
	      elog_complain(1,"malloc failed in StatPacket:");
	      davisCleanup(-1);
	    }
	  
	  pktchan->data[0]=oRXCheckData.iPacketsReceived;
	  pktchan->time=rxchecktime;
	  
	  strncpy(pktchan->net,srcparts.src_net,PKT_TYPESIZE);
	  strncpy(pktchan->sta,srcparts.src_sta,PKT_TYPESIZE);

	  sprintf(buf,"%s{Channels}{RXCHECKPacketsReceived}",oConfig.sBaseSrcName);
	  tmp=pfget_string(configpf,buf);
	  if (tmp == NULL)
	    {
	      elog_complain(0,"Parameter file (%s) missing channel name for variable \'%s\'.\n",oConfig.sParamFileName,buf);
	      davisCleanup(-1);
	    }
	  if (strlen(tmp)>8)
	    {
	      elog_complain(0,"Parameter file (%s): channel name for variable \'%s\' longer than 8 characters (%d chars).\n",oConfig.sParamFileName,buf,strlen(tmp));
	      davisCleanup(-1);
	    }
	  strncpy(pktchan->chan,tmp,PKT_TYPESIZE);

	  *(pktchan->loc)='\0';
	  strncpy(pktchan->segtype,"c",2);
	  pktchan->calib=1.0;
	  pktchan->calper=-1;

	  if (oConfig.iRepeatInterval)
	    pktchan->samprate=1.0/oConfig.iRepeatInterval;
	  else
	    pktchan->samprate=1;
	  
	  pushtbl(orbpkt->channels,pktchan);
	  orbpkt->nchannels++;

	  pktchan=newPktChannel();
	  if (pktchan==NULL)
	    {
	      elog_complain(1,"creating newPktChannel in StatPacket:");
	      davisCleanup(-1);
	    }
	  pktchan->datasz=1;
	  pktchan->nsamp=1;
	  
	  pktchan->data=malloc(4);
	  if (pktchan->data==NULL)
	    {
	      elog_complain(1,"malloc failed in StatPacket:");
	      davisCleanup(-1);
	    }
	  
	  pktchan->data[0]=oRXCheckData.iPacketsMissed;
	  pktchan->time=rxchecktime;
	  
	  strncpy(pktchan->net,srcparts.src_net,PKT_TYPESIZE);
	  strncpy(pktchan->sta,srcparts.src_sta,PKT_TYPESIZE);

	  sprintf(buf,"%s{Channels}{RXCHECKPacketsMissed}",oConfig.sBaseSrcName);
	  tmp=pfget_string(configpf,buf);
	  if (tmp == NULL)
	    {
	      elog_complain(0,"Parameter file (%s) missing channel name for variable \'%s\'.\n",oConfig.sParamFileName,buf);
	      davisCleanup(-1);
	    }
	  if (strlen(tmp)>8)
	    {
	      elog_complain(0,"Parameter file (%s): channel name for variable \'%s\' longer than 8 characters (%d chars).\n",oConfig.sParamFileName,buf,strlen(tmp));
	      davisCleanup(-1);
	    }
	  strncpy(pktchan->chan,tmp,PKT_TYPESIZE);

	  *(pktchan->loc)='\0';
	  strncpy(pktchan->segtype,"c",2);
	  pktchan->calib=1.0;
	  pktchan->calper=-1;

	  if (oConfig.iRepeatInterval)
	    pktchan->samprate=1.0/oConfig.iRepeatInterval;
	  else
	    pktchan->samprate=1;
	  
	  pushtbl(orbpkt->channels,pktchan);
	  orbpkt->nchannels++;

	  pktchan=newPktChannel();
	  if (pktchan==NULL)
	    {
	      elog_complain(1,"creating newPktChannel in StatPacket:");
	      davisCleanup(-1);
	    }
	  pktchan->datasz=1;
	  pktchan->nsamp=1;
	  
	  pktchan->data=malloc(4);
	  if (pktchan->data==NULL)
	    {
	      elog_complain(1,"malloc failed in StatPacket:");
	      davisCleanup(-1);
	    }
	  
	  pktchan->data[0]=oRXCheckData.iResyncCount;
	  pktchan->time=rxchecktime;
	  
	  strncpy(pktchan->net,srcparts.src_net,PKT_TYPESIZE);
	  strncpy(pktchan->sta,srcparts.src_sta,PKT_TYPESIZE);

	  sprintf(buf,"%s{Channels}{RXCHECKResyncCount}",oConfig.sBaseSrcName);
	  tmp=pfget_string(configpf,buf);
	  if (tmp == NULL)
	    {
	      elog_complain(0,"Parameter file (%s) missing channel name for variable \'%s\'.\n",oConfig.sParamFileName,buf);
	      davisCleanup(-1);
	    }
	  if (strlen(tmp)>8)
	    {
	      elog_complain(0,"Parameter file (%s): channel name for variable \'%s\' longer than 8 characters (%d chars).\n",oConfig.sParamFileName,buf,strlen(tmp));
	      davisCleanup(-1);
	    }
	  strncpy(pktchan->chan,tmp,PKT_TYPESIZE);

	  *(pktchan->loc)='\0';
	  strncpy(pktchan->segtype,"c",2);
	  pktchan->calib=1.0;
	  pktchan->calper=-1;

	  if (oConfig.iRepeatInterval)
	    pktchan->samprate=1.0/oConfig.iRepeatInterval;
	  else
	    pktchan->samprate=1;
	  
	  pushtbl(orbpkt->channels,pktchan);
	  orbpkt->nchannels++;

	  pktchan=newPktChannel();
	  if (pktchan==NULL)
	    {
	      elog_complain(1,"creating newPktChannel in StatPacket:");
	      davisCleanup(-1);
	    }
	  pktchan->datasz=1;
	  pktchan->nsamp=1;
	  
	  pktchan->data=malloc(4);
	  if (pktchan->data==NULL)
	    {
	      elog_complain(1,"malloc failed in StatPacket:");
	      davisCleanup(-1);
	    }
	  
	  pktchan->data[0]=oRXCheckData.iLargestPacketBlock;
	  pktchan->time=rxchecktime;
	  
	  strncpy(pktchan->net,srcparts.src_net,PKT_TYPESIZE);
	  strncpy(pktchan->sta,srcparts.src_sta,PKT_TYPESIZE);

	  sprintf(buf,"%s{Channels}{RXCHECKLargestPacketBlock}",oConfig.sBaseSrcName);
	  tmp=pfget_string(configpf,buf);
	  if (tmp == NULL)
	    {
	      elog_complain(0,"Parameter file (%s) missing channel name for variable \'%s\'.\n",oConfig.sParamFileName,buf);
	      davisCleanup(-1);
	    }
	  if (strlen(tmp)>8)
	    {
	      elog_complain(0,"Parameter file (%s): channel name for variable \'%s\' longer than 8 characters (%d chars).\n",oConfig.sParamFileName,buf,strlen(tmp));
	      davisCleanup(-1);
	    }
	  strncpy(pktchan->chan,tmp,PKT_TYPESIZE);

	  *(pktchan->loc)='\0';
	  strncpy(pktchan->segtype,"c",2);
	  pktchan->calib=1.0;
	  pktchan->calper=-1;

	  if (oConfig.iRepeatInterval)
	    pktchan->samprate=1.0/oConfig.iRepeatInterval;
	  else
	    pktchan->samprate=1;
	  
	  pushtbl(orbpkt->channels,pktchan);
	  orbpkt->nchannels++;	  

	  pktchan=newPktChannel();
	  if (pktchan==NULL)
	    {
	      elog_complain(1,"creating newPktChannel in StatPacket:");
	      davisCleanup(-1);
	    }
	  pktchan->datasz=1;
	  pktchan->nsamp=1;
	  
	  pktchan->data=malloc(4);
	  if (pktchan->data==NULL)
	    {
	      elog_complain(1,"malloc failed in StatPacket:");
	      davisCleanup(-1);
	    }
	  
	  pktchan->data[0]=oRXCheckData.iCRCErrorCount;
	  pktchan->time=rxchecktime;
	  
	  strncpy(pktchan->net,srcparts.src_net,PKT_TYPESIZE);
	  strncpy(pktchan->sta,srcparts.src_sta,PKT_TYPESIZE);

	  sprintf(buf,"%s{Channels}{RXCHECKCRCErrorCount}",oConfig.sBaseSrcName);
	  tmp=pfget_string(configpf,buf);
	  if (tmp == NULL)
	    {
	      elog_complain(0,"Parameter file (%s) missing channel name for variable \'%s\'.\n",oConfig.sParamFileName,buf);
	      davisCleanup(-1);
	    }
	  if (strlen(tmp)>8)
	    {
	      elog_complain(0,"Parameter file (%s): channel name for variable \'%s\' longer than 8 characters (%d chars).\n",oConfig.sParamFileName,buf,strlen(tmp));
	      davisCleanup(-1);
	    }
	  strncpy(pktchan->chan,tmp,PKT_TYPESIZE);

	  *(pktchan->loc)='\0';
	  strncpy(pktchan->segtype,"c",2);
	  pktchan->calib=1.0;
	  pktchan->calper=-1;

	  if (oConfig.iRepeatInterval)
	    pktchan->samprate=1.0/oConfig.iRepeatInterval;
	  else
	    pktchan->samprate=1;
	  
	  pushtbl(orbpkt->channels,pktchan);
	  orbpkt->nchannels++;

	  /* console battery voltage */
	  pktchan=newPktChannel();
	  if (pktchan==NULL)
	    {
	      elog_complain(1,"creating newPktChannel in StatPacket:");
	      davisCleanup(-1);
	    }
	  pktchan->datasz=1;
	  pktchan->nsamp=1;
	  
	  pktchan->data=malloc(4);
	  if (pktchan->data==NULL)
	    {
	      elog_complain(1,"malloc failed in StatPacket:");
	      davisCleanup(-1);
	    }
	  
	  pktchan->data[0]=batt;
	  pktchan->time=battchecktime;
	  
	  strncpy(pktchan->net,srcparts.src_net,PKT_TYPESIZE);
	  strncpy(pktchan->sta,srcparts.src_sta,PKT_TYPESIZE);

	  sprintf(buf,"%s{Channels}{ConsoleBatteryVoltage}",oConfig.sBaseSrcName);
	  tmp=pfget_string(configpf,buf);
	  if (tmp == NULL)
	    {
	      elog_complain(0,"Parameter file (%s) missing channel name for variable \'%s\'.\n",oConfig.sParamFileName,buf);
	      davisCleanup(-1);
	    }
	  if (strlen(tmp)>8)
	    {
	      elog_complain(0,"Parameter file (%s): channel name for variable \'%s\' longer than 8 characters (%d chars).\n",oConfig.sParamFileName,buf,strlen(tmp));
	      davisCleanup(-1);
	    }
	  strncpy(pktchan->chan,tmp,PKT_TYPESIZE);

	  *(pktchan->loc)='\0';
	  strncpy(pktchan->segtype,"v",2);
	  pktchan->calib=(300/512.0)/100.0;
	  pktchan->calper=-1;

	  if (oConfig.iRepeatInterval)
	    pktchan->samprate=1.0/oConfig.iRepeatInterval;
	  else
	    pktchan->samprate=1;
	  
	  pushtbl(orbpkt->channels,pktchan);
	  orbpkt->nchannels++;

	  /* transmitter battery status */
	  pktchan=newPktChannel();
	  if (pktchan==NULL)
	    {
	      elog_complain(1,"creating newPktChannel in StatPacket:");
	      davisCleanup(-1);
	    }
	  pktchan->datasz=1;
	  pktchan->nsamp=1;
	  
	  pktchan->data=malloc(4);
	  if (pktchan->data==NULL)
	    {
	      elog_complain(1,"malloc failed in StatPacket:");
	      davisCleanup(-1);
	    }
	  
	  pktchan->data[0]=tranbat;
	  pktchan->time=battchecktime;
	  
	  strncpy(pktchan->net,srcparts.src_net,PKT_TYPESIZE);
	  strncpy(pktchan->sta,srcparts.src_sta,PKT_TYPESIZE);

	  sprintf(buf,"%s{Channels}{TransmitterBatteryStatus}",oConfig.sBaseSrcName);
	  tmp=pfget_string(configpf,buf);
	  if (tmp == NULL)
	    {
	      elog_complain(0,"Parameter file (%s) missing channel name for variable \'%s\'.\n",oConfig.sParamFileName,buf);
	      davisCleanup(-1);
	    }
	  if (strlen(tmp)>8)
	    {
	      elog_complain(0,"Parameter file (%s): channel name for variable \'%s\' longer than 8 characters (%d chars).\n",oConfig.sParamFileName,buf,strlen(tmp));
	      davisCleanup(-1);
	    }
	  strncpy(pktchan->chan,tmp,PKT_TYPESIZE);

	  *(pktchan->loc)='\0';
	  strncpy(pktchan->segtype,"c",2);
	  pktchan->calib=1.0;
	  pktchan->calper=-1;

	  if (oConfig.iRepeatInterval)
	    pktchan->samprate=1.0/oConfig.iRepeatInterval;
	  else
	    pktchan->samprate=1;
	  
	  pushtbl(orbpkt->channels,pktchan);
	  orbpkt->nchannels++;

	  if (bartrend<70 && bartrend>-70 && bartrend != -1)
	  {
	      /* Barometeric Trend */
	      pktchan=newPktChannel();
	      if (pktchan==NULL)
	      {
		  elog_complain(1,"creating newPktChannel in StatPacket:");
		  davisCleanup(-1);
	      }
	      pktchan->datasz=1;
	      pktchan->nsamp=1;
	      
	      pktchan->data=malloc(4);
	      if (pktchan->data==NULL)
	      {
		  elog_complain(1,"malloc failed in StatPacket:");
		  davisCleanup(-1);
	      }
	      
	      pktchan->data[0]=bartrend;
	      pktchan->time=battchecktime;
	      
	      strncpy(pktchan->net,srcparts.src_net,PKT_TYPESIZE);
	      strncpy(pktchan->sta,srcparts.src_sta,PKT_TYPESIZE);
	      
	      sprintf(buf,"%s{Channels}{BarometricTrend}",oConfig.sBaseSrcName);
	      tmp=pfget_string(configpf,buf);
	      if (tmp == NULL)
	      {
		  elog_complain(0,"Parameter file (%s) missing channel name for variable \'%s\'.\n",oConfig.sParamFileName,buf);
		  davisCleanup(-1);
	      }
	      if (strlen(tmp)>8)
	      {
		  elog_complain(0,"Parameter file (%s): channel name for variable \'%s\' longer than 8 characters (%d chars).\n",oConfig.sParamFileName,buf,strlen(tmp));
		  davisCleanup(-1);
	      }
	      strncpy(pktchan->chan,tmp,PKT_TYPESIZE);
	      
	      *(pktchan->loc)='\0';
	      strncpy(pktchan->segtype,"c",2);
	      pktchan->calib=1.0;
	      pktchan->calper=-1;
	      
	      if (oConfig.iRepeatInterval)
		  pktchan->samprate=1.0/oConfig.iRepeatInterval;
	      else
		  pktchan->samprate=1;
	      
	      pushtbl(orbpkt->channels,pktchan);
	      orbpkt->nchannels++;
	  }
      }
      else
	{
	  elog_complain(0,"davisGetRXCheck failed when called from StatPacket()\n");
	  return RESULT_FAILURE;
	}
    }

  if (orbpkt->nchannels>0)
    {
      if (stuffPkt(orbpkt,generatedSourceName,&timeskewtime,&packet,&nbytes,&packetsz)<0)
	{
	  elog_complain(0,"stuffPKt() failed in StatPacket\n");
	  davisCleanup(-1);
	}
      
      split_srcname(generatedSourceName,&srcparts);
      strncpy(srcparts.src_subcode,"stat",PKT_TYPESIZE);
      join_srcname(&srcparts,generatedSourceName);
      
      if (oConfig.bVerboseModeFlag == TRUE)
	showPkt(0,generatedSourceName,timeskewtime,packet,nbytes,stderr,PKT_UNSTUFF);
      
      if (orbput(orbfd,generatedSourceName,timeskewtime,packet,nbytes))
	{
	  elog_complain(0,"orbput() failed in StatPacket\n");
	  davisCleanup(-1);
	}
      
      if (oConfig.bVerboseModeFlag == TRUE)    
	elog_notify(0,"packet submitted under %s\n",generatedSourceName);
    }

  freePkt(orbpkt);
  return RESULT_SUCCESS;
}

int getDavisBatt(int *iHandle, int *batt, int *tranbat, int *bartrend)
{
    char sResponse[5000];

    if (oConfig.bVerboseModeFlag)
	elog_notify(0,"Attempting to retrieve battery voltage from davis LOOP packet\n");

    if (davisExecCommand("LOOP 1",sResponse,COMMAND_ACK)==RESULT_FAILURE)
    {
	elog_complain(0,"getDavisBatt: failed to send LOOP 1 command to davis\n");
	close(*iHandle);
	*iHandle=-1;
	return RESULT_FAILURE;
    }

    if (doReadBytes(iHandle,sResponse,99,TRUE)==RESULT_FAILURE)
    {
	elog_complain(0,"getDavisBatt: failed to read davis response to LOOP 1 command\n");
	close(*iHandle);
	*iHandle=-1;
	return RESULT_FAILURE;
    }

    if (calcCRC_CCITTBuffer((unsigned char*)sResponse,99))
    {
	elog_complain(0,"getDavisBatt: Checksum failed for response from LOOP 1 command\n");
	close(*iHandle);
	*iHandle=-1;
	return RESULT_FAILURE;
    }

    *bartrend=(signed char)sResponse[3];
    *batt=(unsigned char)sResponse[88]*256+(unsigned char)sResponse[87];
    *tranbat=(unsigned char)sResponse[86];
    if (oConfig.bVerboseModeFlag)
	elog_notify(0,"got response from \"LOOP 1\" command. Extracted console battery voltage=%f, tranbat=%d, bartrend=%d.\n",(*batt)*(300/512.0)/100.0,*tranbat,*bartrend);

    return RESULT_SUCCESS;
}

double davisDateTimeToEpoch(unsigned short iDateStamp, unsigned short iTimeStamp)
{
  int day, month, year;
  int hour, min;
  char buf[500];
  double timestamp=0;

  if (iDateStamp == 0xFFFF || iTimeStamp == 0xFFFF)
    return -1;

  day=iDateStamp%32;
  month=iDateStamp%512/32;
  year=iDateStamp/512+2000;
 
  hour=iTimeStamp/100;
  min=iTimeStamp%100;

  if (oConfig.sTimeZone)
    sprintf(buf,"%04d-%02d-%02d %d:%02d %s",year,month,day,hour,min,oConfig.sTimeZone);
  else
    sprintf(buf,"%04d-%02d-%02d %d:%02d",year,month,day,hour,min);
  
  if (zstr2epoch(buf,&timestamp))
    {
      elog_complain(0,"davisDateTimeToEpoch: failed to parse time from davis (%s): contained %d errors\n",buf,-zstr2epoch(buf,&timestamp));

      close(iConnectionHandle);
      iConnectionHandle = INVALID_HANDLE;
    }

  return(timestamp);
}

int sendPkt(struct stArchiveData *aArchiveData, int iRecordCount, int firstvalidrec, double aftertime)
{
  double samprate;
  int *data=NULL;
  double timestamp;
  char buf[5000];
  int lcv;
  double firsttimestamp=0;
  Packet *orbpkt;
  char generatedSourceName[500];
  static char *packet=NULL;
  static int packetsz=0;
  int nbytes;
  Srcname srcparts;
  PktChannel *pktchan=NULL;
  double previoustimestamp=0;
  double lastdownloadtimestamp_start;

  lastdownloadtimestamp_start=lastdownloadtimestamp;

  split_srcname(oConfig.sBaseSrcName,&srcparts);

  data=malloc(4*iRecordCount);
  if (data==NULL)
  {
    elog_complain(1,"malloc failed in sendPkt");
    davisCleanup(-1);
  }

  sprintf(buf,"%s{DavisLoggerSampleRate}",oConfig.sBaseSrcName);
  samprate=1.0/(pfget_int(configpf,buf)*60.0);

  orbpkt=newPkt();
  if (orbpkt==NULL)
    {
      elog_complain(1,"creating newPkt in sendPkt:");
      davisCleanup(-1);
    }

  orbpkt->nchannels=0;
  orbpkt->pkttype=suffix2pkttype("MGENC");

  /***** SEARCH THROUGH ALL OF THE DATA AND LOAD CHANNELS *****/
  
  /* outside temp */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=(aArchiveData[lcv].iOutsideTemp)-320;

      if (data[lcv-firstvalidrec]==32767-320)
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_complain(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_notify(0,"You can use the force flag to ignore this (-f)\n");
	      elog_notify(0,"Additional Debug info: previoustimestamp=%f currenttimesampe=%f\n",previoustimestamp,timestamp);
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("OutsideTemp",data,lcv-firstvalidrec,samprate,firsttimestamp,0.1*5/9,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }

     
  /* high outside temp */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      if ((signed short)(aArchiveData[lcv].iHighOutsideTemp)!=-32768)
	data[lcv-firstvalidrec]=(aArchiveData[lcv].iHighOutsideTemp)-320;
      else
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins) (at sample #%d)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0,lcv);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("HighOutsideTemp",data,lcv-firstvalidrec,samprate,firsttimestamp,0.1*5.0/9.0,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* low outside temp */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      if ((aArchiveData[lcv].iLowOutsideTemp)!=32767)
	data[lcv-firstvalidrec]=(aArchiveData[lcv].iLowOutsideTemp)-320;
      else
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("LowOutsideTemp",data,lcv-firstvalidrec,samprate,firsttimestamp,0.1*5.0/9.0,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }
  previoustimestamp=0;


  /* rain fall */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=(aArchiveData[lcv].iRainLevel);

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("RainFall",data,lcv-firstvalidrec,samprate,firsttimestamp,0.1,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* High Rain Rate */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=(aArchiveData[lcv].iHighRainRate);

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("HighRainRate",data,lcv-firstvalidrec,samprate,firsttimestamp,0.1,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* Pressure */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      if (aArchiveData[lcv].iOutsideTemp!=0)
	data[lcv-firstvalidrec]=(aArchiveData[lcv].iBarometer);
      else
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("Barometer",data,lcv-firstvalidrec,samprate,firsttimestamp,0.0338,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* solar radiation */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      if ((aArchiveData[lcv].iSolarRadiation)!=32767)
	data[lcv-firstvalidrec]=(aArchiveData[lcv].iSolarRadiation);
      else
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("SolarRadiation",data,lcv-firstvalidrec,samprate,firsttimestamp,1,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* Wind Samp Count */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      if ((aArchiveData[lcv].iWindSampleCount)!=0)
	data[lcv-firstvalidrec]=(aArchiveData[lcv].iWindSampleCount);
      else
	data[lcv-firstvalidrec]=0;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("WindSampleCount",data,lcv-firstvalidrec,samprate,firsttimestamp,1,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* Inside Temp */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      if ((aArchiveData[lcv].iInsideTemp)!=32767)
	data[lcv-firstvalidrec]=(aArchiveData[lcv].iInsideTemp)-320;
      else
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("InsideTemp",data,lcv-firstvalidrec,samprate,firsttimestamp,0.1*5.0/9.0,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* inside humidity */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      if (aArchiveData[lcv].iInsideHumidity!=255)
	data[lcv-firstvalidrec]=aArchiveData[lcv].iInsideHumidity;
      else
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("InsideHumidity",data,lcv-firstvalidrec,samprate,firsttimestamp,1,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }

     
  /* outside humidity */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      if (aArchiveData[lcv].iOutsideHumidity!=255)
	data[lcv-firstvalidrec]=aArchiveData[lcv].iOutsideHumidity;
      else
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("OutsideHumidity",data,lcv-firstvalidrec,samprate,firsttimestamp,1,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }

     
  /* Avg Wind Speed */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      if (aArchiveData[lcv].iAvgWindSpeed!=255)
	data[lcv-firstvalidrec]=aArchiveData[lcv].iAvgWindSpeed;
      else
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("AvgWindSpeed",data,lcv-firstvalidrec,samprate,firsttimestamp,0.44704,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }

     
  /* High Wind Speed */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      if (aArchiveData[lcv].iHighWindSpeed!=0)
	data[lcv-firstvalidrec]=aArchiveData[lcv].iHighWindSpeed;
      else
	data[lcv-firstvalidrec]=0;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("HighWindSpeed",data,lcv-firstvalidrec,samprate,firsttimestamp,0.44704,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }

     
  /* Dir High Wind Speed */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {
      switch (aArchiveData[lcv].iHighWindSpeedDir)
	{
	    case 0: data[lcv-firstvalidrec]=0; break; /* N */
	    case 1: data[lcv-firstvalidrec]=225; break;  /* NNE */
	    case 2: data[lcv-firstvalidrec]=450; break;  /* NE */
	    case 3: data[lcv-firstvalidrec]=675; break; /* ENE */
	    case 4: data[lcv-firstvalidrec]=900; break; /* E */
	    case 5: data[lcv-firstvalidrec]=1125; break; /* ESE */
	    case 6: data[lcv-firstvalidrec]=1350; break; /* SE */
	    case 7: data[lcv-firstvalidrec]=1575; break; /* SSE */
	    case 8: data[lcv-firstvalidrec]=1800; break; /* S */
	    case 9: data[lcv-firstvalidrec]=2025; break; /* SSW */
	    case 10: data[lcv-firstvalidrec]=2250; break; /* SW */
	    case 11: data[lcv-firstvalidrec]=2475; break; /* WSW */
	    case 12: data[lcv-firstvalidrec]=2700; break; /* W */
	    case 13: data[lcv-firstvalidrec]=2925; break; /* WNW */
	    case 14: data[lcv-firstvalidrec]=3150; break; /* NW */
	    case 15: data[lcv-firstvalidrec]=3375; break; /* NNW */
	    case 255: data[lcv-firstvalidrec]=ORB_GAP_FILL;  break;
	    default: data[lcv-firstvalidrec]=ORB_GAP_FILL; break;
	}

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("HighWindDir",data,lcv-firstvalidrec,samprate,firsttimestamp,0.1,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }

  /* Dir High Wind Speed Alternate Representation */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=aArchiveData[lcv].iHighWindSpeedDir;

      if (data[lcv-firstvalidrec]==255)
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("HighWindDirAlt",data,lcv-firstvalidrec,samprate,firsttimestamp,1,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* Dir of Prevailing Wind */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {
      switch (aArchiveData[lcv].iHighWindSpeedDir)
	{
	    case 0: data[lcv-firstvalidrec]=0; break; /* N */
	    case 1: data[lcv-firstvalidrec]=225; break; /* NNE */
	    case 2: data[lcv-firstvalidrec]=450; break; /* NE */
	    case 3: data[lcv-firstvalidrec]=675; break; /* ENE */
	    case 4: data[lcv-firstvalidrec]=900; break; /* E */
	    case 5: data[lcv-firstvalidrec]=1125; break; /* ESE */
	    case 6: data[lcv-firstvalidrec]=1350; break; /* SE */
	    case 7: data[lcv-firstvalidrec]=1575; break; /* SSE */
	    case 8: data[lcv-firstvalidrec]=1800; break; /* S */
	    case 9: data[lcv-firstvalidrec]=2025; break; /* SSW */
	    case 10: data[lcv-firstvalidrec]=2250; break; /* SW */
	    case 11: data[lcv-firstvalidrec]=2475; break; /* WSW */
	    case 12: data[lcv-firstvalidrec]=2700; break; /* W */
	    case 13: data[lcv-firstvalidrec]=2925; break; /* WNW */
	    case 14: data[lcv-firstvalidrec]=3150; break; /* NW */
	    case 15: data[lcv-firstvalidrec]=3375; break; /* NNW */
	    case 255: data[lcv-firstvalidrec]=ORB_GAP_FILL; break;
	    default: data[lcv-firstvalidrec]=ORB_GAP_FILL; break;
	}

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("AvgWindDir",data,lcv-firstvalidrec,samprate,firsttimestamp,0.1,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }

  /* Dir of Prevailing Wind Alternate Representation */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=aArchiveData[lcv].iPrevailingWindDir;

      if (data[lcv-firstvalidrec]==255)
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("AvgWindDirAlt",data,lcv-firstvalidrec,samprate,firsttimestamp,1,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }

  /* avg ultraviolet */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      if (aArchiveData[lcv].iAvgUVIndex!=255)
	data[lcv-firstvalidrec]=aArchiveData[lcv].iAvgUVIndex;
      else
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("AvgUVIndex",data,lcv-firstvalidrec,samprate,firsttimestamp,0.1,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* Evapotranspiration */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=aArchiveData[lcv].iET;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("EvapoTranspiration",data,lcv-firstvalidrec,samprate,firsttimestamp,0.001*25.4,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* high solar radiation */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      if ((aArchiveData[lcv].iHighSolarRadiation)!=32767)
	data[lcv-firstvalidrec]=(aArchiveData[lcv].iHighSolarRadiation);
      else
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("HighSolarRad",data,lcv-firstvalidrec,samprate,firsttimestamp,1,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* high UV index */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=aArchiveData[lcv].iHighUVIndex;

      if (data[lcv-firstvalidrec]==255)
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("HighUVIndex",data,lcv-firstvalidrec,samprate,firsttimestamp,0.1,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* Forecast Rule */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=aArchiveData[lcv].iForecastRule;

      if (data[lcv-firstvalidrec]==193)
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("ForecastRule",data,lcv-firstvalidrec,samprate,firsttimestamp,1,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* Leaf Temp #1 */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=aArchiveData[lcv].aLeafTemps[0]-90-32;

      if (data[lcv-firstvalidrec]==133)
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("LeafTemp1",data,lcv-firstvalidrec,samprate,firsttimestamp,5.0/9.0,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* Leaf Temp #2 */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=aArchiveData[lcv].aLeafTemps[1]-90-32;

      if (data[lcv-firstvalidrec]==133)
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("LeafTemp2",data,lcv-firstvalidrec,samprate,firsttimestamp,5.0/9.0,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* Leaf Wetness #1 */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=aArchiveData[lcv].aLeafMoistures[0];

      if (data[lcv-firstvalidrec]==255)
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("LeafMoisture1",data,lcv-firstvalidrec,samprate,firsttimestamp,1,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* Leaf Wetness #2 */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=aArchiveData[lcv].aLeafMoistures[2];

      if (data[lcv-firstvalidrec]==255)
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("LeafMoisture2",data,lcv-firstvalidrec,samprate,firsttimestamp,1,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* soil temp 1 */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=aArchiveData[lcv].aSoilTemps[0]-32-90;

      if (data[lcv-firstvalidrec]==133)
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("SoilTemp1",data,lcv-firstvalidrec,samprate,firsttimestamp,5.0/9.0,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* soil temp 2 */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=aArchiveData[lcv].aSoilTemps[1]-32-90;

      if (data[lcv-firstvalidrec]==133)
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("SoilTemp2",data,lcv-firstvalidrec,samprate,firsttimestamp,5.0/9.0,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* soil temp 3 */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=aArchiveData[lcv].aSoilTemps[2]-32-90;

      if (data[lcv-firstvalidrec]==133)
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("SoilTemp3",data,lcv-firstvalidrec,samprate,firsttimestamp,5.0/9.0,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* soil temp 4 */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=aArchiveData[lcv].aSoilTemps[3]-32-90;

      if (data[lcv-firstvalidrec]==133)
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("SoilTemp4",data,lcv-firstvalidrec,samprate,firsttimestamp,5.0/9.0,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* download record type 0xFF=A 0x00=B */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=aArchiveData[lcv].iDownloadRecType;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("DownloadRecType",data,lcv-firstvalidrec,samprate,firsttimestamp,1,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* Extra Humidity 1 */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=aArchiveData[lcv].aExtraHumidities[0];

      if (data[lcv-firstvalidrec]==255)
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("ExtraHumidity1",data,lcv-firstvalidrec,samprate,firsttimestamp,1,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* Extra Humidity 2 */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=aArchiveData[lcv].aExtraHumidities[1];

      if (data[lcv-firstvalidrec]==255)
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("ExtraHumidity2",data,lcv-firstvalidrec,samprate,firsttimestamp,1,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* extra temp 1 */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=aArchiveData[lcv].aExtraTemps[0]-90-32;

      if (data[lcv-firstvalidrec]==133)
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("ExtraTemp1",data,lcv-firstvalidrec,samprate,firsttimestamp,5.0/9.0,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* extra temp 2 */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=aArchiveData[lcv].aExtraTemps[1]-90-32;

      if (data[lcv-firstvalidrec]==133)
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("ExtraTemp2",data,lcv-firstvalidrec,samprate,firsttimestamp,5.0/9.0,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* extra temp 3 */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=aArchiveData[lcv].aExtraTemps[2]-90-32;

      if (data[lcv-firstvalidrec]==133)
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("ExtraTemp3",data,lcv-firstvalidrec,samprate,firsttimestamp,5.0/9.0,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* extra soil moisture 1 */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=aArchiveData[lcv].aSoilMoistures[0];

      if (data[lcv-firstvalidrec]==255)
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("SoilMoisture1",data,lcv-firstvalidrec,samprate,firsttimestamp,1,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* extra soil moisture 2 */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=aArchiveData[lcv].aSoilMoistures[1];

      if (data[lcv-firstvalidrec]==255)
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("SoilMoisture2",data,lcv-firstvalidrec,samprate,firsttimestamp,1,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* extra soil moisture 3 */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=aArchiveData[lcv].aSoilMoistures[2];

      if (data[lcv-firstvalidrec]==255)
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("SoilMoisture3",data,lcv-firstvalidrec,samprate,firsttimestamp,1,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /* extra soil moisture 4 */
  previoustimestamp=lastdownloadtimestamp_start;
  for (lcv=firstvalidrec;lcv<iRecordCount && (previoustimestamp < davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp)) && aArchiveData[lcv].iDateStamp!=0xFFFF && aArchiveData[lcv].iTimeStamp!=0xFFFF;lcv++)
    {

      data[lcv-firstvalidrec]=aArchiveData[lcv].aSoilMoistures[3];

      if (data[lcv-firstvalidrec]==255)
	data[lcv-firstvalidrec]=ORB_GAP_FILL;

      timestamp=davisDateTimeToEpoch(aArchiveData[lcv].iDateStamp,aArchiveData[lcv].iTimeStamp);

      if (previoustimestamp>0)
	{
	  if (abs(timestamp-previoustimestamp-1/samprate)>(0.05*1/samprate) && oConfig.bForceIgnoreTiming==FALSE)
	    {
	      elog_notify(0,"Data Gap or timing error exceeding 5 percent allowance (gap=%0.2f desired gap=%0.2f mins)\n",(timestamp-previoustimestamp)/60.0,(1.0/samprate)/60.0);
	      elog_complain(0,"Help! someone should have implemented a work around for this\n");
	      davisCleanup(-1);
	    }
	}

      previoustimestamp=timestamp;
      if (lastdownloadtimestamp<timestamp)
	lastdownloadtimestamp=timestamp;
      if (firsttimestamp<1)
	firsttimestamp=timestamp;
    }
  
  pktchan=buildChannel("SoilMoisture4",data,lcv-firstvalidrec,samprate,firsttimestamp,1,srcparts);
  if (pktchan != NULL)
    {
      pushtbl(orbpkt->channels,pktchan);
      orbpkt->nchannels++;
    }


  /***** END SEARCH THROUGH ALL OF THE DATA AND LOAD CHANNELS *****/

  if (oConfig.bVerboseModeFlag == TRUE)
    {
      if (lcv==iRecordCount || aArchiveData[lcv].iDateStamp==0xFFFF || aArchiveData[lcv].iTimeStamp==0xFFFF)
	elog_notify(0,"Finished processing records because we exhausted the number of records downloaded.\n");
      else
	elog_notify(0,"Finished processing records because we ran into a record that was in the wrong order. This should be ok.\n");
    }

  if (orbpkt->nchannels>0)
    {
      if (stuffPkt(orbpkt,generatedSourceName,&firsttimestamp,&packet,&nbytes,&packetsz)<0)
	{
	  elog_complain(0,"stuffPKt() failed in sendPkt\n");
	  davisCleanup(-1);
	}
      
      if (oConfig.bVerboseModeFlag == TRUE)
	showPkt(0,generatedSourceName,firsttimestamp,packet,nbytes,stderr,PKT_UNSTUFF);
      
      if (orbput(orbfd,generatedSourceName,firsttimestamp,packet,nbytes))
	{
	  elog_complain(0,"orbput() failed in sendPkt\n");
	  davisCleanup(-1);
	}

      bury(); /* update state file */

      if (oConfig.bVerboseModeFlag == TRUE)
	elog_notify(0,"packet submitted under %s\n",generatedSourceName);
    }
  
  freePkt(orbpkt);
  return RESULT_SUCCESS;
}


PktChannel* buildChannel(char *sChan_Name, int *data, int numsamp, double samprate, double firstsampletime, double calib, Srcname srcparts)
{
  char *tmp, buf[5000], tmp2[5000];
  Tbl *channametbl=NULL;
  PktChannel *pktchan;
  int lcv;

  for (lcv=0;lcv<numsamp && data[lcv]==ORB_GAP_FILL;lcv++)
    ;

  if (lcv==numsamp)
    {
     if (oConfig.bVerboseModeFlag == TRUE)
       elog_notify(0,"channel %s: did not contain any valid data in %d records. It will not be included in the orb packet.\n",sChan_Name,numsamp);
     return(NULL);
    }

  /* Send the data to the ORB */
  pktchan=newPktChannel();
  if (pktchan==NULL)
    {
      elog_complain(1,"creating newPktChannel in buildChannel:");
      davisCleanup(-1);
    }

  pktchan->datasz=numsamp;
  pktchan->nsamp=numsamp;
	  
  pktchan->data=malloc(4*numsamp);
  if (pktchan->data==NULL)
    {
      elog_complain(1,"malloc failed in buildChannel:");
      davisCleanup(-1);
    }
	  
  bcopy(data,pktchan->data,4*numsamp);
  pktchan->time=firstsampletime;
	  
  strncpy(pktchan->net,srcparts.src_net,PKT_TYPESIZE);
  strncpy(pktchan->sta,srcparts.src_sta,PKT_TYPESIZE);

  sprintf(buf,"%s{Channels}{%s}",oConfig.sBaseSrcName,sChan_Name);
  tmp=pfget_string(configpf,buf);
  if (tmp == NULL)
    {
      elog_complain(0,"Parameter file (%s) missing channel name for variable \'%s\'.\n",oConfig.sParamFileName,buf);
      davisCleanup(-1);
    }

  strncpy(tmp2,tmp,5000);
  channametbl=split(tmp2,' ');
  if (maxtbl(channametbl)>=1 && !strcmp(gettbl(channametbl,0),"NULL"))
    {
      if (oConfig.bVerboseModeFlag == TRUE)
	elog_notify(0,"Channel name for %s marked as NULL, ignoring this channel\n",buf);
      freetbl(channametbl,0);
      freePktChannel(pktchan);
      return NULL;
    }
  else if (maxtbl(channametbl)!=2)
    {
      elog_complain(0,"channel incorrectly specified in parameter file (%s) for variable %s\nPlease use:\n\t%s    chan_name segtype\n\n\twhere segtype is one character, chan_name is a name for this channel in less than 8 chars and the two items are seperated by regular spaces.\n",oConfig.sParamFileName,buf,sChan_Name);
      davisCleanup(-1);
    }

  if (strlen(gettbl(channametbl,0))>8)
    {
      elog_complain(0,"Parameter file (%s): channel name for variable \'%s\' longer than 8 characters (%d chars).\n",oConfig.sParamFileName,buf,strlen(gettbl(channametbl,0)));
      davisCleanup(-1);
    }
  strncpy(pktchan->chan,gettbl(channametbl,0),PKT_TYPESIZE);

  *(pktchan->loc)='\0';
  strncpy(pktchan->segtype,gettbl(channametbl,1),2);
  pktchan->calib=calib;
  pktchan->calper=-1;
  pktchan->samprate=samprate;

  if (oConfig.bVerboseModeFlag == TRUE)
    elog_notify(0,"added channel %s: %d samples\n",gettbl(channametbl,0),numsamp);

  freetbl(channametbl,0);

  return pktchan;
}

int davisSetScreenMode ()
{
  /* Initialize */
  char   sCmd         [MAX_BUFFER_SIZE];
  char   sCmdResponse [MAX_BUFFER_SIZE];

  if (oConfig.bVerboseModeFlag)
      elog_notify(0,"Sending RXTEST command to davis to make sure screen is in the correct mode\n");
  
  if (davisExecCommand("RXTEST",sCmdResponse,COMMAND_LF)==RESULT_FAILURE)
  {
      elog_complain(0,"Failed to send RXTEST command to davis.\n");
      return RESULT_FAILURE;
  }
  
  /* I send this in case the above command does not exist, then I will still have text to read so I won't loose my socket */
  if (davisExecCommand("TEST",sCmdResponse,COMMAND_LF)==RESULT_FAILURE)       
  {                                                                             
      elog_complain(0,"Failed to send TEST command to davis.\n");             
      return RESULT_FAILURE;                                                    
  }     

  if (strncmp(sCmdResponse,"\rOK",3) && strncmp(sCmdResponse,"OK",2))
  {
  	elog_complain(0,"RXTEST response not OK! Response=\"%s\". Perhaps it is not supported in this davis?\n",sCmdResponse);
      	/* we don't want to fail since this command may not be supported by all loggers and it may not be neccessary */
  }
  
  return flushOutput(&iConnectionHandle); /* since we are unsure of the state of the buffer */
}

/*
**  Sets the current Sample Rate of the Davis to the specified value.  Note
**  that the value is in minutes and must be one of the following:
**
**    (1, 5, 10, 15, 30, 60, 120)
**
**  Returns RESULT_SUCCESS if the setting was successful, or 
**  RESULT_FAILURE if an error occurred.
**
**  WARNING: Setting the Davis sample rate will cause all historical data to
**           be deleted.  The Davis does this to ensure all data is sampled
**           at the same rate.
*/

int davisSetSampleRate (int iSampleRate) 
{
  /* Initialize */
  char   sCmd         [MAX_BUFFER_SIZE];
  char   sCmdResponse [MAX_BUFFER_SIZE];
 

  /* Check for valid sample rate */
  if ((iSampleRate != 1) &&
      (iSampleRate != 5) &&
      (iSampleRate != 10) &&
      (iSampleRate != 15) &&
      (iSampleRate != 30) &&
      (iSampleRate != 60) &&
      (iSampleRate != 120)) 
    {
      elog_complain (0, "davisSetSampleRate(): Invalid sample rate (%i) specified.  Must be 1, 5, 10, 15, 30, 60, or 120 minutes.", iSampleRate);
      
      return RESULT_FAILURE;
    }

 

  /* Wake up the Davis and continue */
  if (davisWakeUp () == RESULT_SUCCESS) 
    {
      /* Send the SETPER command to set sample rate */
      sprintf (sCmd, "SETPER %d", iSampleRate);
      if (davisExecCommand (sCmd, sCmdResponse, COMMAND_OK_NOEXTRA) != RESULT_SUCCESS) 
	{
	  elog_complain (0, "davisSetSampleRate(): Error executing 'SETPER' command.\n");	
	  return RESULT_FAILURE;
	}

      /* Send the CLRLOG command to clear data */
      sprintf (sCmd, "CLRLOG");
      if (davisExecCommand (sCmd, sCmdResponse, COMMAND_ACK) != RESULT_SUCCESS) 
	{
	  elog_complain (0, "davisSetSampleRate(): Error executing 'CLRLOG' command.\n");	
	  return RESULT_FAILURE;
	}
      
      /* Send the START command to make sure the data logger archive mode is enabled */
      sprintf (sCmd, "START");
      if (davisExecCommand (sCmd, sCmdResponse, COMMAND_OK_NOEXTRA) != RESULT_SUCCESS)
        {
          elog_complain (0, "davisSetSampleRate(): Error executing 'START' command.\n");
          return RESULT_FAILURE;
        }
      
      if (oConfig.bVerboseModeFlag == TRUE)
	elog_notify(0,"Set davis internal sample interval successful.\n");

      /* Return SUCCESS if we got here */
      return RESULT_SUCCESS;
    }

  return RESULT_FAILURE;
}

/* 267 byte packet -> structure */
void unpackArchivePage(struct stArchivePage *ap, unsigned char *buf)
{
  int lcv=0;
  
  ap->iSequenceNumber=((unsigned char*)buf)[0];
  for (lcv=0;lcv<5;lcv++)
  {
    ap->aArchiveData[lcv].iDateStamp=buf[lcv*52+1+1]*256+buf[lcv*52+1+0];
    ap->aArchiveData[lcv].iTimeStamp=buf[lcv*52+1+3]*256+buf[lcv*52+1+2];
    ap->aArchiveData[lcv].iOutsideTemp=(buf[lcv*52+1+5]*256+buf[lcv*52+1+4]);
    ap->aArchiveData[lcv].iHighOutsideTemp=(buf[lcv*52+1+7]*256+buf[lcv*52+1+6]);
    ap->aArchiveData[lcv].iLowOutsideTemp=(buf[lcv*52+1+9]*256+buf[lcv*52+1+8]);
    ap->aArchiveData[lcv].iRainLevel=(buf[lcv*52+1+11]*256+buf[lcv*52+1+10]);
    ap->aArchiveData[lcv].iHighRainRate=buf[lcv*52+1+13]*256+buf[lcv*52+1+12];
    ap->aArchiveData[lcv].iBarometer=buf[lcv*52+1+15]*256+buf[lcv*52+1+14];
    ap->aArchiveData[lcv].iSolarRadiation=buf[lcv*52+1+17]*256+buf[lcv*52+1+16];
    ap->aArchiveData[lcv].iWindSampleCount=buf[lcv*52+1+19]*256+buf[lcv*52+1+18];
    ap->aArchiveData[lcv].iInsideTemp=buf[lcv*52+1+21]*256+buf[lcv*52+1+22];
    ap->aArchiveData[lcv].iInsideHumidity=buf[lcv*52+1+22];
    ap->aArchiveData[lcv].iOutsideHumidity=buf[lcv*52+1+23];
    ap->aArchiveData[lcv].iAvgWindSpeed=buf[lcv*52+1+24];
    ap->aArchiveData[lcv].iHighWindSpeed=buf[lcv*52+1+25];
    ap->aArchiveData[lcv].iHighWindSpeedDir=buf[lcv*52+1+26];
    ap->aArchiveData[lcv].iPrevailingWindDir=buf[lcv*52+1+27];
    ap->aArchiveData[lcv].iAvgUVIndex=buf[lcv*52+1+28];
    ap->aArchiveData[lcv].iET=buf[lcv*52+1+29];
    ap->aArchiveData[lcv].iHighSolarRadiation=buf[lcv*52+1+31]*256+buf[lcv*52+1+30];
    ap->aArchiveData[lcv].iHighUVIndex=buf[lcv*52+1+32];
    ap->aArchiveData[lcv].iForecastRule=buf[lcv*52+1+33];
    ap->aArchiveData[lcv].aLeafTemps[0]=buf[lcv*52+1+34];
    ap->aArchiveData[lcv].aLeafTemps[1]=buf[lcv*52+1+35];
    ap->aArchiveData[lcv].aLeafMoistures[0]=buf[lcv*52+1+36];
    ap->aArchiveData[lcv].aLeafMoistures[1]=buf[lcv*52+1+37];
    ap->aArchiveData[lcv].aSoilTemps[0]=buf[lcv*52+1+38];
    ap->aArchiveData[lcv].aSoilTemps[1]=buf[lcv*52+1+39];
    ap->aArchiveData[lcv].aSoilTemps[2]=buf[lcv*52+1+40];
    ap->aArchiveData[lcv].aSoilTemps[3]=buf[lcv*52+1+41];
    ap->aArchiveData[lcv].iDownloadRecType=buf[lcv*52+1+42];
    ap->aArchiveData[lcv].aExtraHumidities[0]=buf[lcv*52+1+43];
    ap->aArchiveData[lcv].aExtraHumidities[1]=buf[lcv*52+1+44];
    ap->aArchiveData[lcv].aExtraTemps[0]=buf[lcv*52+1+45];
    ap->aArchiveData[lcv].aExtraTemps[1]=buf[lcv*52+1+46];
    ap->aArchiveData[lcv].aExtraTemps[2]=buf[lcv*52+1+47];
    ap->aArchiveData[lcv].aSoilMoistures[0]=buf[lcv*52+1+48];
    ap->aArchiveData[lcv].aSoilMoistures[1]=buf[lcv*52+1+49];
    ap->aArchiveData[lcv].aSoilMoistures[2]=buf[lcv*52+1+50];
    ap->aArchiveData[lcv].aSoilMoistures[3]=buf[lcv*52+1+51];

  }
  ap->iCRC=((unsigned char*)buf)[262]*256+((unsigned char*)buf)[263];

}

void unpackDMPAFTResp(struct stDavisDMPAFTResponse *resp, unsigned char *buf)
{
  resp->iRecordCount=buf[1]*256+buf[0];
  resp->iFirstValidRecord=buf[3]*256+buf[2];
  resp->iCRC=buf[4]*256+buf[5];
}

void unpackstDavisGetSetTime(struct stDavisGetSetTime *t, unsigned char *buf)
{
  t->iSecond=buf[0];
  t->iMinute=buf[1];
  t->iHour=buf[2];
  t->iDay=buf[3];
  t->iMonth=buf[4];
  t->iYearMinus1900=buf[5];
  t->iCRC=buf[7]*256+buf[6];
}

void packstDavisGetSetTime(struct stDavisGetSetTime *t, unsigned char *buf)
{
  buf[0]=t->iSecond;
  buf[1]=t->iMinute;
  buf[2]=t->iHour;
  buf[3]=t->iDay;
  buf[4]=t->iMonth;
  buf[5]=t->iYearMinus1900;
  buf[6]=t->iCRC/256;
  buf[7]=t->iCRC%256;
}

/*
**  Main program loop
*/
int main (int iArgCount, char *aArgList []) {

  int                    iRecordCount = 0; 
  double                 previous_start=0;
  double                 parameter_lastdownloadtimestamp=0;
  int                    ch;
  Relic                  relic;
  int                    year, mon, day, hour, min;
  int                    zerodata_current=FALSE;
  int                    bReprogramDavisNow=FALSE;
  double                 time_firstzerodata_download; 
                         /* time stamp from the first zero data download */

  /* Initialize */
  elog_init (iArgCount, aArgList);

  /* Parse out commandline options */
  if (parseCommandLineOptions (iArgCount, aArgList) == RESULT_SUCCESS) {

    /* Connect to ORB */
    if ((orbfd=orbopen(oConfig.sOrbName,"w&"))<0) {
      elog_complain (1, "orbopen: unable to connect to ORB \"%s\".",oConfig.sOrbName);
      davisCleanup (-1);
    }

    if (oConfig.sStateFileName != NULL)
      {
	ch=exhume(oConfig.sStateFileName,NULL,0,0);
	if (ch<0)
	  {
	    elog_complain(1,"state file exhume failed, returned %d for %s.\n",ch,oConfig.sStateFileName);
	    davisCleanup(-1);
	  }

	if (ch==0)
	  elog_notify(0,"no saved state file found. starting from scratch (%s).\n",oConfig.sStateFileName);
	
	if (lastdownloadtimestamp>0)
	  parameter_lastdownloadtimestamp=lastdownloadtimestamp;

	relic.dp=&lastdownloadtimestamp;
	if(resurrect("previousTimestamp",relic,DOUBLE_RELIC)==0)
	  fprintf(stderr,"resurrected previousTimestamp %f\n",lastdownloadtimestamp);
	else
	  lastdownloadtimestamp=0;

	if (oConfig.bKickStateFile==TRUE)
	  {
	    lastdownloadtimestamp=0;
	    elog_notify(0,"kicking the statefile. I will start from the beginning of the buffer and I will overwrite the value in the state file.");
	  }

	if (parameter_lastdownloadtimestamp>0)
	  {
	    elog_notify(0,"ignoring state file timestamp using %f (%s) from -t command line argument.\n",parameter_lastdownloadtimestamp,strtime(parameter_lastdownloadtimestamp));
	    lastdownloadtimestamp=parameter_lastdownloadtimestamp;
	  }
      }

    /*** BEGIN MAIN LOOP ***/
    do {

      /* Try reading the param file */
      if (paramFileRead () == RESULT_FAILURE) {
        elog_complain (1, "main(): Error encountered during paramFileRead() operation.");
        davisCleanup (-1);
      }

      /* Try connecting to the Davis */
      if (davisConnect (oConfig.iConnectionType, oConfig.sConnectionParams) == RESULT_SUCCESS) {

	if (davisWakeUp() != RESULT_SUCCESS)
	{
	  elog_complain(0,"Failed to wakeup Davis\n");
	  close(iConnectionHandle);
	  iConnectionHandle=-1;
	}

        if (iConnectionHandle>=0 && oConfig.bInitalizeDavis == FALSE)
	  StatPacket(&iConnectionHandle);

        if (iConnectionHandle>=0 && oConfig.bInitalizeDavis == FALSE)
	{
	    /* Execute DMPAFT to get new data since last date/time stored in state file */
	    if (lastdownloadtimestamp>0)
	      {
		if (oConfig.sTimeZone)
		  {
		    year=atoi(zepoch2str(lastdownloadtimestamp,"%Y", oConfig.sTimeZone));
		    mon=atoi(zepoch2str(lastdownloadtimestamp,"%m", oConfig.sTimeZone));
		    day=atoi(zepoch2str(lastdownloadtimestamp,"%d", oConfig.sTimeZone));
		    hour=atoi(zepoch2str(lastdownloadtimestamp,"%H", oConfig.sTimeZone));
		    min=atoi(zepoch2str(lastdownloadtimestamp,"%M", oConfig.sTimeZone));
		  }
		else
		  {
		    year=atoi(epoch2str(lastdownloadtimestamp,"%Y"));
		    mon=atoi(epoch2str(lastdownloadtimestamp,"%m"));
		    day=atoi(epoch2str(lastdownloadtimestamp,"%d"));
		    hour=atoi(epoch2str(lastdownloadtimestamp,"%H"));
		    min=atoi(epoch2str(lastdownloadtimestamp,"%M"));
		  }

		iRecordCount = davisExecDMPAFT (year, mon, day, hour, min, FALSE); 
	      }
	    else
	      iRecordCount = davisExecDMPAFT (2000, 0, 0, 0, 0, FALSE); 
	}

        if (iRecordCount == RESULT_FAILURE && oConfig.bInitalizeDavis == FALSE) {
          elog_complain (1, "main(): Error encountered during davisExecDMPAFT () operation. Disconnecting.");
	  close(iConnectionHandle);
	  iConnectionHandle=-1;
        }
	
	if (iRecordCount > 0)
	  zerodata_current=FALSE;	  
	else if (iRecordCount == 0 && zerodata_current == FALSE)
	  {
	    zerodata_current=TRUE;
	    time_firstzerodata_download=now();
	  }

	if (oConfig.bAutoProgramDavis == TRUE && zerodata_current == TRUE && (time_firstzerodata_download < now()-24*3600))
	  {
	    elog_complain(0,"We have been unable to retrieve data from the davis for greater than 24 hrs. Attempting to reinitialize the davis.\n");

	    time_firstzerodata_download=now(); 
	    /* reset this so we don't try to reprogram the davis 
	       more than once per 24 hr period */
	    bReprogramDavisNow=TRUE;
	  }

	/* set the davis screen mode */
	if ((oConfig.bConfigScreenMode == TRUE || bReprogramDavisNow == TRUE) && iConnectionHandle>=0)
	{
	    if (oConfig.bVerboseModeFlag == TRUE)
		elog_notify(0,"setting the davis screen mode to allow internal data archiving.\n");
	    if (davisSetScreenMode() == RESULT_FAILURE)
	      {
		elog_complain(0,"Failed to set davis screen mode\n");
		davisCleanup(-1);
	      }
	}

	/* set davis internal sample rate */
	if ((oConfig.bSkipDavisRateSetFlag==FALSE || bReprogramDavisNow == TRUE) && iConnectionHandle>=0)
	  {
	    if (oConfig.bVerboseModeFlag == TRUE)
	     elog_notify(0,"setting davis internal sample rate to %d\n",oConfig.iDavisSampleInterval_toset);
	    if (davisSetSampleRate (oConfig.iDavisSampleInterval_toset)==RESULT_FAILURE)
	      {
		elog_complain(0,"Failed to set sample rate\n");
		davisCleanup(-1);
	      }
	  }

	/* set time */
	if ((oConfig.bSetDavisClock==TRUE || bReprogramDavisNow == TRUE) && iConnectionHandle>=0)
	  {
	    if (oConfig.bVerboseModeFlag == TRUE)
	     elog_notify(0,"setting davis time\n");
	    if (davisSetTime() == RESULT_FAILURE)
	      {
		elog_complain(0,"Failed to set time in the davis.\n");
		davisCleanup(-1);
	      }
	  }

	bReprogramDavisNow=FALSE;

        /* We're done for now -- close the connection */
	    
	    /* Sleep for specified interval, or exit */
	    if (oConfig.iRepeatInterval > 0 && oConfig.bKickStateFile == FALSE && oConfig.bSkipDavisRateSetFlag == TRUE && oConfig.bSetDavisClock == FALSE && oConfig.bConfigScreenMode == FALSE) {

	      if (iConnectionHandle < 0)
		{
		  elog_notify(0,"since the last run closed the connection to the davis early, we are going to sleep for the full time interval (%d sec)\n",oConfig.iRepeatInterval);
		  sleep(oConfig.iRepeatInterval);
		}
	      else
		{
		  closeAndFreeHandle (&iConnectionHandle);
		  if (oConfig.bAutoAdjustSleepFlag==TRUE)
		    {
		      if (oConfig.bVerboseModeFlag == TRUE)
			elog_notify(0,"AutoAdjusting Sleep Interval will sleep %d seconds (%f %d %d %f)\n",(int)(lastdownloadtimestamp+oConfig.iDavisSampleInterval*60+5-skewlog-now()),lastdownloadtimestamp,oConfig.iDavisSampleInterval*60,skewlog,now());

		      if ((int)(lastdownloadtimestamp+oConfig.iDavisSampleInterval*60+5-skewlog-now())>0 && ((int)(lastdownloadtimestamp+oConfig.iDavisSampleInterval*60+5-skewlog-now())<oConfig.iRepeatInterval*2 || (int)(lastdownloadtimestamp+oConfig.iDavisSampleInterval*60+5-skewlog-now())<2*oConfig.iDavisSampleInterval))
			{
			  sleep(lastdownloadtimestamp+oConfig.iDavisSampleInterval*60+5-skewlog-now());
			}
		      else if ((int)(lastdownloadtimestamp+oConfig.iDavisSampleInterval*60+5-skewlog-now())<=0)
		      {
			  elog_notify(0,"Since sleep would have been less than zero, we are going to sleep %d seconds to avoid an infinite loop.\n",oConfig.iRepeatInterval);
			  sleep(oConfig.iRepeatInterval);
		      }
		      else 
		      {
			  elog_notify(0,"Since sleep would have been longer than 2*repeat interval (%d sec) and longer than 2*sample interval (%d sec), we are going to sleep for %d seconds on the assumption that there is problem with the local clock.\n",oConfig.iRepeatInterval*2,2*oConfig.iDavisSampleInterval*60,2*oConfig.iDavisSampleInterval*60);
			  sleep(2*oConfig.iDavisSampleInterval*60);
		      }
		    }
		  else if (previous_start>0)
		    {
		      if (oConfig.bVerboseModeFlag == TRUE)
			elog_notify (0, "main (): Sleeping for %d second(s).\n", (int)(oConfig.iRepeatInterval-now()+previous_start));
		      if (oConfig.iRepeatInterval-now()+previous_start >= 1)
			sleep (oConfig.iRepeatInterval-now()+previous_start);
		      else 
			{
			  elog_notify(0,"Whoops. We planned to sleep %d seconds. I'm going to sleep %d seconds instead.\n",(int)(oConfig.iRepeatInterval-now()+previous_start),oConfig.iRepeatInterval);
			  sleep(oConfig.iRepeatInterval);
			}
		      
		    }
		  else
		    {
		      if (oConfig.bVerboseModeFlag == TRUE)
			elog_notify (0, "main (): Sleeping for %d second(s).\n", oConfig.iRepeatInterval);
		      sleep (oConfig.iRepeatInterval);	    
		    }
		}
	      previous_start=now();
	      
	      if (oConfig.bVerboseModeFlag == TRUE)
		elog_notify (0, "main (): Done sleeping; waking up.\n");
	    }
	    else {         
	      closeAndFreeHandle (&iConnectionHandle);

	      if (oConfig.bVerboseModeFlag == TRUE)
		{
		  if (oConfig.bKickStateFile == FALSE && oConfig.bSkipDavisRateSetFlag == TRUE && oConfig.bSetDavisClock == FALSE && oConfig.bConfigScreenMode == FALSE)
		    elog_notify (0, "main (): No repeat interval specified; exiting.\n");
		  else
		    elog_notify (0, "main (): repeat interval ignored due to -k, -t, -T, -m, -1, or -i options; exiting.\n");
		}
	      davisCleanup (0);
	    }
      }

      /* Else unable to connect, cleanup with failure (-1) exit code */
      else
        davisCleanup (-1);
    }
    /*** END MAIN LOOP (Run forever until error or forced break-out) ***/
    while (TRUE);
  }

  /* If we got this far, cleanup with success (0) exit code */
  davisCleanup (0);
  return(0);
}
