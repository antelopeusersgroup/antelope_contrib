
/****************** socket_ew_common *************************/
/********************* #INCLUDES *****************************/
/*************************************************************/
#include <errno.h>
#include "socket_ew.h"


/********************** GLOBAL *******************************/
/********************* VARIABLES *****************************/
/*************************************************************/
int SELECT_TIMEOUT_SECONDS=0;
int SELECT_TIMEOUT_uSECONDS=200000;
int EW_SOCKET_DEBUG=0;

extern int SOCKET_SYS_INIT;  /* Global initialization flag.
                                Declared in sys-dependent socket_ew.c,
                                set in SocketSysInit(), 
                                checked in socket_ew()  */                                

/********************* SOCKET_ew *****************************/
/*********** Internal Utility Function Prototypes ************/
/*************************************************************/
struct timeval FAR * resetTimeout(struct timeval FAR *);
Time_ew adjustTimeoutLength(int timeout_msec);


/********************* SOCKET_ew *****************************/
/********************* Functions *****************************/
/*************************************************************/

SOCKET socket_ew (int af, int type, int protocol)
{
  /* socket_ew() allocates a socket descriptor and associated 
     resources. It first makes sure that the Socket system has 
     been initialized, then it calls socket(), and finally sets 
     the socket descriptor to non-blocking mode.
     No network I/O occurs.
     Caller can call socketGetError_ew() for details about any 
     failures.
  */

  SOCKET newSocket;
  static char * MyFuncName = "socket_ew()";
  int retVal;
  unsigned long lOnOff=1;

  if (EW_SOCKET_DEBUG)
    logit("et","Entering %s\n",MyFuncName);

  if (!SOCKET_SYS_INIT)
    SocketSysInit();

  newSocket=socket(af,type,protocol);
  if (newSocket == INVALID_SOCKET  && EW_SOCKET_DEBUG)
    logit("et","Error: %d, occurred in %s\n",
	  socketGetError_ew(),MyFuncName);

  if (newSocket != INVALID_SOCKET)
    {
      retVal=ioctlsocket(newSocket,FIONBIO,&lOnOff);
      if (retVal==SOCKET_ERROR)
	{
	  if(EW_SOCKET_DEBUG)
	    logit("et","Error: %d, occurred in %s during change to non-blocking\n",
		socketGetError_ew(),MyFuncName);
	  closesocket(newSocket);
	  return(SOCKET_ERROR);
	}
    }

  if(EW_SOCKET_DEBUG)
    logit("et","Exiting %s\n",MyFuncName);
  return(newSocket);
}

/*************************************************************/

int connect_ew(SOCKET s, struct sockaddr FAR* name, 
	       int namelen, int timeout_msec)
{
  /* connect_ew() attempts to create a socket connection during a
   * period specified by timeout.  If it succeeds it returns a
   * successful condition.  If it fails either due to a network
   * error, or a timeout, it closes the socket and returns an error.  
   * *Note:  The timeout clock starts after connect_ew() calls
   * connect(), not when connect_ew() starts.
   * A timeout value of -1 causes connect_ew() to revert to a blocking
   * connect call.
   * Caller can call socketGetError_ew() for details about any 
   * failures.
   */

   static char * MyFuncName = "connect_ew()";

   int     retVal, ioctlRetVal, connectRetVal, selectRetVal;
   fd_set  ConnectedSockets;
   long    lOnOff;
   int     lastError;
   struct  timeval SelectTimeout;
  
  if ( EW_SOCKET_DEBUG )
    logit( "et" , "Entering %s, timeout_msec=%d\n", MyFuncName, timeout_msec );

/* If there is no timeout, make the socket blocking
   ************************************************/
   if ( timeout_msec == -1 )
   {
      lOnOff = 0;
      ioctlRetVal = ioctlsocket( s, FIONBIO, &lOnOff );
      if ( ioctlRetVal == SOCKET_ERROR )
      {
         if ( EW_SOCKET_DEBUG )
            logit( "et", "Error: %d, occurred in %s during change to blocking\n",
                socketGetError_ew(), MyFuncName );
         retVal = -1;
         goto Done;
      }

/* Try to get a connection (blocking)
   **********************************/
      if ( connect( s, name, namelen ) == 0 )     /* Got a connection */
         retVal = 0;
      else                                        /* Didn't get a connection */
         retVal = -1;

/* Change the socket back to non-blocking so
   we don't screw up any further operations
   *****************************************/
      lOnOff = 1;
      ioctlRetVal=ioctlsocket(s,FIONBIO,&lOnOff);
      if ( ioctlRetVal == SOCKET_ERROR )
      {
         if ( EW_SOCKET_DEBUG )
           logit( "et", "Error: %d, occurred in %s during change to non-blocking\n",
                 socketGetError_ew(), MyFuncName );
         retVal = -1;
      }
      goto Done;
   }

/* Initiate a non-blocking connection request
   ******************************************/
   connectRetVal = connect( s, name, namelen );

   if ( connectRetVal == 0 )                         /* Got a connection */
   {
      retVal = 0;
      goto Done;
   }

   lastError = socketGetError_ew();

   if ( lastError != CONNECT_WOULDBLOCK_EW )         /* Connect() error */
   {
      if ( EW_SOCKET_DEBUG )
         logit( "et", "Connect request failed in connect_ew(): %s.\n",
                 strerror(lastError) );
      retVal = -1;
      goto Done;
   }

/* Hang around in select() until connect is successful
                        or until timeout period expires
   ****************************************************/
   FD_ZERO( &ConnectedSockets );
   FD_SET( s, &ConnectedSockets );

   SelectTimeout.tv_sec  = timeout_msec/1000;
   SelectTimeout.tv_usec = (timeout_msec%1000)*1000;

   selectRetVal = select( s+1, 0, &ConnectedSockets, 0, &SelectTimeout );

/* select() failed 
   ***************/
   if ( selectRetVal == -1 )                           
   {
      if ( EW_SOCKET_DEBUG )
         logit( "et", "select() failed in connect_ew(). Error: %d\n",
                socketGetError_ew() );
      retVal = -1;
   }

/* select() succeeded; connection may have been completed
   ******************************************************/
   else if ( selectRetVal > 0  &&  FD_ISSET( s, &ConnectedSockets ) )  
   {                                             
   /* NOTE: For Solaris, we must do one more connection test.
    *       Other possible tests besides getsockopt(SO_ERROR) could 
    *       be a zero-length read() or a call to getpeername() 
    */    
      int error, len, rc;
      error = 0;
      len = sizeof(error);
      rc  = getsockopt(s, SOL_SOCKET, SO_ERROR, (char *)&error, &len); 

      if ( rc < 0 )          /* Pending error on some systems  */
      {
         error = socketGetError_ew(); 
         retVal = -1;
      }
      else if ( error )      /* Pending error on others systems */
      {
         retVal = -1;
      }
      else                   /* OK, got a connection! */
      {
         if ( EW_SOCKET_DEBUG ) 
            logit( "et", "Got a connection\n" );
         retVal = 0;
      }

      if ( retVal == -1  &&  EW_SOCKET_DEBUG ) 
         logit("et", "connect_ew() connection failed; "
                     "getsockopt detected error: %s.\n", 
                      strerror(error) ); 
   }

/* Only other possibility: select timed out! 
   *****************************************/
   else
   {
      if ( EW_SOCKET_DEBUG ) /* this line added by Alex 2/9/99 */
      logit( "et", "connect timed out in connect_ew().\n" );
      retVal = -1;
   }

Done:
   if ( retVal == -1 )
    /*closesocket_ew( s, SOCKET_CLOSE_IMMEDIATELY_EW );*/
      closesocket_ew( s, SOCKET_CLOSE_SIMPLY_EW ); /*skip setsockopt()*/

   if ( EW_SOCKET_DEBUG )
      logit("et","Exiting %s\n",MyFuncName);

   return(retVal);
}

/*************************************************************/

int bind_ew (SOCKET s, struct sockaddr FAR* name, int namelen )
{
  /* bind_ew() attempts to bind the socket s to a name/port number.
     Caller can call socketGetError_ew() for details about any failures.  
  */

  int retVal;
  static char * MyFuncName = "bind_ew()";

  if(EW_SOCKET_DEBUG)
    logit("et","Entering %s\n",MyFuncName);

  retVal=bind(s,name,namelen);
  if (retVal < 0  && EW_SOCKET_DEBUG)
    logit("et","Error: %d, occurred in %s\n",
	  socketGetError_ew(),MyFuncName);

  if(EW_SOCKET_DEBUG)
    logit("et","Exiting %s\n",MyFuncName);

  return(retVal);
}

/*************************************************************/

int listen_ew (SOCKET s, int backlog ) 
{
  /* listen_ew() signals the mysterious protocol stack god, that the
     socket is ready to accept connections.
     Caller can call socketGetError_ew() for details about any failures.  
  */

  int retVal;
  static char * MyFuncName = "listen_ew()";

  if(EW_SOCKET_DEBUG)
    logit("et","Entering %s\n",MyFuncName);

  retVal=listen(s,backlog);
  if (retVal < 0  && EW_SOCKET_DEBUG)
    logit("et","Error: %d, occurred in %s\n",
	  socketGetError_ew(),MyFuncName);

  if(EW_SOCKET_DEBUG)
    logit("et","Exiting %s\n",MyFuncName);

  return(retVal);
}

/*************************************************************/

SOCKET accept_ew(SOCKET s, struct sockaddr FAR* addr, int FAR* addrlen,
		 int timeout_msec)
{
  /* accept_ew() attempts to accept a connection on a socket.
     timeout is the length of time in millisec. that accept_ew() 
     will wait before returning.  Timeout is measure from the
     point after the initial accept() call.  Pass -1 for accept_ew to 
     revert to blocking accept() call.  If a successful
     connection is not accepted before the timeout expires, or
     if an error occurs, the function returns INVALID_SOCKET.  
     If the latest socket error was WOULDBLOCK_EW, then 
     no connections were requested during the timeout period.
     Caller can call socketGetError_ew() for details about 
     any failures. 
  */

  SOCKET newSocket;
  static char * MyFuncName = "accept_ew()";
  Time_ew StartTime;
  fd_set AcceptedSockets;
  struct timeval SelectTimeout; 
  Time_ew timeout=adjustTimeoutLength(timeout_msec);
  int retVal;
  long lOnOff;
  
  if(EW_SOCKET_DEBUG)
    logit("et","Entering %s\n",MyFuncName);

  /* If there is no timeout, make the socket blocking */
  if(timeout_msec == -1)
    {
      lOnOff = 0;
      retVal=ioctlsocket(s,FIONBIO,&lOnOff);
      if (retVal==SOCKET_ERROR)
	{
	  if(EW_SOCKET_DEBUG)
	    logit("et","Error: %d, occurred in %s during change to blocking\n",
		socketGetError_ew(),MyFuncName);
	  goto Abort;
	}
    }

  newSocket=accept(s,addr,addrlen);
  /* If there is no timeout, then the call was made blocking,
     change it back so that we don't screw up any further operations
  */
  if(timeout_msec == -1)
    {
      lOnOff = 1;
      retVal=ioctlsocket(s,FIONBIO,&lOnOff);
      if (retVal==SOCKET_ERROR)
	{
	  if(EW_SOCKET_DEBUG)
	    logit("et","Error: %d, occurred in %s during change to non-blocking\n",
		socketGetError_ew(),MyFuncName);
	  goto Abort;
	}
    }

  if (newSocket == INVALID_SOCKET)
    {
      if (socketGetError_ew() == WOULDBLOCK_EW)
	{
	  FD_ZERO(&AcceptedSockets);
	  FD_SET(s,&AcceptedSockets);
	  StartTime=GetTime_ew();
	  while((!select(s+1,&AcceptedSockets,0,0,resetTimeout(&SelectTimeout)))
		&& ((GetTime_ew()-timeout) < StartTime))
	    {
	      FD_ZERO(&AcceptedSockets);
	      FD_SET(s,&AcceptedSockets);
	      sleep_ew(1000);  /* Sleep for a second, and then try again.*/
	    }
	  newSocket=accept(s,addr,addrlen);
	}
      if(newSocket == INVALID_SOCKET && EW_SOCKET_DEBUG)
	{
	  logit("et","Error: %d, occurred in %s\n",
                socketGetError_ew(),MyFuncName);
	}
    }

  if(EW_SOCKET_DEBUG)
    logit("et","Exiting %s\n",MyFuncName);

  return(newSocket);
Abort:
  if (newSocket > 0) closesocket_ew(newSocket, 0);
  closesocket_ew(s, 0);
}

/*************************************************************/

int recv_all (SOCKET s,char FAR* buf,int len,int flags, int timeout_msec)
{
  /* recv_all attempts to receive data on a connection oriented scoket.
     timeout is the length of time in millisec. that the recv_ew() will wait
     before returning(if no data is received), after making the initial recv()
     call.  

     if timeout_msec > 0, recv_all() returns when the sooner of two things
     happens: 
     1.  The timeout measured in millisec. from the time of the first
     send() call, expires; 
     2.  "len" bytes of data are received.
     recv_all() returns the number of bytes of data received, or SOCKET_ERROR
     on error.  The caller is responsible for noting any discrepencies in the
     difference between the number of bytes requested to be sent, and the
     number of reported bytes sent.  If there is a discrepency, then a timeout
     occured.  Caller can call socketGetError_ew() for details about any
     failures.
     if timeout_msec == -1, recv_all() sets the socket to blocking and returns
     when:
     1. "len" bytes of data are received.
     2. EOF is detected by recv returning 0 bytes.
     */

  int retVal,ioctlRetVal;
  static char * MyFuncName = "recv_all()";
  fd_set ReadableSockets;
  Time_ew StartTime;
  struct timeval SelectTimeout; 
  Time_ew timeout=adjustTimeoutLength(timeout_msec);
  int BytesToRecv = len;
  int BytesRcvd = 0;
  int BytesJustRcvd;
  long lOnOff;
  
  if(EW_SOCKET_DEBUG)
    logit("et","Entering %s\n",MyFuncName);

  /* If there is no timeout, make the socket blocking */
  if(timeout_msec == -1)
    {
      lOnOff = 0;
      ioctlRetVal=ioctlsocket(s,FIONBIO,&lOnOff);
      if (ioctlRetVal==SOCKET_ERROR)
	{
	  if(EW_SOCKET_DEBUG)
	    logit("et","Error: %d, occurred in %s during change to blocking\n",
		socketGetError_ew(),MyFuncName);
	  return ioctlRetVal;
	}
    }

  StartTime = GetTime_ew();
  while ( BytesRcvd < BytesToRecv )
    {
      if ( (timeout_msec > 0) && ((GetTime_ew() - timeout) > StartTime ))
	{
	  retVal = BytesRcvd;
	  goto Done;
	}
      BytesJustRcvd = recv(s, buf + BytesRcvd, BytesToRecv - BytesRcvd, flags);
      if ( (BytesJustRcvd == 0)/* && (timeout_msec == -1)*/ )
	/* apparently EOF */
	{
	  retVal = BytesRcvd;
	  goto Done;
	}
      if ( BytesJustRcvd <= 0 )
	{
	  if ( (BytesJustRcvd == 0) || (socketGetError_ew() == WOULDBLOCK_EW) )
	    {
	      FD_ZERO(&ReadableSockets);
	      FD_SET(s,&ReadableSockets);
	      while((!select(s+1, &ReadableSockets, 0,0,
			     resetTimeout(&SelectTimeout)))
		    && ((GetTime_ew()-timeout) < StartTime))
		{
		  FD_ZERO(&ReadableSockets);
		  FD_SET(s,&ReadableSockets);
		  sleep_ew(100);  /* Wait a while, and then try
				     again */
		}
	      /* Set BytesJustRcvd, so that we are not kicked out of the
		 while loop because of a hard error on a recv.  Note:  we
		 will still be kicked out if we have exceeded the timeout.
		 */
	      BytesJustRcvd = 0;
	    }
	  else  /* some other error occured */
	    {
	      if(EW_SOCKET_DEBUG)
		logit("et","Error: %d, occurred in %s\n",
		      socketGetError_ew(),MyFuncName);
	      retVal = BytesJustRcvd; /* the error condition */
	      goto Done;
	    }
	}  /* End of If there was an error on recv() */
      else
	{
	  BytesRcvd += BytesJustRcvd;
	}
    }  /* End: while not all data sent */
  retVal = BytesRcvd;
  
  Done:
  /* If there is no timeout, then the call was made blocking,
     change it back so that we don't screw up any further operations
  */
  if(timeout_msec == -1)
    {
      lOnOff = 1;
      ioctlRetVal=ioctlsocket(s,FIONBIO,&lOnOff);
      if (ioctlRetVal==SOCKET_ERROR)
	{
	  if(EW_SOCKET_DEBUG)
	    logit("et","Error: %d, occurred in %s during change to non-blocking\n",
		socketGetError_ew(),MyFuncName);
	}
    }

  if(EW_SOCKET_DEBUG)
    logit("et","Exiting %s\n",MyFuncName);

  return(retVal);
}

/*************************************************************/

int recv_ew (SOCKET s,char FAR* buf,int len,int flags, int timeout_msec)
{
  /* recv_ew attempts to receive data on a connection oriented scoket.
     timeout is the length of time in millisec. that the recv_ew() 
     will wait before returning(if no data is received), after making
     the initial recv() call.  If data (or a shutdown request) is not
     received before the timeout expires, or if an error occurs, the 
     function returns SOCKET_ERROR.  If the latest socket error is
     WOULDBLOCK_EW, then no data was received during the timeout 
     period. As soon as data is received, the function returns.  
     The function does not attempt to completely fill the buffer 
     before returning.  
     Caller can call socketGetError_ew() for details about any failures.
     If (-1) is passed for timeout_msec, then recv_ew() reverts to a blocking
     recv() call.
  */

  int retVal,ioctlRetVal;
  static char * MyFuncName = "recv_ew()";
  fd_set ReadableSockets;
  Time_ew StartTime;
  struct timeval SelectTimeout; 
  Time_ew timeout=adjustTimeoutLength(timeout_msec);
  long lOnOff;
  
	if(EW_SOCKET_DEBUG)
		logit("et","Entering %s\n",MyFuncName);

  /* If there is no timeout, make the socket blocking */
  if(timeout_msec == -1)
  {
    lOnOff = 0;
    ioctlRetVal=ioctlsocket(s,FIONBIO,&lOnOff);
      if (ioctlRetVal==SOCKET_ERROR)
      {
	    	logit("et","Error: %d, occurred in %s during change to blocking\n",
                   socketGetError_ew(),MyFuncName);
      }
  }

  retVal=recv(s,buf,len,flags);

  /* If there is no timeout, then the call was made blocking,
     change it back so that we don't screw up any further operations
  */
  if(timeout_msec == -1)
  {
    lOnOff = 1;
    ioctlRetVal=ioctlsocket(s,FIONBIO,&lOnOff);
      if (ioctlRetVal==SOCKET_ERROR)
      {
	    	logit("et","Error: %d, occurred in %s during change to non-blocking\n",
                   socketGetError_ew(),MyFuncName);
      }
  }

  if (retVal < 0 && socketGetError_ew() == WOULDBLOCK_EW)
  {
    FD_ZERO(&ReadableSockets);
    FD_SET(s,&ReadableSockets);
    StartTime=GetTime_ew();
    while((!select(s+1,&ReadableSockets,0,0,resetTimeout(&SelectTimeout)))
           && ((GetTime_ew()-timeout) < StartTime))
    {
      FD_ZERO(&ReadableSockets);
      FD_SET(s,&ReadableSockets);
      sleep_ew(100);  /* Wait a while, and then try
                    again */
    }
    retVal=recv(s,buf,len,flags);
  }

  if(retVal <0  && EW_SOCKET_DEBUG)
  {
    logit("et","Error: %d, occurred in %s\n",
              socketGetError_ew(),MyFuncName);
  }

  if(EW_SOCKET_DEBUG)
		logit("et","Exiting %s\n",MyFuncName);

  return(retVal);
}

/*************************************************************/

int recvfrom_ew (SOCKET s, char FAR* buf, int len, int flags, 
			  struct sockaddr FAR* from, int FAR* fromlen,
			  int timeout_msec)
{

  /* recvfrom_ew() is similar to recv_ew(), except used for datagram
     sockets.  timeout is specified in milliseconds.  Caller can call 
     socketGetError_ew() for details about any failures. 
  */

  int retVal, ioctlRetVal;
  static char * MyFuncName = "recvfrom_ew()";
  fd_set ReadableSockets;
  Time_ew StartTime;
  struct timeval SelectTimeout; 
  Time_ew timeout=adjustTimeoutLength(timeout_msec);
  long lOnOff;
  
	if(EW_SOCKET_DEBUG)
		logit("et","Entering %s\n",MyFuncName);


  /* If there is no timeout, make the socket blocking */
  if(timeout_msec == -1)
  {
    lOnOff = 0;
    ioctlRetVal=ioctlsocket(s,FIONBIO,&lOnOff);
      if (ioctlRetVal==SOCKET_ERROR)
      {
	    	logit("et","Error: %d, occurred in %s during change to blocking\n",
                   socketGetError_ew(),MyFuncName);
      }
  }
  retVal=recvfrom(s,buf,len,flags,from,fromlen);
  /* If there is no timeout, then the call was made blocking,
     change it back so that we don't screw up any further operations
  */
  if(timeout_msec == -1)
  {
    lOnOff = 1;
    ioctlRetVal=ioctlsocket(s,FIONBIO,&lOnOff);
      if (ioctlRetVal==SOCKET_ERROR)
      {
	    	logit("et","Error: %d, occurred in %s during change to non-blocking\n",
                   socketGetError_ew(),MyFuncName);
      }
  }

  if (retVal < 0 && socketGetError_ew() == WOULDBLOCK_EW)
  {
    FD_ZERO(&ReadableSockets);
    FD_SET(s,&ReadableSockets);
    StartTime=GetTime_ew();
    while((!select(s+1,&ReadableSockets,0,0,resetTimeout(&SelectTimeout)))
           && ((GetTime_ew()-timeout) < StartTime))
    {
      FD_ZERO(&ReadableSockets);
      FD_SET(s,&ReadableSockets);
      sleep_ew(100);  /* Wait a while, and then try
                    again */
    }
    retVal=recvfrom(s,buf,len,flags,from,fromlen);
  }

  if(retVal <0  && EW_SOCKET_DEBUG)
  {
    logit("et","Error: %d, occurred in %s\n",
              socketGetError_ew(),MyFuncName);
  }

  if(EW_SOCKET_DEBUG)
		logit("et","Exiting %s\n",MyFuncName);

  return(retVal);
}

/*************************************************************/

int send_ew ( SOCKET s, const char FAR * buf, int len, int flags, 
	      int timeout_msec)
{
  /* If timeout_msec > 0, send_ew() returns when the sooner of two things 
     happens:  
     1.  The timeout measured in millisec. from the time of 
     the first send() call, expires;  
     2.  All of the data provided by the caller is sent.
     If timeout_msec == -1, the socket is set to blocking and send_ew() 
     returns when all the data is sent.
     send_ew() always returns when an unexpected error occurs.
     send_ew() returns the number of bytes of data sent, or
     SOCKET_ERROR on error.  The caller is responsible for noting
     any discrepencies in the difference between the number of bytes
     requested to be sent, and the number of reported bytes sent.  If
     there is a discrepency, then a timeout occured.
     Caller can call socketGetError_ew() for details about any failures.
     */

  int retVal, ioctlRetVal;
  static char * MyFuncName = "send_ew()";
  int BytesToSend=len;
  int BytesSent=0;
  int BytesJustSent=0;
  Time_ew StartTime;
  fd_set WriteableSockets;
  struct timeval SelectTimeout; 
  Time_ew timeout=adjustTimeoutLength(timeout_msec);
  long lOnOff;
  
  if(EW_SOCKET_DEBUG)
    logit("et","Entering %s\n",MyFuncName);
  
  /* If there is no timeout, make the socket blocking */
  if(timeout_msec == -1)
  {
    lOnOff = 0;
    ioctlRetVal=ioctlsocket(s,FIONBIO,&lOnOff);
    if (ioctlRetVal==SOCKET_ERROR)
    {
      if(EW_SOCKET_DEBUG)
        logit("et","Error: %d, occurred in %s during change to blocking\n",
        socketGetError_ew(),MyFuncName);
      return (ioctlRetVal);
    }
  }

  StartTime=GetTime_ew();
  while( BytesSent < BytesToSend )
  {
    if ( (timeout_msec >= 0) && ((GetTime_ew() - timeout) > StartTime ))
    {
      retVal = BytesSent;
      goto Done;
    }
    BytesJustSent=send(s, buf+BytesSent, min(len-BytesSent, MAXSENDSIZE_EW),
      flags);
    if (BytesJustSent <= 0)
    {
      if (BytesJustSent == 0 || socketGetError_ew() == WOULDBLOCK_EW)
      {
        FD_ZERO(&WriteableSockets);
        FD_SET(s,&WriteableSockets);
        while((!select(s+1, 0, &WriteableSockets, 0,
          resetTimeout(&SelectTimeout)))
          && ((GetTime_ew()-timeout) < StartTime))
        {
          FD_ZERO(&WriteableSockets);
          FD_SET(s,&WriteableSockets);
          sleep_ew(100);  /* Wait a while, and then try
          again */
        }

        /* Set BytesJustSent, so that we are not kicked out of the
        while loop because of a hard error on a send.  Note:  we
        will still be kicked out if we have exceeded the timeout.
        */
        BytesJustSent=0;
      }
      else  /* some other error occured */
      {
        if(EW_SOCKET_DEBUG)
          logit("et","Error: %d, occurred in %s\n",
          socketGetError_ew(),MyFuncName);
        retVal = BytesJustSent;
        goto Done;
      }
    }  /* End of If there was an error on send() */
    else
    {
      BytesSent+=BytesJustSent;
    }
  }  /* End: while not all data sent */
  retVal=BytesSent;

Done:
  /* If there is no timeout, then the call was made blocking,
  change it back so that we don't screw up any further operations
  */
  if(timeout_msec == -1)
  {
    lOnOff = 1;
    ioctlRetVal=ioctlsocket(s,FIONBIO,&lOnOff);
    if (ioctlRetVal==SOCKET_ERROR)
    {
      if(EW_SOCKET_DEBUG)
        logit("et","Error: %d, occurred in %s during change to non-blocking\n",
        socketGetError_ew(),MyFuncName);
      retVal = ioctlRetVal;
    }
  }

  if(EW_SOCKET_DEBUG)
    logit("et","Exiting %s\n",MyFuncName);

  return(retVal);
}

/*************************************************************/

int sendto_ew (SOCKET s, const char FAR * buf, int len, 
	       int flags, const struct sockaddr FAR * to,
	       int tolen, int timeout_msec)
{
  /* sendto_ew() is similar to send_ew(), except used for datagram
     sockets. Once the socket is ready for sending, sendto_ew calls
     sendto() only once.
     Timeout is specified in milliseconds; value of -1 sets socket to 
     blocking mode and turns off timing. 
     Caller can call socketGetError_ew() for details about any failures. 
  */

  int retVal, ioctlRetVal;
  static char * MyFuncName = "sendto_ew()";
  Time_ew StartTime;
  fd_set WriteableSockets;
  struct timeval SelectTimeout; 
  Time_ew timeout=adjustTimeoutLength(timeout_msec);
  long lOnOff;
  
  if(EW_SOCKET_DEBUG)
    logit("et","Entering %s\n",MyFuncName);

  /* If there is no timeout, make the socket blocking */
  if(timeout_msec == -1)
  {
      lOnOff = 0;
      ioctlRetVal=ioctlsocket(s,FIONBIO,&lOnOff);
      if (ioctlRetVal==SOCKET_ERROR)
      {
        if(EW_SOCKET_DEBUG)
        {
          logit("et","Error: %d, occurred in %s during change to blocking\n",
            socketGetError_ew(),MyFuncName);
        }
        return ioctlRetVal;
      }
  }

  StartTime=GetTime_ew();
  retVal=sendto(s,buf,len,flags,to,tolen);

  /* If there is no timeout, then the call was made blocking,
     change it back so that we don't screw up any further operations
  */
  if(timeout_msec == -1)
  {
    lOnOff = 1;
    ioctlRetVal=ioctlsocket(s,FIONBIO,&lOnOff);
    if (ioctlRetVal==SOCKET_ERROR)
    {
      if(EW_SOCKET_DEBUG)
        logit("et","Error: %d, occurred in %s during change to non-blocking\n",
        socketGetError_ew(),MyFuncName);
      return ioctlRetVal;
    }
  }

  if (retVal < 0 && socketGetError_ew() == WOULDBLOCK_EW)
  {
    FD_ZERO(&WriteableSockets);
    FD_SET(s,&WriteableSockets);
    while((!select(s+1,0,&WriteableSockets,0,resetTimeout(&SelectTimeout)))
      && ((GetTime_ew()-timeout) < StartTime))
    {
      FD_ZERO(&WriteableSockets);
      FD_SET(s,&WriteableSockets);
      sleep_ew(100);  
      /* Wait a while, and then try again */
    }
    retVal=sendto(s,buf,len,flags,to,tolen);
  }
  
  if(retVal <0  && EW_SOCKET_DEBUG)
    {
      logit("et","Error: %d, occurred in %s\n",
	    socketGetError_ew(),MyFuncName);
    }

  if(EW_SOCKET_DEBUG)
    logit("et","Exiting %s\n",MyFuncName);

  return(retVal);
}

/*************************************************************/

struct timeval FAR * resetTimeout(struct timeval FAR * pSelectTimeout)
{

  /* resetTimeout() reinitializes the TIMEVAL structure used in
     select() calls.  Depending on the OS, the timeout value
     maybe altered during the select() call, and therefore needs
     to be reinitialized before every select() call.
     */
  static char * MyFuncName = "resetTimeout()";
  static int EW_SOCKET_DEBUG_R=0;
  if(EW_SOCKET_DEBUG_R)
    logit("et","Entering %s\n",MyFuncName);

  pSelectTimeout->tv_sec=SELECT_TIMEOUT_SECONDS;
  pSelectTimeout->tv_usec=SELECT_TIMEOUT_uSECONDS;

  if(EW_SOCKET_DEBUG_R)
    logit("et","Exiting %s\n",MyFuncName);

  return(pSelectTimeout);
}

/*************************************************************/

int closesocket_ew(SOCKET s,int HowToClose)
{
  /* closesocket_ew() closes the socket s.  HowToClose indicates
     whether the socket should be closed gracefully or immediately.
     Use SOCKET_CLOSE_IMMEDIATELY_EW or SOCKET_CLOSE_GRACEFULLY_EW
     to indicate closure method.  Caller can call socketGetError_ew()
     for details about any failures.
  */

  /*
    #define SOCKET_CLOSE_IMMEDIATELY_EW 0
    #define SOCKET_CLOSE_GRACEFULLY_EW -1
    #define SOCKET_CLOSE_SIMPLY_EW     -2
  */

  static char * MyFuncName = "closesocket_ew()";
  struct linger Linger_Value;
  int retVal;

  if(EW_SOCKET_DEBUG)
    logit("et","Entering %s\n",MyFuncName);

/* Note1: setsockopt(SO_LINGER) doesn't seem to work on x86 Solaris 2.5, 
          at least when the socket isn't connected. WMK 981019   
   Note2: Added the case SOCKET_CLOSE_SIMPLY_EW to skip the call to 
          setsockopt. This case is used in connect_ew when the connection
          has failed.  LDD 981022
 *************************************************************************/
  if ( HowToClose != SOCKET_CLOSE_SIMPLY_EW )
  {
     if ( HowToClose == SOCKET_CLOSE_IMMEDIATELY_EW )
     {
        Linger_Value.l_onoff=1;     /* Reset or hard close */
        Linger_Value.l_linger=0;    /* Set timeout to 0 seconds */
     }
     else
     {
        Linger_Value.l_onoff=0;     /* Non-blocking graceful close (NBGC) */
        Linger_Value.l_linger=0;
     }

     if ( setsockopt(s,SOL_SOCKET,SO_LINGER,(char *) &Linger_Value,
             sizeof(struct linger)) == -1 )
     {
        if(EW_SOCKET_DEBUG)
           logit( "et", "closesocket_ew:setsockopt error: %s\n", 
                   strerror(socketGetError_ew()) );
     }
  }

  retVal=closesocket(s);

  if(EW_SOCKET_DEBUG)
    logit("et","Exiting %s\n",MyFuncName);

  return (retVal);
}

/*************************************************************/

int select_ew (int nfds, fd_set FAR * readfds, fd_set FAR * writefds, 
	       fd_set FAR * exceptfds, 
	       int timeout_msec)

     /* select_ew() determines the state of sets of sockets, by 
     calling select().  Timeout is in milliseconds, and is
     converted by select_ew to the select() timeout structure, and
     passed on (to select()). No "-1" feature here; if you are willing to
     block indefinitely in select(), you might as well wait in the actual
     I/O call instead.
     Caller can call socketGetError_ew() for details about any failures.
  */
{
  int retVal;
  static char * MyFuncName = "select_ew()";
  struct timeval SelectTimeout={0,0};


  if(EW_SOCKET_DEBUG)
    logit("et","Entering %s\n",MyFuncName);

  SelectTimeout.tv_usec=1000 * timeout_msec;

  retVal=select(nfds,readfds,writefds,exceptfds,&SelectTimeout);
  if (retVal < 0  && EW_SOCKET_DEBUG)
    logit("et","Error: %d, occurred in %s\n",
	  socketGetError_ew(),MyFuncName);

  if(EW_SOCKET_DEBUG)
    logit("et","Exiting %s\n",MyFuncName);

  return(retVal);
}

/*************************************************************/

int setSocket_ewDebug(int debug)
{
  /* setSocket_ewDebug() turns debugging on or off for 
     the SOCKET_ew routines.
  */
  EW_SOCKET_DEBUG=debug;
  return(0);
}

/*************************************************************/

int setSocket_ewSelectTimeout(unsigned int Timeout)
{
  /* setSocket_ewSelectTimeout() sets the timeout period
     passed to select() calls made internally within the 
     SOCKET_ew routines.  The timeout period is in 
     milliseconds.
  */
  SELECT_TIMEOUT_uSECONDS=1000*Timeout;
  return(0);
}


