
/*
 *   THIS FILE IS UNDER RCS - DO NOT MODIFY UNLESS YOU HAVE
 *   CHECKED IT OUT USING THE COMMAND CHECKOUT.
 *
 *    $Id$
 *
 *    Revision history:
 *     $Log$
 *     Revision 1.4  2003/06/01 08:25:40  lindquis
 *     Upgrade Iceworm libraries to Earthworm6.2. Add some rudimentary man
 *     pages. Preparation for the rewritten ew2orb.
 *
 *     Revision 1.5  2003/02/04 17:57:56  davidk
 *     Added a call to socketSetError_ew() to set the socket error when
 *     connect() fails during the select() loop in connect_ew().
 *     Normally, ON A NON-BLOCKING SOCKET, connect() is called,
 *     it returns a WOULDBLOCK/INPROGRESS condition, then we go
 *     into a select() loop to check for socket-writability within a timeout
 *     period.  There was an error in the code, that did not notice the
 *     CORRECT error (atleast on Solaris) and so the code would return
 *     a TIMEOUT error instead of the actual error.
 *
 *     The change only applies to the connect_ew function() when run
 *     in non-blocking mode (clients connecting to a server - using a timeout value).
 *     The change only applies when a socket-error occurs while the function is
 *     waiting for the connect to happen.  The instance where you will most-likely
 *     see a difference, is when you try connecting to a non-existent socket.
 *     Previously the function would return TIMEOUT, now it will return
 *     connection REFUSED.
 *
 *     Revision 1.4  2000/12/01 23:48:54  lombard
 *     Fixed a few more logit format errors.
 *
 *     Revision 1.3  2000/07/10 21:14:51  lombard
 *     Fixed bug in recvfrom_ew where improper arguments were used in recvfrom calls
 *
 *     Revision 1.2  2000/06/28 17:17:54  lombard
 *     Fixed bug in format for logit calls after select errors, several places
 *
 *     Revision 1.1  2000/02/14 18:51:48  lucky
 *     Initial revision
 *
 *
 */

/****************** socket_ew_common *************************/

/*
 * Changes:
 * 12/7/1999, PNL
 * Accept_ew now sets the new socket to non-blocking mode.
 *  Previously it was assumed that the new socket inherited non-blocking
 *  from the original socket; this is not true in WinNT or Solaris 2.6.
 * Errors from select() calls are now handled.
 */
/********************* #INCLUDES *****************************/
/*************************************************************/
#include <errno.h>
#include <socket_ew.h>

/********************** GLOBAL *******************************/
/********************* VARIABLES *****************************/
/*************************************************************/
/* Timeout used for select() calls: 0.2 seconds */
int SELECT_TIMEOUT_SECONDS=0;
int SELECT_TIMEOUT_uSECONDS=200000;

int EW_SOCKET_DEBUG=0;       /* Set by setSocket_ewDebug() */

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
     Arguments af, type and protocol are passed directly to socket().
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

  newSocket = socket(af,type,protocol);
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
   * period specified by timeout.  
   * Arguments s, name, and namelenare passed directly to connect().
   * On success conect_ew() returns zero.
   * On failure, either due to a network error, or a timeout, conect_ew
   * closes the socket and returns SOCKET_ERROR.
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
    logit( "et" , "Entering %s\n", MyFuncName );

/* If there is no timeout, make the socket blocking
   ************************************************/
   if ( timeout_msec == -1 )
   {
     lOnOff = 0;
     ioctlRetVal = ioctlsocket( s, FIONBIO, &lOnOff );
     if ( ioctlRetVal < 0 )
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
      if ( ioctlRetVal < 0 )
      {
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
       socketSetError_ew(error);
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
   {
     closesocket_ew( s, SOCKET_CLOSE_SIMPLY_EW ); /*skip setsockopt()*/
     retVal = SOCKET_ERROR;
   }
   
   if ( EW_SOCKET_DEBUG )
     logit("et","Exiting %s\n",MyFuncName);
   
   return(retVal);
}

/*************************************************************/

int bind_ew (SOCKET s, struct sockaddr FAR* name, int namelen )
{
  /* bind_ew() attempts to bind the socket s to a name/port number.
     This is basicly same as normal bind() call, with some logging added.
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
     Arguments are passed directly to listen().
     Caller can call socketGetError_ew() for details about any failures.  
  */

  int retVal;
  static char * MyFuncName = "listen_ew()";

  if(EW_SOCKET_DEBUG)
    logit("et","Entering %s\n",MyFuncName);

  retVal=listen(s, backlog);
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
     Arguments s, addr, addrlen are passed directly to accept(),
     timeout_msec: length of time in milliseconds that accept_ew() 
      will wait before returning. Timeout is measure from the
      point after the initial accept() call. 
     Pass timeout of -1 for accept_ew to revert to blocking 
      accept() call.
     If no connection is accepted before the timeout expires, 
      or if an error occurs, the function returns INVALID_SOCKET.  
     Caller can call socketGetError_ew() for details about 
     any failures. 
     If the latest socket error was WOULDBLOCK_EW, then 
      no connections were made during the timeout period.
  */

  SOCKET newSocket;
  static char * MyFuncName = "accept_ew()";
  Time_ew StartTime;
  fd_set AcceptedSockets;
  struct timeval SelectTimeout; 
  Time_ew timeout=adjustTimeoutLength(timeout_msec);
  int retVal;
  long lOnOff;
  int sel;
  
  if(EW_SOCKET_DEBUG)
    logit("et","Entering %s\n",MyFuncName);

  /* If there is no timeout, make the socket blocking */
  if(timeout_msec == -1)
  {
    lOnOff = 0;
    retVal=ioctlsocket(s,FIONBIO,&lOnOff);
    
    if (retVal < 0)
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
      while( (sel = select(s+1, &AcceptedSockets, 0, 0,
                           resetTimeout(&SelectTimeout))) == 0)
      { /* select timed out; if timeout hasn't expired, reset and try again */
        if ( GetTime_ew() - timeout > StartTime )
          return INVALID_SOCKET;
        
        FD_ZERO(&AcceptedSockets);
        FD_SET(s,&AcceptedSockets);
        sleep_ew(1000);  /* Sleep for a second, and then try again.*/
      }
      if (sel < 0 && EW_SOCKET_DEBUG)
      {
        logit("et", "Error %d occured during select() in %s\n",
              socketGetError_ew(), MyFuncName);
        goto Abort;
      }
      newSocket=accept(s,addr,addrlen);
    }
    if(newSocket == INVALID_SOCKET && EW_SOCKET_DEBUG)
    {
      logit("et","Error: %d, occurred in %s\n",
            socketGetError_ew(),MyFuncName);
    }
  }
  
  /* Set the new socket to non-blocking mode */
  lOnOff = 1;
  retVal = ioctlsocket(newSocket,FIONBIO,&lOnOff);
  if (retVal == SOCKET_ERROR)
  {
    if (EW_SOCKET_DEBUG)
      logit("et","Error: %d, occurred in %s setting new socket to non-blocking\n",
            socketGetError_ew(),MyFuncName);
    goto Abort;
  }
  return(newSocket);

Abort:
  if (newSocket > 0) closesocket_ew(newSocket, 0);
  newSocket = INVALID_SOCKET;
  closesocket_ew(s, 0);
  s = INVALID_SOCKET;
  return(newSocket);
}

/*************************************************************/

int recv_all (SOCKET s,char FAR* buf,int len,int flags, int timeout_msec)
{
  /* recv_all attempts to receive data on a connection oriented scoket.
     buf:     buffer for incoming data, which must be provided by the caller
     len:     number of bytes to read; buffer must be at least len + 1 bytes.
     flags:   flags that are passed directly to recv().
     timeout: length of time in milliseconds that the recv_ew() will wait
     before returning(if no data is received), after making the initial recv()
     call.  

     If timeout_msec > 0, recv_all() returns when the sooner of two things
     happens: 
     1.  The timeout from the time of the first recv() call, expires; 
     2.  "len" bytes of data are received.

     recv_all() returns the number of bytes of data received, or SOCKET_ERROR
     on error.  The caller is responsible for noting any discrepencies in the
     difference between the number of bytes requested to be sent, and the
     number of reported bytes sent.  If there is a discrepency, then a timeout
     or error occured. Caller can call socketGetError_ew() for details about 
     any failures.

     If timeout_msec == -1, recv_all() sets the socket to blocking and returns
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
  int sel;
  
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
    {  /* Time is up; return what we got */
      retVal = BytesRcvd;
      goto Done;
    }
    BytesJustRcvd = recv(s, buf + BytesRcvd, BytesToRecv - BytesRcvd, flags);
    if ( BytesJustRcvd == 0 )        /* apparently EOF */
    {
      retVal = BytesRcvd;
      goto Done;
    }
    if ( BytesJustRcvd < 0 ) /* Error happened */
    {
      if ( socketGetError_ew() == WOULDBLOCK_EW )
      {
        FD_ZERO(&ReadableSockets);
        FD_SET(s,&ReadableSockets);
        while( (sel = select(s+1, &ReadableSockets, 0, 0,
                       resetTimeout(&SelectTimeout))) == 0)
        { /* select timed out; if timeout hasn't expired, reset and try again */
          if ( GetTime_ew() - timeout > StartTime )
          {
            retVal = BytesRcvd;
            goto Done;
          }
          FD_ZERO(&ReadableSockets);
          FD_SET(s,&ReadableSockets);
          sleep_ew(100);  /* Wait a while, and then try
                             again */
        }
        if (sel < 0)
        {
          logit("et", "Error %d occured during select() in %s\n",
                socketGetError_ew(), MyFuncName);
          retVal = BytesRcvd;
          goto Done;
        }
        
        /* Set BytesJustRcvd, so that we are not kicked out of the
           while loop because of a hard error on a recv.  Note: we
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

int recv_ew (SOCKET s, char FAR* buf, int len, int flags, int timeout_msec)
{
  /* recv_ew attempts to receive data on a connection oriented scoket.
     buf:     buffer for incoming data, which must be provided by the caller
     len:     length of the buffer.
     flags:   flags that are passed directly to recv().
     timeout: length of time in milliseconds. that the recv_ew() will wait 
     before returning(if no data is received), after making
     the initial recv() call. If data (or a shutdown request) is not
     received before the timeout expires, or if an error occurs, the 
     function returns SOCKET_ERROR. As soon as any data is received, 
     the function returns; the function does not attempt to completely
     fill the buffer before returning.  
     Caller can call socketGetError_ew() for details about any failures.
     If the latest socket error is WOULDBLOCK_EW, then recv_ew timed out
     before receiving any data,
     
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
  int sel;
  
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

  /* Use select() to wait for something to read. We use a small time interval
   * (0.2 seconds) in select, and check the clock against timeout_msec (here
   * converted to seconds) in a while() loop.
   */
  if (retVal < 0 && socketGetError_ew() == WOULDBLOCK_EW)
  {
    FD_ZERO(&ReadableSockets);
    FD_SET(s,&ReadableSockets);
    StartTime=GetTime_ew();
    while( (sel = select(s+1, &ReadableSockets, 0, 0, 
                         resetTimeout(&SelectTimeout))) == 0 )
    {  /* select timed out; if timeout hasn't expired, reset and try again */
      
      if ( GetTime_ew() - timeout > StartTime )
        break;
      FD_ZERO(&ReadableSockets);
      FD_SET(s,&ReadableSockets);
      sleep_ew(100);  /* Wait a while, and then try again */
    }
    if (sel < 0)
    {
      logit("et", "Error %d occured during select() in %s\n",
            socketGetError_ew(), MyFuncName);
      return(SOCKET_ERROR);
    }
    /* Try to read, even if select() timed out */
    retVal=recv(s,buf,len,flags);
  }

  if(retVal <0  && EW_SOCKET_DEBUG)
  {
    if (sel == 0)
      logit("et", "Timeout occured in %s\n", MyFuncName);
    else
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
  int sel, flen;
  
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
      /* Should we return this error, or continue? */
    }
  }
  /* Use a local copy of fromlen (because recvfrom sets fromlen=0 if socket is
     non-blocking and there's no data => fromlen=0 input to second recvfrom) */
  flen = *fromlen;
  retVal = recvfrom(s,buf,len,flags,from,&flen);

  /* If there is no timeout, then the call was made blocking,
     change it back so that we don't screw up any further operations */
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
    while( (sel = select(s+1, &ReadableSockets, 0, 0,
                         resetTimeout(&SelectTimeout))) == 0 )
    {  /* select timed out; if timeout hasn't expired, reset and try again */
      
      if ( GetTime_ew() - timeout > StartTime )
        break;

      FD_ZERO(&ReadableSockets);
      FD_SET(s,&ReadableSockets);
      sleep_ew(100);  /* Wait a while, and then try
                    again */
    }
    if (sel < 0)
    {
      logit("et", "Error %d occured during select() in %s\n",
            socketGetError_ew(), MyFuncName);
      return(SOCKET_ERROR);
    }
    /* Try to read, even if select() timed out */
    flen = *fromlen;
    retVal = recvfrom(s,buf,len,flags,from,&flen);
  }

  if(retVal <0  && EW_SOCKET_DEBUG)
  {
    logit("et","Error: %d, occurred in %s\n",
              socketGetError_ew(),MyFuncName);
  }

  if(EW_SOCKET_DEBUG)
		logit("et","Exiting %s\n",MyFuncName);

  *fromlen = flen;
  return(retVal);
}

/*************************************************************/

int send_ew ( SOCKET s, const char FAR * buf, int len, int flags, 
	      int timeout_msec)
{
  /* Send `len' bytes from `buf' out a socket `s'.
     Argument `flags' is passed directly to send().
     If timeout_msec > 0, send_ew() returns when the sooner of two things 
     happens:  
     1.  The timeout measured in milliseconds expires;  
     2.  All of the data provided by the caller is sent.
     If timeout_msec == -1, the socket is set to blocking and send_ew() 
     returns when all the data is sent or an error occured.
     send_ew() always returns when an unexpected error occurs.
     send_ew() returns the number of bytes of data sent, or
     SOCKET_ERROR on error.  The caller is responsible for noting
     any discrepencies in the difference between the number of bytes
     requested to be sent, and the number of reported bytes sent.  If
     there is a discrepency, then a timeout may have occured.
     Caller can call socketGetError_ew() for details about any failures.
     If the latest socket error was WOULDBLOCK_EW, then 
      the timeout occured before all the data was sent.
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
  int sel;
  
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
  
  StartTime = GetTime_ew();
  while( BytesSent < BytesToSend )
  {
    if ( (timeout_msec >= 0) && ((GetTime_ew() - timeout) > StartTime ))
    {
      retVal = BytesSent;
      goto Done;
    }
    BytesJustSent = send(s, buf+BytesSent, min(len-BytesSent, MAXSENDSIZE_EW),
                         flags);
    if (BytesJustSent <= 0)
    {
      if (BytesJustSent == 0 || socketGetError_ew() == WOULDBLOCK_EW)
      {
        FD_ZERO(&WriteableSockets);
        FD_SET(s,&WriteableSockets);
        while( (sel = select(s+1, 0, &WriteableSockets, 0,
                             resetTimeout(&SelectTimeout))) == 0)
        {  /* select timed out; if timeout hasn't expired, reset and try again */
          if ( GetTime_ew() - timeout > StartTime )
          {
            retVal = BytesSent;
            goto Done;
          }
          
          FD_ZERO(&WriteableSockets);
          FD_SET(s,&WriteableSockets);
          sleep_ew(100);  /* Wait a while, and then try again */
        }
        if (sel < 0)
        {
          logit("et", "Error %d occured during select() in %s\n",
                socketGetError_ew(), MyFuncName);
          retVal = BytesSent;
          goto Done;
        }

        /* Set BytesJustSent, so that we are not kicked out of the
        while loop because of a hard error on a send.  Note:  we
        will still be kicked out if we have exceeded the timeout.
        */
        BytesJustSent = 0;
      }
      else  /* some other error occured */
      {
        if(EW_SOCKET_DEBUG)
          logit("et","Error: %d, occurred in %s\n",
                socketGetError_ew(),MyFuncName);
        retVal = BytesSent;
        goto Done;
      }
    }  /* End of If there was an error on send() */
    else
    {
      BytesSent += BytesJustSent;
    }
  }  /* End: while not all data sent */
  retVal = BytesSent;

Done:
  /* If there is no timeout, then the call was made blocking,
  change it back so that we don't screw up any further operations
  */
  if(timeout_msec == -1)
  {
    lOnOff = 1;
    ioctlRetVal=ioctlsocket(s,FIONBIO,&lOnOff);
    if (ioctlRetVal < 0)
    {
      if(EW_SOCKET_DEBUG)
        logit("et","Error: %d, occurred in %s during change to non-blocking\n",
        socketGetError_ew(),MyFuncName);
      retVal = SOCKET_ERROR;
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
     sendto() only once. No checks are made to ensure all data is sent.
     Arguments s, flags, to,  and tolen are passed directly to sendto().
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
  int sel;
  
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
  retVal = sendto(s,buf,len,flags,to,tolen);
  
  /* If there is no timeout, then the call was made blocking,
     change it back so that we don't screw up any further operations
  */
  if(timeout_msec == -1)
  {
    lOnOff = 1;
    ioctlRetVal = ioctlsocket(s,FIONBIO,&lOnOff);
    if (ioctlRetVal < 0)
    {
      if(EW_SOCKET_DEBUG)
        logit("et","Error: %d, occurred in %s during change to non-blocking\n",
        socketGetError_ew(),MyFuncName);
      return SOCKET_ERROR;
    }
  }

  if (retVal < 0 && socketGetError_ew() == WOULDBLOCK_EW)
  {
    FD_ZERO(&WriteableSockets);
    FD_SET(s,&WriteableSockets);
    while( (sel = select(s+1, 0, &WriteableSockets, 0,
                         resetTimeout(&SelectTimeout))) == 0)
    {  /* select timed out; if timeout hasn't expired, reset and try again */
      if ( GetTime_ew() - timeout > StartTime )
        return SOCKET_ERROR;

      FD_ZERO(&WriteableSockets);
      FD_SET(s,&WriteableSockets);
      sleep_ew(100);  
      /* Wait a while, and then try again */
    }
    if (sel < 0)
    {
      logit("et", "Error %d occured during select() in %s\n",
            socketGetError_ew(), MyFuncName);
      return SOCKET_ERROR;
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
  
  retVal = select(nfds,readfds,writefds,exceptfds,&SelectTimeout);
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


