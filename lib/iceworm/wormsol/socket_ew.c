
/*
 *   THIS FILE IS UNDER RCS - DO NOT MODIFY UNLESS YOU HAVE
 *   CHECKED IT OUT USING THE COMMAND CHECKOUT.
 *
 *    $Id$
 *
 *    Revision history:
 *     $Log$
 *     Revision 1.3  2003/06/01 08:25:38  lindquis
 *     Upgrade Iceworm libraries to Earthworm6.2. Add some rudimentary man
 *     pages. Preparation for the rewritten ew2orb.
 *
 *     Revision 1.2  2003/02/04 17:57:38  davidk
 *     Added a new function socketSetError_ew() that sets errno to set
 *     the error for the most recent socket call..
 *
 *     Revision 1.1  2000/02/14 18:46:17  lucky
 *     Initial revision
 *
 *
 */

/* 
 *  socket_ew.c for Solaris
 *
 *  Contains system-dependent functions for dealing with
 *  sockets.
 */

#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <socket_ew.h>


ulong MS_PER_TICK;
struct tms mytimestruct;

int SOCKET_SYS_INIT = 0;   /* Global initialization flag,
                              set in SocketSysInit(), 
                              checked in socket_ew()  */


/********************** SocketSysInit *********************
   Initialize a socket system (dummy function in Solaris)
**********************************************************/

void SocketSysInit( void )
{
   SOCKET_SYS_INIT++;
   return;
}

/********************** SocketClose **********************
                      Close a Socket
**********************************************************/

void SocketClose( int soko )
{
   close( soko );
   return;
}

/********************** SocketPerror *********************
                    Print an error message
**********************************************************/

void SocketPerror( char *note )
{
   perror( note );
   return;
}


/************************ sendall() ***********************
*       looks like the standard send(), but does not      *
*       return until either error, or all has been sent   *
*	Also, we break sends into lumps as some           *
*	implementations can't send too much at once.      *
*	Will found this out.
***********************************************************/

#define SENDALL_MAX_LUMP 1024	/* no send() larger  than this */

int sendall(int socket, const char *msg, long msgLen, int flags)
{
	int   ret;  /* number of bytes actually sent, or error */
	long  nextByte;
	int   nsend;

	nsend = SENDALL_MAX_LUMP; /* try sending in lumps of this size */
	nextByte = 0;

	while ( nextByte<msgLen )
		{
		if ( msgLen-nextByte < nsend ) nsend = msgLen-nextByte; /* last small send? */
		ret = send(socket, (const char*)&msg[nextByte], nsend, flags);
		if (ret < 0)
			{
			logit("t","send error %d\n",ret);
			return( ret );
			}
		nextByte += ret;  /* we actually sent only this many */
		}
	return ( msgLen );
}


/********************** socketGetError_ew *****************
     Returns the error code for the most recent socket error.
**********************************************************/
int socketGetError_ew()
{
  return((int)errno);
}

/********************** socketSetError_ew *****************
     Sets the error code for the most recent socket error.
**********************************************************/
void socketSetError_ew(int error)
{
  errno=error;
}


Time_ew GetTime_ew()
{
  int retVal;
  struct timeval CurrentTime;

  retVal=gettimeofday(&CurrentTime,0);
  return(CurrentTime.tv_sec);
}

Time_ew adjustTimeoutLength(int timeout_msec)
{
  return((timeout_msec+999)/1000); /* Convert miliseconds to
                                      seconds.  Don't truncate */
}


