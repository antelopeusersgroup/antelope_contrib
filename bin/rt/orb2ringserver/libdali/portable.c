/***********************************************************************//**
 * @file portable.c:
 * 
 * Platform portability routines.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * as published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License (GNU-LGPL) for more details.  The
 * GNU-LGPL and further information can be found here:
 * http://www.gnu.org/
 *
 * @author Chad Trabant, IRIS Data Management Center
 *
 * modified: 2008.193
 ***************************************************************************/

#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <time.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "portable.h"
#include "libdali.h"


/************************************************************************//**
 * @brief Start up socket subsystem (only does something for Win32)
 *
 * Startup the network socket layer.  At the moment this is only meaningful
 * for the WIN32 platform and requests Windows sockets version 2.2.
 *
 * @return -1 on errors and 0 on success.
 ***************************************************************************/
int
dlp_sockstartup (void)
{
#if defined(DLP_WIN32)
  WORD wVersionRequested;
  WSADATA wsaData;

  /* Check for Windows sockets version 2.2 */
  wVersionRequested = MAKEWORD( 2, 2 );

  if ( WSAStartup( wVersionRequested, &wsaData ) )
    return -1;

#endif

  return 0;
}  /* End of dlp_sockstartup() */


/***********************************************************************//**
 * @brief Connect a network socket
 *
 * Connect a network socket and perform error checking.
 *
 * @param socket Network socket descriptor
 * @param inetaddr The struct sockaddr, passed to connect()
 * @param addrlen Length of @a inetaddr, passed to connect()
 *
 * @return -1 on errors and 0 on success.
 ***************************************************************************/
int
dlp_sockconnect (int socket, struct sockaddr * inetaddr, int addrlen)
{
#if defined(DLP_WIN32)
  if ((connect (socket, inetaddr, addrlen)) == SOCKET_ERROR)
    {
      if (WSAGetLastError() != WSAEWOULDBLOCK)
	return -1;
    }
#else
  if ((connect (socket, inetaddr, addrlen)) == -1)
    {
      if (errno != EINPROGRESS)
	return -1;
    }
#endif

  return 0;
}  /* End of dlp_sockconnect() */


/***********************************************************************//**
 * @brief Close a network socket
 *
 * Close a network socket.
 *
 * @param socket Network socket descriptor
 *
 * @return -1 on errors and 0 on success.
 ***************************************************************************/
int
dlp_sockclose (int socket)
{
#if defined(DLP_WIN32)
  return closesocket (socket);
#else
  return close (socket);
#endif
}  /* End of dlp_sockclose() */


/***********************************************************************//**
 * @brief Set a network socket to blocking mode
 *
 * Set a network socket to blocking mode.
 *
 * @param socket Network socket descriptor
 *
 * @return -1 on errors and 0 on success.
 ***************************************************************************/
int
dlp_sockblock (int socket)
{
#if defined(DLP_WIN32)
  u_long flag = 0;
  
  if (ioctlsocket(socket, FIONBIO, &flag) == -1)
    return -1;
  
#else
  int flags = fcntl(socket, F_GETFL, 0);
  
  flags &= (~O_NONBLOCK);
  
  if (fcntl(socket, F_SETFL, flags) == -1)
    return -1;

#endif

  return 0;
}  /* End of dlp_sockblock() */


/***********************************************************************//**
 * @brief Set a network socket to non-blocking mode
 *
 * Set a network socket to non-blocking mode.
 *
 * @param socket Network socket descriptor
 *
 * @return -1 on errors and 0 on success.
 ***************************************************************************/
int
dlp_socknoblock (int socket)
{
#if defined(DLP_WIN32)
  u_long flag = 1;

  if (ioctlsocket(socket, FIONBIO, &flag) == -1)
    return -1;

#else
  int flags = fcntl(socket, F_GETFL, 0);

  flags |= O_NONBLOCK;
  if (fcntl(socket, F_SETFL, flags) == -1)
    return -1;

#endif

  return 0;
}  /* End of dlp_socknoblock() */


/***********************************************************************//**
 * @brief Check if socket action would have blocked
 *
 * Check the global error status and test if the error indicates that
 * the recent socket action failed because it would have blocked.
 *
 * @return -1 on error and 0 on success (meaning no data for a non-blocking
 * socket).
 ***************************************************************************/
int
dlp_noblockcheck (void)
{
#if defined(DLP_WIN32)
  if (WSAGetLastError() != WSAEWOULDBLOCK)
    return -1;

#else
  if (errno != EWOULDBLOCK)
    return -1;

#endif

  /* no data available for NONBLOCKing IO */
  return 0;
}  /* End of dlp_noblockcheck() */


/***********************************************************************//**
 * @brief Set socket I/O timeout
 *
 * Set socket I/O timeout if such an option exists.  On Win32 and
 * other platforms where SO_RCVTIMEO and SO_SNDTIMEO are defined this
 * sets the SO_RCVTIMEO and SO_SNDTIMEO socket options using
 * setsockopt() to the @a timeout value (specified in seconds).
 *
 * Solaris does not implelement socket-level timeout options.
 *
 * @param socket Network socket descriptor
 * @param timeout Alarm timeout in seconds
 *
 * @return -1 on error, 0 when not possible and 1 on success.
 ***************************************************************************/
int
dlp_setsocktimeo (int socket, int timeout)
{
#if defined(DLP_WIN32)
  int tval = timeout * 1000;
  
  if ( setsockopt (socket, SOL_SOCKET, SO_RCVTIMEO, (char*)&tval, sizeof(tval)) )
    {
      return -1;
    }
  tval = timeout * 1000;
  if ( setsockopt (socket, SOL_SOCKET, SO_SNDTIMEO, (char*)&tval, sizeof(tval)) )
    {
      return -1;
    }
  
#else
  /* Set socket I/O timeouts if socket options are defined */
#if defined (SO_RCVTIMEO) && defined (SO_SNDTIMEO)
  struct timeval tval;
  
  tval.tv_sec = timeout;
  tval.tv_usec = 0;
  
  if ( setsockopt (socket, SOL_SOCKET, SO_RCVTIMEO, &tval, sizeof (tval)) )
    {
      return -1;
    }
  if ( setsockopt (socket, SOL_SOCKET, SO_SNDTIMEO, &tval, sizeof (tval)) )
    {
      return -1;
    }
#else
  return 0;
#endif
  
#endif
  
  return 1;
}  /* End of dlp_setsocktimeo() */


/***********************************************************************//**
 * @brief Set a network I/O real time alarm
 *
 * Set a network I/O alarm timer, the @a timeout is specified in
 * seconds.  The timer is disabled by setting the timeout to zero.  On
 * most platforms this will cause a SIGALARM signal to be sent to the
 * calling process after @a timeout seconds have elapsed.  This
 * function does nothing under Win32.
 *
 * @param timeout Alarm timeout in seconds
 *
 * @return -1 on error and 0 on success.
 ***************************************************************************/
int
dlp_setioalarm (int timeout)
{
#if defined(DLP_WIN32)
  /* Non-operation for Win32 */  
  
#else
  struct itimerval itval;
  
  itval.it_interval.tv_sec = 0;
  itval.it_interval.tv_usec = 0;
  itval.it_value.tv_sec = timeout;
  itval.it_value.tv_usec = 0;
  
  if ( setitimer (ITIMER_REAL, &itval, NULL) )
    {
      return -1;
    }
  
#endif
  
  return 0;
}  /* End of dlp_setioalarm() */


/***********************************************************************//**
 * @brief Resolve IP address and prepare paramters for connect()
 *
 * Resolve IP addresses and provide parameters needed for connect().
 * On Win32 this will use gethostbyname() for portability (only newer
 * Windows platforms support getaddrinfo).  On Linux (glibc2) and
 * Solaris the reentrant gethostbyname_r() is used.
 *
 * The real solution to name resolution is to use POSIX 1003.1g
 * getaddrinfo() because it is standardized, thread-safe and protocol
 * independent (i.e. IPv4, IPv6, etc.).  Unfortunately it is not
 * supported on many older platforms.
 *
 * @param nodename Hostname to resolve
 * @param nodeport Port number to connect to
 * @param addr Returned struct sockaddr for connect()
 * @param addrlen Returned length of @a addr
 *
 * @return 0 on success and non-zero on error.
 ***************************************************************************/
int
dlp_getaddrinfo (char *nodename, char *nodeport,
		 struct sockaddr *addr, size_t *addrlen)
{
#if defined(DLP_WIN32)
  struct hostent *result;
  struct sockaddr_in inet_addr;
  long int nport;
  char *tail;

  if ( (result = gethostbyname (nodename)) == NULL )
    {
      return -1;
    }

  nport = strtoul (nodeport, &tail, 0);

  memset (&inet_addr, 0, sizeof (inet_addr));
  inet_addr.sin_family = AF_INET;
  inet_addr.sin_port = htons ((unsigned short int)nport);
  inet_addr.sin_addr = *(struct in_addr *) result->h_addr_list[0];
  
  *addr = *((struct sockaddr *) &inet_addr);
  *addrlen = sizeof(inet_addr);

#elif defined(DLP_GLIBC2) || defined(DLP_SOLARIS)
  /* 512 bytes should be enough for the vast majority of cases.  If
     not (e.g. the node has a lot of aliases) this call will fail. */

  char buffer[512];
  struct hostent *result;
  struct hostent result_buffer;
  struct sockaddr_in inet_addr;
  int my_error;
  long int nport;
  char *tail;

  #if defined(DLP_GLIBC2)
  gethostbyname_r (nodename, &result_buffer,
		   buffer, sizeof(buffer) - 1,
		   &result, &my_error);
  #endif

  #if defined(DLP_SOLARIS)
  result = gethostbyname_r (nodename, &result_buffer,
                            buffer, sizeof(buffer) - 1,
			    &my_error);
  #endif

  if ( !result )
    return my_error;

  nport = strtoul (nodeport, &tail, 0);

  memset (&inet_addr, 0, sizeof (inet_addr));
  inet_addr.sin_family = AF_INET;
  inet_addr.sin_port = htons ((unsigned short int)nport);
  inet_addr.sin_addr = *(struct in_addr *) result->h_addr_list[0];
  
  *addr = *((struct sockaddr *) &inet_addr);
  *addrlen = sizeof(inet_addr);

#else
  /* This will be used by all others, it is not properly supported
     by some but this is the future of name resolution. */

  struct addrinfo *result;
  struct addrinfo hints;

  memset (&hints, 0, sizeof(hints));
  hints.ai_family = PF_INET;
  hints.ai_socktype = SOCK_STREAM;
  
  if ( getaddrinfo (nodename, nodeport, &hints, &result) )
    {
      return -1;
    }

  *addr = *(result->ai_addr);
  *addrlen = result->ai_addrlen;

  freeaddrinfo (result);

#endif

  return 0;
}  /* End of dlp_getaddrinfo() */


/***********************************************************************//**
 * @brief Open a file stream
 *
 * Open a specified file and return the file descriptor.  The @a perm
 * character is interpreted the following way:
 *
 * @a perm:
 *  'r', open file with read-only permissions
 *  'w', open file with read-write permissions, creating if necessary.
 *
 * @param filename File to open
 * @param perm Permission flag
 *
 * @return The return value of open(), generally this is a positive
 * file descriptor on success and -1 on error.
 ***************************************************************************/
int
dlp_openfile (const char *filename, char perm)
{
#if defined(DLP_WIN32)
  int flags = (perm == 'w') ? (_O_RDWR | _O_CREAT | _O_BINARY) : (_O_RDONLY | _O_BINARY);
  int mode = (_S_IREAD | _S_IWRITE);
#else
  int flags = (perm == 'w') ? (O_RDWR | O_CREAT) : O_RDONLY;
  mode_t mode = (S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
#endif
  
  return open (filename, flags, mode);
}  /* End of dlp_openfile() */


/***********************************************************************//**
 * @brief Return a description of the last system error.
 *
 * @return A description of the last system error, in the case of Win32
 * this will be the last Windows Sockets error.
 ***************************************************************************/
const char *
dlp_strerror (void)
{
#if defined(DLP_WIN32)
  static char errorstr[100];

  snprintf (errorstr, sizeof(errorstr), "%d", WSAGetLastError());
  return (const char *) errorstr;

#else
  return (const char *) strerror (errno);

#endif
}  /* End of dlp_strerror() */


/***********************************************************************//**
 * @brief Determine the current system time
 *
 * Determine the current time from the system as a dltime_t value.  On
 * the WIN32 platform this function has millisecond resulution, on
 * Unix platforms this function has microsecond resolution.
 *
 * @return Current time as a dltime_t value.
 ***************************************************************************/
int64_t
dlp_time (void)
{
#if defined(DLP_WIN32)
  
  static const __int64 SECS_BETWEEN_EPOCHS = 11644473600;
  static const __int64 SECS_TO_100NS = 10000000; /* 10^7 */
  
  __int64 dltime;
  __int64 UnixTime;
  SYSTEMTIME SystemTime;
  FILETIME FileTime;
  
  GetSystemTime(&SystemTime);
  SystemTimeToFileTime(&SystemTime, &FileTime);
  
  /* Get the full win32 epoch value, in 100ns */
  UnixTime = ((__int64)FileTime.dwHighDateTime << 32) + 
    FileTime.dwLowDateTime;
  
  /* Convert to the Unix epoch */
  UnixTime -= (SECS_BETWEEN_EPOCHS * SECS_TO_100NS);
  
  UnixTime /= SECS_TO_100NS; /* now convert to seconds */
  
  dltime = ((__int64)UnixTime * DLTMODULUS) +
    ((__int64)SystemTime.wMilliseconds * (DLTMODULUS/1000));
  
  return dltime;
  
#else
  
  int64_t dltime;
  struct timeval tv;
  
  if ( gettimeofday (&tv, (struct timezone *) 0) )
    {
      return DLTERROR;
    }
  
  dltime = ((int64_t)tv.tv_sec * DLTMODULUS) +
    ((int64_t)tv.tv_usec * (DLTMODULUS/1000000));
  
  return dltime;
  
#endif
}  /* End of dlp_time() */


/***********************************************************************//**
 * @brief Sleep for a specified number of microseconds
 * 
 * Sleep for a given number of microseconds.  Under Win32 use SleepEx()
 * and for all others use the POSIX.4 nanosleep(), which can be
 * interrupted by signals.
 *
 * @param useconds Microseconds to sleep.
 ***************************************************************************/
void
dlp_usleep (unsigned long int useconds)
{
#if defined(DLP_WIN32)

  SleepEx ((useconds / 1000), 1);

#else
  
  struct timespec treq, trem;
  
  treq.tv_sec = (time_t) (useconds / 1e6);
  treq.tv_nsec = (long) ((useconds * 1e3) - (treq.tv_sec * 1e9));
  
  nanosleep (&treq, &trem);

#endif
}  /* End of dlp_usleep() */


/***********************************************************************//**
 * @brief Generate a DataLink client ID from system & process information
 * 
 * Generate a client ID composed of the program name, the current user
 * name and the current process ID as a string where the fields are
 * separated by colons:
 *
 * "progname:username:pid:arch"
 *
 * The client ID string is written into a supplied string which must
 * already be allocated.
 *
 * @param progname Name of program, usually argv[0]
 * @param clientid Generated client ID string will be written to this string
 * @param maxsize Maximum bytes to write to @a clientid string.
 *
 * @return the number of charaters written to clientid on success and
 * -1 on error.
 ***************************************************************************/
int
dlp_genclientid (char *progname, char *clientid, size_t maxsize)
{
#if defined(DLP_WIN32)
  char osver[100];
  char *prog = 0;
  char user[256];
  DWORD max_user = 256;
  DWORD dwVersion = 0;
  DWORD dwMajorVersion = 0;
  DWORD dwMinorVersion = 0;
  DWORD dwBuild = 0;
  int pid = getpid();

   /* Do a simple basename() for any supplied progname */
  if ( progname && (prog = strrchr (progname, '\\')) )
    {
      prog++;
    }
  else if ( progname )
    {
      prog = progname;
    }
  
  
  /* Look up current user name */
  if ( ! GetUserName (user, &max_user) )
    {
      user[0] = '\0';
    }
  
  /* Get Windows version */
  dwVersion = GetVersion();
  dwMajorVersion = (DWORD)(LOBYTE(LOWORD(dwVersion)));
  dwMinorVersion = (DWORD)(HIBYTE(LOWORD(dwVersion)));
  if (dwVersion < 0x80000000)
    dwBuild = (DWORD)(HIWORD(dwVersion));
  
  snprintf (clientid, maxsize, "%s:%s:%ld:Win32-%d.%d (%d)",
	    (prog)?prog:"",
	    (user)?user:"",
	    (long) pid,
	    dwMajorVersion, dwMinorVersion, dwBuild);
  
  return 0;
#else
  char osver[100];
  char *prog = 0;
  char *user = 0;
  pid_t pid = getpid ();
  struct passwd *pw;
  struct utsname myname;
  
  /* Do a simple basename() for any supplied progname */
  if ( progname && (prog = strrchr (progname, '/')) )
    {
      prog++;
    }
  else if ( progname )
    {
      prog = progname;
    }
 
  /* Look up real user name */
  if ( (pw = getpwuid(getuid())) )
    {
      user = pw->pw_name;
    }
  
  /* Lookup system name and release */
  if ( uname (&myname) >= 0 )
    {
      snprintf (osver, sizeof(osver), "%s-%s",
		myname.sysname, myname.release);
    }
  else
    {
      osver[0] = '\0';
    }
  
  snprintf (clientid, maxsize, "%s:%s:%ld:%s",
	    (prog)?prog:"",
	    (user)?user:"",
	    (long) pid,
	    osver);
  
  return 0;
#endif
}  /* End of dlp_genclientid() */
