/***********************************************************************/ /**
 * @file portable.c:
 *
 * Platform portability routines.
 *
 * This file is part of the DataLink Library.
 *
 * Copyright (c) 2020 Chad Trabant, IRIS Data Management Center
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ***************************************************************************/

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>

#include "libdali.h"
#include "portable.h"

/************************************************************************/ /**
 * @brief Start up socket subsystem (only does something for WIN)
 *
 * Startup the network socket layer.  At the moment this is only meaningful
 * for the WIN platform and requests Windows sockets version 2.2.
 *
 * @return -1 on errors and 0 on success.
 ***************************************************************************/
int
dlp_sockstartup (void)
{
#if defined(DLP_WIN)
  WORD wVersionRequested;
  WSADATA wsaData;

  /* Check for Windows sockets version 2.2 */
  wVersionRequested = MAKEWORD (2, 2);

  if (WSAStartup (wVersionRequested, &wsaData))
    return -1;

#endif

  return 0;
} /* End of dlp_sockstartup() */

/***********************************************************************/ /**
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
dlp_sockconnect (SOCKET socket, struct sockaddr *inetaddr, int addrlen)
{
#if defined(DLP_WIN)
  if ((connect (socket, inetaddr, addrlen)) == SOCKET_ERROR)
  {
    if (WSAGetLastError () != WSAEWOULDBLOCK)
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
} /* End of dlp_sockconnect() */

/***********************************************************************/ /**
 * @brief Close a network socket
 *
 * Close a network socket.
 *
 * @param socket Network socket descriptor
 *
 * @return -1 on errors and 0 on success.
 ***************************************************************************/
int
dlp_sockclose (SOCKET socket)
{
#if defined(DLP_WIN)
  return closesocket (socket);
#else
  return close (socket);
#endif
} /* End of dlp_sockclose() */

/***********************************************************************/ /**
 * @brief Set a network socket to blocking mode
 *
 * Set a network socket to blocking mode.
 *
 * @param socket Network socket descriptor
 *
 * @return -1 on errors and 0 on success.
 ***************************************************************************/
int
dlp_sockblock (SOCKET socket)
{
#if defined(DLP_WIN)
  u_long flag = 0;

  if (ioctlsocket (socket, FIONBIO, &flag) == -1)
    return -1;

#else
  int flags = fcntl (socket, F_GETFL, 0);

  flags &= (~O_NONBLOCK);

  if (fcntl (socket, F_SETFL, flags) == -1)
    return -1;

#endif

  return 0;
} /* End of dlp_sockblock() */

/***********************************************************************/ /**
 * @brief Set a network socket to non-blocking mode
 *
 * Set a network socket to non-blocking mode.
 *
 * @param socket Network socket descriptor
 *
 * @return -1 on errors and 0 on success.
 ***************************************************************************/
int
dlp_socknoblock (SOCKET socket)
{
#if defined(DLP_WIN)
  u_long flag = 1;

  if (ioctlsocket (socket, FIONBIO, &flag) == -1)
    return -1;

#else
  int flags = fcntl (socket, F_GETFL, 0);

  flags |= O_NONBLOCK;
  if (fcntl (socket, F_SETFL, flags) == -1)
    return -1;

#endif

  return 0;
} /* End of dlp_socknoblock() */

/***********************************************************************/ /**
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
#if defined(DLP_WIN)
  if (WSAGetLastError () != WSAEWOULDBLOCK)
    return -1;

#else
  if (errno != EWOULDBLOCK)
    return -1;

#endif

  /* no data available for NONBLOCKing IO */
  return 0;
} /* End of dlp_noblockcheck() */

/***********************************************************************/ /**
 * @brief Set socket I/O timeout
 *
 * Set socket I/O timeout if such an option exists.  On WIN and
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
dlp_setsocktimeo (SOCKET socket, int timeout)
{
#if defined(DLP_WIN)
  int tval = timeout * 1000;

  if (setsockopt (socket, SOL_SOCKET, SO_RCVTIMEO, (char *)&tval, sizeof (tval)))
  {
    return -1;
  }
  tval = timeout * 1000;
  if (setsockopt (socket, SOL_SOCKET, SO_SNDTIMEO, (char *)&tval, sizeof (tval)))
  {
    return -1;
  }

#else
/* Set socket I/O timeouts if socket options are defined */
#if defined(SO_RCVTIMEO) && defined(SO_SNDTIMEO)
  struct timeval tval;

  tval.tv_sec  = timeout;
  tval.tv_usec = 0;

  if (setsockopt (socket, SOL_SOCKET, SO_RCVTIMEO, &tval, sizeof (tval)))
  {
    return -1;
  }
  if (setsockopt (socket, SOL_SOCKET, SO_SNDTIMEO, &tval, sizeof (tval)))
  {
    return -1;
  }
#else
  return 0;
#endif

#endif

  return 1;
} /* End of dlp_setsocktimeo() */

/***********************************************************************/ /**
 * @brief Set a network I/O real time alarm
 *
 * Set a network I/O alarm timer, the @a timeout is specified in
 * seconds.  The timer is disabled by setting the timeout to zero.  On
 * most platforms this will cause a SIGALARM signal to be sent to the
 * calling process after @a timeout seconds have elapsed.  This
 * function does nothing under WIN.
 *
 * @param timeout Alarm timeout in seconds
 *
 * @return -1 on error and 0 on success.
 ***************************************************************************/
int
dlp_setioalarm (int timeout)
{
#if defined(DLP_WIN)
/* Non-operation for WIN */

#else
  struct itimerval itval;

  itval.it_interval.tv_sec  = 0;
  itval.it_interval.tv_usec = 0;
  itval.it_value.tv_sec     = timeout;
  itval.it_value.tv_usec    = 0;

  if (setitimer (ITIMER_REAL, &itval, NULL))
  {
    return -1;
  }

#endif

  return 0;
} /* End of dlp_setioalarm() */

/***********************************************************************/ /**
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
#if defined(DLP_WIN)
  int flags = (perm == 'w') ? (_O_RDWR | _O_CREAT | _O_BINARY) : (_O_RDONLY | _O_BINARY);
  int mode  = (_S_IREAD | _S_IWRITE);
#else
  int flags   = (perm == 'w') ? (O_RDWR | O_CREAT) : O_RDONLY;
  mode_t mode = (S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
#endif

  return open (filename, flags, mode);
} /* End of dlp_openfile() */

/***********************************************************************/ /**
 * @brief Return a description of the last system error.
 *
 * @return A description of the last system error, in the case of WIN
 * this will be the last Windows Sockets error.
 ***************************************************************************/
const char *
dlp_strerror (void)
{
#if defined(DLP_WIN)
  static char errorstr[100];

  snprintf (errorstr, sizeof (errorstr), "%d", WSAGetLastError ());
  return (const char *)errorstr;

#else
  return (const char *)strerror (errno);

#endif
} /* End of dlp_strerror() */

/***********************************************************************/ /**
 * @brief Determine the current system time
 *
 * Determine the current time from the system as a dltime_t value.  On
 * the WIN platform this function has millisecond resulution, on
 * Unix platforms this function has microsecond resolution.
 *
 * @return Current time as a dltime_t value.
 ***************************************************************************/
int64_t
dlp_time (void)
{
#if defined(DLP_WIN)

  static const __int64 SECS_BETWEEN_EPOCHS = 11644473600;
  static const __int64 SECS_TO_100NS       = 10000000; /* 10^7 */

  __int64 dltime;
  __int64 UnixTime;
  SYSTEMTIME SystemTime;
  FILETIME FileTime;

  GetSystemTime (&SystemTime);
  SystemTimeToFileTime (&SystemTime, &FileTime);

  /* Get the full Windows epoch value, in 100ns */
  UnixTime = ((__int64)FileTime.dwHighDateTime << 32) +
             FileTime.dwLowDateTime;

  /* Convert to the Unix epoch */
  UnixTime -= (SECS_BETWEEN_EPOCHS * SECS_TO_100NS);

  UnixTime /= SECS_TO_100NS; /* now convert to seconds */

  dltime = ((__int64)UnixTime * DLTMODULUS) +
           ((__int64)SystemTime.wMilliseconds * (DLTMODULUS / 1000));

  return dltime;

#else

  int64_t dltime;
  struct timeval tv;

  if (gettimeofday (&tv, (struct timezone *)0))
  {
    return DLTERROR;
  }

  dltime = ((int64_t)tv.tv_sec * DLTMODULUS) +
           ((int64_t)tv.tv_usec * (DLTMODULUS / 1000000));

  return dltime;

#endif
} /* End of dlp_time() */

/***********************************************************************/ /**
 * @brief Sleep for a specified number of microseconds
 *
 * Sleep for a given number of microseconds.  Under WIN use SleepEx()
 * and for all others use the POSIX.4 nanosleep(), which can be
 * interrupted by signals.
 *
 * @param useconds Microseconds to sleep.
 ***************************************************************************/
void
dlp_usleep (unsigned long int useconds)
{
#if defined(DLP_WIN)

  SleepEx ((useconds / 1000), 1);

#else

  struct timespec treq, trem;

  treq.tv_sec  = (time_t) (useconds / 1e6);
  treq.tv_nsec = (long)((useconds * 1e3) - (treq.tv_sec * 1e9));

  nanosleep (&treq, &trem);

#endif
} /* End of dlp_usleep() */

/***********************************************************************/ /**
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
 * @return the number of characters written to clientid on success and
 * -1 on error.
 ***************************************************************************/
int
dlp_genclientid (char *progname, char *clientid, size_t maxsize)
{
#if defined(DLP_WIN)
  char *prog = 0;
  char user[256];
  DWORD max_user       = 256;
  DWORD dwVersion      = 0;
  DWORD dwMajorVersion = 0;
  DWORD dwMinorVersion = 0;
  DWORD dwBuild        = 0;
  int pid              = _getpid ();

  /* Do a simple basename() for any supplied progname */
  if (progname && (prog = strrchr (progname, '\\')))
  {
    prog++;
  }
  else if (progname)
  {
    prog = progname;
  }

  /* Look up current user name */
  if (!GetUserName (user, &max_user))
  {
    user[0] = '\0';
  }

  /* Get Windows version */
  dwVersion      = GetVersion ();
  dwMajorVersion = (DWORD) (LOBYTE (LOWORD (dwVersion)));
  dwMinorVersion = (DWORD) (HIBYTE (LOWORD (dwVersion)));
  if (dwVersion < 0x80000000)
    dwBuild = (DWORD) (HIWORD (dwVersion));

  snprintf (clientid, maxsize, "%s:%s:%ld:WIN-%d.%d (%d)",
            (prog) ? prog : "",
            (user) ? user : "",
            (long)pid,
            dwMajorVersion, dwMinorVersion, dwBuild);

  return (int)strlen (clientid);
#else
  char osver[100];
  char *prog = 0;
  char *user = 0;
  pid_t pid  = getpid ();
  struct passwd *pw;
  struct utsname myname;

  /* Do a simple basename() for any supplied progname */
  if (progname && (prog = strrchr (progname, '/')))
  {
    prog++;
  }
  else if (progname)
  {
    prog = progname;
  }

  /* Look up real user name */
  if ((pw = getpwuid (getuid ())))
  {
    user = pw->pw_name;
  }

  /* Lookup system name and release */
  if (uname (&myname) >= 0)
  {
    snprintf (osver, sizeof (osver), "%.50s-%.48s",
              myname.sysname, myname.release);
  }
  else
  {
    osver[0] = '\0';
  }

  snprintf (clientid, maxsize, "%s:%s:%ld:%s",
            (prog) ? prog : "",
            (user) ? user : "",
            (long)pid,
            osver);

  return (int)strlen (clientid);
#endif
} /* End of dlp_genclientid() */
