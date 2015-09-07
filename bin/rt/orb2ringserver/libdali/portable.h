/***********************************************************************//**
 * @file portable.h:
 * 
 * Platform specific headers.  This file provides a basic level of platform
 * portability.
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
 * Written by Chad Trabant, IRIS Data Management Center
 *
 * modified: 2008.193
 ***************************************************************************/

#ifndef PORTABLE_H
#define PORTABLE_H 1

#ifdef __cplusplus
extern "C" {
#endif


  /* Portability to the XScale (ARM) architecture
   * requires a packed attribute in certain places
   * but this only works with GCC for now.
   */

#if defined (__GNUC__)
  #define DLP_PACKED __attribute__ ((packed))
#else
  #define DLP_PACKED
#endif

  /* Make some guesses about the system libraries based
   * on the architecture.  Currently the assumptions are:
   * Linux => glibc2 (DLP_GLIBC2)
   * Sun => Solaris (DLP_SOLARIS)
   * BSD => BSD libraries, including Apple Mac OS X (DLP_BSD)
   * WIN32 => WIN32 and Windows Sockets 2 (DLP_WIN32)
   */

#if defined(__linux__) || defined(__linux)
  #define DLP_GLIBC2 1

  #include <stdlib.h>
  #include <unistd.h>
  #include <stdarg.h>
  #include <inttypes.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #include <netdb.h>
  #include <sys/time.h>
  #include <sys/utsname.h>
  #include <sys/types.h>
  #include <pwd.h>
   
#elif defined(__sun__) || defined(__sun)
  #define DLP_SOLARIS 1

  #include <stdlib.h>
  #include <unistd.h>
  #include <stdarg.h>
  #include <inttypes.h>
  #include <errno.h>
  #include <sys/types.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #include <netdb.h>
  #include <sys/time.h>
  #include <sys/utsname.h>
  #include <sys/types.h>
  #include <pwd.h>

#elif defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
  #define DLP_BSD 1

  #include <stdlib.h>
  #include <stdio.h>
  #include <unistd.h>
  #include <stdarg.h>
  #include <inttypes.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #include <netdb.h>
  #include <sys/time.h>
  #include <sys/utsname.h>
  #include <sys/types.h>
  #include <string.h>
  #include <ctype.h>
  #include <pwd.h>

#elif defined(WIN32)
  #define DLP_WIN32 1

  #include <windows.h>
  #include <stdarg.h>
  #include <winsock.h>
  #include <stdio.h>
  #include <io.h>
  #include <process.h>

  #define snprintf _snprintf
  #define vsnprintf _vsnprintf
  #define strncasecmp _strnicmp

  typedef signed char int8_t;
  typedef unsigned char uint8_t;
  typedef signed short int int16_t;
  typedef unsigned short int uint16_t;
  typedef signed int int32_t;
  typedef unsigned int uint32_t;
  typedef signed __int64 int64_t;
  typedef unsigned __int64 uint64_t;

#else
  typedef signed char int8_t;
  typedef unsigned char uint8_t;
  typedef signed short int int16_t;
  typedef unsigned short int uint16_t;
  typedef signed int int32_t;
  typedef unsigned int uint32_t;
  typedef signed long long int64_t;
  typedef unsigned long long uint64_t;

#endif

extern int dlp_sockstartup (void);
extern int dlp_sockconnect (int socket, struct sockaddr * inetaddr, int addrlen);
extern int dlp_sockclose (int socket);
extern int dlp_sockblock (int socket);
extern int dlp_socknoblock (int socket);
extern int dlp_noblockcheck (void);
extern int dlp_setsocktimeo (int socket, int timeout);
extern int dlp_setioalarm (int timeout);
extern int dlp_getaddrinfo (char * nodename, char * nodeport,
			    struct sockaddr * addr, size_t * addrlen);
extern int dlp_openfile (const char *filename, char perm);
extern const char *dlp_strerror (void);
extern int64_t dlp_time (void);
extern void dlp_usleep (unsigned long int useconds);
extern int dlp_genclientid (char *progname, char *clientid, size_t maxsize);

#ifdef __cplusplus
}
#endif
 
#endif /* PORTABLE_H */
