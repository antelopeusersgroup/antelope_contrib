/***********************************************************************//**
 * @file genutils.c
 *
 * General utility functions.
 *
 * @author Chad Trabant, IRIS Data Management Center
 *
 * Version: 2008.192
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libdali.h"

/***********************************************************************//**
 * @brief Split a stream ID into separate components: "W_X_Y_Z/TYPE"
 *
 * Split stream ID into separate components from the composite form:
 * "W_X_Y_Z/TYPE" where the underscores and slash separate the
 * components.  Memory for each component must already be allocated.
 * If a specific component is not desired set the appropriate argument
 * to NULL.
 *
 * While the stream name components are completely generic the
 * (strongly) suggested form for geophysical data is
 * "NET_STA_LOC_CHAN/TYPE" where NETwork, STAtion, LOCation and
 * CHANnel follow the FDSN SEED conventions.
 *
 * @return 0 on success and -1 on error.
 ***************************************************************************/
int
dl_splitstreamid (char *streamid, char *w, char *x, char *y, char *z, char *type)
{
  char *id;
  char *ptr, *top, *next;
  
  if ( ! streamid )
    return -1;
  
  /* Duplicate stream ID */
  if ( ! (id = strdup(streamid)) )
    return -1;
  
  /* First truncate after the type if included */
  if ( (ptr = strrchr (id, '/')) )
    {
      *ptr++ = '\0';
      
      /* Copy the type if requested */
      if ( type )
        strcpy (type, ptr);
    }
  
  /* W */
  top = id;
  if ( (ptr = strchr (top, '_')) )
    {
      next = ptr + 1;
      *ptr = '\0';
      
      if ( w )
        strcpy (w, top);
      
      top = next;
    }
  /* X */
  if ( (ptr = strchr (top, '_')) )
    {
      next = ptr + 1;
      *ptr = '\0';

      if ( x )
        strcpy (x, top);
      
      top = next;
    }
  /* Y */
  if ( (ptr = strchr (top, '_')) )
    {
      next = ptr + 1;
      *ptr = '\0';
      
      if ( y )
        strcpy (y, top);
      
      top = next;
    }
  /* Z */
  if ( *top && z )
    {
      strcpy (z, top);
    }
  
  /* Free duplicated stream ID */
  if ( id )
    free (id);
  
  return 0;
}  /* End of dl_splitstreamid() */


/***********************************************************************//**
 * @brief Determine byte order of host machine
 *
 * Determine the byte order of the host machine.  Due to the lack of
 * portable defines to determine host byte order this run-time test is
 * provided.  The code actually tests for little-endianess, the only
 * other alternative is assumed to be big endian.
 * 
 * @return 0 if the host is little endian, otherwise 1.
 ***************************************************************************/
int
dl_bigendianhost ()
{
  int16_t host = 1;
  return !(*((int8_t *)(&host)));
}  /* End of dl_bigendianhost() */


/***********************************************************************//**
 * @brief Return absolute value of double value
 *
 * Determine the absolute value of an input double, actually just test
 * if the input double is positive multiplying by -1.0 if not and
 * return it.
 * 
 * @return Positive value of input double.
 ***************************************************************************/
double
dl_dabs (double value)
{
  if ( value < 0.0 )
    value *= -1.0;
  return value;
}  /* End of dl_dabs() */


/***********************************************************************//**
 * @brief Read a line from a file stream
 *
 * Read characters from a stream (specified as a file descriptor)
 * until a newline character '\n' is read and place them into the
 * supplied buffer.  Reading stops when either a newline character is
 * read or buflen-1 characters have been read.  The buffer will always
 * contain a NULL-terminated string.
 *
 * @return The number of characters read on success and -1 on error.
 ***************************************************************************/
int
dl_readline (int fd, char *buffer, int buflen)
{
  int nread = 0;
  
  if ( ! buffer )
    return -1;
  
  /* Read data from stream until newline character or max characters */
  while ( nread < (buflen-1) )
    {
      /* Read a single character from the stream */
      if ( read (fd, buffer+nread, 1) != 1 )
        {
          return -1;
        }
      
      /* Trap door for newline character */
      if ( buffer[nread] == '\n' )
        {
          break;
        }
      
      nread++;
    }
  
  /* Terminate string in buffer */
  buffer[nread] = '\0';
  
  return nread;
}  /* End of dl_readline() */
