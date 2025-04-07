/***************************************************************************
 * config.c:
 *
 * Routines to assist with the configuration of a SeedLink connection
 * description.
 *
 * This file is part of the SeedLink Library.
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
 *
 * Copyright (C) 2024:
 * @author Chad Trabant, EarthScope Data Services
 ***************************************************************************/

#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "libslink.h"

/**********************************************************************/ /**
 * @brief Add a list of streams and selectors from a file to the ::SLCD
 *
 * Streams and selectors are added to the stream list for the ::SLCD
 * for configuring a multi-station connection.
 *
 * If \a defselect is not NULL it will be used as the default selectors
 * for entries will no specific selectors indicated.
 *
 * The file is expected to be repeating lines of the form:
 *   StationID [selectors]
 *
 * For example:
 * --------
 * # Comment lines begin with a '#'
 * GE_ISP  BH?
 * NL_HGN
 * MN_AQU  BH? HH? LH?
 * --------
 *
 * @param slconn The ::SLCD to which to add streams
 * @param streamfile The file containing the stream list
 * @param defselect Default selectors to use when not specified (optional)
 *
 * @returns the number of streams configured or -1 on error.
 *
 * @sa sl_add_streamlist(), sl_add_stream()
 ***************************************************************************/
int
sl_add_streamlist_file (SLCD *slconn, const char *streamfile,
                        const char *defselect)
{
  FILE *fp;
  char *cp;
  char selectors[200] = {0};
  char stationid[64] = {0};
  char line[200];
  int fields = 0;
  int streamcount = 0;
  int linecount = 0;

  /* Open the stream list file */
  if ((fp = fopen (streamfile, "rb")) == NULL)
  {
    if (errno == ENOENT)
    {
      sl_log_r (slconn, 2, 0, "could not find stream list file: %s\n", streamfile);
      return -1;
    }
    else
    {
      sl_log_r (slconn, 2, 0, "opening stream list file, %s\n", strerror (errno));
      return -1;
    }
  }

  sl_log_r (slconn, 1, 1, "Reading stream list from %s\n", streamfile);

  while (fgets (line, sizeof (line), fp))
  {
    linecount += 1;
    memset (stationid, 0, sizeof(stationid));
    memset (selectors, 0, sizeof(selectors));

    /* Terminate string at first carriage return or newline */
    if ((cp = strchr (line, '\r')) != NULL || (cp = strchr (line, '\n')) != NULL)
      *cp = '\0';

    fields = sscanf (line, "%63s %199c", stationid, selectors);

    /* Skip blank or comment lines */
    if (fields <= 0 || stationid[0] == '#')
      continue;

    if (fields < 1)
    {
      sl_log_r (slconn, 2, 0, "cannot parse line %d of stream list: '%s'\n",
                linecount, line);
      continue;
    }

    /* Add this stream to the stream list */
    if (fields == 2)
    {
      sl_add_stream (slconn, stationid, selectors, SL_UNSETSEQUENCE, NULL);
      streamcount++;
    }
    else
    {
      sl_add_stream (slconn, stationid, defselect, SL_UNSETSEQUENCE, NULL);
      streamcount++;
    }
  }

  if (ferror (fp))
  {
    sl_log_r (slconn, 2, 0, "file read error for %s\n", streamfile);
  }

  if (streamcount == 0)
  {
    sl_log_r (slconn, 2, 0, "no streams defined in %s\n", streamfile);
  }
  else if (streamcount > 0)
  {
    sl_log_r (slconn, 1, 2, "Read %d streams from %s\n", streamcount, streamfile);
  }

  if (fclose (fp))
  {
    sl_log_r (slconn, 2, 0, "closing stream list file, %s\n", strerror (errno));
    return -1;
  }

  return streamcount;
} /* End of sl_read_streamlist() */

/**********************************************************************/ /**
 * @brief Add a list of streams and selectors from a string to the ::SLCD
 *
 * Parsed streams and selectors are added to the stream list for configuring
 * a multi-station connection.
 *
 * The string should be of the following form:
 * \c "stream1[:selectors1],stream2[:selectors2],..."
 *
 * For example:
 * "IU_COLA:*_B_H_? *_L_H_?"
 * "IU_KONO:B_H_E B_H_N,GE_WLF,MN_AQU:H_H_?"
 * "IU_KONO:B_H_?:3,GE_WLF:*:3"
 *
 * @param slconn The ::SLCD to which to add streams
 * @param streamlist A string bufffer containing the stream list
 * @param defselect Default selectors to use when not specified (optional)
 *
 * @returns the number of streams configured or -1 on error.
 *
 * @sa sl_add_streamlist_file(), sl_add_stream()
 ***************************************************************************/
int
sl_add_streamlist (SLCD *slconn, const char *streamlist,
                   const char *defselect)
{
  char *parselist;
  char *stream;
  char *nextstream;
  char *selectors;
  int streamcount = 0;

  if (!slconn || !streamlist)
    return -1;

  /* Make a copy that can freely be modified */
  parselist = strdup (streamlist);

  stream = parselist;
  while (stream)
  {
    /* Determine start of next stream and terminate current stream */
    if ((nextstream = strchr (stream, ',')) != NULL)
    {
      *nextstream = '\0';
      nextstream++;
    }

    /* Check for first ':' denoting trailing selector(s) */
    if ((selectors = strchr (stream, ':')) != NULL)
    {
      *selectors = '\0';
      selectors++;
    }

    /* Add non-empty streams to list, using default selectors if none parsed */
    if (strlen (stream) > 0)
    {
      sl_add_stream (slconn, stream,
                     (selectors) ? selectors : defselect,
                     SL_UNSETSEQUENCE, NULL);
    }

    streamcount++;
    stream = nextstream;
  }

  free (parselist);

  return streamcount;
} /* End of sl_parse_streamlist() */
