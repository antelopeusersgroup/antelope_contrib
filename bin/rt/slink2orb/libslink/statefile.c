/***************************************************************************
 * statefile.c:
 *
 * Routines to save and recover SeedLink sequence numbers to/from a file.
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

#define HEADER_V2 "#V2 StationID  Sequence  [Timestamp]"

/**********************************************************************/ /**
 * @brief Save the sequence numbers and time stamps into the given state file.
 *
 * The state file is a simple text file with one line per stream containing
 * the station identifier, sequence number, and time stamp.  This information
 * is intended to be used to re-start a connection and continue from where it
 * left off.
 *
 * The current "V2" line format is (header line and example):
 * ```
 *   #V2 StationID  Sequence  [Timestamp]
 *   IU_COLA       1234567890 2024-08-03T17:23:18.0Z
 * ```
 *
 * @param slconn    The ::SLCD connection to save state
 * @param statefile The name of the state file to write
 *
 * @returns 0 on success and -1 on error
 *
 * @sa sl_recoverstate()
 ***************************************************************************/
int
sl_savestate (SLCD *slconn, const char *statefile)
{
  SLstream *curstream;
  FILE *fp;
  char line[200];
  int linelen;

  curstream = slconn->streams;

  /* Open the state file */
  if ((fp = fopen (statefile, "wb")) == NULL)
  {
    sl_log_r (slconn, 2, 0, "cannot open state file for writing\n");
    return -1;
  }

  sl_log_r (slconn, 1, 2, "saving connection state to state file\n");

  /* Write header line */
  fputs (HEADER_V2"\n", fp);

  /* Traverse stream list and write sequence numbers and time stamps*/
  while (curstream != NULL)
  {
    if (curstream->seqnum == SL_UNSETSEQUENCE)
      linelen = snprintf (line, sizeof (line), "%s UNSET %s\n",
                          curstream->stationid,
                          curstream->timestamp);
    else
      linelen = snprintf (line, sizeof (line), "%s %" PRIu64 " %s\n",
                          curstream->stationid,
                          curstream->seqnum,
                          curstream->timestamp);

    if (linelen <= 0)
    {
      sl_log_r (slconn, 2, 0, "Error creating state file entry for: %s\n",
                curstream->stationid);
    }
    else if (fputs (line, fp) == EOF)
    {
      sl_log_r (slconn, 2, 0, "cannot write to state file, %s\n", strerror (errno));
      return -1;
    }

    curstream = curstream->next;
  }

  if (fclose (fp))
  {
    sl_log_r (slconn, 2, 0, "cannot close state file, %s\n", strerror (errno));
    return -1;
  }

  return 0;
} /* End of sl_savestate() */

/**********************************************************************/ /**
 * @brief Recover the state of a SeedLink connection from a file.
 *
 * The specified state file is read, and details of streams, selectors, etc.
 * are added to the specified ::SLCD.
 *
 * The state file format is a simple text file with one line per stream
 * containing the station identifier, sequence number, and time stamp.
 *
 * The current "V2" line format is (header line and examples):
 * ```
 * #V2 StationID  Sequence  [Timestamp]
 * IU_COLA       1234567890 2024-08-03T17:23:18.0Z
 * XX_NONE       UNSET
 * ```
 *
 * where the timestamp is optional, and the special value "UNSET" can be
 * used to indicate that the sequence number is not set.
 *
 * The supported legacy line format takes this form (no header):
 * ```
 * Network Station Sequence#  [Timestamp]
 * IU      COLA    1234567890 2024,08,03,17,23,18
 * XX      NONE    -1
 *```
 *
 * @param slconn    The ::SLCD connection to restore from a saved state
 * @param statefile The name of the state file to read
 *
 * @returns status of the operation:
 * @retval -1 : error
 * @retval  0 : completed successfully
 * @retval  1 : file could not be opened (probably not found)
 *
 * @sa sl_savestate()
 ***************************************************************************/
int
sl_recoverstate (SLCD *slconn, const char *statefile)
{
  SLstream *curstream;
  FILE *fp;

  char line[200];
  char *field[5];
  int format = 0;
  int fields;
  int idx;
  int retval = 0;

  char stationid[22] = {0};
  char timestamp[31] = {0};
  char *stationstr;
  char *sequencestr;
  char *timestr;
  char *endptr;
  char *ptr;

  uint64_t seqnum;
  int count;

  /* Open the state file */
  if ((fp = fopen (statefile, "rb")) == NULL)
  {
    if (errno == ENOENT)
    {
      sl_log_r (slconn, 1, 0, "could not find state file: %s\n", statefile);
      return 1;
    }
    else
    {
      sl_log_r (slconn, 2, 0, "could not open state file, %s\n", strerror (errno));
      return -1;
    }
  }

  sl_log_r (slconn, 1, 1, "recovering connection state from state file: %s\n", statefile);

  count = 1;

  while (fgets (line, sizeof (line), fp))
  {
    stationstr = NULL;
    sequencestr = NULL;
    timestr = NULL;

    /* Terminate at first carriage return or newline */
    if ((ptr = strchr (line, '\r')) != NULL || (ptr = strchr (line, '\n')) != NULL)
      *ptr = '\0';

    /* Store pointers to space-separated fields & convert spaces to terminators */
    for (idx = 0, fields = 0;
         line[idx] && fields < 5;
         idx++)
    {
      if (!isspace(line[idx]))
      {
        if (idx == 0 || line[idx - 1] == '\0')
        {
          field[fields] = line + idx;
          fields++;
        }
      }
      else
      {
        line[idx] = '\0';
      }
    }

    /* Check for recognized version declaration */
    if (fields >= 1 && strncasecmp (field[0], "#V2", 2) == 0)
    {
      format = 2;
      count++;
      continue;
    }
    /* Skip empty or comment lines */
    if (fields == 0 || *field[0] == '#')
    {
      count++;
      continue;
    }

    /* Format V2: StationID Sequence# [Timestamp] */
    if (format == 2 && fields >= 2)
    {
      stationstr  = field[0];
      sequencestr = field[1];
      timestr     = (fields >= 3) ? field[2] : NULL;
    }
    /* Legacy format: NET STA Sequence# [Timestamp] */
    else if (format == 0 && fields >= 3)
    {
      /* Convert special case of legacy uni-station entry to all-station mode */
      if (strcmp (field[0], "XX") == 0 && strcmp (field[1], "UNI") == 0)
      {
        strcpy (stationid, "*");
      }
      else
      {
        snprintf (stationid, sizeof (stationid), "%s_%s", field[0], field[1]);
      }

      stationstr  = stationid;
      sequencestr = field[2];
      timestr     = (fields >= 4) ? field[3] : NULL;
    }
    else
    {
      sl_log_r (slconn, 2, 0, "could not parse line %d of state file: %s\n", count, line);
      retval = -1;
      continue;
    }

    /* Convert legacy SeedLink, comma-delimited date-time to ISO-compatible format
     * Example: '2021,11,19,17,23,18' => '2021-11-18T17:23:18.0Z' */
    if (timestr && format == 0)
    {
      if (sl_isodatetime (timestamp, timestr) != NULL)
      {
        timestr = timestamp;
      }
      else
      {
        sl_log_r (slconn, 1, 0, "could not convert timestamp on line %d of statefile: '%s'\n",
                  count, timestr);
        retval = -1;
        continue;
      }
    }

    if (strcmp (sequencestr, "UNSET") == 0 || strcmp (sequencestr, "-1") == 0)
    {
      seqnum = SL_UNSETSEQUENCE;
    }
    else
    {
      seqnum = (uint64_t)strtoull (sequencestr, &endptr, 10);

      if (*endptr)
      {
        sl_log_r (slconn, 2, 0, "could not parse sequence number from line %d of state file: '%s'\n",
                  count, sequencestr);
        break;
      }
    }

    /* Set the sequence number and time stamp for a matching entry in the stream list */
    curstream = slconn->streams;
    while (curstream != NULL)
    {
      if (!strcmp (stationstr, curstream->stationid))
      {
        curstream->seqnum = seqnum;

        if (timestr)
          strncpy (curstream->timestamp, timestr, sizeof(curstream->timestamp));

        break;
      }

      curstream = curstream->next;
    }

    count++;
  }

  if (ferror (fp))
  {
    sl_log_r (slconn, 2, 0, "file read error for %s\n", statefile);
    retval = -1;
  }

  if (fclose (fp))
  {
    sl_log_r (slconn, 2, 0, "could not close state file, %s\n", strerror (errno));
    retval = -1;
  }

  return retval;
} /* End of sl_recoverstate() */
