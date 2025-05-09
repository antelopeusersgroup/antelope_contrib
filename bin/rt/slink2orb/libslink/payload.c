/***************************************************************************
 * payload.c:
 *
 * Generic routines to parse basic information from SeedLink payloads.
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "libslink.h"
#include "mseedformat.h"


/**********************************************************************/ /**
 * @brief Generate a summary string for a specified packet
 *
 * The summary generally includes the source identifier, number of samples,
 * sample rate, and start time.
 *
 * @param[in] log Use the logging parameters specified in ::SLlog
 * @param[in] packetinfo The packet information structure
 * @param[in] plbuffer A buffer containing the packet payload
 * @param[in] plbuffer_size The size of the payload buffer in bytes
 * @param[out] summary A buffer to hold the summary string
 * @param[out] summary_size The size of the summary buffer in bytes
 *
 * @returns The number of bytes that would have been written to the summary
 * string if the size were unlimited on success, -1 on error.
 ***************************************************************************/
int
sl_payload_summary (const SLlog *log, const SLpacketinfo *packetinfo,
                    const char *plbuffer, uint32_t plbuffer_size,
                    char *summary, size_t summary_size)
{
  char sourceid[64] = {0};
  char starttimestr[32] = {0};
  double samplerate = 0.0;
  uint32_t samplecount = 0;

  if (!packetinfo || !plbuffer || plbuffer_size == 0 || !summary || summary_size == 0)
  {
    sl_log_rl (log, 2, 1, "%s(): invalid input parameters\n", __func__);
    return -1;
  }

  if (sl_payload_info (log, packetinfo, plbuffer, plbuffer_size,
                       sourceid, sizeof(sourceid),
                       starttimestr, sizeof(starttimestr),
                       &samplerate, &samplecount) != 0)
  {
    sl_log_rl (log, 2, 1, "%s(): unable to extract payload information\n", __func__);
    return -1;
  }

  return snprintf (summary, summary_size, "%s, %d samples, %g sps, %s ",
                   sourceid, samplecount, samplerate, starttimestr);
} /* End of sl_payload_summary() */

/**********************************************************************/ /**
 * @brief Return selected values (if possible) from a SeedLink packet
 *
 * Parses a SeedLink packet payload and optionally return extracted values.
 * Values that can be returned inclue:
 *  - source identifier
 *  - start time string
 *  - sample rate (in Hz)
 *  - number of samples
 *
 * The returned strings are truncated to fit within the specified size,
 * and are always null terminated.
 *
 * @param[in] log Use the logging parameters specified in ::SLlog
 * @param[in] packetinfo The packet information structure
 * @param[in] plbuffer A buffer containing the packet payload
 * @param[in] plbuffer_size The size of the payload buffer in bytes
 * @param[out] sourceid A buffer to hold the source identifier string
 * @param[out] sourceid_size The size of the source identifier buffer in bytes
 * @param[out] starttimestr A buffer to hold the start time string
 * @param[out] starttimestr_size The size of the start time buffer in bytes
 * @param[out] samplerate A pointer to a double to store the sample rate in Hz
 * @param[out] samplecount A pointer to a uint32_t to store the number of samples
 *
 * @returns 0 on sucess, -1 on error.
 ***************************************************************************/
int
sl_payload_info (const SLlog *log, const SLpacketinfo *packetinfo,
                 const char *plbuffer, uint32_t plbuffer_size,
                 char *sourceid, size_t sourceid_size,
                 char *starttimestr, size_t starttimestr_size,
                 double *samplerate,  uint32_t *samplecount)
{
  uint8_t swapflag = 0; /* byte swapping flag */

  if (!packetinfo || !plbuffer || plbuffer_size == 0)
  {
    sl_log_rl (log, 2, 1, "%s(): invalid input parameters\n", __func__);
    return -1;
  }

  /* Parse requested details from miniSEED v2 */
  if (packetinfo->payloadformat == SLPAYLOAD_MSEED2)
  {
    if (packetinfo->payloadlength < 48)
    {
      sl_log_rl (log, 2, 1, "%s(): payload too short for miniSEEDv2\n", __func__);
      return -1;
    }

    if (sourceid)
    {
      char net[3]  = {0};
      char sta[6]  = {0};
      char loc[3]  = {0};
      char chan[6] = {0};

      sl_strncpclean (net, pMS2FSDH_NETWORK (plbuffer), 2);
      sl_strncpclean (sta, pMS2FSDH_STATION (plbuffer), 5);
      sl_strncpclean (loc, pMS2FSDH_LOCATION (plbuffer), 2);

      /* Map 3 channel codes to BAND_SOURCE_POSITION */
      chan[0] = *pMS2FSDH_CHANNEL (plbuffer);
      chan[1] = '_';
      chan[2] = *(pMS2FSDH_CHANNEL (plbuffer) + 1);
      chan[3] = '_';
      chan[4] = *(pMS2FSDH_CHANNEL (plbuffer) + 2);

      /* Construct FDSN Source Identifier from extracted SEED codes */
      snprintf (sourceid, sourceid_size, "FDSN:%s_%s_%s_%s",
                net, sta, loc, chan);
    }

    /* Check to see if byte swapping is needed by checking for sane year and day */
    if (starttimestr || samplerate || samplecount)
    {
      if (!MS_ISVALIDYEARDAY (*pMS2FSDH_YEAR (plbuffer), *pMS2FSDH_DAY (plbuffer)))
        swapflag = 1;
    }

    if (starttimestr)
    {
      uint16_t year;
      uint16_t yday;
      uint8_t hour;
      uint8_t min;
      uint8_t sec;
      uint16_t fsec;
      int month = 0;
      int mday = 0;

      year = HO2u(*pMS2FSDH_YEAR (plbuffer), swapflag);
      yday = HO2u(*pMS2FSDH_DAY (plbuffer), swapflag);
      hour = *pMS2FSDH_HOUR (plbuffer);
      min  = *pMS2FSDH_MIN (plbuffer);
      sec  = *pMS2FSDH_SEC (plbuffer);
      fsec = HO2u(*pMS2FSDH_FSEC (plbuffer), swapflag);

      sl_doy2md (year, yday, &month, &mday);

      /* Construct time string */
      snprintf (starttimestr, starttimestr_size, "%04d-%02d-%02dT%02d:%02d:%02d.%04dZ",
                year, month, mday, hour, min, sec, fsec);
    }

    if (samplerate)
    {
      int16_t samprate_fact;
      int16_t samprate_mult;

      samprate_fact = HO2d (*pMS2FSDH_SAMPLERATEFACT (plbuffer), swapflag);
      samprate_mult = HO2d (*pMS2FSDH_SAMPLERATEMULT (plbuffer), swapflag);

      if (samprate_fact > 0)
        *samplerate = (double)samprate_fact;
      else if (samprate_fact < 0)
        *samplerate = -1.0 / (double)samprate_fact;

      if (samprate_mult > 0)
        *samplerate = *samplerate * (double)samprate_mult;
      else if (samprate_mult < 0)
        *samplerate = -1.0 * (*samplerate / (double)samprate_mult);
    }

    if (samplecount)
    {
      *samplecount = HO2u(*pMS2FSDH_NUMSAMPLES (plbuffer), swapflag);
    }
  }
  /* Parse requested details from miniSEED v3 */
  else if (packetinfo->payloadformat == SLPAYLOAD_MSEED3)
  {
    if (packetinfo->payloadlength < MS3FSDH_LENGTH + *pMS3FSDH_SIDLENGTH(plbuffer))
    {
      sl_log_rl (log, 2, 1, "%s(): payload too short for miniSEEDv3\n", __func__);
      return -1;
    }

    if (sourceid)
    {
      snprintf (sourceid, sourceid_size, "%.*s",
                (int)*pMS3FSDH_SIDLENGTH (plbuffer),
                pMS3FSDH_SID (plbuffer));
    }

    if (starttimestr || samplerate || samplecount)
    {
      swapflag = (sl_littleendianhost ()) ? 0 : 1;
    }

    if (starttimestr)
    {
      uint16_t year;
      uint16_t yday;
      uint8_t hour;
      uint8_t min;
      uint8_t sec;
      uint32_t nsec;
      int month = 0;
      int mday = 0;

      year = HO2u(*pMS3FSDH_YEAR (plbuffer), swapflag);
      yday = HO2u(*pMS3FSDH_DAY (plbuffer), swapflag);
      hour = *pMS3FSDH_HOUR (plbuffer);
      min  = *pMS3FSDH_MIN (plbuffer);
      sec  = *pMS3FSDH_SEC (plbuffer);
      nsec = HO4u(*pMS3FSDH_NSEC (plbuffer), swapflag);

      sl_doy2md (year, yday, &month, &mday);

      /* Construct time string */
      snprintf (starttimestr, starttimestr_size, "%04d-%02d-%02dT%02d:%02d:%02d.%09dZ",
                year, month, mday, hour, min, sec, nsec);
    }

    if (samplerate)
    {
      *samplerate = HO8f(*pMS3FSDH_SAMPLERATE (plbuffer), swapflag);
    }

    if (samplecount)
    {
      *samplecount = HO4u(*pMS3FSDH_NUMSAMPLES (plbuffer), swapflag);
    }
  }
  else
  {
    sl_log_rl (log, 2, 1, "%s(): unsupported payload format: %c\n",
               __func__, packetinfo->payloadformat);
    return -1;
  }

  return 0;
} /* End of sl_payload_info() */
