/***************************************************************************
 * slutils.c
 *
 * Routines for managing a connection with a SeedLink server
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
#include <stdlib.h>
#include <string.h>
#include <signal.h>

#include "globmatch.h"
#include "libslink.h"
#include "mseedformat.h"

/* Function(s) only used in this source file */
static int receive_header (SLCD *slconn, uint8_t *buffer, uint32_t bytesavailable);
static int64_t receive_payload (SLCD *slconn, char *plbuffer, uint32_t plbuffersize,
                                uint8_t *buffer, uint32_t bytesavailable);
static int update_stream (SLCD *slconn, const char *payload);
static int64_t detect (const char *record, uint64_t recbuflen, char *payloadformat);

/* Initialize the global termination handler */
SLCD *global_termination_SLCD = NULL;


/**********************************************************************/ /**
 * @brief Managage a connection to a SeedLink server and collect packets
 *
 * Designed to run in a loop of a client program, this function manages
 * the connection to the server and returns received packets.  This
 * routine will send keepalives if configured for the connection and
 * can operate in blocking or non-blocking mode.
 *
 * The returned \a packetinfo contains the details including: sequence
 * number, payload length, payload type, and how much of the payload
 * has been returned so far.
 *
 * If the \a slconn.noblock flags is set, the function will return
 * quickly even if no data are available.  If the flag is not set,
 * the function will block and only return if data are available.
 *
 * If \a SLTOOLARGE is returned, the \a plbuffer is not large enough to
 * hold the payload.  The payload length is available at
 * \a packetinfo.payloadlength and the caller may choose to reallocate
 * the buffer to accommodate the payload.  Note that buffer may contain
 * partial payload data and should be preserved if reallocated,
 * specifically the first \a packetinfo.payloadcollected bytes.
 *
 * @param[in]  slconn   SeedLink connection description
 * @param[out] packetinfo  Pointer to pointer to ::SLpacketinfo describing payload
 * @param[out] plbuffer  Destination buffer for packet payload
 * @param[in]  plbuffersize  Length of destination buffer
 *
 * @returns @ref collect-status
 * @retval SLPACKET Complete packet returned
 * @retval SLTERMINATE Connection termination or error
 * @retval SLNOPACKET  No packet available, call again
 * @retval SLTOOLARGE  Payload is larger than allowed maximum
 ***************************************************************************/
int
sl_collect (SLCD *slconn, const SLpacketinfo **packetinfo,
            char *plbuffer, uint32_t plbuffersize)
{
  int64_t bytesread;
  int64_t current_time;
  uint32_t bytesconsumed;
  uint32_t bytesavailable;
  int poll_state;

  if (!slconn || !packetinfo || (plbuffersize > 0 && !plbuffer))
    return SLTERMINATE;

  while (slconn->terminate < 2)
  {
    current_time = sl_nstime();

    if (slconn->link == -1)
    {
      slconn->stat->conn_state = DOWN;
    }

    /* Throttle the loop while delaying */
    if (slconn->stat->conn_state == DOWN &&
        slconn->stat->netdly_time &&
        slconn->stat->netdly_time > current_time)
    {
      sl_usleep (500000);
    }

    /* Connect to server if disconnected */
    if (slconn->stat->conn_state == DOWN &&
        slconn->stat->netdly_time < current_time)
    {
      if (sl_connect (slconn, 1) != -1)
      {
        slconn->stat->conn_state = UP;
      }
      slconn->stat->netto_time     = 0;
      slconn->stat->netdly_time    = 0;
      slconn->stat->keepalive_time = 0;
    }

    /* Negotiate/configure the connection */
    if (slconn->stat->conn_state == UP)
    {
      if (slconn->streams)
      {
        if (sl_configlink (slconn) == -1)
        {
          sl_log_r (slconn, 2, 0, "[%s] %s(): negotiation with server failed\n",
                    slconn->sladdr, __func__);
          slconn->link              = sl_disconnect (slconn);
          slconn->stat->netdly_time = 0;
        }
      }

      slconn->stat->conn_state = STREAMING;
    }

    /* Send INFO request if one not in progress */
    if (slconn->stat->conn_state == STREAMING &&
        slconn->stat->query_state == NoQuery &&
        slconn->info)
    {
      if (sl_send_info (slconn, slconn->info, 1) != -1)
      {
        slconn->stat->query_state = InfoQuery;
      }
      else
      {
        slconn->stat->query_state = NoQuery;
      }

      free (slconn->info);
      slconn->info = NULL;
    }

    /* Read incoming data stream */
    if (slconn->stat->conn_state == STREAMING)
    {
      /* Receive data into internal buffer */
      if (slconn->terminate == 0)
      {
        bytesread = sl_recvdata (slconn,
                                 slconn->recvbuffer + slconn->recvdatalen,
                                 sizeof (slconn->recvbuffer) - slconn->recvdatalen,
                                 slconn->sladdr);

        if (bytesread < 0)
        {
          break;
        }
        else if (bytesread > 0)
        {
          slconn->recvdatalen += bytesread;
        }
        else if (slconn->recvdatalen == 0) /* bytesread == 0 */
        {
          /* Wait up to 1/2 second when blocking, otherwise 1 millisecond */
          poll_state = sl_poll (slconn, 1, 0, (slconn->noblock) ? 1 : 500);

          if (poll_state < 0 && slconn->terminate == 0)
          {
            sl_log_r (slconn, 2, 0, "[%s] %s(): polling error: %s\n",
                      slconn->sladdr, __func__, sl_strerror ());
            break;
          }
        }
      }

      /* Process data in internal buffer */
      bytesconsumed = 0;

      /* Check for special cases of the server reporting end of streaming or errors
       * while awaiting a header (i.e. in between packets) */
      if (slconn->stat->stream_state == HEADER)
      {
        if (slconn->recvdatalen - bytesconsumed >= 3 &&
            memcmp (slconn->recvbuffer + bytesconsumed, "END", 3) == 0)
        {
          sl_log_r (slconn, 1, 1, "[%s] End of selected time window or stream (FETCH/dial-up mode)\n",
                    slconn->sladdr);

          bytesconsumed += 3;
          break;
        }

        if (slconn->recvdatalen - bytesconsumed >= 5 &&
            memcmp (slconn->recvbuffer + bytesconsumed, "ERROR", 5) == 0)
        {
          sl_log_r (slconn, 2, 0, "[%s] Server reported an error with the last command\n",
                    slconn->sladdr);

          bytesconsumed += 5;
          break;
        }
      }

      /* Read next header */
      if (slconn->stat->stream_state == HEADER)
      {
        bytesavailable = slconn->recvdatalen - bytesconsumed;

        if ((slconn->protocol & SLPROTO3X && bytesavailable >= SLHEADSIZE_V3) ||
            (slconn->protocol & SLPROTO40 && bytesavailable >= SLHEADSIZE_V4))
        {
          bytesread = receive_header (slconn,
                                      slconn->recvbuffer + bytesconsumed,
                                      bytesavailable);

          if (bytesread < 0)
          {
            break;
          }
          else if (bytesread > 0)
          {
            /* Set state for station ID or payload collection */
            if (slconn->stat->packetinfo.stationidlength > 0)
            {
              slconn->stat->packetinfo.stationid[0] = '\0';
              slconn->stat->stream_state            = STATIONID;
            }
            else
            {
              slconn->stat->packetinfo.payloadcollected = 0;
              slconn->stat->stream_state                = PAYLOAD;
            }

            bytesconsumed += bytesread;
          }
        }
      } /* Done reading header */

      /* Read station ID */
      if (slconn->stat->stream_state == STATIONID &&
          slconn->stat->packetinfo.stationidlength > 0 &&
          (slconn->recvdatalen - bytesconsumed) >= slconn->stat->packetinfo.stationidlength)
      {
        if (slconn->stat->packetinfo.stationidlength > (sizeof (slconn->stat->packetinfo.stationid) - 1))
        {
          sl_log_r (slconn, 2, 0,
                    "[%s] %s() received station ID is too large (%u) for buffer (%zu)\n",
                    slconn->sladdr, __func__,
                    slconn->stat->packetinfo.stationidlength,
                    sizeof (slconn->stat->packetinfo.stationid) - 1);

          break;
        }
        else
        {
          memcpy (slconn->stat->packetinfo.stationid,
                  slconn->recvbuffer + bytesconsumed,
                  slconn->stat->packetinfo.stationidlength);

          slconn->stat->packetinfo.stationid[slconn->stat->packetinfo.stationidlength] = '\0';

          /* Set state for payload collection */
          slconn->stat->packetinfo.payloadcollected = 0;
          slconn->stat->stream_state                = PAYLOAD;

          bytesconsumed += slconn->stat->packetinfo.stationidlength;
        }
      } /* Done reading station ID */

      /* Read payload */
      if (slconn->stat->stream_state == PAYLOAD)
      {
        bytesavailable = slconn->recvdatalen - bytesconsumed;

        /* If payload length is known, return SLTOOLARGE if buffer is not sufficient */
        if (slconn->stat->packetinfo.payloadlength > 0 &&
            slconn->stat->packetinfo.payloadlength > plbuffersize)
        {
          /* Shift any remaining data in the buffer to the start */
          if (bytesconsumed > 0 && bytesconsumed < slconn->recvdatalen)
          {
            memmove (slconn->recvbuffer,
                     slconn->recvbuffer + bytesconsumed,
                     slconn->recvdatalen - bytesconsumed);
          }

          slconn->recvdatalen -= bytesconsumed;
          bytesconsumed = 0;

          *packetinfo = &slconn->stat->packetinfo;
          return SLTOOLARGE;
        }

        bytesread = receive_payload (slconn, plbuffer, plbuffersize,
                                     slconn->recvbuffer + bytesconsumed,
                                     bytesavailable);

        if (bytesread < 0)
        {
          break;
        }
        if (bytesread > 0)
        {
          slconn->stat->netto_time     = 0;
          slconn->stat->keepalive_time = 0;

          bytesconsumed += bytesread;
        }

        /* Payload is complete */
        if (slconn->stat->packetinfo.payloadlength > 0 &&
            slconn->stat->packetinfo.payloadcollected == slconn->stat->packetinfo.payloadlength)
        {
          /* Shift any remaining data in the buffer to the start */
          if (bytesconsumed > 0 && bytesconsumed < slconn->recvdatalen)
          {
            memmove (slconn->recvbuffer,
                     slconn->recvbuffer + bytesconsumed,
                     slconn->recvdatalen - bytesconsumed);
          }

          slconn->recvdatalen -= bytesconsumed;
          bytesconsumed = 0;

          /* Set state for header collection if payload is complete */
          slconn->stat->stream_state = HEADER;

          /* V3 Keepalive INFO responses are not returned to the caller */
          if (slconn->stat->query_state == KeepAliveQuery &&
              (slconn->stat->packetinfo.payloadformat == SLPAYLOAD_MSEED2INFOTERM ||
               slconn->stat->packetinfo.payloadformat == SLPAYLOAD_MSEED2INFO))
          {
            if (slconn->stat->packetinfo.payloadformat == SLPAYLOAD_MSEED2INFOTERM)
            {
              sl_log_r (slconn, 1, 2, "[%s] Keepalive message received\n", slconn->sladdr);

              slconn->stat->query_state = NoQuery;
            }
          }
          /* V4 Keepalive INFO responses are not returned to caller */
          else if (slconn->stat->query_state == KeepAliveQuery &&
                   slconn->stat->packetinfo.payloadformat == SLPAYLOAD_JSON &&
                   slconn->stat->packetinfo.payloadsubformat == SLPAYLOAD_JSON_INFO)
          {
            sl_log_r (slconn, 1, 2, "[%s] Keepalive message received\n", slconn->sladdr);

            slconn->stat->query_state = NoQuery;
          }
          /* All other payloads are returned to the caller */
          else
          {
            /* Update streaming tracking */
            if (update_stream (slconn, plbuffer) == -1)
            {
              sl_log_r (slconn, 2, 0, "[%s] %s(): cannot update stream tracking\n",
                        slconn->sladdr, __func__);
              return -1;
            }

            *packetinfo = &slconn->stat->packetinfo;
            return SLPACKET;
          }
        }
      } /* Done reading payload */

      /* If a viable amount of data exists but has not been consumed something is wrong with the stream */
      if (slconn->recvdatalen > SL_MIN_PAYLOAD && bytesconsumed == 0)
      {
        sl_log_r (slconn, 2, 0, "[%s] %s(): cannot process received data, terminating.\n",
                  slconn->sladdr, __func__);
        sl_log_r (slconn, 2, 0, "[%s]  recvdatalen: %u, stream_state: %d, bytesconsumed: %u\n",
                  slconn->sladdr, slconn->recvdatalen, slconn->stat->stream_state, bytesconsumed);
        break;
      }

      /* Shift any remaining data in the buffer to the start */
      if (bytesconsumed > 0 && bytesconsumed < slconn->recvdatalen)
      {
        memmove (slconn->recvbuffer,
                 slconn->recvbuffer + bytesconsumed,
                 slconn->recvdatalen - bytesconsumed);
      }

      slconn->recvdatalen -= bytesconsumed;
      bytesconsumed = 0;

      /* Set termination flag to level 2 if less than viable number of bytes in buffer */
      if (slconn->terminate == 1 && slconn->recvdatalen < SL_MIN_PAYLOAD)
      {
        slconn->terminate = 2;
      }
    } /* Done reading data in STREAMING state */

    /* Update timing variables */
    current_time = sl_nstime ();

    /* Check for network idle timeout */
    if (slconn->stat->conn_state == STREAMING &&
        slconn->netto && slconn->stat->netto_time &&
        slconn->stat->netto_time < current_time)
    {
      sl_log_r (slconn, 1, 0, "[%s] network timeout (%ds), reconnecting in %ds\n",
                slconn->sladdr, slconn->netto, slconn->netdly);
      sl_disconnect (slconn);
      slconn->link              = -1;
      slconn->stat->conn_state  = DOWN;
      slconn->stat->netto_time  = 0;
      slconn->stat->netdly_time = 0;
    }

    /* Check if keepalive packet needs to be sent */
    if (slconn->stat->conn_state == STREAMING &&
        slconn->stat->query_state == NoQuery &&
        slconn->keepalive && slconn->stat->keepalive_time &&
        slconn->stat->keepalive_time < current_time)
    {
      sl_log_r (slconn, 1, 2, "[%s] Sending keepalive message\n", slconn->sladdr);

      if (sl_send_info (slconn, "ID", 3) == -1)
      {
        break;
      }

      slconn->stat->query_state     = KeepAliveQuery;
      slconn->stat->keepalive_time = 0;
    }

    /* Network timeout */
    if (slconn->netto && slconn->stat->netto_time == 0)
    {
      slconn->stat->netto_time = current_time + SL_EPOCH2SLTIME (slconn->netto);
    }

    /* Network connection delay */
    if (slconn->netdly && slconn->stat->netdly_time == 0)
    {
      slconn->stat->netdly_time = current_time + SL_EPOCH2SLTIME (slconn->netdly);
    }

    /* Keepalive/heartbeat interval */
    if (slconn->keepalive && slconn->stat->keepalive_time == 0)
    {
      slconn->stat->keepalive_time = current_time + SL_EPOCH2SLTIME (slconn->keepalive);
    }

    /* Return if not waiting for data and no data in internal buffer */
    if (slconn->noblock && slconn->recvdatalen == 0)
    {
      *packetinfo = NULL;
      return SLNOPACKET;
    }

    /* Termination in any connection state but UP is immediate */
    if (slconn->terminate && slconn->stat->conn_state != UP)
    {
      break;
    }
  } /* End of primary loop */

  /* Terminating */
  sl_disconnect (slconn);
  slconn->link = -1;

  *packetinfo = NULL;
  return SLTERMINATE;
} /* End of sl_collect() */

/***************************************************************************
 * receive_header:
 *
 * Receive packet header.
 *
 * Returns:
 * bytes : Size of header read
 * -1 :  on error
 ***************************************************************************/
static int
receive_header (SLCD *slconn, uint8_t *buffer, uint32_t bytesavailable)
{
  uint32_t bytesread = 0;
  char sequence[7] = {0};
  char *tail = NULL;

  if (!slconn)
    return -1;

  /* Zero the destination packet info structure */
  memset (&slconn->stat->packetinfo, 0, sizeof (SLpacketinfo));

  if (slconn->protocol & SLPROTO3X && bytesavailable >= SLHEADSIZE_V3)
  {
    /* Parse v3 INFO header */
    if (memcmp (buffer, INFOSIGNATURE, 6) == 0)
    {
      slconn->stat->packetinfo.seqnum        = SL_UNSETSEQUENCE;
      slconn->stat->packetinfo.payloadlength = 0;
      slconn->stat->packetinfo.payloadformat = (buffer[SLHEADSIZE_V3 - 1] == '*') ? SLPAYLOAD_MSEED2INFO : SLPAYLOAD_MSEED2INFOTERM;
    }
    /* Parse v3 data header */
    else if (memcmp (buffer, SIGNATURE_V3, 2) == 0)
    {
      memcpy (sequence, buffer + 2, 6);
      slconn->stat->packetinfo.seqnum = strtoul (sequence, &tail, 16);

      if (*tail)
      {
        sl_log_r (slconn, 2, 0, "[%s] %s() cannot parse sequence number from v3 header: %8.8s\n",
                  slconn->sladdr, __func__, buffer + 2);
        return -1;
      }

      slconn->stat->packetinfo.payloadlength = 0;
      slconn->stat->packetinfo.payloadformat = SLPAYLOAD_UNKNOWN;
    }
    else
    {
      sl_log_r (slconn, 2, 0, "[%s] %s(): unexpected V3 header signature found: %2.2s)\n",
                slconn->sladdr, __func__, buffer);
      return -1;
    }

    bytesread = SLHEADSIZE_V3;
  }
  else if (slconn->protocol & SLPROTO40 && bytesavailable >= SLHEADSIZE_V4)
  {
    /* Parse v4 header */
    if (memcmp (buffer, SIGNATURE_V4, 2) == 0)
    {
      slconn->stat->packetinfo.payloadformat    = buffer[2];
      slconn->stat->packetinfo.payloadsubformat = buffer[3];
      memcpy (&slconn->stat->packetinfo.payloadlength, buffer + 4, 4);
      memcpy (&slconn->stat->packetinfo.seqnum, buffer + 8, 8);
      memcpy (&slconn->stat->packetinfo.stationidlength, buffer + 16, 1);

      if (!sl_littleendianhost ())
      {
        sl_gswap8 (&slconn->stat->packetinfo.seqnum);
        sl_gswap4 (&slconn->stat->packetinfo.payloadlength);
      }
    }
    else
    {
      sl_log_r (slconn, 2, 0, "[%s] %s(): unexpected V4 header signature found: %2.2s)\n",
                slconn->sladdr, __func__, buffer);
      return -1;
    }

    bytesread = SLHEADSIZE_V4;
  }
  else
  {
    sl_log_r (slconn, 2, 0, "[%s] %s(): unexpected header signature found (instead: %2.2s)\n",
              slconn->sladdr, __func__, buffer);
    return -1;
  }

  return bytesread;
} /* End of receive_header() */

/***************************************************************************
 * receive_payload:
 *
 * Copy payload data to supplied buffer.
 *
 * The supplied buffer must be large enough for payload detection,
 * defined as SL_MIN_PAYLOAD bytes.
 *
 * Returns
 * bytes : Number of bytes consumed on success
 * -1 :  on error
 ***************************************************************************/
int64_t
receive_payload (SLCD *slconn, char *plbuffer, uint32_t plbuffersize,
                 uint8_t *buffer, uint32_t bytesavailable)
{
  SLpacketinfo *packetinfo = NULL;
  uint32_t bytestoconsume = 0;
  int64_t detectedlength;
  char payloadformat = SLPAYLOAD_UNKNOWN;

  if (!slconn || !plbuffer)
    return -1;

  packetinfo = &slconn->stat->packetinfo;

  /* Return for more data if the minimum for detection is not available */
  if (packetinfo->payloadlength == 0 && bytesavailable < SL_MIN_PAYLOAD)
  {
    return 0;
  }

  /* If payload length is unknown, consume up to 128 bytes */
  if (packetinfo->payloadlength == 0)
  {
    bytestoconsume = (bytesavailable < 128) ? bytesavailable : 128;
  }
  /* If remaining payload is smaller than available, consume remaining */
  else if ((packetinfo->payloadlength - packetinfo->payloadcollected) < bytesavailable)
  {
    bytestoconsume = packetinfo->payloadlength - packetinfo->payloadcollected;
  }
  /* Otherwise, all available data is payload */
  else
  {
    bytestoconsume = bytesavailable;
  }

  if (bytestoconsume > plbuffersize - packetinfo->payloadcollected)
  {
    sl_log_r (slconn, 2, 0, "[%s] %s(): provided buffer size (%u) is insufficient for payload (%u)\n",
              slconn->sladdr, __func__, plbuffersize,
              (packetinfo->payloadlength == 0) ? bytestoconsume : packetinfo->payloadlength);
    return -1;
  }

  /* Copy payload data from internal buffer to payload buffer */
  memcpy (plbuffer + packetinfo->payloadcollected, buffer, bytestoconsume);
  packetinfo->payloadcollected += bytestoconsume;

  /* If payload length is not yet known for V3, try to detect from payload */
  if (slconn->protocol & SLPROTO3X && packetinfo->payloadlength == 0)
  {
    detectedlength = detect (plbuffer, packetinfo->payloadcollected, &payloadformat);

    /* Return error if no recognized payload detected */
    if (detectedlength < 0)
    {
      sl_log_r (slconn, 2, 0,
                "[%s] %s(): non-miniSEED packet received for v3 protocol! Terminating.\n",
                slconn->sladdr, __func__);
      return -1;
    }
    /* Update packet info if length detected */
    else if (detectedlength > 0)
    {
      if (packetinfo->payloadformat == SLPAYLOAD_UNKNOWN)
      {
        packetinfo->payloadformat = payloadformat;
      }

      packetinfo->payloadlength = detectedlength;
    }
  }

  return bytestoconsume;
} /* End of receive_payload() */

/***************************************************************************
 * update_stream:
 *
 * Update the appropriate stream list entries.  Length of the payload
 * must be at least enough to determine stream details.
 *
 * The slconn->stat->packetinfo.stationid value is also populated from
 * the payload if not already set.
 *
 * Returns 0 if successfully updated and -1 if not found or error.
 ***************************************************************************/
static int
update_stream (SLCD *slconn, const char *payload)
{
  SLpacketinfo *packetinfo = NULL;
  SLstream *curstream;
  int updates  = 0;

  char timestamp[32] = {0};
  char sourceid[64] = {0};
  char *cp;
  size_t count;

  if (!slconn || !payload)
    return -1;

  packetinfo = &slconn->stat->packetinfo;

  /* No updates for info and error packets */
  if (packetinfo->payloadformat == SLPAYLOAD_MSEED2INFO ||
      packetinfo->payloadformat == SLPAYLOAD_MSEED2INFOTERM ||
      (packetinfo->payloadformat == SLPAYLOAD_JSON &&
       (packetinfo->payloadsubformat == SLPAYLOAD_JSON_INFO ||
        packetinfo->payloadsubformat == SLPAYLOAD_JSON_ERROR)))
  {
    return 0;
  }

  /* Extract start time stamp and source ID (if needed) from payload if miniSEED */
  if (packetinfo->payloadformat == SLPAYLOAD_MSEED2 ||
      packetinfo->payloadformat == SLPAYLOAD_MSEED3)
  {
    if (sl_payload_info (slconn->log, packetinfo,
                         payload, packetinfo->payloadlength,
                         (packetinfo->stationidlength == 0) ? sourceid : NULL, sizeof (sourceid),
                         timestamp, sizeof (timestamp),
                         NULL, NULL) == -1)
    {
      sl_log_r (slconn, 2, 0, "[%s] %s(): cannot extract payload info for miniSEED\n",
                slconn->sladdr, __func__);
      return -1;
    }

    /* Set station ID if it was not included in SeedLink header (e.g. v3 protocol) */
    if (packetinfo->stationidlength == 0)
    {
      /* Extract NET_STA from FDSN Source Identifier returned by sl_payload_info() */
      if (strlen (sourceid) >= 8 && strncmp (sourceid, "FDSN:", 5) == 0)
      {
        /* Copy from ':' to 2nd '_' from "FDSN:NET_STA_LOC_B_S_SS" */
        if ((cp = strchr (sourceid + 5, '_')))
        {
          if ((cp = strchr (cp + 1, '_')))
          {
            count = (cp - sourceid) - 5;

            if (count >= sizeof (packetinfo->stationid))
            {
              sl_log_r (slconn, 2, 0, "[%s] %s(): extracted NET_STA ID from miniSEED is too large (%zu)\n",
                        slconn->sladdr, __func__, count);
              return -1;
            }

            memcpy (packetinfo->stationid, sourceid + 5, sizeof (packetinfo->stationid));
            packetinfo->stationid[count] = '\0';
            packetinfo->stationidlength = count;
          }
        }
      }
    }
  }

  curstream = slconn->streams;

  /* For all-station mode */
  if (curstream != NULL &&
      strcmp (curstream->stationid, "*") == 0)
  {
    curstream->seqnum = packetinfo->seqnum;
    strcpy (curstream->timestamp, timestamp);

    return 0;
  }

  /* For multi-station mode, search the stream list and update all matching entries */
  while (curstream != NULL)
  {
    /* Use glob matching to match wildcarded station ID codes */
    if (sl_globmatch (packetinfo->stationid, curstream->stationid))
    {
      curstream->seqnum = packetinfo->seqnum;
      strcpy (curstream->timestamp, timestamp);

      updates++;
    }

    curstream = curstream->next;
  }

  /* If no updates then no match was found */
  if (updates == 0)
    sl_log_r (slconn, 2, 0, "[%s] unexpected data received: %s\n",
              slconn->sladdr, packetinfo->stationid);

  return (updates == 0) ? -1 : 0;
  } /* End of update_stream() */

/**********************************************************************/ /**
 * @brief Initialize a new ::SLCD
 *
 * Allocate a new ::SLCD and set default values.
 *
 * The \a clientname must be specified and should be a string
 * describing the name of the client program. The \a clientversion is
 * optional and should be the version of the client program.  These
 * values are passed directly to sl_set_clientname().
 *
 * @param[in] clientname     Name of the client program
 * @param[in] clientversion  Version of the client program
 *
 * @returns An initialized ::SLCD on success, NULL on error.
 ***************************************************************************/
SLCD *
sl_initslcd (const char *clientname, const char *clientversion)
{
  SLCD *slconn;

  slconn = (SLCD *)malloc (sizeof (SLCD));

  if (slconn == NULL)
  {
    sl_log_r (NULL, 2, 0, "%s(): error allocating memory\n", __func__);
    return NULL;
  }

  memset (slconn, 0, sizeof (SLCD));

  /* Set defaults */
  slconn->sladdr        = NULL;
  slconn->slhost        = NULL;
  slconn->slport        = NULL;
  slconn->clientname    = NULL;
  slconn->clientversion = NULL;
  slconn->start_time    = NULL;
  slconn->end_time      = NULL;
  slconn->keepalive     = 0;
  slconn->iotimeout     = 60;
  slconn->netto         = 600;
  slconn->netdly        = 30;
  slconn->auth_value    = NULL;
  slconn->auth_finish   = NULL;
  slconn->auth_data     = NULL;
  slconn->streams       = NULL;
  slconn->info          = NULL;
  slconn->noblock       = 0;
  slconn->dialup        = 0;
  slconn->batchmode     = 0;
  slconn->lastpkttime   = 1;
  slconn->terminate     = 0;
  slconn->resume        = 1;
  slconn->multistation  = 0;

  slconn->link             = -1;
  slconn->protocol         = UNSET_PROTO;
  slconn->server_protocols = 0;
  slconn->capabilities     = NULL;
  slconn->caparray         = NULL;
  slconn->tls              = 0;
  slconn->tlsctx           = NULL;

  /* Allocate the associated persistent state struct */
  if ((slconn->stat = (SLstat *)malloc (sizeof (SLstat))) == NULL)
  {
    sl_log_r (NULL, 2, 0, "%s(): error allocating memory\n", __func__);
    free (slconn);
    return NULL;
  }

  memset (slconn->stat, 0, sizeof (SLstat));

  slconn->stat->packetinfo.seqnum = SL_UNSETSEQUENCE;
  slconn->stat->packetinfo.payloadlength = 0;
  slconn->stat->packetinfo.payloadcollected = 0;
  slconn->stat->packetinfo.payloadformat = SLPAYLOAD_UNKNOWN;

  slconn->stat->netto_time     = 0;
  slconn->stat->netdly_time    = 0;
  slconn->stat->keepalive_time = 0;

  slconn->stat->conn_state   = DOWN;
  slconn->stat->stream_state = HEADER;
  slconn->stat->query_state  = NoQuery;

  slconn->log = NULL;

  slconn->recvdatalen = 0;

  /* Store copies of client name and version */
  if (clientname && sl_set_clientname (slconn, clientname, clientversion))
  {
    sl_freeslcd (slconn);
    return NULL;
  }

  return slconn;
} /* End of sl_newslcd() */

/**********************************************************************/ /**
 * @brief Free all memory associated with a ::SLCD
 *
 * Free all memory associated with a SLCD struct including the
 * associated stream list and persistent connection state.
 *
 * @param[in] slconn     SeedLink connection description to free
 ***************************************************************************/
void
sl_freeslcd (SLCD *slconn)
{
  SLstream *curstream;
  SLstream *nextstream;

  curstream = slconn->streams;

  /* Traverse the stream list and free memory */
  while (curstream != NULL)
  {
    nextstream = curstream->next;

    if (curstream->selectors != NULL)
      free (curstream->selectors);
    free (curstream);

    curstream = nextstream;
  }

  free (slconn->sladdr);
  free (slconn->slhost);
  free (slconn->slport);
  free (slconn->start_time);
  free (slconn->end_time);
  free (slconn->capabilities);
  free (slconn->caparray);
  free (slconn->clientname);
  free (slconn->clientversion);
  free (slconn->stat);
  free (slconn->log);
  free (slconn);
} /* End of sl_freeslcd() */

/**********************************************************************/ /**
 * @brief Set client name and version reported to server (v4 only)
 *
 * Set the program name and, optionally, version that will be send to
 * the server in protocol v4 version.  These values will be combined
 * into a value with the pattern:
 *   NAME[/VERSION]
 *
 * @param[in] slconn     SeedLink connection description
 * @param[in] name       Name of the client program
 * @param[in] version    Version of the client program
 *
 * @retval  0 : success
 * @retval -1 : error
 ***************************************************************************/
int
sl_set_clientname (SLCD *slconn, const char *name, const char *version)
{
  if (!slconn || !name)
    return -1;

  free (slconn->clientname);
  free (slconn->clientversion);

  slconn->clientname = strdup (name);

  if (slconn->clientname == NULL)
  {
    sl_log_r (NULL, 2, 0, "%s(): error allocating memory\n", __func__);
    return -1;
  }

  if (version)
  {
    slconn->clientversion = strdup (version);

    if (slconn->clientversion == NULL)
    {
      sl_log_r (NULL, 2, 0, "%s(): error allocating memory\n", __func__);
      return -1;
    }
  }

  return 0;
} /* End of sl_set_clientname() */

/**********************************************************************/ /**
 * @brief Set SeedLink server address (and port)
 *
 * Set the address (and port) of the SeedLink server to connect to.  The
 * \p server_address string should be in the following format:
 *
 *  \c HOST:PORT
 *
 * where \c HOST and \c PORT are both optional.  If \c HOST is not
 * specified the value of \a SL_DEFAULT_HOST (usually "localhost") will
 * be used.  If \c PORT is not specified the value of \a SL_DEFAULT_PORT
 * (usually "18000") will be used.
 *
 * The \c HOST value can be an IPv4 or IPv6 address, or a hostname.  The
 * \c PORT value must be a valid port number.
 *
 * The following variations are supported:
 * ```
 * 192.168.0.1
 * :18000
 * 192.168.0.1:18000
 * seedlink.datacenter.org:18500
 * 2607:f8b0:400a:805::200e
 * [2607:f8b0:400a:805::200e]:18000
 * ```
 *
 * This routine will also set the \a tls flag if the port is the default
 * for a secure connection, aka \a SL_SECURE_PORT.
 *
 * @param slconn          SeedLink connection description
 * @param server_address  Server address in \c HOST:PORT format
 *
 * @retval  0 : success
 * @retval -1 : error
 ***************************************************************************/
int
sl_set_serveraddress (SLCD *slconn, const char *server_address)
{
  char host[300] = {0};
  char port[100] = {0};
  const char *separator;
  const char *search;
  const char *open;
  const char *close;

  if (!slconn || !server_address)
    return -1;

  /* Check for host enclosed in square brackets, e.g. for raw IPv6 addresses */
  if ((open = strchr (server_address, '[')) != NULL &&
      (close = strchr (server_address, ']')) != NULL &&
      open < close)
  {
    search = close + 1;
  }
  else
  {
    search = server_address;
  }

  /* Search address for host-port separator, i.e. last ':' */
  if ((separator = strrchr (search, ':')) == NULL)
  {
    separator = NULL;
  }

  /* If address begins with the separator */
  if (server_address == separator)
  {
    if (server_address[1] == '\0') /* Only a separator */
    {
      strncpy (host, SL_DEFAULT_HOST, sizeof (host) - 1);
      strncpy (port, SL_DEFAULT_PORT, sizeof (port) - 1);
    }
    else /* Only a port */
    {
      strncpy (host, SL_DEFAULT_HOST, sizeof (host) - 1);
      strncpy (port, server_address + 1, sizeof (port) - 1);
    }
  }
  /* Otherwise if no separator, use default port */
  else if (separator == NULL)
  {
    strncpy (host, server_address, sizeof (host) - 1);
    strncpy (port, SL_DEFAULT_PORT, sizeof (port) - 1);
  }
  /* Otherwise separate host and port */
  else
  {
    size_t minlen = (separator - server_address);

    if (minlen > sizeof (host))
      minlen = sizeof (host) - 1;

    strncpy (host, server_address, minlen);

    /* Handle case of separator present but nothing following */
    if (strlen (separator + 1) > 0)
      strncpy (port, separator + 1, sizeof (port) - 1);
    else
      strncpy (port, SL_DEFAULT_PORT, sizeof (port) - 1);
  }

  /* Remove brackets from host if present, i.e. for raw IPv6 addresses */
  if (host[0] == '[' && host[strlen (host) - 1] == ']')
  {
    memmove (host, host + 1, strlen (host) - 2);
    host[strlen (host) - 2] = '\0';
  }

  /* Store the user-supplied address if not set directly */
  if (server_address != slconn->sladdr)
  {
    free (slconn->sladdr);
    slconn->sladdr = strdup (server_address);
  }

  free (slconn->slhost);
  free (slconn->slport);

  slconn->slhost = strdup (host);
  slconn->slport = strdup (port);

  if (slconn->sladdr == NULL ||
      slconn->slhost == NULL ||
      slconn->slport == NULL)
  {
    sl_log_r (NULL, 2, 0, "%s(): error allocating memory\n", __func__);
    return -1;
  }

  /* Set TLS flag if port is the TLS default */
  if (strcmp(slconn->slport, SL_SECURE_PORT) == 0)
  {
    sl_set_tlsmode (slconn, 1);
  }

  return 0;
} /* End of sl_set_serveraddress() */

/**********************************************************************/ /**
 * @brief Set SeedLink connection time window (begin and end times)
 *
 * Set the connection time window limits.  This will trigger the
 * connection to be negotiated with the \c TIME command for v3 connections
 * and a time range to be included with the \c DATA command of v4 connections.
 *
 * No validation of the time strings is done, so the user must ensure that
 * the target SeedLink server supports time string format supplied.  A
 * relatively safe format is <code>yyyy-mm-ddTHH:MM:SS</code>.
 *
 * @param slconn      SeedLink connection description
 * @param start_time  Starting time string in <code>yyyy-mm-ddTHH:MM:SS</code> format
 * @param end_time    Ending time string in <code>yyyy-mm-ddTHH:MM:SS</code> format
 *
 * @retval  0 : success
 * @retval -1 : error
 ***************************************************************************/
int
sl_set_timewindow (SLCD *slconn, const char *start_time, const char *end_time)
{
    if (!slconn || (!start_time && !end_time))
        return -1;

    free (slconn->start_time);
    free (slconn->end_time);
    slconn->start_time = NULL;
    slconn->end_time = NULL;

    if (start_time && (slconn->start_time = strdup (start_time)) == NULL)
    {
        sl_log_r (NULL, 2, 0, "%s(): error allocating memory\n", __func__);
        return -1;
    }

    if (end_time && (slconn->end_time = strdup (end_time)) == NULL)
    {
        sl_log_r (NULL, 2, 0, "%s(): error allocating memory\n", __func__);
        return -1;
    }

    return 0;
} /* End of sl_set_timewindow() */

/**********************************************************************/ /**
 * @brief Set SeedLink connection authentication parameters (v4 only)
 *
 * Set the callback functions and callback data used for authentication
 * of SeedLink connections.  This is only relevant for the v4 protocol.
 *
 * The \a auth_value callback is executed to retrieve the authentication
 * value to be sent to the server.  This value is transmitted with the
 * \c AUTH command for v4 connections.  The following values are specified
 * by the v4 protocol:
 * ```
 * USERPASS <username> <password>
 * ```
 * and
 * ```
 * JWT <token>
 * ```
 *
 * The \a auth_finish callback is executed when authentication is complete.
 * This can be used to free memory or perform other cleanup tasks.
 *
 * The \a auth_data parameter is a pointer to caller-supplied data that
 * is passed to the callback functions.
 *
 * There is no requirement that servers must support authentication, so
 * the user must ensure that the target server supports authentication.
 *
 * @param slconn        SeedLink connection description
 * @param auth_value    Callback executed to retrieve the authentication value
 * @param auth_finish   Callback executed when authentication is complete
 * @param auth_data     Caller-supplied data passed to the callback functions
 *
 * @retval  0 : success
 * @retval -1 : error
 ***************************************************************************/
int
sl_set_auth_params (SLCD *slconn,
                    const char *(*auth_value) (const char *server, void *auth_data),
                    void (*auth_finish) (const char *server, void *auth_data),
                    void *auth_data)
{
    if (!slconn)
        return -1;

    slconn->auth_value  = auth_value;
    slconn->auth_finish = auth_finish;
    slconn->auth_data   = auth_data;

    return 0;
} /* End of sl_set_auth_params() */

/**********************************************************************/ /**
 * @brief Set SeedLink connection keep alive interval in seconds
 *
 * Keep alive packets are sent to the server at the specified interval
 * when no data is being received to maintain the connection.
 *
 * By default, keep alive packets are disabled.
 *
 * @param slconn       SeedLink connection description
 * @param keepalive    Keep alive interval in seconds, 0 to disable
 *
 * @retval  0 : success
 * @retval -1 : error
 ***************************************************************************/
int
sl_set_keepalive (SLCD *slconn, int keepalive)
{
    if (!slconn)
        return -1;

    slconn->keepalive = keepalive;

    return 0;
} /* End of sl_set_keepalive() */

/**********************************************************************/ /**
 * @brief Set SeedLink connection I/O timeout in seconds
 *
 * Set the I/O timeout for the SeedLink connection.  This is the maximum
 * time allowed for a read or write operation to complete before the
 * connection is considered to be in a failed state and disconnected.
 *
 * By default, the I/O timeout is set to 60 seconds.
 *
 * @param slconn       SeedLink connection description
 * @param iotimeout    I/O timeout in seconds, 0 to disable
 *
 * @retval  0 : success
 * @retval -1 : error
 ***************************************************************************/
int
sl_set_iotimeout (SLCD *slconn, int iotimeout)
{
    if (!slconn)
        return -1;

    slconn->iotimeout = iotimeout;

    return 0;
} /* End of sl_set_iotimeout() */

/**********************************************************************/ /**
 * @brief Set SeedLink connection idle timeout in seconds
 *
 * Set the idle connection timeout.  This is the maximum time allowed
 * for a connection to be idle, after which it will be disconnected.
 *
 * By default, the network timeout is set to 600 seconds.
 *
 * @param slconn       SeedLink connection description
 * @param idletimeout  Network timeout in seconds, 0 to disable
 *
 * @retval  0 : success
 * @retval -1 : error
 ***************************************************************************/
int
sl_set_idletimeout (SLCD *slconn, int idletimeout)
{
    if (!slconn)
        return -1;

    slconn->netto = idletimeout;

    return 0;
} /* End of sl_set_idletimeout() */

/**********************************************************************/ /**
 * @brief Set SeedLink re-connection delay in seconds
 *
 * Set the re-connection delay.  This is the number of seconds to wait
 * before attempting to re-connect to the SeedLink server after a
 * connection has been lost.
 *
 * By default, the network delay is set to 30 seconds.
 *
 * @param slconn          SeedLink connection description
 * @param reconnectdelay  Network delay in seconds, 0 to disable
 *
 * @retval  0 : success
 * @retval -1 : error
 ***************************************************************************/
int
sl_set_reconnectdelay (SLCD *slconn, int reconnectdelay)
{
    if (!slconn)
        return -1;

    slconn->netdly = reconnectdelay;

    return 0;
} /* End of sl_set_reconnectdelay() */

/**********************************************************************/ /**
 * @brief Set or unset the SeedLink connection blocking mode
 *
 * Set the SeedLink connction to block or non-blocking mode.  In blocking
 * mode sl_collect() will block until data is received or the connection
 * is closed.  In non-blocking mode sl_collect() will return quickly.
 *
 * By default, the connection is set to blocking mode.
 *
 * @param slconn        SeedLink connection description
 * @param nonblock      Boolean flag, if non-zero set to non-blocking mode

 * @retval  0 : success
 * @retval -1 : error
 *
 * @sa sl_collect()
 ***************************************************************************/
int
sl_set_blockingmode (SLCD *slconn, int nonblock)
{
    if (!slconn)
        return -1;

    slconn->noblock = (nonblock) ? 1 : 0;

    return 0;
} /* End of sl_set_blockingmode() */

/**********************************************************************/ /**
 * @brief Set or unset the SeedLink connection dial-up mode
 *
 * Set the SeedLink connction to dial-up mode.  In dial-up mode the
 * connection will be closed after the last data packet available
 * from the server is transmitted.
 *
 * By default, the connection is set to remain open.
 *
 * @param slconn        SeedLink connection description
 * @param dialup        Boolean flag, if non-zero set to dial-up mode
 *
 * @retval  0 : success
 * @retval -1 : error
 ***************************************************************************/
int
sl_set_dialupmode (SLCD *slconn, int dialup)
{
    if (!slconn)
        return -1;

    slconn->dialup = (dialup) ? 1 : 0;

    return 0;
} /* End of sl_set_dialupmode() */

/**********************************************************************/ /**
 * @brief Set or unset the SeedLink connection batch mode (v3 only)
 *
 * Set the SeedLink connction to batch mode.  In batch mode the client
 * can send multiple commands to the server before waiting for a response.
 * The server will send the responses in the order the commands were received.
 *
 * By default, the connection is set to non-batch mode.
 *
 * @param slconn        SeedLink connection description
 * @param batchmode     Boolean flag, if non-zero set to batch mode
 *
 * @retval  0 : success
 * @retval -1 : error
 ***************************************************************************/
int
sl_set_batchmode (SLCD *slconn, int batchmode)
{
    if (!slconn)
        return -1;

    slconn->batchmode = (batchmode) ? 1 : 0;

    return 0;
} /* End of sl_set_batchmode() */

/**********************************************************************/ /**
 * @brief Enable or disable TLS for the SeedLink connection
 *
 * By default, TLS is enabled for port number 18500, and for all other
 * ports it is disabled.  This function can be used to expliclty enable
 * or disable TLS for the connection.
 *
 * This function must be run after sl_set_serveraddress() to disable TLS
 * on port 18500.
 *
 * @param slconn     SeedLink connection description
 * @param tlsmode    Boolean flag, if non-zero enable TLS
 *
 * @retval  0 : success
 * @retval -1 : error
 ***************************************************************************/
int
sl_set_tlsmode (SLCD *slconn, int tlsmode)
{
    if (!slconn)
        return -1;

    slconn->tls = (tlsmode) ? 1 : 0;

    return 0;
} /* End of sl_set_tlsmode() */

/**********************************************************************/ /**
 * sl_addstream:
 *
 * Add a new stream entry to the stream list for the given ::SLCD
 * struct.  No checking is done for duplicate streams.
 *
 * The use of this function will enable multi-station mode.
 *
 * The \a seqnum parameter should be the last received sequence number
 * for the stream to resume the connection from previous data transfer.
 * The \a seqnum can also be ::SL_UNSETSEQUENCE to start from the next
 * available data.  The \a seqnum be ::SL_ALLDATASEQUENCE to request
 * all available data from the server (v4 only).
 *
 * The stream list is sorted alphanumerically by station ID,
 * and partitioned by the presence of wildcard characters in the
 * station ID, starting with more specific entries first.
 *
 * @param[in] slconn     SeedLink connection description
 * @param[in] stationid  Station ID
 * @param[in] selectors  Selectors for the station ID, NULL if none
 * @param[in] seqnum     Last received sequence number or special value
 * @param[in] timestamp  Start time for the stream, NULL if not used
 *
 * @retval  0 : success
 * @retval -1 : error
 ***************************************************************************/
int
sl_add_stream (SLCD *slconn, const char *stationid,
               const char *selectors, uint64_t seqnum,
               const char *timestamp)
{
  SLstream *curstream;
  SLstream *newstream;
  SLstream *followstream = NULL;
  int newparitition = 0;
  int partition = 0;

  if (!slconn || !stationid)
    return -1;

  /* Sanity, check for a all-station mode entry */
  if (slconn->streams)
  {
    if (strcmp (slconn->streams->stationid, "*") == 0)
    {
      sl_log_r (slconn, 2, 0, "[%s] %s(): all-station mode already configured!\n",
                slconn->sladdr, __func__);
      return -1;
    }
  }

  newstream = (SLstream *)malloc (sizeof (SLstream));

  if (newstream == NULL)
  {
    sl_log_r (slconn, 2, 0, "%s(): error allocating memory\n", __func__);
    return -1;
  }

  strncpy (newstream->stationid, stationid, sizeof (newstream->stationid) - 1);

  if (selectors)
    newstream->selectors = strdup (selectors);
  else
    newstream->selectors = NULL;

  newstream->seqnum = seqnum;

  if (timestamp)
    strncpy (newstream->timestamp, timestamp, sizeof(newstream->timestamp) - 1);
  else
    newstream->timestamp[0] = '\0';

  /* Convert old comma-delimited date-time to ISO-compatible format if needed
   * Example: '2021,11,19,17,23,18' => '2021-11-18T17:23:18.0Z' */
  if (newstream->timestamp[0])
  {
    if (sl_isodatetime(newstream->timestamp, newstream->timestamp) == NULL)
    {
      sl_log_r (slconn, 2, 0, "%s(): could not convert timestamp for %s entry: '%s'\n",
                __func__, stationid, newstream->timestamp);
      return -1;
    }
  }

  /* Search the stream list to find the proper insertion point.
   * The resulting list is sorted alphanumerically and partitioned by:
   * 1) no-wildcards in NET_STA, followed by
   * 2) ? wildcards in NET_STA, followed by
   * 3) * wildcards in NET_STA. */
  newparitition = (strchr (stationid, '*')) ? 3 : (strchr (stationid, '?')) ? 2 : 1;
  curstream = slconn->streams;
  while (curstream)
  {
    /* Determine wildcard partition */
    partition = (strchr (curstream->stationid, '*')) ? 3 : (strchr (curstream->stationid, '?')) ? 2 : 1;

    /* Compare partitions */
    if (newparitition < partition)
    {
      break;
    }
    else if (newparitition > partition)
    {
      followstream = curstream;
      curstream = curstream->next;
      continue;
    }

    /* Compare alphanumerically */
    if (strcmp (curstream->stationid, stationid) > 0)
    {
      break;
    }

    followstream = curstream;
    curstream  = curstream->next;
  }

  /* Add new entry to the list */
  if (followstream)
  {
    newstream->next    = followstream->next;
    followstream->next = newstream;
  }
  else
  {
    newstream->next = slconn->streams;
    slconn->streams = newstream;
  }

  slconn->multistation = 1;

  return 0;
} /* End of sl_add_stream() */

/**********************************************************************/ /**
 * @brief Set the parameters for an all-station mode connection
 *
 * Set the parameters for all-station mode using a wildcard (*) for the
 * station ID.  If the stream entry already exists, overwrite the previous
 * settings.
 *
 * For SeedLink v3 this is "uni-station" mode.
 *
 * Also set the multistation flag to false (0).
 *
 * @param[in] slconn     SeedLink connection description
 * @param[in] selectors  Selectors for the station ID, NULL if none
 * @param[in] seqnum     Last received sequence number or ::SL_UNSETSEQUENCE
 * @param[in] timestamp  Start time for the stream, NULL if not used
 *
 * @retval  0 : success
 * @retval -1 : error
 ***************************************************************************/
int
sl_set_allstation_params (SLCD *slconn, const char *selectors,
                          uint64_t seqnum, const char *timestamp)
{
  SLstream *newstream;

  if (!slconn)
    return -1;

  newstream = slconn->streams;

  if (newstream == NULL)
  {
    newstream = (SLstream *)malloc (sizeof (SLstream));

    if (newstream == NULL)
    {
      sl_log_r (slconn, 2, 0, "%s(): error allocating memory\n", __func__);
      return -1;
    }
  }
  else if (strcmp (newstream->stationid, "*") != 0)
  {
    sl_log_r (slconn, 2, 0, "[%s] %s(): multi-station mode already configured!\n",
              slconn->sladdr, __func__);
    return -1;
  }

  /* Set the station ID to an all-matching, single wildcard */
  strncpy (newstream->stationid, "*", sizeof (newstream->stationid));

  if (selectors)
    newstream->selectors = strdup (selectors);
  else
    newstream->selectors = NULL;

  newstream->seqnum = seqnum;

  if (timestamp)
    strncpy (newstream->timestamp, timestamp, sizeof (newstream->timestamp) - 1);
  else
    newstream->timestamp[0] = '\0';

  /* Convert old comma-delimited date-time to ISO-compatible format if needed
   * Example: '2021,11,19,17,23,18' => '2021-11-18T17:23:18.0Z' */
  if (newstream->timestamp[0])
  {
    if (sl_isodatetime(newstream->timestamp, newstream->timestamp) == NULL)
    {
      sl_log_r (slconn, 2, 0, "%s(): could not convert timestamp for all-station mode: '%s'\n",
                __func__, newstream->timestamp);
      free (newstream);
      return -1;
    }
  }

  newstream->next = NULL;

  slconn->streams = newstream;

  slconn->multistation = 0;

  return 0;
} /* End of sl_set_allstation_params() */

/**********************************************************************/ /**
 * @brief Submit an INFO request to the server at the next opportunity
 *
 * Add an INFO request to the SeedLink Connection Description.
 *
 * @param[in] slconn     SeedLink connection description
 * @param[in] infostr    INFO level to request
 *
 * @retval  0 : success
 * @retval -1 : error
 ***************************************************************************/
int
sl_request_info (SLCD *slconn, const char *infostr)
{
  if (slconn->info != NULL)
  {
    sl_log_r (slconn, 2, 0, "[%s] Cannot request INFO '%.20s', another is pending\n",
              slconn->sladdr, infostr);
    return -1;
  }
  else
  {
    slconn->info = strdup(infostr);
    return 0;
  }
} /* End of sl_request_info() */

/**********************************************************************/ /**
 * @brief Check if server capabilities include specified value
 *
 * The server capabilities returned during connection negotiation are
 * searched for matches to the specified \a capability.
 *
 * NOTE: Only the capabilities listed in the response to the \a HELLO
 * command are available for checking.  Full server capabilities are
 * available with a \a INFO request.
 *
 * @param[in] slconn     SeedLink connection description
 * @param[in] capability Capabilty string to search for (case sensitive)
 *
 * @retval 0 Capability is not supported or unknown
 * @retval >0 Capability is supported
 ***************************************************************************/
int
sl_hascapability (SLCD *slconn, char *capability)
{
  int length;
  int start;
  int idx;

  if (!slconn || !capability)
    return 0;

  if (!slconn->capabilities)
    return 0;

  length = strlen (slconn->capabilities);
  /* Create capabilities array if needed */
  if (slconn->caparray == NULL)
  {
    /* Copy and replace spaces with terminating NULLs */
    slconn->caparray = strdup(slconn->capabilities);

    for (idx = 0; idx < length; idx++)
    {
      if (slconn->caparray[idx] == ' ')
        slconn->caparray[idx] = '\0';
    }
  }

  /* Search capabilities array for a matching entry */
  for (idx = 0, start = -1; idx < length; idx++)
  {
    /* Determine if at the start of a capability flag:
       either initial state or following a terminating NULL */
    if (slconn->caparray[idx] == '\0')
      start = -1;
    else if (start == -1)
      start = 1;
    else
      start = 0;

    if (start == 1 && strcmp (slconn->caparray + idx, capability) == 0)
      return 1;
  }

  return 0;
} /* End of sl_hascapablity() */

/**********************************************************************/ /**
 * @brief Trigger a termination of the SeedLink connection
 *
 * Set the terminate flag in the SLCD, which will cause the
 * connection to be terminated at the next opportunity.
 *
 * @param[in] slconn     SeedLink connection description
 ***************************************************************************/
void
sl_terminate (SLCD *slconn)
{
  sl_log_r (slconn, 1, 1, "[%s] Terminating connection\n", slconn->sladdr);

  slconn->terminate = 1;
} /* End of sl_terminate() */

/* Internal termination routine for use as a signal handler */
static void
internal_term_handler (int sig)
{
  (void)sig;
  sl_terminate (global_termination_SLCD);
}

/**********************************************************************/ /**
 * @brief Set signal handlers that trigger connection shutdown.
 *
 * @warning This function is not thread safe due to use of static variables.
 *
 * This routine will set the signal handlers for `SIGINT` and `SIGTERM`
 * that trigger connection shutdown for the specified SLCD.  On Windows
 * the `SIGABRT` signal is also set.  On all other platforms the
 * `SIGQUIT` signal is also set.
 *
 * @return 0 on success and -1 on error.
 ***************************************************************************/
int
sl_set_termination_handler (SLCD *slconn)
{
  if (slconn == NULL)
    return -1;

  global_termination_SLCD = slconn;

#if defined(SLP_WIN)
  signal(SIGINT, internal_term_handler);
  signal(SIGTERM, internal_term_handler);
  signal(SIGABRT, internal_term_handler);
#else
  struct sigaction sa;

  sigemptyset (&sa.sa_mask);
  sa.sa_flags = SA_RESTART;

  sa.sa_handler = internal_term_handler;
  sigaction (SIGINT, &sa, NULL);
  sigaction (SIGTERM, &sa, NULL);
  sigaction (SIGQUIT, &sa, NULL);
#endif

  return 0;
} /* End of sl_set_termination_handler() */

/**********************************************************************/ /**
 * @brief Print user parameters of the SeedLink connection description
 *
 * Useful for diagnostic purposes, this routine will print the
 * details of the SeedLink connection description to the logging
 * facility.
 *
 * @param[in] slconn     SeedLink connection description
 ***************************************************************************/
void
sl_printslcd (SLCD *slconn)
{
  SLstream *curstream;
  char sequence[32];

  if (!slconn)
    return;

  sl_log_r (slconn, 0, 0, "SeedLink connection description:\n");
  sl_log_r (slconn, 0, 0, "             Address: %s\n", slconn->sladdr ? slconn->sladdr : "NULL");
  sl_log_r (slconn, 0, 0, "                Host: %s\n", slconn->slhost ? slconn->slhost : "NULL");
  sl_log_r (slconn, 0, 0, "                Port: %s\n", slconn->slport ? slconn->slport : "NULL");
  sl_log_r (slconn, 0, 0, "         Client name: %s\n", slconn->clientname ? slconn->clientname : "NULL");
  sl_log_r (slconn, 0, 0, "      Client version: %s\n", slconn->clientversion ? slconn->clientversion : "NULL");
  sl_log_r (slconn, 0, 0, "          Start time: %s\n", slconn->start_time ? slconn->start_time : "NULL");
  sl_log_r (slconn, 0, 0, "            End time: %s\n", slconn->end_time ? slconn->end_time : "NULL");
  sl_log_r (slconn, 0, 0, "          Keep alive: %d seconds\n", slconn->keepalive);
  sl_log_r (slconn, 0, 0, "         I/O timeout: %d seconds\n", slconn->iotimeout);
  sl_log_r (slconn, 0, 0, "        Idle timeout: %d seconds\n", slconn->netto);
  sl_log_r (slconn, 0, 0, "     Reconnect delay: %d seconds\n", slconn->netdly);
  sl_log_r (slconn, 0, 0, "        auth_value(): %p\n", slconn->auth_value);
  sl_log_r (slconn, 0, 0, "       auth_finish(): %p\n", slconn->auth_finish);
  sl_log_r (slconn, 0, 0, "           auth_data: %p\n", slconn->auth_data);
  sl_log_r (slconn, 0, 0, "   Non-blocking mode: %d\n", slconn->noblock);
  sl_log_r (slconn, 0, 0, "        Dial-up mode: %d\n", slconn->dialup);
  sl_log_r (slconn, 0, 0, "          Batch mode: %d\n", slconn->batchmode);
  sl_log_r (slconn, 0, 0, "Use last packet time: %d\n", slconn->lastpkttime);
  sl_log_r (slconn, 0, 0, "           Terminate: %d\n", slconn->terminate);
  sl_log_r (slconn, 0, 0, "Resume with sequence: %d\n", slconn->resume);
  sl_log_r (slconn, 0, 0, "  Multi-station mode: %d\n", slconn->multistation);
  sl_log_r (slconn, 0, 0, "        INFO request: %s\n", slconn->info ? slconn->info : "NULL");
  sl_log_r (slconn, 0, 0, "         Stream list:\n");
  curstream = slconn->streams;
  while (curstream)
  {
    if (curstream->seqnum == SL_UNSETSEQUENCE)
      strcpy (sequence, "UNSET");
    else if (curstream->seqnum == SL_ALLDATASEQUENCE)
      strcpy (sequence, "ALLDATA");
    else
      snprintf (sequence, sizeof(sequence), "%" PRIu64, curstream->seqnum);

    sl_log_r (slconn, 0, 0, "             Station ID: %s\n", curstream->stationid);
    sl_log_r (slconn, 0, 0, "                  Selectors: %s\n", curstream->selectors ? curstream->selectors : "NULL");
    sl_log_r (slconn, 0, 0, "                   Sequence: %s\n", sequence);
    sl_log_r (slconn, 0, 0, "                 Time stamp: %s\n", curstream->timestamp);
    curstream = curstream->next;
  }
} /* End of sl_printslcd() */

/**********************************************************************/ /**
 * @brief Detect miniSEED record in buffer
 *
 * Determine if the buffer contains a miniSEED data record by
 * verifying known signatures (fields with known limited values).
 *
 * At least SL_MIN_PAYLOAD bytes of data are required for detection.
 *
 * If miniSEED 2.x is detected, search the record up to recbuflen
 * bytes for a 1000 blockette. If no blockette 1000 is found, search
 * at 64-byte offsets for the fixed section of the next header,
 * thereby implying the record length.
 *
 * @param[in] buffer Buffer to test for known data types
 * @param[in] buflen Length of buffer
 * @param[out] payloadformat Payload type detected
 *
 * @retval -1 Data record not detected or error
 * @retval 0 Data record detected but could not determine length
 * @retval >0 Size of the record in bytes
 ***************************************************************************/
static int64_t
detect (const char *buffer, uint64_t buflen, char *payloadformat)
{
  uint8_t swapflag = 0; /* Byte swapping flag */
  int64_t reclen = -1;  /* Size of record in bytes */

  uint16_t blkt_offset; /* Byte offset for next blockette */
  uint16_t blkt_type;
  uint16_t next_blkt;
  const char *nextfsdh;

  if (!buffer || !payloadformat)
    return -1;

  if (buflen < SL_MIN_PAYLOAD)
    return -1;

  /* Check for valid header, set format version */
  *payloadformat = SLPAYLOAD_UNKNOWN;
  if (MS3_ISVALIDHEADER (buffer))
  {
    *payloadformat = SLPAYLOAD_MSEED3;

    if (!sl_littleendianhost ())
      swapflag = 1;

    uint16_t extralength = HO2u(*pMS3FSDH_EXTRALENGTH (buffer), swapflag);
    uint32_t datalength = HO2u(*pMS3FSDH_DATALENGTH (buffer), swapflag);

    reclen = MS3FSDH_LENGTH                 /* Length of fixed portion of header */
             + *pMS3FSDH_SIDLENGTH (buffer) /* Length of source identifier */
             + extralength                  /* Length of extra headers */
             + datalength;                  /* Length of data payload */
  }
  else if (MS2_ISVALIDHEADER (buffer))
  {
    *payloadformat = SLPAYLOAD_MSEED2;
    reclen = 0;

    /* Check to see if byte swapping is needed by checking for sane year and day */
    if (!MS_ISVALIDYEARDAY (*pMS2FSDH_YEAR(buffer), *pMS2FSDH_DAY(buffer)))
      swapflag = 1;

    blkt_offset = HO2u(*pMS2FSDH_BLOCKETTEOFFSET (buffer), swapflag);

    /* Loop through blockettes as long as number is non-zero and viable */
    while (blkt_offset != 0 &&
           blkt_offset > 47 &&
           blkt_offset <= buflen)
    {
      memcpy (&blkt_type, buffer + blkt_offset, 2);
      memcpy (&next_blkt, buffer + blkt_offset + 2, 2);

      if (swapflag)
      {
        sl_gswap2 (&blkt_type);
        sl_gswap2 (&next_blkt);
      }

      /* Found a 1000 blockette, not truncated */
      if (blkt_type == 1000 &&
          (blkt_offset + 8) <= buflen)
      {
        /* Field 3 of B1000 is a uint8_t value describing the buffer
         * length as 2^(value).  Calculate 2-raised with a shift. */
        reclen = (unsigned int)1 << *pMS2B1000_RECLEN(buffer+blkt_offset);

        break;
      }

      /* Safety check for invalid offset */
      if (next_blkt != 0 && (next_blkt < 4 || (next_blkt - 4) <= blkt_offset))
      {
        sl_log (2, 0, "Invalid miniSEED2 blockette offset (%d) less than or equal to current offset (%d)\n",
                next_blkt, blkt_offset);
        return -1;
      }

      blkt_offset = next_blkt;
    }

    /* If record length was not determined by a 1000 blockette scan the buffer
     * and search for the next record header. */
    if (reclen == -1)
    {
      nextfsdh = buffer + 64;

      /* Check for record header or blank/noise record at 64-byte offsets */
      while ((size_t)((nextfsdh - buffer) + 48) < buflen)
      {
        if (MS2_ISVALIDHEADER (nextfsdh))
        {
          reclen = nextfsdh - buffer;

          break;
        }

        nextfsdh += 64;
      }
    }
  } /* End of miniSEED 2.x detection */

  return reclen;
} /* End of detect() */
