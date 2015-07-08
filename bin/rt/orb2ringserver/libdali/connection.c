/***********************************************************************//**
 * @file connection.c
 *
 * Routines for managing a connection with a DataLink server.
 *
 * @author Chad Trabant, IRIS Data Management Center
 *
 * modified: 2013.210
 ***************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

#include "libdali.h"
#include "portable.h"

/***********************************************************************//**
 * @brief Create a new DataLink Connection Parameter (DLCP) structure
 *
 * Allocate, initialze and return a pointer to a new DLCP struct.
 *
 * @param address Address of DataLink server in "host:port" format
 * @param progname Name of program, usually argv[0]
 *
 * @return allocated DLCP struct on success, NULL on error.
 ***************************************************************************/
DLCP *
dl_newdlcp (char *address, char *progname)
{
  DLCP *dlconn;
  
  dlconn = (DLCP *) malloc (sizeof(DLCP));
  
  if ( dlconn == NULL )
    {
      dl_log_r (NULL, 2, 0, "dl_newdlcp(): error allocating memory\n");
      return NULL;
    }
  
  /* Set defaults */
  strncpy (dlconn->addr, address, sizeof(dlconn->addr));
  dlp_genclientid (progname, dlconn->clientid, sizeof(dlconn->clientid));
  dlconn->keepalive    = 600;
  dlconn->iotimeout    = 60;
  dlconn->link         = -1;
  dlconn->serverproto  = 0.0;
  dlconn->maxpktsize   = 0;
  dlconn->writeperm    = 0;
  dlconn->pktid        = 0;
  dlconn->pkttime      = 0;
  dlconn->keepalive_trig = -1;
  dlconn->keepalive_time = 0.0;
  dlconn->terminate    = 0;
  dlconn->streaming    = 0;
  
  dlconn->log = NULL;
  
  return dlconn;
}  /* End of dl_newdlcp() */


/***********************************************************************//**
 * @brief Free a DataLink Connection Parameter (DLCP) structure
 *
 * Free all memory associated with a DLCP struct.
 *
 * @param dlconn DLCP to free
 ***************************************************************************/
void
dl_freedlcp (DLCP *dlconn)
{
  if ( dlconn->log )
    free (dlconn->log);
  
  free (dlconn);
}  /* End of dl_freedlcp() */


/***********************************************************************//**
 * @brief Send the ID command to the DataLink server and parse response
 *
 * Send the ID command including the client ID and optionally parse
 * the capability flags from the server response.  This routine is
 * always called when a connection is first made using dl_connect()
 * and shouldn't normally need to be called again.
 *
 * @param dlconn DataLink Connection Parameters
 * @param parseresp Flag to control parsing of server response.
 *
 * @return -1 on errors, 0 on success.
 ***************************************************************************/
int
dl_exchangeIDs (DLCP *dlconn, int parseresp)
{
  char sendstr[255];		/* Buffer for command strings */
  char respstr[255];		/* Buffer for server response */  
  char *capptr;                 /* Pointer to capabilities flags */  
  int respsize;
  int ret = 0;

  if ( ! dlconn )
    return -1;
  
  /* Sanity check that connection is not in streaming mode */
  if ( dlconn->streaming )
    {
      dl_log_r (dlconn, 1, 1, "[%s] dl_exchangeIDs(): Connection in streaming mode, cannot continue\n",
		dlconn->addr);
      return -1;
    }
  
  /* Send ID command including client ID */
  snprintf (sendstr, sizeof(sendstr), "ID %s",
	    (dlconn->clientid) ? dlconn->clientid : "");
  dl_log_r (dlconn, 1, 2, "[%s] sending: %s\n", dlconn->addr, sendstr);
  
  respsize = dl_sendpacket (dlconn, sendstr, strlen (sendstr), NULL, 0,
			    respstr, sizeof(respstr));
  
  /* Check for errors */
  if ( respsize < 0 )
    {
      return -1;
    }
  
  /* Check minimum server ID response size */
  if ( respsize < 11 )
    {
      dl_log_r (dlconn, 1, 2, "[%s] Server ID response too short: %d\n",
		dlconn->addr, respstr);
      return -1;
    }
  
  /* Make sure the response string is terminated */
  respstr[respsize] = '\0';
  
  /* Verify DataLink signature in server response */
  if ( strncasecmp (respstr, "ID DATALINK", 11) )
    {
      dl_log_r (dlconn, 1, 1,
                "[%s] dl_exchangeIDs(): Unrecognized server ID: %11.11s\n",
                dlconn->addr, respstr);
      return -1;
    }
  
  /* Parse the response from the server if requested */
  if ( parseresp )
    {
      /* Search for capabilities flags in server ID by looking for "::"
       * The expected format of the complete server ID is:
       * "ID DataLink <optional text> <:: optional capability flags>"
       */
      capptr = strstr (respstr, "::");
      if ( capptr )
	{
	  /* Truncate server ID portion of string */
	  *capptr = '\0';
	  
	  /* Move pointer to beginning of flags */
	  capptr += 2;
	  
	  /* Move capptr up to first non-space character */
	  while ( *capptr == ' ' )
	    capptr++;
	}
      
      /* Report received server ID (without the initial "ID ") */
      dl_log_r (dlconn, 1, 1, "[%s] connected to: %s\n", dlconn->addr, respstr+3);
      if ( capptr )
	dl_log_r (dlconn, 1, 1, "[%s] capabilities: %s\n", dlconn->addr, capptr);
      
      /* Check capabilities flags */
      if ( capptr )
	{
	  char *tptr;
	  
	  /* Parse protocol version flag: "DLPROTO:<#.#>" if present */
	  if ( (tptr = strstr(capptr, "DLPROTO")) )
	    {
	      /* Parse protocol version as a float */
	      ret = sscanf (tptr, "DLPROTO:%f", &dlconn->serverproto);
	      
	      if ( ret != 1 )
		dl_log_r (dlconn, 1, 1,
			  "[%s] dl_exchangeIDs(): could not parse protocol version from DLPROTO flag: %s\n",
			  dlconn->addr, tptr);
	    }
	  
	  /* Parse server packet size flag: "PACKETSIZE:<#>" if present */
	  if ( (tptr = strstr(capptr, "PACKETSIZE")) )
	    {
	      /* Parse protocol version as an integer */
	      ret = sscanf (tptr, "PACKETSIZE:%d", &dlconn->maxpktsize);
	      
	      if ( ret != 1 )
		dl_log_r (dlconn, 1, 1,
			  "[%s] dl_exchangeIDs(): could not parse packet size from PACKETSIZE flag: %s\n",
			  dlconn->addr, tptr);
	    }
	  
	  /* Search for write permission flag */
	  if ( (tptr = strstr(capptr, "WRITE")) )
	    {
	      dlconn->writeperm = 1;
	    }
	}
    }
  
  return 0;
}  /* End of dl_exchangeIDs() */


/***********************************************************************//**
 * @brief Position the client read position
 *
 * Set the client read position to a specified packet ID and packet
 * time.  A packet ID and time together uniquely identify a packet in
 * a DataLink server.  The packet time must match the packet ID
 * currently in the server's buffer or the positioning request will
 * fail.
 *
 * As a special case @a pktid maybe be set to @a LIBDALI_POSITION_EARLIEST
 * or @a LIBDALI_POSITION_LATEST to set the client read position to
 * the earliest or latest packet.  In both cases the @a pkttime value
 * is ignored.
 *
 * @param dlconn DataLink Connection Parameters
 * @param pktid Packet ID to set position to
 * @param pkttime Packet time cooresponding to @a pktid
 *
 * @return A positive packet ID on success, 0 when packet is not found
 * and -1 on error.
 ***************************************************************************/
int64_t
dl_position (DLCP *dlconn, int64_t pktid, dltime_t pkttime)
{
  int64_t replyvalue = 0;
  char reply[255];
  char header[255];
  int headerlen;
  int replylen;
  int rv;
  
  if ( ! dlconn )
    return -1;
  
  if ( dlconn->link < 0 )
    return -1;
  
  if ( pktid < 0 && pktid != LIBDALI_POSITION_EARLIEST && pktid != LIBDALI_POSITION_LATEST )
    return -1;
  
  /* Sanity check that connection is not in streaming mode */
  if ( dlconn->streaming )
    {
      dl_log_r (dlconn, 1, 1, "[%s] dl_position(): Connection in streaming mode, cannot continue\n",
		dlconn->addr);
      return -1;
    }
  
  /* When positioning to earliest or latest packet in the ring, ignore pkttime */
  if ( pktid == LIBDALI_POSITION_EARLIEST )
    {
      /* Create packet header with command: "POSITION SET EARLIEST" */
      headerlen = snprintf (header, sizeof(header), "POSITION SET EARLIEST");
    }
  else if ( pktid == LIBDALI_POSITION_LATEST )
    {
      /* Create packet header with command: "POSITION SET LATEST" */
      headerlen = snprintf (header, sizeof(header), "POSITION SET LATEST");
    }
  else 
    {
      /* Create packet header with command: "POSITION SET pktid pkttime" */
      headerlen = snprintf (header, sizeof(header), "POSITION SET %lld %lld",
			    (long long int)pktid, (long long int)pkttime);
    }
  
  /* Send command to server */
  replylen = dl_sendpacket (dlconn, header, headerlen, NULL, 0,
			    reply, sizeof(reply));
  
  if ( replylen <= 0 )
    {
      dl_log_r (dlconn, 2, 0, "[%s] dl_position(): problem sending POSITION command\n",
		dlconn->addr);
      return -1;
    }
  
  /* Reply message, if sent, will be placed into the reply buffer */
  rv = dl_handlereply (dlconn, reply, sizeof(reply), &replyvalue);
  
  /* Log server reply message */
  if ( rv >= 0 )
    dl_log_r (dlconn, 1, 1, "[%s] %s\n", dlconn->addr, reply);

  return ( rv < 0 || replyvalue < 0 ) ? -1 : replyvalue;
}  /* End of dl_position() */


/***********************************************************************//**
 * @brief Position the client read position based on data time
 *
 * Set the client read position to the first packet with a data end
 * time after a reference @a datatime.  The reference time must be
 * specified as a dltime_t value, see dl_time2dltime() and friends to
 * generate these time values.
 *
 * @param dlconn DataLink Connection Parameters
 * @param datatime Reference data time as a dltime_t value
 *
 * @return A positive packet ID on success and -1 on error.
 ***************************************************************************/
int64_t
dl_position_after (DLCP *dlconn, dltime_t datatime)
{
  int64_t replyvalue = 0;
  char reply[255];
  char header[255];
  int headerlen;
  int replylen;
  int rv;
  
  if ( ! dlconn )
    return -1;
  
  if ( dlconn->link < 0 )
    return -1;
  
  /* Sanity check that connection is not in streaming mode */
  if ( dlconn->streaming )
    {
      dl_log_r (dlconn, 1, 1, "[%s] dl_position_after(): Connection in streaming mode, cannot continue\n",
		dlconn->addr);
      return -1;
    }
  
  /* Create packet header with command: "POSITION AFTER datatime" */
  headerlen = snprintf (header, sizeof(header), "POSITION AFTER %lld",
			(long long int)datatime);
  
  /* Send command to server */
  replylen = dl_sendpacket (dlconn, header, headerlen, NULL, 0,
			    reply, sizeof(reply));
  
  if ( replylen <= 0 )
    {
      dl_log_r (dlconn, 2, 0, "[%s] dl_position_after(): problem sending POSITION command\n",
		dlconn->addr);
      return -1;
    }
  
  /* Reply message, if sent, will be placed into the reply buffer */
  rv = dl_handlereply (dlconn, reply, sizeof(reply), &replyvalue);
  
  /* Log server reply message */
  if ( rv >= 0 )
    dl_log_r (dlconn, 1, 1, "[%s] %s\n", dlconn->addr, reply);
  
  return ( rv < 0 ) ? -1 : replyvalue;
}  /* End of dl_position_after() */


/***********************************************************************//**
 * @brief Set the packet match parameters for a connection
 *
 * Send new match pattern to server or reset matching.  If @a
 * matchpattern is NULL a zero length pattern command is sent to the
 * server which resets the client matching setting.
 *
 * The packet match pattern limits which packets are sent to the
 * client in streaming mode, this is the mode used for dl_collect()
 * and dl_collect_nb() requests.
 *
 * @param dlconn DataLink Connection Parameters
 * @param matchpattern Match regular expression
 *
 * @return the count of currently matched streams on success and -1
 * on error.
 ***************************************************************************/
int64_t
dl_match (DLCP *dlconn, char *matchpattern)
{
  int64_t replyvalue = 0;
  char reply[255];
  char header[255];
  long int patternlen;
  int headerlen;
  int replylen;
  int rv;
  
  if ( ! dlconn )
    return -1;
  
  if ( dlconn->link < 0 )
    return -1;
  
  /* Sanity check that connection is not in streaming mode */
  if ( dlconn->streaming )
    {
      dl_log_r (dlconn, 1, 1, "[%s] dl_match(): Connection in streaming mode, cannot continue\n",
		dlconn->addr);
      return -1;
    }
  
  patternlen = ( matchpattern ) ? strlen(matchpattern) : 0;
  
  /* Create packet header with command: "MATCH size" */
  headerlen = snprintf (header, sizeof(header), "MATCH %ld",
			patternlen);
  
  /* Send command and pattern to server */
  replylen = dl_sendpacket (dlconn, header, headerlen,
			    matchpattern, patternlen,
			    reply, sizeof(reply));
  
  if ( replylen <= 0 )
    {
      dl_log_r (dlconn, 2, 0, "[%s] dl_match(): problem sending MATCH command\n",
		dlconn->addr);
      return -1;
    }
  
  /* Reply message, if sent, will be placed into the reply buffer */
  rv = dl_handlereply (dlconn, reply, sizeof(reply), &replyvalue);
 
  /* Log server reply message */
  if ( rv >= 0 )
    dl_log_r (dlconn, 1, 1, "[%s] %s\n", dlconn->addr, reply);
  
  return ( rv < 0 ) ? -1 : replyvalue;
}  /* End of dl_match() */


/***********************************************************************//**
 * @brief Set the packet reject parameters for a connection
 *
 * Send new reject pattern to server or reset rejecting.  If @a
 * rejectpattern is NULL a zero length pattern command is sent to the
 * server which resets the client rejecting setting.
 *
 * The packet reject pattern limits which packets are sent to the
 * client in streaming mode, this is the mode used for dl_collect()
 * and dl_collect_nb() requests.
 *
 * @param dlconn DataLink Connection Parameters
 * @param rejectpattern Reject regular expression
 *
 * @return the count of currently rejected streams on success and -1
 * on error.
 ***************************************************************************/
int64_t
dl_reject (DLCP *dlconn, char *rejectpattern)
{
  int64_t replyvalue = 0;
  char reply[255];
  char header[255];
  long int patternlen;
  int headerlen;
  int replylen;
  int rv;
  
  if ( ! dlconn )
    return -1;
  
  if ( dlconn->link < 0 )
    return -1;
  
  /* Sanity check that connection is not in streaming mode */
  if ( dlconn->streaming )
    {
      dl_log_r (dlconn, 1, 1, "[%s] dl_reject(): Connection in streaming mode, cannot continue\n",
		dlconn->addr);
      return -1;
    }

  patternlen = ( rejectpattern ) ? strlen(rejectpattern) : 0;
  
  /* Create packet header with command: "REJECT size" */
  headerlen = snprintf (header, sizeof(header), "REJECT %ld",
			patternlen);
  
  /* Send command and pattern to server */
  replylen = dl_sendpacket (dlconn, header, headerlen,
			    rejectpattern, patternlen,
			    reply, sizeof(reply));
  
  if ( replylen <= 0 )
    {
      dl_log_r (dlconn, 2, 0, "[%s] dl_reject(): problem sending REJECT command\n",
		dlconn->addr);
      return -1;
    }
  
  /* Reply message, if sent, will be placed into the reply buffer */
  rv = dl_handlereply (dlconn, reply, sizeof(reply), &replyvalue);
  
  /* Log server reply message */
  if ( rv >= 0 )
    dl_log_r (dlconn, 1, 1, "[%s] %s\n", dlconn->addr, reply);
  
  return ( rv < 0 ) ? -1 : replyvalue;
}  /* End of dl_reject() */


/***********************************************************************//**
 * @brief Send a packet to the DataLink server
 *
 * Send a packet to the server and optionally request and process an
 * acknowledgement from the server.  An appropriate DataLink packet
 * header is created from the supplied parameters and sent with the
 * packet data.
 *
 * When an acknowledgement from the server has been requested this
 * routine will receive the response from the server and parse it, a
 * successful acknowledgement is indicated by the return value.
 *
 * @param dlconn DataLink Connection Parameters
 * @param packet Packet data buffer to send
 * @param packetlen Length of data in bytes to send from @a packet
 * @param streamid Stream ID of packet
 * @param datastart Data start time for packet
 * @param dataend Data end time for packet
 * @param ack Acknowledgement flag, if true request acknowledgement
 *
 * @return -1 on error and 0 on success when no acknowledgement is
 * requested and a positive packet ID on success when acknowledgement
 * is requested.
 ***************************************************************************/
int64_t
dl_write (DLCP *dlconn, void *packet, int packetlen, char *streamid,
	  dltime_t datastart, dltime_t dataend, int ack)
{
  int64_t replyvalue = 0;
  char reply[255];
  char header[255];
  char *flags = ( ack ) ? "A" : "N";
  int headerlen;
  int replylen;
  int rv;
  
  if ( ! dlconn || ! packet || ! streamid )
    {
      dl_log_r (dlconn, 1, 1, "dl_write(): dlconn || packet || streamid is not anticipated value \n");
      return -1;
    }
  
  if ( dlconn->link < 0 )
    {
      dl_log_r (dlconn, 1, 3, "[%s] dl_write(): dlconn->link = %d, expect >=0 \n", dlconn->addr, dlconn->link);
      return -1;
    }
  
  /* Sanity check that connection is not in streaming mode */
  if ( dlconn->streaming )
    {
      dl_log_r (dlconn, 1, 1, "[%s] dl_write(): Connection in streaming mode, cannot continue\n",
		dlconn->addr);
      return -1;
    }
  
  /* Sanity check that packet data is not larger than max packet size if known */
  if ( dlconn->maxpktsize > 0 && packetlen > dlconn->maxpktsize )
    {
      dl_log_r (dlconn, 1, 1, "[%s] dl_write(): Packet length (%d) greater than max packet size (%d)\n",
		dlconn->addr, packetlen, dlconn->maxpktsize);
      return -1;
    }
  
  /* Create packet header with command: "WRITE streamid hpdatastart hpdataend flags size" */
  headerlen = snprintf (header, sizeof(header),
			"WRITE %s %lld %lld %s %d",
			streamid, (long long int)datastart, (long long int)dataend,
			flags, packetlen);
  
  /* Send command and packet to server */
  replylen = dl_sendpacket (dlconn, header, headerlen,
			    packet, packetlen,
			    (ack)?reply:NULL, (ack)?sizeof(reply):0);
  
  if ( replylen < 0 )
    {
      dl_log_r (dlconn, 2, 0, "[%s] dl_write(): problem sending WRITE command\n",
		dlconn->addr);
      return -1;
    }
  else if ( replylen > 0 )
    {
      /* Reply message, if sent, will be placed into the reply buffer */
      rv = dl_handlereply (dlconn, reply, sizeof(reply), &replyvalue);
      
      /* Log server reply message */
      if ( rv == 0 )
	{
	  dl_log_r (dlconn, 1, 3, "[%s] %s\n", dlconn->addr, reply);
	}
      else if ( rv == 1 )
	{
	  dl_log_r (dlconn, 1, 0, "[%s] %s\n", dlconn->addr, reply);
	  replyvalue = -1;
	}
      else
	{
	  replyvalue = -1;
	}
    }
  
  return replyvalue;
}  /* End of dl_write() */


/***********************************************************************//**
 * @brief Request a packet from the DataLink server
 *
 * Request a specific packet from the server.
 *
 * A maximum of @a maxdatasize will be written to @a packetdata.  If
 * the packet data is larger than this maximum size an error will be
 * logged and 0 will be returned; the packet data will be recv'd and
 * discarded in order to leave the connection in a usable state.
 *
 * If this routine returns -1 the connection should be considered to
 * be in a bad state and should be shut down.
 *
 * @param dlconn DataLink Connection Parameters
 * @param pktid Packet ID to request
 * @param packet Pointer to a DLPacket struct for the received packet header information
 * @param packetdata Pointer to a buffer for received packet data
 * @param maxdatasize Maximum data size to write to @a packetdata
 *
 * @return Number of bytes of packet data received on success and -1
 * on error.
 ***************************************************************************/
int
dl_read (DLCP *dlconn, int64_t pktid, DLPacket *packet, void *packetdata,
	 size_t maxdatasize)
{
  char header[255];
  int headerlen;
  int rv = 0;
  
  long long int spktid;
  long long int spkttime;
  long long int sdatastart;
  long long int sdataend;
  long long int sdatasize;
  
  if ( ! dlconn || ! packet || ! packetdata )
    return -1;
  
  if ( dlconn->link < 0 )
    return -1;
  
  /* Sanity check that connection is not in streaming mode */
  if ( dlconn->streaming )
    {
      dl_log_r (dlconn, 1, 1, "[%s] dl_read(): Connection in streaming mode, cannot continue\n",
		dlconn->addr);
      return -1;
    }
  
  /* Request a specific packet */
  if ( pktid > 0 )
    {
      /* Create packet header with command: "READ pktid" */
      headerlen = snprintf (header, sizeof(header), "READ %lld", (long long int)pktid);
      
      /* Send command and packet to server */
      if ( dl_sendpacket (dlconn, header, headerlen, NULL, 0, NULL, 0) < 0 )
	{
	  dl_log_r (dlconn, 2, 0, "[%s] dl_read(): problem sending READ command\n",
		    dlconn->addr);
	  return -1;
	}
    }
  
  /* Receive packet header, blocking until received */
  if ( (rv = dl_recvheader (dlconn, header, sizeof(header), 1)) < 0 )
    {
      /* Only log an error if the connection was not shut down */
      if ( rv < -1 )
	dl_log_r (dlconn, 2, 0, "[%s] dl_read(): problem receving packet header\n",
		  dlconn->addr);
      return -1;
    }
  
  if ( ! strncmp (header, "PACKET", 6) )
    {
      /* Parse PACKET header */
      rv = sscanf (header, "PACKET %s %lld %lld %lld %lld %lldd",
		   packet->streamid, &spktid, &spkttime,
		   &sdatastart, &sdataend, &sdatasize);
      
      if ( rv != 6 )
	{
	  dl_log_r (dlconn, 2, 0, "[%s] dl_read(): cannot parse PACKET header\n",
		    dlconn->addr);
	  return -1;
	}
      
      packet->pktid = spktid;
      packet->pkttime = spkttime;
      packet->datastart = sdatastart;
      packet->dataend = sdataend;
      packet->datasize = sdatasize;
      
      /* Check that the packet data size is not beyond the max receive buffer size */
      if ( packet->datasize > maxdatasize )
	{
	  char *discard;
	  
	  dl_log_r (dlconn, 2, 0,
		    "[%s] dl_read(): packet data larger (%ld) than receiving buffer (%ld)\n",
		    dlconn->addr, packet->datasize, maxdatasize);
	  
	  /* Allocate temporary buffer */
	  if ( ! (discard = (char *) malloc (packet->datasize)) )
	    {
	      dl_log_r (dlconn, 2, 0,
			"[%s] dl_read(): cannot allocate %d bytes for temporary buffer\n",
			dlconn->addr, packet->datasize);
	      return -1;
	    }
	  
	  /* Consume packet data */
	  if ( (rv = dl_recvdata (dlconn, discard, packet->datasize, 1)) != packet->datasize )
	    {
	      /* Only log an error if the connection was not shut down */
	      if ( rv < -1 )
		dl_log_r (dlconn, 2, 0, "[%s] dl_read(): problem receiving packet data\n",
			  dlconn->addr);
	      return -1;
	    }
	  
	  if ( discard )
	    free (discard);
	  
	  return 0;
	}
      
      /* Receive packet data, blocking until complete */
      if ( (rv = dl_recvdata (dlconn, packetdata, packet->datasize, 1)) != packet->datasize )
	{
	  /* Only log an error if the connection was not shut down */
	  if ( rv < -1 )
	    dl_log_r (dlconn, 2, 0, "[%s] dl_read(): problem receiving packet data\n",
		      dlconn->addr);
	  return -1;
	}
      
      /* Update most recently received packet ID and time */
      dlconn->pktid = packet->pktid;
      dlconn->pkttime = packet->pkttime;
    }
  else if ( ! strncmp (header, "ERROR", 5) )
    {
      /* Reply message, if sent, will be placed into the header buffer */
      rv = dl_handlereply (dlconn, header, sizeof(header), NULL);
      
      /* Log server reply message */
      if ( rv >= 0 )
	dl_log_r (dlconn, 2, 0, "[%s] %s\n", dlconn->addr, header);
      
      return -1;
    }
  else
    {
      dl_log_r (dlconn, 2, 0, "[%s] dl_read(): Unrecognized reply string %.6s\n",
		dlconn->addr, header);
      return -1;
    }
  
  return packet->datasize;
}  /* End of dl_read() */


/***********************************************************************//**
 * @brief Request information from the DataLink server
 *
 * Request and receive information from the server using the DataLink
 * INFO command.  The INFO response is placed in the supplied @a
 * infodata buffer.  All DataLink INFO responses are returned as XML.
 *
 * If @a maxinfosize argument is 0 memory will be allocated as needed
 * for the INFO data result and the infodata pointer will be set to
 * this new buffer; it is up to the caller to free this memory.  If an
 * infomatch string is supplied it will be appended to the INFO
 * request sent to the server.
 *
 * @param dlconn DataLink Connection Parameters
 * @param infotype The INFO type to request
 * @param infomatch An optional match pattern
 * @param infodata Buffer to place the INFO response into
 * @param maxinfosize Maximum number of bytes to write to @a infodata buffer
 *
 * @return The length of the INFO response in bytes on success and -1
 * on error.
 ***************************************************************************/
int
dl_getinfo (DLCP *dlconn, const char *infotype, char *infomatch,
	    char **infodata, size_t maxinfosize)
{
  char header[255];
  char type[255];
  int headerlen;
  int infosize = 0;
  int rv = 0;
  
  if ( ! dlconn || ! infotype || ! infodata )
    return -1;
  
  if ( maxinfosize && ! *infodata )
    return -1;
  
  if ( dlconn->link < 0 )
    return -1;
  
  /* Sanity check that connection is not in streaming mode */
  if ( dlconn->streaming )
    {
      dl_log_r (dlconn, 1, 1, "[%s] dl_getinfo(): Connection in streaming mode, cannot continue\n",
		dlconn->addr);
      return -1;
    }
  
  /* Request information */
  /* Create packet header with command: "INFO type" */
  headerlen = snprintf (header, sizeof(header), "INFO %s %s", infotype,
			(infomatch)?infomatch:"");
  
  /* Send command and packet to server */
  if ( dl_sendpacket (dlconn, header, headerlen, NULL, 0, NULL, 0) < 0 )
    {
      dl_log_r (dlconn, 2, 0, "[%s] dl_getinfo(): problem sending INFO command\n",
		dlconn->addr);
      return -1;
    }
  
  /* Receive packet header, blocking until complete: INFO <size> */
  if ( (rv = dl_recvheader (dlconn, header, sizeof(header), 1)) < 0 )
    {
      /* Only log an error if the connection was not shut down */
      if ( rv < -1 )
	dl_log_r (dlconn, 2, 0, "[%s] dl_getinfo(): problem receving packet header\n",
		  dlconn->addr);
      return -1;
    }
  
  if ( ! strncmp (header, "INFO", 4) )
    {
      /* Parse INFO header */
      rv = sscanf (header, "INFO %s %d", type, &infosize);
      
      if ( rv != 2 )
	{
	  dl_log_r (dlconn, 2, 0, "[%s] dl_getinfo(): cannot parse INFO header\n",
		    dlconn->addr);
	  return -1;
	}
      
      if ( strncasecmp (infotype, type, strlen(infotype)) )
	{
	  dl_log_r (dlconn, 2, 0, "[%s] dl_getinfo(): requested type %s but received type %s\n",
		    dlconn->addr, infotype, type);
	  return -1;
	}
      
      /* If a maximum buffer size was specified check that it's large enough */
      if ( maxinfosize && infosize > maxinfosize )
	{
	  dl_log_r (dlconn, 2, 0, "[%s] dl_getinfo(): INFO data larger (%d) than the maximum size (%d)\n",
		    dlconn->addr, infosize, maxinfosize);
	  return -1;
	}
      
      /* Allocate the infobuffer if needed */
      if ( maxinfosize == 0 )
	{
	  if ( ! (*infodata = malloc (infosize)) )
	    {
	      dl_log_r (dlconn, 2, 0, "[%s] dl_getinfo(): error allocating receving buffer of %d bytes\n",
			dlconn->addr, infosize);
	      return -1;
	    }
	}
      
      /* Receive INFO data, blocking until complete */
      if ( (rv = dl_recvdata (dlconn, *infodata, infosize, 1)) != infosize )
	{
	  /* Only log an error if the connection was not shut down */
	  if ( rv < -1 )
	    dl_log_r (dlconn, 2, 0, "[%s] dl_getinfo(): problem receiving INFO data\n",
		      dlconn->addr);
	  return -1;
	}
    }
  else if ( ! strncmp (header, "ERROR", 5) )
    {
      /* Reply message, if sent, will be placed into the header buffer */
      rv = dl_handlereply (dlconn, header, sizeof(header), NULL);
      
      /* Log server reply message */
      if ( rv >= 0 )
	dl_log_r (dlconn, 2, 0, "[%s] %s\n", dlconn->addr, header);
      
      return -1;
    }
  else
    {
      dl_log_r (dlconn, 2, 0, "[%s] dl_getinfo(): Unrecognized reply string %.6s\n",
		dlconn->addr, header);
      return -1;
    }
  
  return infosize;
}  /* End of dl_getinfo() */


/***********************************************************************//**
 * @brief Collect packets streaming from the DataLink server
 *
 * Collect packets streaming from the DataLink server.  If the
 * connection is not already in streaming mode the STREAM command will
 * first be sent.  This routine will block until a packet is received
 * sending keepalive packets to the server based on the DLCP.keepalive
 * parameter.
 *
 * Designed to run in a tight loop at the heart of a client program,
 * this function will return every time a packet is received.  On
 * successfully receiving a packet @a dlpack will be populated and the
 * packet data will be copied into @a packetdata.
 *
 * If the endflag is true the ENDSTREAM command is sent which
 * instructs the server to stop streaming packets; a client must
 * continue collecting packets until DLENDED is returned in order to
 * get any packets that were in-the-air when ENDSTREAM was requested.
 * The stream ending sequence must be completed if the connection is
 * to be used after streaming mode.
 *
 * @retval DLPACKET when a packet is received.
 * @retval DLENDED when the stream ending sequence was completed or the connection was shut down.
 * @retval DLERROR when an error occurred.
 ***************************************************************************/
int
dl_collect (DLCP *dlconn, DLPacket *packet, void *packetdata,
	    size_t maxdatasize, int8_t endflag)
{
  dltime_t now;
  char header[255];
  int  headerlen;
  int  rv;

  long long int spktid;
  long long int spkttime;
  long long int sdatastart;
  long long int sdataend;
  long long int sdatasize;
  
  /* For select()ing during the read loop */
  struct timeval select_tv;
  fd_set         select_fd;
  int            select_ret;
  
  if ( ! dlconn || ! packet || ! packetdata )
    return DLERROR;
  
  if ( dlconn->link == -1 )
    return DLERROR;
  
  /* If not streaming send the STREAM command */
  if ( ! dlconn->streaming && ! endflag )
    {
      /* Create packet header with command: "STREAM" */
      headerlen = snprintf (header, sizeof(header), "STREAM");
      
      /* Send command to server */
      if ( dl_sendpacket (dlconn, header, headerlen, NULL, 0, NULL, 0) < 0 )
	{
	  dl_log_r (dlconn, 2, 0, "[%s] dl_collect(): problem sending STREAM command\n",
		    dlconn->addr);
	  return DLERROR;
	}
      
      dlconn->streaming = 1;
      dlconn->keepalive_trig = -1;
      dl_log_r (dlconn, 1, 2, "[%s] STREAM command sent to server\n", dlconn->addr);
    }
  
  /* If streaming and end is requested send the ENDSTREAM command */
  if ( dlconn->streaming == 1 && endflag )
    {
      /* Create packet header with command: "ENDSTREAM" */
      headerlen = snprintf (header, sizeof(header), "ENDSTREAM");
      
      /* Send command to server */
      if ( dl_sendpacket (dlconn, header, headerlen, NULL, 0, NULL, 0) < 0 )
	{
	  dl_log_r (dlconn, 2, 0, "[%s] dl_collect(): problem sending ENDSTREAM command\n",
		    dlconn->addr);
	  return DLERROR;
	}
      
      dlconn->streaming = -1;
      dlconn->keepalive_trig = -1;
      dl_log_r (dlconn, 1, 2, "[%s] ENDSTREAM command sent to server\n", dlconn->addr);
    }
  
  /* Start the primary loop */
  while ( ! dlconn->terminate )
    {
      /* Check if a keepalive packet needs to be sent */
      if ( dlconn->keepalive && dlconn->keepalive_trig > 0 )
	{
	  dl_log_r (dlconn, 1, 2, "[%s] Sending keepalive packet\n", dlconn->addr);
	  
	  /* Send ID as a keepalive packet exchange */
	  headerlen = snprintf (header, sizeof(header), "ID %s",
				(dlconn->clientid) ? dlconn->clientid : "");
	  
	  if ( dl_sendpacket (dlconn, header, headerlen, NULL, 0, NULL, 0) < 0 )
	    {
	      dl_log_r (dlconn, 2, 0, "[%s] dl_collect(): problem sending keepalive packet\n",
			dlconn->addr);
	      return DLERROR;
	    }
	  
	  dlconn->keepalive_trig = -1;
	}
      
      /* Poll the socket for available data */
      FD_ZERO (&select_fd);
      FD_SET ((unsigned int)dlconn->link, &select_fd);
      select_tv.tv_sec = 0;
      select_tv.tv_usec = 500000;  /* Block up to 0.5 seconds */
      
      select_ret = select ((dlconn->link + 1), &select_fd, NULL, NULL, &select_tv);
      
      /* Check the return from select(), an interrupted system call error
	 will be reported if a signal handler was used.  If the terminate
	 flag is set this is not an error. */
      if ( select_ret > 0 )
	{
	  if ( ! FD_ISSET (dlconn->link, &select_fd) )
	    {
	      dl_log_r (dlconn, 2, 0, "[%s] select() reported data but socket not in set!\n",
			dlconn->addr);
	    }
	  else
	    {
	      /* Receive packet header, blocking until complete */
	      if ( (rv = dl_recvheader (dlconn, header, sizeof(header), 1)) < 0 )
		{
		  if ( rv == -1 )
		    return DLENDED;
		  
		  dl_log_r (dlconn, 2, 0, "[%s] dl_collect(): problem receving packet header\n",
			    dlconn->addr);
		  return DLERROR;
		}
	      
	      /* Reset keepalive trigger */
	      dlconn->keepalive_trig = -1;
	      
	      if ( ! strncmp (header, "PACKET", 6) )
		{
		  /* Parse PACKET header */
		  rv = sscanf (header, "PACKET %s %lld %lld %lld %lld %lld",
			       packet->streamid, &spktid, &spkttime,
			       &sdatastart, &sdataend, &sdatasize);
		  
		  if ( rv != 6 )
		    {
		      dl_log_r (dlconn, 2, 0, "[%s] dl_collect(): cannot parse PACKET header\n",
				dlconn->addr);
		      return DLERROR;
		    }
		  
		  packet->pktid = spktid;
		  packet->pkttime = spkttime;
		  packet->datastart = sdatastart;
		  packet->dataend = sdataend;
		  packet->datasize = sdatasize;
		  
		  if ( packet->datasize > maxdatasize )
		    {
		      dl_log_r (dlconn, 2, 0,
				"[%s] dl_collect(): packet data larger (%ld) than receiving buffer (%ld)\n",
				dlconn->addr, packet->datasize, maxdatasize);
		      return DLERROR;
		    }
		  
		  /* Receive packet data, blocking until complete */
		  if ( (rv = dl_recvdata (dlconn, packetdata, packet->datasize, 1)) != packet->datasize )
		    {
		      if ( rv == -1 )
			return DLENDED;
		      
		      dl_log_r (dlconn, 2, 0, "[%s] dl_collect(): problem receiving packet data\n",
				dlconn->addr);
		      return DLERROR;
		    }
		  
		  /* Update most recently received packet ID and time */
		  dlconn->pktid = packet->pktid;
		  dlconn->pkttime = packet->pkttime;
		  
		  return DLPACKET;
		}
	      else if ( ! strncmp (header, "ID", 2) )
		{
		  dl_log_r (dlconn, 1, 2, "[%s] Received keepalive from server\n",
			    dlconn->addr);
		}
	      else if ( ! strncmp (header, "ENDSTREAM", 9) )
		{
		  dl_log_r (dlconn, 1, 2, "[%s] Received end-of-stream from server\n",
			    dlconn->addr);
		  dlconn->streaming = 0;
		  return DLENDED;
		}
	      else
		{
		  dl_log_r (dlconn, 2, 0, "[%s] dl_collect(): Unrecognized packet header %.6s\n",
			    dlconn->addr, header);
		  return DLERROR;
		}
	    }
	}
      else if ( select_ret < 0 && ! dlconn->terminate )
	{
	  dl_log_r (dlconn, 2, 0, "[%s] select() error: %s\n", dlconn->addr, dlp_strerror ());
	  return DLERROR;
	}
      
      /* Update timing variables */
      now = dlp_time ();
      
      /* Keepalive/heartbeat interval timing logic */
      if ( dlconn->keepalive )
	{
	  if ( dlconn->keepalive_trig == -1 )  /* reset timer */
	    {
	      dlconn->keepalive_time = now;
	      dlconn->keepalive_trig = 0;
	    }
	  else if ( dlconn->keepalive_trig == 0 &&
		    (now - dlconn->keepalive_time) > (dlconn->keepalive * DLTMODULUS) )
	    {
	      dlconn->keepalive_trig = 1;
	    }
	}
    }  /* End of primary loop */
  
  return DLENDED;
}  /* End of dl_collect() */


/***********************************************************************//**
 * @brief Collect packets streaming from the DataLink server without blocking
 *
 * Collect packets streaming from the DataLink server.  If the
 * connection is not already in streaming mode the STREAM command will
 * first be sent.  This routine is a non-blocking version of
 * dl_collect() and will return quickly whether data is received or
 * not.  Keep alive packets are sent to the server based on the
 * DLCP.keepalive parameter.
 *
 * Designed to run in a tight loop at the heart of a client program,
 * this function will return every time a packet is received.  On
 * successfully receiving a packet @a dlpack will be populated and the
 * packet data will be copied into @a packetdata.
 *
 * If the @a endflag is true the ENDSTREAM command is sent which
 * instructs the server to stop streaming packets; a client must
 * continue collecting packets until DLENDED is returned in order to
 * get any packets that were in-the-air when ENDSTREAM was requested.
 * The stream ending sequence must be completed if the connection is
 * to be used after streaming mode.
 *
 * @retval DLPACKET A packet is received.
 * @retval DLNOPACKET No packet is received.
 * @retval DLENDED when the stream ending sequence was completed or the connection was shut down.
 * @retval DLERROR when an error occurred.
 ***************************************************************************/
int
dl_collect_nb (DLCP *dlconn, DLPacket *packet, void *packetdata,
	       size_t maxdatasize, int8_t endflag)
{
  dltime_t now;
  char header[255];
  int  headerlen;
  int  rv;
  
  long long int spktid;
  long long int spkttime;
  long long int sdatastart;
  long long int sdataend;
  long long int sdatasize;

  if ( ! dlconn || ! packet || ! packetdata )
    return DLERROR;
  
  if ( dlconn->link == -1 )
    return DLERROR;
  
  /* If not streaming send the STREAM command */
  if ( ! dlconn->streaming && ! endflag )
    {
      /* Create packet header with command: "STREAM" */
      headerlen = snprintf (header, sizeof(header), "STREAM");
      
      /* Send command to server */
      if ( dl_sendpacket (dlconn, header, headerlen, NULL, 0, NULL, 0) < 0 )
	{
	  dl_log_r (dlconn, 2, 0, "[%s] dl_collect_nb(): problem sending STREAM command\n",
		    dlconn->addr);
	  return DLERROR;
	}
      
      dlconn->streaming = 1;
      dlconn->keepalive_trig = -1;
      dl_log_r (dlconn, 1, 2, "[%s] STREAM command sent to server", dlconn->addr);
    }
  
  /* If streaming and end is requested send the ENDSTREAM command */
  if ( dlconn->streaming == 1 && endflag )
    {
      /* Create packet header with command: "ENDSTREAM" */
      headerlen = snprintf (header, sizeof(header), "ENDSTREAM");
      
      /* Send command to server */
      if ( dl_sendpacket (dlconn, header, headerlen, NULL, 0, NULL, 0) < 0 )
	{
	  dl_log_r (dlconn, 2, 0, "[%s] dl_collect_nb(): problem sending ENDSTREAM command\n",
		    dlconn->addr);
	  return DLERROR;
	}
      
      dlconn->streaming = -1;
      dlconn->keepalive_trig = -1;
      dl_log_r (dlconn, 1, 2, "[%s] ENDSTREAM command sent to server", dlconn->addr);
    }
  
  if ( ! dlconn->terminate )
    {
      /* Check if a keepalive packet needs to be sent */
      if ( dlconn->keepalive && dlconn->keepalive_trig > 0 )
	{
	  dl_log_r (dlconn, 1, 2, "[%s] Sending keepalive packet\n", dlconn->addr);
	  
	  /* Send ID as a keepalive packet exchange */
	  headerlen = snprintf (header, sizeof(header), "ID %s",
				(dlconn->clientid) ? dlconn->clientid : "");
	  
	  if ( dl_sendpacket (dlconn, header, headerlen,
			      NULL, 0, NULL, 0) < 0 )
	    {
	      dl_log_r (dlconn, 2, 0, "[%s] dl_collect_nb(): problem sending keepalive packet\n",
			dlconn->addr);
	      return DLERROR;
	    }
	  
	  dlconn->keepalive_trig = -1;
	}
    }
  
  /* Receive packet header if it's available */
  if ( (rv = dl_recvheader (dlconn, header, sizeof(header), 0)) < 0 )
    {
      if ( rv == -1 )
	return DLENDED;
      
      dl_log_r (dlconn, 2, 0, "[%s] dl_collect_nb(): problem receving packet header\n",
		dlconn->addr);
      return DLERROR;
    }
  
  /* Process data if header received */
  if ( rv > 0 )
    {
      /* Reset keepalive trigger */
      dlconn->keepalive_trig = -1;
      
      if ( ! strncmp (header, "PACKET", 6) )
	{
	  /* Parse PACKET header */
	  rv = sscanf (header, "PACKET %s %lld %lld %lld %lld %lld",
		       packet->streamid, &spktid, &spkttime,
		       &sdatastart, &sdataend, &sdatasize);
	  
	  if ( rv != 6 )
	    {
	      dl_log_r (dlconn, 2, 0, "[%s] dl_collect_nb(): cannot parse PACKET header\n",
			dlconn->addr);
	      return DLERROR;
	    }
	  
	  packet->pktid = spktid;
	  packet->pkttime = spkttime;
	  packet->datastart = sdatastart;
	  packet->dataend = sdataend;
	  packet->datasize = sdatasize;
	  
	  if ( packet->datasize > maxdatasize )
	    {
	      dl_log_r (dlconn, 2, 0,
			"[%s] dl_collect_nb(): packet data larger (%ld) than receiving buffer (%ld)\n",
			dlconn->addr, packet->datasize, maxdatasize);
	      return DLERROR;
	    }
	  
	  /* Receive packet data, blocking until complete */
	  if ( (rv = dl_recvdata (dlconn, packetdata, packet->datasize, 1)) != packet->datasize )
	    {
	      if ( rv == -1 )
		return DLENDED;
	      
	      dl_log_r (dlconn, 2, 0, "[%s] dl_collect_nb(): problem receiving packet data\n",
			dlconn->addr);
	      return DLERROR;
	    }
	  
	  /* Update most recently received packet ID and time */
	  dlconn->pktid = packet->pktid;
	  dlconn->pkttime = packet->pkttime;
	  
	  return DLPACKET;
	}
      else if ( ! strncmp (header, "ID", 2) )
	{
	  dl_log_r (dlconn, 1, 2, "[%s] Received keepalive (ID) from server\n", dlconn->addr);
	}
      else if ( ! strncmp (header, "ENDSTREAM", 9) )
	{
	  dl_log_r (dlconn, 1, 2, "[%s] Received end-of-stream from server\n", dlconn->addr);
	  dlconn->streaming = 0;
	  return DLENDED;
	}
      else
	{
	  dl_log_r (dlconn, 2, 0, "[%s] dl_collect_nb(): Unrecognized packet header %.6s\n",
		    dlconn->addr, header);
	  return DLERROR;
	}
    }
  
  /* Update timing variables */
  now = dlp_time ();
  
  /* Keepalive/heartbeat interval timing logic */
  if ( dlconn->keepalive )
    {
      if ( dlconn->keepalive_trig == -1 )  /* reset timer */
	{
	  dlconn->keepalive_time = now;
	  dlconn->keepalive_trig = 0;
	}
      else if ( dlconn->keepalive_trig == 0 &&
		(now - dlconn->keepalive_time) > (dlconn->keepalive * DLTMODULUS) )
	{
	  dlconn->keepalive_trig = 1;
	}
    }
  
  return ( dlconn->terminate ) ? DLENDED: DLNOPACKET;
}  /* End of dl_collect_nb() */


/***********************************************************************//**
 * @brief Handle the server reply to a command
 *
 * Handle the server reply to a command.  This routine is used by
 * other library routines to process replies from the server.
 *
 * Server replies are of the form:
 *
 * "OK|ERROR value size"
 *
 * followed by an optional server message of size bytes.  If size is
 * greater than zero it will be read from the connection and placed
 * into @a buffer.  The server message, if included, will always be a
 * NULL-terminated string.
 *
 * @retval -1 Error
 * @retval 0 "OK" received
 * @retval 1 "ERROR" received
 ***************************************************************************/
int
dl_handlereply (DLCP *dlconn, void *buffer, int buflen, int64_t *value)
{
  char status[10];
  char *cbuffer = buffer;
  long long int pvalue;
  long long int size = 0;
  int rv = 0;
  
  if ( ! dlconn || ! buffer )
    return -1;
  
  /* Make sure buffer if terminated */
  cbuffer[buflen] = '\0';
  
  /* Parse reply header */
  if ( sscanf (buffer, "%10s %lld %lld", status, &pvalue, &size) != 3 )
    {
      dl_log_r (dlconn, 2, 0, "[%s] dl_handlereply(): Unable to parse reply header: '%s'\n",
		dlconn->addr, buffer);
      return -1;
    }
  
  /* Store reply value if requested */
  if ( value )
    *value = pvalue;
  
  /* Check that reply message will fit into buffer */
  if ( size > buflen )
    {
      dl_log_r (dlconn, 2, 0, "[%s] dl_handlereply(): Reply message too large (%d) for buffer (%d)\n",
		dlconn->addr, size, buflen);
      return -1;      
    }
  
  /* Receive reply message if included */
  if ( size > 0 )
    {
      /* Receive reply message, blocking until complete */
      if ( (rv = dl_recvdata (dlconn, buffer, size, 1)) != size )
	{
	  /* Only log an error if the connection was not shut down */
	  if ( rv < -1 )
	    dl_log_r (dlconn, 2, 0, "[%s] dl_handlereply(): Problem receiving reply message\n",
		      dlconn->addr);
	  return -1;
	}
      
      if ( size < buflen )
	cbuffer[size] = '\0';
      else
	cbuffer[buflen-1] = '\0';
    }
  /* Make sure buffer is terminated */
  else
    {
      cbuffer[0] = '\0';
    }
  
  /* Check for "OK" status in reply header */
  if ( ! strncmp (status, "OK", 2) )
    {
      rv = 0;
    }
  else if ( ! strncmp (status, "ERROR", 5) )
    {
      rv = 1;
    }
  else
    {
      dl_log_r (dlconn, 2, 0, "[%s] dl_handlereply(): Unrecognized reply string %.5s\n",
		dlconn->addr, buffer);
      rv = -1;
    }
  
  return rv;
}  /* End of dl_handlereply() */


/***********************************************************************//**
 * @brief Set the terminate parameter of a DataLink connection
 *
 * Set the terminate parameter/flag in the @a DLCP and log a
 * diagnostic message indicating that the connection is terminating.
 * Some of the library routines watch the terminate parameter as an
 * indication that the client program is requesting a shut down.  This
 * routine is typically used in a signal handler.
 ***************************************************************************/
void
dl_terminate (DLCP *dlconn)
{
  dl_log_r (dlconn, 1, 1, "[%s] Terminating connection\n", dlconn->addr);
  
  dlconn->terminate = 1;
}  /* End of dl_terminate() */
