
/***************************************************************************
 * network.c
 *
 * Network communication routines for SeedLink
 *
 * Written by Chad Trabant, ORFEUS/EC-Project MEREDIAN
 *
 * Originally based on the SeedLink interface of the modified Comserv in
 * SeisComP written by Andres Heinloo
 *
 * Version: 2004.170
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libslink.h"

/* Functions only used in this source file */
int sl_sayhello (SLCD * slconn);
int sl_negotiate_uni (SLCD * slconn);
int sl_negotiate_multi (SLCD * slconn);


/***************************************************************************
 * sl_configlink():
 * Configure/negotiate data stream(s) with the remote SeedLink
 * server.  Negotiation will be either uni or multi-station
 * depending on the value of 'multistation' in the SLCD
 * struct.
 *
 * Returns -1 on errors, otherwise returns the link descriptor.
 ***************************************************************************/
int
sl_configlink (SLCD * slconn)
{
  int ret = -1;

  if (slconn->multistation)
    {
      if (sl_checkversion (slconn, 2.5) >= 0)
	{
	  ret = sl_negotiate_multi (slconn);
	}
      else
	{
	  sl_log_r (slconn, 1, 0,
		    "[%s] detected  SeedLink version (%.3f) does not support multi-station protocol\n",
		    slconn->sladdr, slconn->server_version);
	  ret = -1;
	}
    }
  else
    ret = sl_negotiate_uni (slconn);

  return ret;
}


/***************************************************************************
 * sl_sayhello():
 * Send the HELLO command and attempt to parse the server version
 * number from the returned string.  The server version is set to 0.0
 * if it can not be parsed from the returned string, which indicates
 * minimum protocol functionality.
 *
 * Returns -1 on errors, 0 on success.
 ***************************************************************************/
int
sl_sayhello (SLCD * slconn)
{
  int ret = 0;
  int servcnt = 0;
  int sitecnt = 0;
  char sendstr[100];		/* A buffer for command strings */
  char servstr[100];		/* The remote server ident */
  char sitestr[100];		/* The site/data center ident */
  char servid[100];		/* Server ID string, i.e. 'SeedLink' */

  /* Send HELLO */
  sprintf (sendstr, "HELLO\r");
  sl_log_r (slconn, 0, 2, "[%s] sending: HELLO\n", slconn->sladdr);
  sl_senddata (slconn, (void *) sendstr, strlen (sendstr), slconn->sladdr,
	       NULL, 0);
  
  /* Recv the two lines of response */
  if ( sl_recvresp (slconn, (void *) servstr, (size_t) sizeof (servstr), 
		    sendstr, slconn->sladdr) < 0 )
    {
      return -1;
    }
  
  if ( sl_recvresp (slconn, (void *) sitestr, (size_t) sizeof (sitestr),
		    sendstr, slconn->sladdr) < 0 )
    {
      return -1;
    }
  
  servcnt = strcspn (servstr, "\r");
  if ( servcnt > 90 )
    {
      servcnt = 90;
    }
  servstr[servcnt] = '\0';

  sitecnt = strcspn (sitestr, "\r");
  if ( sitecnt > 90 )
    {
      sitecnt = 90;
    }
  sitestr[sitecnt] = '\0';
  
  sl_log_r (slconn, 0, 1, "[%s] connected to: %s\n", slconn->sladdr, servstr);
  sl_log_r (slconn, 0, 1, "[%s] organization: %s\n", slconn->sladdr, sitestr);
  
  /* Parse the server ID and version from the returned string.
   * The expected format is:
   * "seedlink v#.# <optional text>"
   * where 'seedlink' is case insensitive and '#.#' is the server/protocol version.
   */
  servstr[servcnt] = ' '; servstr[servcnt+1] = '\0';
  ret = sscanf (servstr, "%s v%f ", &servid[0], &slconn->server_version);
  
  if ( ret != 2 || strncasecmp (servid, "SEEDLINK", 8) )
    {
      sl_log_r (slconn, 0, 1,
                "[%s] unknown server version, assuming minimum functionality\n",
                slconn->sladdr, servstr);
      slconn->server_version = 0.0;
    }
  
  return 0;
}				/* End of sl_sayhello() */


/***************************************************************************
 * sl_negotiate_uni():
 * Negotiate a SeedLink connection in uni-station mode and issue
 * the DATA command.  This is compatible with SeedLink Protocol
 * version 2 or greater.
 * If 'selectors' != 0 then the string is parsed on space and each
 * selector is sent.
 * If 'seqnum' != -1 and the SLCD 'resume' flag is true then data is
 * requested starting at seqnum.
 *
 * Returns -1 on errors, otherwise returns the link descriptor.
 ***************************************************************************/
int
sl_negotiate_uni (SLCD * slconn)
{
  int sellen = 0;
  int bytesread = 0;
  int acceptsel = 0;		/* Count of accepted selectors */
  char *selptr;
  char sendstr[100];		/* A buffer for command strings */
  char readbuf[100];		/* A buffer for responses */
  SLstream *curstream;

  /* Point to the stream chain */
  curstream = slconn->streams;

  selptr = curstream->selectors;

  /* Send the selector(s) and check the response(s) */
  if (curstream->selectors != 0)
    {
      while (1)
	{
	  selptr += sellen;
	  selptr += strspn (selptr, " ");
	  sellen = strcspn (selptr, " ");

	  if (sellen == 0)
	    break;		/* end of while loop */

	  else if (sellen > SELSIZE)
	    {
	      sl_log_r (slconn, 0, 0, "[%s] invalid selector: %.*s\n", slconn->sladdr,
			sellen, selptr);
	      selptr += sellen;
	    }
	  else
	    {

	      /* Build SELECT command, send it and receive response */
	      sprintf (sendstr, "SELECT %.*s\r", sellen, selptr);
	      sl_log_r (slconn, 0, 2, "[%s] sending: SELECT %.*s\n", slconn->sladdr,
			sellen, selptr);
	      bytesread = sl_senddata (slconn, (void *) sendstr,
				       strlen (sendstr), slconn->sladdr,
				       readbuf, sizeof (readbuf));
	      if (bytesread < 0)
		{		/* Error from sl_senddata() */
		  return -1;
		}

	      /* Check response to SELECT */
	      if (!strncmp (readbuf, "OK\r\n", bytesread) && bytesread == 4)
		{
		  sl_log_r (slconn, 0, 2, "[%s] selector %.*s is OK\n", slconn->sladdr,
			    sellen, selptr);
		  acceptsel++;
		}
	      else if (!strncmp (readbuf, "ERROR\r\n", bytesread) &&
		       bytesread == 7)
		{
		  sl_log_r (slconn, 1, 0, "[%s] selector %.*s not accepted\n",
			    slconn->sladdr, sellen, selptr);
		}
	      else
		{
		  sl_log_r (slconn, 1, 0,
			    "[%s] invalid response to SELECT command: %.*s\n",
			    slconn->sladdr, bytesread, readbuf);
		  return -1;
		}
	    }
	}

      /* Fail if none of the given selectors were accepted */
      if (!acceptsel)
	{
	  sl_log_r (slconn, 1, 0, "[%s] No data stream selector(s) accepted",
		    slconn->sladdr);
	  return -1;
	}
      else
	{
	  sl_log_r (slconn, 0, 2, "[%s] %d selector(s) accepted\n",
		    slconn->sladdr, acceptsel);
	}
    }				/* End of selector processing */

  /* Issue the DATA, FETCH or TIME action commands.  A specified start (and
     optionally, stop time) takes precedence over the resumption from any
     previous sequence number. */
  if (slconn->begin_time != NULL)
    {
      if (sl_checkversion (slconn, (float)2.92) >= 0)
	{
	  if (slconn->end_time == NULL)
	    {
	      sprintf (sendstr, "TIME %.25s\r", slconn->begin_time);
	    }
	  else
	    {
	      sprintf (sendstr, "TIME %.25s %.25s\r", slconn->begin_time,
		       slconn->end_time);
	    }
	  sl_log_r (slconn, 0, 1, "[%s] requesting specified time window\n",
		    slconn->sladdr);
	}
      else
	{
	  sl_log_r (slconn, 1, 0,
		    "[%s] detected SeedLink version (%.3f) does not support TIME windows\n",
		    slconn->sladdr, slconn->server_version);
	}
    }
  else if (curstream->seqnum != -1 && slconn->resume )
    {
      char cmd[10];

      if ( slconn->dialup )
	{
	  sprintf (cmd, "FETCH");
	}
      else
	{
	  sprintf (cmd, "DATA");
	}

      /* Append the last packet time if the feature is enabled and server is >= 2.93 */
      if (slconn->lastpkttime &&
	  sl_checkversion (slconn, (float)2.93) >= 0 &&
	  strlen (curstream->timestamp))
	{
	  /* Increment sequence number by 1 */
	  sprintf (sendstr, "%s %06X %.25s\r", cmd,
		   (curstream->seqnum + 1) & 0xffffff, curstream->timestamp);

	  sl_log_r (slconn, 0, 1, "[%s] resuming data from %06X (Dec %d) at %.25s\n",
		    slconn->sladdr, (curstream->seqnum + 1) & 0xffffff,
		    (curstream->seqnum + 1), curstream->timestamp);
	}
      else
	{
	  /* Increment sequence number by 1 */
	  sprintf (sendstr, "%s %06X\r", cmd, (curstream->seqnum + 1) & 0xffffff);

	  sl_log_r (slconn, 0, 1, "[%s] resuming data from %06X (Dec %d)\n",
		    slconn->sladdr, (curstream->seqnum + 1) & 0xffffff,
		    (curstream->seqnum + 1));
	}
    }
  else
    {
      if ( slconn->dialup )
	{
	  sprintf (sendstr, "FETCH\r");
	}
      else
	{
	  sprintf (sendstr, "DATA\r");
	}

      sl_log_r (slconn, 0, 1, "[%s] requesting next available data\n", slconn->sladdr);
    }

  if (sl_senddata (slconn, (void *) sendstr, strlen (sendstr),
		   slconn->sladdr, (void *) NULL, 0) < 0)
    {
      sl_log_r (slconn, 1, 0, "[%s] error sending DATA/FETCH/TIME request\n", slconn->sladdr);
      return -1;
    }

  return slconn->link;
}				/* End of sl_configlink() */


/***************************************************************************
 * sl_negotiate_multi():
 * Negotiate a SeedLink connection using multi-station mode and 
 * issue the END action command.  This is compatible with SeedLink
 * Protocol version 3, multi-station mode.
 * If 'curstream->selectors' != 0 then the string is parsed on space
 * and each selector is sent.
 * If 'curstream->seqnum' != -1 and the SLCD 'resume' flag is true then data
 * is requested starting at
 * seqnum.
 *
 * Returns -1 on errors, otherwise returns the link descriptor.
 ***************************************************************************/
int
sl_negotiate_multi (SLCD * slconn)
{
  int sellen = 0;
  int bytesread = 0;
  int acceptsta = 0;		/* Count of accepted stations */
  int acceptsel = 0;		/* Count of accepted selectors */
  char *selptr;
  char sendstr[100];		/* A buffer for command strings */
  char readbuf[100];		/* A buffer for responses */
  char slring[12];		/* Keep track of the ring name */
  SLstream *curstream;

  /* Point to the stream chain */
  curstream = slconn->streams;

  /* Loop through the stream chain */
  while (curstream != NULL)
    {

      /* A ring identifier */
      snprintf (slring, sizeof (slring), "%s_%s",
		curstream->net, curstream->sta);

      /* Send the STATION command */
      sprintf (sendstr, "STATION %s %s\r", curstream->sta, curstream->net);
      sl_log_r (slconn, 0, 2, "[%s] sending: STATION %s %s\n",
		slring, curstream->sta, curstream->net);
      bytesread = sl_senddata (slconn, (void *) sendstr,
			       strlen (sendstr), slring, readbuf,
			       sizeof (readbuf));
      if (bytesread < 0)
	{
	  return -1;
	}

      /* Check the response */
      if (!strncmp (readbuf, "OK\r\n", bytesread) && bytesread == 4)
	{
	  sl_log_r (slconn, 0, 2, "[%s] station is OK (selected)\n", slring);
	  acceptsta++;
	}
      else if (!strncmp (readbuf, "ERROR\r\n", bytesread) && bytesread == 7)
	{
	  sl_log_r (slconn, 1, 0, "[%s] station not accepted, skipping\n", slring);
	  /* Increment the loop control and skip to the next stream */
	  curstream = curstream->next;
	  continue;
	}
      else
	{
	  sl_log_r (slconn, 1, 0, "[%s] invalid response to STATION command: %.*s\n",
		    slring, bytesread, readbuf);
	  return -1;
	}

      selptr = curstream->selectors;
      sellen = 0;

      /* Send the selector(s) and check the response(s) */
      if (curstream->selectors != 0)
	{
	  while (1)
	    {
	      selptr += sellen;
	      selptr += strspn (selptr, " ");
	      sellen = strcspn (selptr, " ");

	      if (sellen == 0)
		break;		/* end of while loop */

	      else if (sellen > SELSIZE)
		{
		  sl_log_r (slconn, 0, 0, "[%s] invalid selector: %.*s\n",
			    slring, sellen, selptr);
		  selptr += sellen;
		}
	      else
		{

		  /* Build SELECT command, send it and receive response */
		  sprintf (sendstr, "SELECT %.*s\r", sellen, selptr);
		  sl_log_r (slconn, 0, 2, "[%s] sending: SELECT %.*s\n", slring, sellen,
			    selptr);
		  bytesread =
		    sl_senddata (slconn, (void *) sendstr,
				 strlen (sendstr), slring, readbuf,
				 sizeof (readbuf));
		  if (bytesread < 0)
		    {		/* Error from sl_senddata() */
		      return -1;
		    }

		  /* Check response to SELECT */
		  if (!strncmp (readbuf, "OK\r\n", bytesread)
		      && bytesread == 4)
		    {
		      sl_log_r (slconn, 0, 2, "[%s] selector %.*s is OK\n", slring,
				sellen, selptr);
		      acceptsel++;
		    }
		  else if (!strncmp (readbuf, "ERROR\r\n", bytesread) &&
			   bytesread == 7)
		    {
		      sl_log_r (slconn, 1, 0, "[%s] selector %.*s not accepted\n",
				slring, sellen, selptr);
		    }
		  else
		    {
		      sl_log_r (slconn, 1, 0,
				"[%s] invalid response to SELECT command: %.*s\n",
				slring, bytesread, readbuf);
		      return -1;
		    }
		}
	    }

	  /* Fail if none of the given selectors were accepted */
	  if (!acceptsel)
	    {
	      sl_log_r (slconn, 1, 0, "[%s] No data stream selector(s) accepted",
			slring);
	      return -1;
	    }
	  else
	    {
	      sl_log_r (slconn, 0, 2, "[%s] %d selector(s) accepted\n", slring,
			acceptsel);
	    }

	  acceptsel = 0;	/* Reset the accepted selector count */

	}			/* End of selector processing */

      /* Issue the DATA, FETCH or TIME action commands.  A specified start (and
	 optionally, stop time) takes precedence over the resumption from any
	 previous sequence number. */
      if (slconn->begin_time != NULL)
	{
	  if (sl_checkversion (slconn, (float)2.92) >= 0)
	    {
	      if (slconn->end_time == NULL)
		{
		  sprintf (sendstr, "TIME %.25s\r", slconn->begin_time);
		}
	      else
		{
		  sprintf (sendstr, "TIME %.25s %.25s\r", slconn->begin_time,
			   slconn->end_time);
		}
	      sl_log_r (slconn, 0, 1, "[%s] requesting specified time window\n",
			slring);
	    }
	  else
	    {
	      sl_log_r (slconn, 1, 0,
			"[%s] detected SeedLink version (%.3f) does not support TIME windows\n",
			slring, slconn->server_version);
	    }
	}
      else if (curstream->seqnum != -1 && slconn->resume )
	{
	  char cmd[10];
	  
	  if ( slconn->dialup )
	    {
	      sprintf (cmd, "FETCH");
	    }
	  else
	    {
	      sprintf (cmd, "DATA");
	    }
	  
	  /* Append the last packet time if the feature is enabled and server is >= 2.93 */
	  if (slconn->lastpkttime &&
	      sl_checkversion (slconn, (float)2.93) >= 0 &&
	      strlen (curstream->timestamp))
	    {
	      /* Increment sequence number by 1 */
	      sprintf (sendstr, "%s %06X %.25s\r", cmd,
		       (curstream->seqnum + 1) & 0xffffff, curstream->timestamp);

	      sl_log_r (slconn, 0, 1, "[%s] resuming data from %06X (Dec %d) at %.25s\n",
			slconn->sladdr, (curstream->seqnum + 1) & 0xffffff,
			(curstream->seqnum + 1), curstream->timestamp);
	    }	  
	  else
	    { /* Increment sequence number by 1 */
	      sprintf (sendstr, "%s %06X\r", cmd,
		       (curstream->seqnum + 1) & 0xffffff);

	      sl_log_r (slconn, 0, 1, "[%s] resuming data from %06X (Dec %d)\n", slring,
			(curstream->seqnum + 1) & 0xffffff,
			(curstream->seqnum + 1));
	    }
	}
      else
	{
	  if ( slconn->dialup )
	    {
	      sprintf (sendstr, "FETCH\r");
	    }
	  else
	    {
	      sprintf (sendstr, "DATA\r");
	    }
	  
	  sl_log_r (slconn, 0, 1, "[%s] requesting next available data\n", slring);
	}

      /* Send the TIME/DATA/FETCH command and receive response */
      bytesread = sl_senddata (slconn, (void *) sendstr,
			       strlen (sendstr), slring, readbuf,
			       sizeof (readbuf));
      if (bytesread < 0)
	{
	  sl_log_r (slconn, 1, 0, "[%s] error with DATA/FETCH/TIME request\n", slring);
	  return -1;
	}

      /* Check response to DATA/FETCH/TIME request */
      if (!strncmp (readbuf, "OK\r\n", bytesread) && bytesread == 4)
	{
	  sl_log_r (slconn, 0, 2, "[%s] DATA/FETCH/TIME command is OK\n", slring);
	}
      else if (!strncmp (readbuf, "ERROR\r\n", bytesread) && bytesread == 7)
	{
	  sl_log_r (slconn, 1, 0, "[%s] DATA/FETCH/TIME command is not accepted\n", slring);
	}
      else
	{
	  sl_log_r (slconn, 1, 0, "[%s] invalid response to DATA/FETCH/TIME command: %.*s\n",
		    slring, bytesread, readbuf);
	  return -1;
	}

      /* Point to the next stream */
      curstream = curstream->next;

    }				/* End of stream and selector config (end of stream chain). */

  /* Fail if no stations were accepted */
  if (!acceptsta)
    {
      sl_log_r (slconn, 1, 0, "[%s] No station(s) accepted\n", slconn->sladdr);
      return -1;
    }
  else
    {
      sl_log_r (slconn, 0, 1, "[%s] %d station(s) accepted\n",
	      slconn->sladdr, acceptsta);
    }

  /* Issue END action command */
  sprintf (sendstr, "END\r");
  sl_log_r (slconn, 0, 2, "[%s] sending: END\n", slconn->sladdr);
  if (sl_senddata (slconn, (void *) sendstr, strlen (sendstr),
		   slconn->sladdr, (void *) NULL, 0) < 0)
    {
      sl_log_r (slconn, 1, 0, "[%s] error sending END command\n", slconn->sladdr);
      return -1;
    }

  return slconn->link;
}				/* End of sl_configlink_multi() */


/***************************************************************************
 * sl_send_info():
 * Send a request for the specified INFO level.  The verbosity level
 * can be specified, allowing control of when the request should be
 * logged.
 *
 * Returns -1 on errors, otherwise the socket descriptor.
 ***************************************************************************/
int
sl_send_info (SLCD * slconn, const char * info_level, int verb_level)
{
  char sendstr[40];		/* A buffer for command strings */

  if (sl_checkversion (slconn, (float)2.92) >= 0)
    {
      sprintf (sendstr, "INFO %.15s\r", info_level);

      sl_log_r (slconn, 0, verb_level, "[%s] requesting INFO level %s\n",
		slconn->sladdr, info_level);

      if (sl_senddata (slconn, (void *) sendstr, strlen (sendstr),
		       slconn->sladdr, (void *) NULL, 0) < 0)
	{
	  sl_log_r (slconn, 1, 0, "[%s] error sending INFO request\n", slconn->sladdr);
	  return -1;
	}
    }
  else
    {
      sl_log_r (slconn, 1, 0,
		"[%s] detected SeedLink version (%.3f) does not support INFO requests\n",
		slconn->sladdr, slconn->server_version);   
      return -1;
    }

  return slconn->link;
}				/* End of sl_request_info() */


/***************************************************************************
 * sl_connect():
 * Open a network socket connection to a SeedLink server and set
 * 'slconn->link' to the new descriptor.  Expects 'slconn->sladdr'
 * to be in 'host:port' format.
 *
 * Returns -1 on errors, otherwise the socket descriptor created.
 ***************************************************************************/
int
sl_connect (SLCD * slconn)
{
  int sock;
  int on = 1;
  int sockstat;
  long int nport;
  char *host_name, *ptr, *tail;
  size_t addrlen;
  struct sockaddr addr;

  if ( slp_sockstartup() ) {
    sl_log_r (slconn, 1, 0, "Could not initialize network sockets\n");
    return -1;
  }

  if ((host_name = strdup (slconn->sladdr)) == NULL)
    {
      sl_log_r (slconn, 1, 0, "strdup(): cannot copy SeedLink address string\n");
      return -1;
    }				/* ? needed? a hang over from the original */

  /* Sanity check for the host and port specified */
  if ((ptr = strchr (host_name, ':')) == NULL)
    {
      sl_log_r (slconn, 1, 0, "[%s] server address is not in `[hostname]:port' format\n",
		slconn->sladdr);
      return -1;
    }
  *ptr++ = '\0';

  nport = strtoul (ptr, &tail, 0);
  if ( *tail || (nport < 0 || nport > 0xffff) )
    {
      sl_log_r (slconn, 1, 0, "Server port specified incorrectly\n");
      return -1;
    }
  
  if ( slp_getaddrinfo (host_name, ptr, &addr, &addrlen) )
    {
      sl_log_r (slconn, 0, 0, "Cannot resolve hostname %s\n", host_name );
      return -1;
    }

  free (host_name);

  if ((sock = socket (PF_INET, SOCK_STREAM, 0)) < 0)
    {
      sl_log_r (slconn, 1, 0, "[%s] socket(): %s\n", slconn->sladdr, slp_strerror ());
      slp_sockclose (sock);
      return -1;
    }

  /* Set non-blocking IO */
  if ( slp_socknoblock(sock) )
    {
      sl_log_r (slconn, 1, 0, "Error setting socket to non-blocking\n");
    }

  if ( (slp_sockconnect (sock, (struct sockaddr *) &addr, addrlen)) )
    {
      sl_log_r (slconn, 1, 0, "[%s] connect(): %s\n", slconn->sladdr, slp_strerror ());
      slp_sockclose (sock);
      return -1;
    }

  /* Give a second for connecting, this is stupid but needed at the moment.
     Some firewalls that proxy/NAT TCP connections will report that the TCP
     connection is ready even when they are still opening the "other" end.
     Maybe a new test should be implemented to test for readiness.
   */
  slp_usleep (1000000);

  /* Wait up to 10 seconds for the socket to be connected */
  if ((sockstat = sl_checksock (sock, 10, 0)) <= 0)
    {
      if (sockstat < 0)
	{			/* select() returned error */
	  sl_log_r (slconn, 1, 1, "[%s] socket connect error\n", slconn->sladdr);
	}
      else
	{			/* socket time-out */
	  sl_log_r (slconn, 0, 1, "[%s] socket connect time-out (10s)\n",
		    slconn->sladdr);
	}
      slp_sockclose (sock);
      return -1;
    }
  else
    {				/* socket connected */
      sl_log_r (slconn, 0, 1, "[%s] network socket opened\n", slconn->sladdr);

      /* Set the SO_KEEPALIVE socket option, not really useful in this case */
      if (setsockopt
	  (sock, SOL_SOCKET, SO_KEEPALIVE, (char *) &on, sizeof (on)) < 0)
	sl_log_r (slconn, 0, 1, "[%s] cannot set SO_KEEPALIVE socket option\n",
		  slconn->sladdr);

      slconn->link = sock;

      /* Everything should be connected, say hello */
      if (sl_sayhello (slconn) == -1)
	{
	  slp_sockclose (sock);
	  return -1;
	}

      return sock;
    }
}				/* End of sl_connect() */


/***************************************************************************
 * sl_disconnect():
 * Close the network socket associated with connection
 *
 * Returns -1, historically used to set the old descriptor
 ***************************************************************************/
int
sl_disconnect (SLCD * slconn)
{
  if (slconn->link != -1)
    {
      if (slp_sockclose (slconn->link))
	{
	  sl_log_r (slconn, 1, 1, "[%s] slp_sockclose(%d) failed: %s\n", slconn->sladdr,
		    slconn->link, slp_strerror ());
	}
      else
	{
	  slconn->link = -1;
	}

      sl_log_r (slconn, 0, 1, "[%s] network socket closed\n", slconn->sladdr);
    }

  return -1;
}				/* End of sl_disconnect() */


/***************************************************************************
 * sl_checksock():
 * Check a socket for write ability using select() and read ability
 * using recv(... MSG_PEEK).  Time-out values are also passed (seconds
 * and microseconds) for the select() call.
 *
 * Returns:
 *  1 = success
 *  0 = if time-out expires
 * -1 = errors
 ***************************************************************************/
int
sl_checksock (int sock, int tosec, int tousec)
{
  int sret;
  int ret = -1;			/* default is failure */
  char testbuf[1];
  fd_set checkset;
  struct timeval to;

  FD_ZERO (&checkset);
  FD_SET ((unsigned int)sock, &checkset);

  to.tv_sec = tosec;
  to.tv_usec = tousec;

  /* Check write ability with select() */
  if ((sret = select (sock + 1, NULL, &checkset, NULL, &to)) > 0)
    ret = 1;
  else if (sret == 0)
    ret = 0;			/* time-out expired */

  /* Check read ability with recv() */
  if (ret && (recv (sock, testbuf, sizeof (char), MSG_PEEK)) <= 0)
    {
      if (! slp_noblockcheck())
	ret = 1;		/* no data for non-blocking IO */
      else
	ret = -1;
    }

  return ret;
}				/* End of sl_checksock() */


/***************************************************************************
 * sl_senddata():
 * send() 'buflen' bytes from 'buffer' to 'slconn->link'.  'ident' is
 * a string to include in error messages for identification, usually
 * the address of the remote server.  If 'resp' is not NULL then read
 * up to 'resplen' bytes into 'resp' after sending 'buffer'.  This is
 * only designed for small pieces of data, specifically the server
 * responses to commands terminated by '\r\n'.
 *
 * Returns -1 on error, and size (in bytes) of the response
 * received (0 if 'resp' == NULL).
 ***************************************************************************/
int
sl_senddata (SLCD * slconn, void *buffer, size_t buflen,
	     const char *ident, void *resp, int resplen)
{
  
  int bytesread = 0;		/* bytes read into resp */
  
  if ( send (slconn->link, buffer, buflen, 0) < 0 )
    {
      sl_log_r (slconn, 1, 0, "[%s] error sending '%.*s '\n", ident,
		strcspn ((char *) buffer, "\r\n"), (char *) buffer);
      return -1;
    }
  
  /* If requested collect the response */
  if ( resp != NULL )
    {
      bytesread = sl_recvresp (slconn, resp, resplen, buffer, ident);
    }
  
  return bytesread;
}				/* End of sl_senddata() */


/***************************************************************************
 * sl_recvdata():
 * recv() 'maxbytes' data from 'slconn->link' into a specified
 * 'buffer'.  'ident' is a string to be included in error messages for
 * identification, usually the address of the remote server.
 *
 * Returns -1 on error/EOF, 0 for no available data and the number
 * of bytes read on success.
 ***************************************************************************/
int
sl_recvdata (SLCD * slconn, void *buffer, size_t maxbytes,
	     const char *ident)
{
  int bytesread = 0;
  
  if ( buffer == NULL )
    {
      return -1;
    }
  
  bytesread = recv (slconn->link, buffer, maxbytes, 0);

  if ( bytesread == 0 )		/* should indicate TCP FIN or EOF */
    {
      sl_log_r (slconn, 1, 1, "[%s] read():%d TCP FIN or EOF received\n",
		ident, bytesread);
      return -1;
    }
  else if ( bytesread < 0 )
    {
      if ( slp_noblockcheck() )
	{
	  sl_log_r (slconn, 1, 0, "[%s] recv():%d %s\n", ident, bytesread,
		    slp_strerror ());
	  return -1;
	}

      /* no data available for NONBLOCKing IO */
      return 0;
    }

  return bytesread;
}				/* End of sl_recvdata() */


/***************************************************************************
 * sl_recvresp():
 * To receive a response to a command recv() one byte at a time until
 * '\r\n' or up to 'maxbytes' is read from 'slconn->link' into a
 * specified 'buffer'.  The function will wait up to 30 seconds for a
 * response to be recv'd.  'command' is a string to be included in
 * error messages indicating which command the response is
 * for. 'ident' is a string to be included in error messages for
 * identification, usually the address of the remote server.
 *
 * Returns -1 on error/EOF and the number of bytes read on success.
 ***************************************************************************/
int
sl_recvresp (SLCD * slconn, void *buffer, size_t maxbytes,
	     const char *command, const char *ident)
{
  
  int bytesread = 0;		/* total bytes read */
  int recvret   = 0;            /* return from sl_recvdata */
  int ackcnt    = 0;		/* counter for the read loop */
  int ackpoll   = 50000;	/* poll at 0.05 seconds for reading */
  
  if ( buffer == NULL )
    {
      return -1;
    }
  
  /* Recv a byte at a time and wait up to 30 seconds for a response */
  while ( bytesread < maxbytes )
    {
      recvret = sl_recvdata (slconn, (char *)buffer + bytesread, 1, ident);
      
      if ( recvret > 0 )
	{
	  bytesread += recvret;
	}
      else if ( recvret < 0 )
	{
	  sl_log_r (slconn, 1, 0, "[%s] bad response to '%.*s'\n",
		    ident, strcspn ((char *) buffer, "\r\n"),
		    (char *) buffer);
	  return -1;
	}
      
      /* Trap door if '\r\n' is recv'd */
      if ( bytesread >= 2 &&
	   *(char *)((char *)buffer + bytesread - 2) == '\r' &&
	   *(char *)((char *)buffer + bytesread - 1) == '\n' )
	{
	  return bytesread;
	}
      
      /* Trap door if 30 seconds has elapsed, (ackpoll x 600) */
      if ( ackcnt > 600 )
        {
	  sl_log_r (slconn, 1, 0, "[%s] timeout waiting for response to '%.*s'\n",
		    ident, strcspn ((char *) buffer, "\r\n"),
		    (char *) buffer);
	  return -1;
	}
      
      /* Delay if no data received */
      if ( recvret == 0 )
	{
	  slp_usleep (ackpoll);
	  ackcnt++;
	}
    }
  
  return bytesread;
}				/* End of sl_recvresp() */
