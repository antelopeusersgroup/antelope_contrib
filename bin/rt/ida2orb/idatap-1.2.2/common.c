/* @(#)common.c	1.18 11/07/97 */
/*======================================================================
 *
 * IDA data exchange protocols, routines common to client and server.
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <errno.h>
#include <netdb.h>
#include <memory.h>
#include <signal.h>
#include <unistd.h>
#include <setjmp.h>
#include <time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <memory.h>
#include "xfer.h"
#include "util.h"

static jmp_buf Jmp_buf;
static volatile int jump_ok = 0;

static int connecting = 0;

static int slen = sizeof(u_short);
static int llen = sizeof(u_long);
u_short sval;
u_long lval;

/*============================ internal use ==========================*/

/* signal handlers */

void xfer_CatchSignal(sig)
int sig;
{
char pname[MAXPATHLEN+1];
static char *fid = "xfer_CatchSignal";


    util_log(connecting ? 2 : 1, "%s", util_sigtoa(sig));
    signal(sig, xfer_CatchSignal);

    if (sig == SIGPIPE) {

        xfer_errno = XFER_EPIPE;
        return;

    } else if (sig == SIGALRM) {

        if (jump_ok) {
            jump_ok = 0;
            util_log(2, "watchdog timer expired, break connection");
            longjmp(Jmp_buf, XFER_ETIMEDOUT);
        } else {
            util_log(1, "spurious alarm ignored");
            return;
        }

    } else {

        util_log(1, "unexpected signal (this should NEVER occur)");
        util_log(1, "abort(), core dumped (pwd = %s)", getwd(pname));
        abort();

    }
}

int xfer_Signals()
{
static int enabled = 0;
static char *fid = "xfer_Signals";

    if (++enabled > 1) return XFER_OK;

/* SIGPIPE handler */

    if (signal(SIGPIPE, (Sigfunc *) xfer_CatchSignal) == SIG_ERR) {
        xfer_errno = XFER_EHANDLER;
        return XFER_ERROR;
    }
    util_log(3, "SIGPIPE handler installed");

/* SIGALRM handler */

    if (signal(SIGALRM, (Sigfunc *) xfer_CatchSignal) == SIG_ERR) {
        xfer_errno = XFER_EHANDLER;
        return XFER_ERROR;
    }
    util_log(3, "SIGALRM handler installed");

    return XFER_OK;
}

/* read/write with timeout control via SIGALRM and longjump() */

long xfer_Read(fd, buffer, count)
int  fd;
char *buffer;
long count;
{
long remain, nr, status = 0;
char *ptr;

    if (setjmp(Jmp_buf) == 0) {

        jump_ok = 1;
        alarm(_xfer_timeout);

    /* read until all data have been transferred */

        ptr    = (char *) buffer;
        remain = count;
        while (remain != 0 && status == 0) {
            if ((nr = read(fd, ptr, remain)) < 0) {
                if (errno == EINTR) {
                    status = 0;
                } else {
                    xfer_errno = XFER_EIO;
                    status = -1;
                }
            } else {
                remain -= nr;
                ptr    += nr;
                xfer_nrecv += nr;
            }
        }
        status = count - remain;

    } else {

    /* longjmp()'d to here, must have timed out in the read */

        xfer_errno = XFER_ETIMEDOUT;
        status = -1;
    }

/* Clear interval timer */

    jump_ok = 0;
    alarm(0);

    return status;
}

long xfer_Write(fd, buffer, count)
int  fd;
char *buffer;
long count;
{
long remain, nw, status = 0;
char *ptr;

    if (setjmp(Jmp_buf) == 0) {

        jump_ok = 1;
        alarm(_xfer_timeout);

    /* write until all data have been transferred */

        ptr    = (char *) buffer;
        remain = count;
        while (remain != 0 && status == 0) {
            if ((nw = write(fd, ptr, remain)) < 0) {
                if (errno == EINTR) {
                    status = 0;
                } else if (errno == EPIPE) {
                    xfer_errno = XFER_EPIPE;
                    status = -1;
                } else {
                    xfer_errno = XFER_EIO;
                    status = -1;
                }
            } else {
                remain -= nw;
                ptr    += nw;
                xfer_nsend += nw;
            }
        }
        status = count - remain;

    } else {

    /* longjmp()'d to here, must have timed out in the write */

        xfer_errno = XFER_ETIMEDOUT;
        status = -1;
    }

/* Clear interval timer */

    jump_ok = 0;
    alarm(0);

    return status;
}

/* Send a message */

int xfer_SendMsg(sd, message_plus, msglen)
int sd;
char *message_plus;
long msglen;
{
long len;
static char *fid = "xfer_SendMsg";

/* Stuff the first 4 bytes with the message length */

    lval = htonl(msglen);
    memcpy(message_plus, &lval, 4);

/* Do a single write to send the byte count and message */

    len = msglen + 4;
    if (xfer_Write(sd, message_plus, len) == len) return XFER_OK;

/* Must be an error at this point */

    return XFER_ERROR;
}

/* Receive a message */

int xfer_RecvMsg(sd, message, maxlen, len)
int sd;
char *message;
long maxlen;
long *len;
{
int msglen;
static char *fid = "xfer_RecvMsg";

/* Get the message length */

    if (maxlen < llen) {
        util_log(1, "%s: message buffer too small (%d < %d)",
            fid, maxlen, llen
        );
        xfer_errno = XFER_ELIMIT;
        return XFER_ERROR;
    }

    if (xfer_Read(sd, (char *) &lval, llen) != llen) return XFER_ERROR;

    *len = msglen = (size_t) ntohl(lval);

/* Zero length messages are OK */

    if (msglen == 0) {
        util_log(3, "BREAK received");
        return XFER_OK;
    }

/* Otherwise make sure we have enough buffer space to hold the message */

    if (msglen > maxlen) {
        util_log(1, "%s: incoming message too big (msglen=%d, maxlen=%d)",
            fid, msglen, maxlen
        );
        xfer_errno = XFER_ETOOBIG;
        return XFER_ERROR;
    }

/* Read the message */

    if (xfer_Read(sd, message, msglen) == msglen) return XFER_OK;

/* Must be an error at this point */

    return XFER_ERROR;
}

/* Convert from epoch time to xfer_time */

struct xfer_time *xfer_time(dtime)
double dtime;
{
static struct xfer_time ntime;
static char *fid = "xfer_time";

    ntime.sec =  (long) dtime;
    ntime.usec = (u_long) ((dtime - (double) ntime.sec)*(double) 1000000.0);

    return &ntime;
}

/* Convert from xfer_time to epoch time */

double xfer_dtime(ntime)
struct xfer_time *ntime;
{
static char *fid = "xfer_dtime";

    return (double) ntime->sec + ((double) ntime->usec/(double) 1000000.0);
}

/*============================ External use ==========================*/

char *Xfer_ErrStr()
{
static char defmsg[] = "unknown error 0x00000000 plus some extra";
static char *fid = "Xfer_ErrStr";

    if (xfer_errno == XFER_EPROTOCOL) {
        return "unsupported protocol";
    } else if (xfer_errno == XFER_EREQUEST) {
        return "unrecognized request code";
    } else if (xfer_errno == XFER_EFORMAT) {
        return "unsupported format";
    } else if (xfer_errno == XFER_EREFUSED) {
        return "unauthorized connection refused by server";
    } else if (xfer_errno == XFER_ELIMIT) {
        return "implementation defined limit exceeded";
    } else if (xfer_errno == XFER_EINVAL) {
        return "illegal data received";
    } else if (xfer_errno == XFER_ETOOBIG) {
        return "message too large to receive";
    } else if (xfer_errno == XFER_ETIMEDOUT) {
        return "connection timed out";
    } else if (xfer_errno == XFER_ECONNECT) {
        return "no connection with peer";
    } else if (xfer_errno == XFER_EIO) {
        return "I/O error";
    } else if (xfer_errno == XFER_EPIPE) {
        return "no peer";
    } else if (xfer_errno == XFER_EREJECT) {
        return "request rejected by server";
    } else if (xfer_errno == XFER_EHANDLER) {
        return "unable to install signal handler";
    } else if (xfer_errno == XFER_ENOSUCH) {
        return "none of the requested stations/channels are available";
    } else if (xfer_errno == XFER_EBUSY) {
        return "server busy";
    } else if (xfer_errno == XFER_EFAULT) {
        return "server fault";
    } else if (xfer_errno == XFER_EHOSTNAME) {
        return "can't resolve hostname/address";
    }

    sprintf(defmsg, "unknown error 0x%x", xfer_errno);
    return defmsg;
}

/* Shutdown and exit */

void Xfer_Exit(sd, status)
int sd;
int status;
{
static char *fid = "Xfer_Exit";

    if (sd > 0) close(sd);
    util_log(1, "exit %d", status);
    exit(status);
}

/* Establish a connection with the server and send the request
 * These really belong in client.c, but are included here in order
 * to be able to use the timeout facility.  The reason we want to
 * use the alarm() based timeout for the connect(), is that we need
 * to be able to have better control over the timeout behavior.  Most
 * connect()'s use a logic that cause it to wait increasingly longer
 * times when re-trying timed out connections.  This can cause problems
 * for on-demand TCP/IP connections that have a significant delay at
 * startup.
 *
 * The connected socket descriptor is returned upon success.  If
 * the conection fails because of a transient error (ie, timeout)
 * the 0 is returned.  Other errors cause a negative value to
 * be returned, and well-behaved clients will give up at this point.
 */

extern int h_errno;
#define DEFAULT_PROTOCOL "tcp"

int xfer_connect(server, service, port, protocol, sndbuf, rcvbuf, tto)
char *server;
char *service;
int  port;
char *protocol;
int sndbuf;
int rcvbuf;
int tto;
{
unsigned long addr;
int sd, addrlen;
struct hostent *hp;
struct servent *sp;
char *host;
char *ipaddress;
int val, ilen;
time_t begin, now;
struct sockaddr_in peer_addr;
static char *fid = "xfer_connect";

    if (server == (char *) NULL) {
        util_log(1, "%s: remote host is not defined!", fid);
        errno = EINVAL;
        return -1;
    }

/*  Address of server is taken from host name  */

    if ((hp = gethostbyname(server)) == NULL) {
        switch (h_errno) {
          case HOST_NOT_FOUND:
            if ((addr = inet_addr(server)) == -1) {
                util_log(1, "%s: %s: host not found", fid, server);
            } else if ((hp = gethostbyaddr((char *)&addr,sizeof(addr),AF_INET)) == NULL) {
                util_log(1, "%s: can't find host `%s'", fid, server);
            }
            break;
          case TRY_AGAIN:
            util_log(1, "%s: gethostbyname: %s: try again later",
                fid, server    
            );
            break;
          case NO_DATA:
            util_log(1, "%s: %s: no address available", fid, server);
            break;
          default:
            util_log(1, "%s: gethostbyname: %s", fid, syserrmsg(errno));
        }
        xfer_errno = XFER_EHOSTNAME;
        return -2;
    }

/*  If communications protocol is not specified, use the default */

    if (protocol == NULL) protocol = DEFAULT_PROTOCOL;

/*  If service name is given, lookup port number  */

    if (service != NULL) {
        if ((sp = getservbyname(service, protocol)) == NULL) {
            util_log(1, "%s: getservbyname(%s, %s): %s",
                fid, service, protocol, syserrmsg(errno)
            );
            return -3;
        }
        port = ntohs(sp->s_port);
    } else if (port <= 0) {
        util_log(1, "%s: both service and port are undefined!", fid);
        return -4;
    }

/* Connect */

    memcpy(&peer_addr.sin_addr, hp->h_addr, hp->h_length);
    peer_addr.sin_family = AF_INET;
    peer_addr.sin_port   = htons(port);

    val  = 1;
    ilen = sizeof(int);

    sd = -1;
    begin = now = time(NULL);
    if (tto <= 0) tto = _xfer_timeout;

    while (sd < 0 && now - begin < _xfer_timeout) {

    /* Create socket */

        if ((sd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
            util_log(1, "%s: socket: %s", fid, syserrmsg(errno));
            return -5;
        }

    /* Set socket options */

        if (setsockopt(sd,SOL_SOCKET,SO_KEEPALIVE,(char *) &val,ilen) != 0){
            util_log(1, "%s: warning: setsockopt(keepalive): %s",
                fid, syserrmsg(errno)
            );
        }

        if (sndbuf > 0) {
            if (
                setsockopt(
                    sd, SOL_SOCKET, SO_SNDBUF, (char *) &sndbuf, ilen
                ) != 0
            ) {
                util_log(1, "%s: warning: setsockopt(sndbuf = %d): %s",
                    fid, sndbuf, syserrmsg(errno)
                );
            } else {
                util_log(2, "socket sndbuf = %d bytes", sndbuf);
            }
        }

        if (rcvbuf > 0) {
            if (
                setsockopt(
                    sd, SOL_SOCKET, SO_RCVBUF, (char *) &rcvbuf, ilen
                ) != 0
            ) {
                util_log(1, "%s: warning: setsockopt(rcvbuf = %d): %s",
                    fid, rcvbuf, syserrmsg(errno)
                );
            } else {
                util_log(2, "socket rcvbuf = %d bytes", rcvbuf);
            }
        }

    /* Do the connection */

        if (setjmp(Jmp_buf) == 0) {

            if (service != NULL) {
                util_log(2, "contact %s.%s, to = %d/%d",
                    server, service, tto, _xfer_timeout
                );
            } else {
                util_log(2, "contact %s.%d, to = %d/%d",
                    server, port, tto, _xfer_timeout
                );
            }

            jump_ok = 1;
            alarm(tto);

            if (
                connect(
                    sd,
                    (struct sockaddr *) &peer_addr,
                    sizeof(struct sockaddr_in)
                ) != 0
            ) {
                close(sd);
                util_log(3, "%s: close(sd)", fid);
                sd = -1;
                if (errno != ETIMEDOUT) {
                    xfer_errno = XFER_ECONNECT;
                    return -6;
                }
            }

        } else {

            /* longjmp()'d to here, must have timed out in the connect */

            close(sd);
            util_log(2, "%s: connect() tto time out (%d)", fid, tto);
            util_log(3, "%s: close(sd)", fid);
            sd = -1;
            errno = ETIMEDOUT;
        }

        alarm(0);
        jump_ok = 0;

        now = time(NULL);
    }

    if (sd < 0) {
        util_log(2, "%s: connect() global time out (%d)",
            fid, _xfer_timeout
        );
        xfer_errno = XFER_ETIMEDOUT;
        errno = ETIMEDOUT;
        return 0;
    }

/* Successful connect at this point... report details */

    addrlen = sizeof(peer_addr);

    if (getpeername(sd, (struct sockaddr *)&peer_addr, &addrlen) == 0) {
        hp = gethostbyaddr((char *) &peer_addr.sin_addr, sizeof(struct in_addr),
                peer_addr.sin_family);
        ipaddress = inet_ntoa(peer_addr.sin_addr);
        host      = (hp != NULL) ? hp->h_name : NULL;
        if (host != NULL) {
            util_log(1, "connected to %s.%d", host, port);
        } else {
            util_log(1, "connected to %s.%d", ipaddress, port);
        }
    } else {
        util_log(1, "%s: warning: getpeername: %s", fid, syserrmsg(errno));
    }

    return sd;
}

int Xfer_Connect2(host, service, port, proto, req, cnf, retry, tto)
char *host;
char *service;
int port;
char *proto;
struct xfer_req *req;
struct xfer_cnf *cnf;
int retry;
int tto;
{
int sd;
unsigned long count = 0;
static char *fid = "Xfer_Connect2";

/* Set the global socket I/O timeout */

    if (req->timeout < XFER_MINTO) {
        util_log(1, "%s: override requested timeout of %d by minimum %d",
            fid, req->timeout, XFER_MINTO
        );
        req->timeout = XFER_MINTO;
    }
    _xfer_timeout = req->timeout;

/* Install signal handlers */

    if (xfer_Signals() != XFER_OK) return -1;

/* If socket buffer lengths are not explicity given, use our defaults */

    if (req->sndbuf <= 0) req->sndbuf = XFER_SO_SNDBUF;
    if (req->rcvbuf <= 0) req->rcvbuf = XFER_SO_RCVBUF;

/* Try to connect once or forever, depending on state of retry flag.
 * Note, a retry of -1 means try forever, regardless of error code.
 * If we do retry, we will sleep for 1, then 2, then 3 times the
 * the timeout interval before attempting again.  After 3 trys we
 * reset the sleep interval back to zero and repeat.
 */

    do {

        sleep((count++ % 4) * _xfer_timeout);

        connecting = 1;
        sd = xfer_connect(
            host, service, port, proto, req->sndbuf, req->rcvbuf, tto
        );
        connecting = 0;

    /* Negative return means something serious happened... quit */

        if (sd < 0 && retry != -1) return sd;

    /* Positive return means we connected... send the request */

        if (sd > 0) {
            util_log(1, "sending request");
            if (xfer_SendReq(sd, req) != XFER_OK) {
                util_log(2, "%s: xfer_SendReq failed: %s",
                    fid, Xfer_ErrStr()
                );
                close(sd);
                if (xfer_errno == XFER_ETIMEDOUT || retry < 0) {
                    sd = 0;
                } else {
                    retry = 0;
                    sd = -1;
                }
            } else {
                util_log(1, "waiting for acknowledgment");
                if (xfer_RecvCnf(sd, cnf) != XFER_OK) {
                    util_log(2, "%s: xfer_RecvCnf failed: %s",
                        fid, Xfer_ErrStr()
                    );
                    close(sd);
                    if (xfer_errno == XFER_ETIMEDOUT || retry < 0) {
                        sd = 0;
                    } else {
                        retry = 0;
                        sd = -1;
                    }
                } else {
                    util_log(1, "request accepted");
                }
            }
        }

    } while (retry && sd == 0);

/* Return the socket descriptor of the connection */

    return sd ? sd : -1;
}
