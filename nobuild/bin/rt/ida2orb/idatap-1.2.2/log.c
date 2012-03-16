/* @(#)log.c	2.8 03/14/97 */
/*======================================================================
 *
 *  MT-safe (Solaris threads) version of the message logging utilities.
 *
 *  It is assumed that the util_logopen() function is called by the
 *  main thread, prior to the possiblity for any other threads to call
 *  util_log().
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <stdio.h>
#include <syslog.h>
#include <stdarg.h>
#include <signal.h>
#include <errno.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>
#include <sys/file.h>
#if defined (SOLARIS) && defined (_REENTRANT)
#include <thread.h>
#include <synch.h>
#endif
#include "util.h"

#define STRLEN  64
#define HOSTNAMLEN 128
#define LOGBUFLEN  ((size_t) 512)
#define DEFTIMEFMT "%y:%j-%H:%M:%S"

static char *Tfmt = DEFTIMEFMT;
static int Min_level, Max_level, Log_level;
static FILE *Fp = NULL;
static int Fd = -1;
static char Prefix[LOGBUFLEN];
static char Hostname[HOSTNAMLEN];
static char Message[LOGBUFLEN];
static char buf1[LOGBUFLEN], buf2[LOGBUFLEN];
static char *Crnt_msg, *Prev_msg;
static int Pid;
static int Count = 0;

#if defined (SOLARIS) && defined (_REENTRANT)
static mutex_t util_log_mutex;
#else
#define mutex_t int
#define mutex_init(a,b,c)
#define mutex_lock(a)
#define mutex_unlock(a)
#endif

void _util_log_catch_sig(sig)
int sig;
{

    util_log(Min_level, "%s", util_sigtoa(sig));

    if (sig == SIGUSR1) {
        signal(SIGUSR1, (Sigfunc *) _util_log_catch_sig);
        return;
    } else if (sig == SIGUSR2) {
        util_log(0, "log level %d", Log_level = Min_level);
        signal(SIGUSR2, (Sigfunc *) _util_log_catch_sig);
        return;
    } else {
        util_log(Min_level, "signal ignored");
        return;
    }
}

void util_incrloglevel()
{
    mutex_lock(&util_log_mutex);
    if (Log_level < Max_level) {
        ++Log_level;
        mutex_unlock(&util_log_mutex);
        util_log(Min_level, "increment to log level %d", Log_level);
    } else {
        mutex_unlock(&util_log_mutex);
        util_log(Min_level, "log level %d (max)", Log_level);
    }
}

void util_rsetloglevel()
{
    mutex_lock(&util_log_mutex);
    Log_level = Min_level;
    mutex_unlock(&util_log_mutex);
    util_log(0, "log level %d", Log_level);
}

int util_logopen(
    char *file,
    int min_level,
    int max_level,
    int log_level,
    char *tfmt, 
    char *fmt, 
    ...
){
va_list ap;
char *ptr;

    mutex_init(&util_log_mutex, USYNC_THREAD, NULL);

    if (min_level>max_level || log_level<min_level || log_level>max_level) {
        errno = EINVAL;
        return -1;
    }

    Min_level = min_level;
    Max_level = max_level;
    Log_level = log_level;
    gethostname(Hostname, HOSTNAMLEN);
    Pid = getpid();

/*  Open log file  */

    if (file == NULL) {
        Fp = stderr;
    } else if (strcmp(file, "syslogd") == 0) {
#ifdef BSDOS
        openlog(NULL, LOG_CONS, LOG_LOCAL0);
#else
        openlog(NULL, LOG_CONS, LOG_USER);
#endif
    } else if (Fp != NULL) {
        errno = EALREADY;
        return -1;
    } else if (strcmp(file, "-") == 0) {
        Fp = stdout;
    } else if ((Fp = fopen(file, "a+")) == NULL) {
        return -1;
    }
    Fd = (Fp == NULL) ? 0 : fileno(Fp);

    if (tfmt != NULL && (Tfmt = strdup(tfmt)) == NULL) return -1;

/*  Set up signal handler for modifying log levels  */

    if (signal(SIGUSR1, (Sigfunc *) _util_log_catch_sig) == SIG_ERR) return -1;
    if (signal(SIGUSR2, (Sigfunc *) _util_log_catch_sig) == SIG_ERR) return -1;

/*  Initialize the message buffers  */

    memset((void *) buf1, 0, LOGBUFLEN); Crnt_msg = buf1;
    memset((void *) buf2, 0, LOGBUFLEN); Prev_msg = buf2;

/*  Create the message prefix  */

    memset((void *) Prefix, 0, LOGBUFLEN);
    if (fmt != (char *) NULL) {
        if (Fd == 0 && tfmt != NULL) {  /* using syslogd */
            sprintf(Prefix, "%s ", tfmt); /* overloaded... syscode */
            ptr = Prefix + strlen(Prefix);
        } else {
            ptr = Prefix;
        }
        va_start(ap, fmt);
        vsprintf(ptr, fmt, ap);
        va_end(ap);
    } else {
        Prefix[0] = (char) 0;
    }

    return 0;

}

void util_logclose(
    void
){

    mutex_lock(&util_log_mutex);

    if (Fp != NULL && Fp != stdout && Fp != stderr) fclose(Fp);
    Fp = NULL;

    mutex_unlock(&util_log_mutex);

    return;
}

void util_log(
    int level, 
    char *fmt, 
    ...
){
va_list ap;
char *ptr;
static time_t ltime;
static char preamble[LOGBUFLEN];
static char timstr[STRLEN];

    mutex_lock(&util_log_mutex);

    if (Fd != 0 && Fp == NULL) {
        mutex_unlock(&util_log_mutex);
        return;
    }

/*  Don't print anything which is above the desired log level  */

    if (level > Log_level) {
        mutex_unlock(&util_log_mutex);
        return;
    }

/*  Build the message string  */

    memset((void *) Crnt_msg, 0, LOGBUFLEN);
    if (Fd == 0) { /* using syslogd */
        sprintf(Crnt_msg, "%s[%d]: ", Prefix, Pid);
        ptr = Crnt_msg + strlen(Crnt_msg);
    } else {
        ptr = Crnt_msg;
    }

    va_start(ap, fmt);
    vsprintf(ptr, fmt, ap);
    va_end(ap);

/*  If using syslogd then we are done  */

    if (Fd == 0) {
        syslog(LOG_INFO, Crnt_msg);
        mutex_unlock(&util_log_mutex);
        return;
    }

    if (Crnt_msg[strlen(Crnt_msg)] != '\n') Crnt_msg[strlen(Crnt_msg)] = '\n';

/*  Don't print duplicate messages  */

    if (strcmp(Crnt_msg, Prev_msg) == 0) {
        ++Count;
        mutex_unlock(&util_log_mutex);
        return;
    }

/*  Build the message preamble  */

    ltime = time(NULL);
    if (Fp != stderr && Fp != stdout) {
        util_wlockw(Fd, 0, SEEK_SET, 0);
        fseek(Fp, 0, SEEK_END);
    }

    if (strftime(timstr, STRLEN, Tfmt, localtime(&ltime))) {
        sprintf(preamble, "%s ", timstr);
    } else {
        sprintf(preamble, "CAN'T DETERMINE TIME STRING! ");
    }

    sprintf(preamble + strlen(preamble), "%s ",  Hostname);
    sprintf(preamble + strlen(preamble), "%s",   Prefix);
    sprintf(preamble + strlen(preamble), "[%d]", Pid);

/*  Print the message  */

    if (Count > 1) {
        fprintf(Fp, "%s ", preamble);
        fprintf(Fp, "previous message repeated %d times\n", Count);
        Count = 0;
    }

    fprintf(Fp, "%s %s", preamble, Crnt_msg);

    fflush(Fp);
    if (Fp != stderr && Fp != stdout) util_unlock(Fd, 0, SEEK_SET, 0);

/*  Save this message for comparison with the next one  */

    ptr      = Prev_msg;
    Prev_msg = Crnt_msg;
    Crnt_msg = ptr;

    mutex_unlock(&util_log_mutex);
    return;
}

#ifdef DEBUG_TEST

main(argc, argv)
int argc;
char *argv[];
{
int i;

    if (argc != 2) {
        fprintf(stderr, "usage: %s logfilename\n", argv[0]);
        exit(1);
    }

    if (strcmp(argv[1], "syslogd") == 0) {
        util_logopen(argv[1], 1, 9, 1, "SYSCODE", argv[0]);
    } else {
        util_logopen(argv[1], 1, 9, 1, NULL, argv[0]);
    }

    for (i = 0; i < 1024; i++) util_log(1, "this is a duplicate message");

    while (1) {
        util_log(4, "log entry %d", i++);
        sleep(1);
    }
}
#endif
