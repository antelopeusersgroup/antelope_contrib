/* @(#)signal.c	2.3 4/4/96 */
/*======================================================================
 *
 *  signal.c
 *
 *  Reliable version of signal(), using POSIX sigaction().
 *
 *  Copied from Advanced Programming in the UNIX Environment,
 *  W. Richard Stevens, page 298.
 *
 *====================================================================*/
#include <signal.h>
#include <stdio.h>
#include "util.h"

Sigfunc *util_signal(signo, func)
int signo;
Sigfunc *func;
{
struct sigaction act, oact;

    act.sa_handler = func;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    if (signo == SIGALRM) {
#ifdef SA_INTERRUPT
        act.sa_flags |= SA_INTERRUPT;  /* SunOS */
#endif
    } else {
#ifdef SA_RESTART
        act.sa_flags |= SA_RESTART;    /* SVR, 4.3+BSD */
#endif
    }
    if (sigaction(signo, &act, &oact) < 0) return SIG_ERR;

    return oact.sa_handler;
}

/*======================================================================
 *
 *  Convert signal codes to strings.
 *
 *====================================================================*/

#define STRLEN  64

char *util_sigtoa(signal)
int signal;
{
static char string[STRLEN];

    if (signal == SIGHUP) {
        sprintf(string, "SIGHUP");
    } else if (signal == SIGINT) {
        sprintf(string, "SIGINT");
    } else if (signal == SIGQUIT) {
        sprintf(string, "SIGQUIT");
    } else if (signal == SIGILL) {
        sprintf(string, "SIGILL");
    } else if (signal == SIGTRAP) {
        sprintf(string, "SIGTRAP");
    } else if (signal == SIGIOT) {
        sprintf(string, "SIGIOT");
    } else if (signal == SIGABRT) {
        sprintf(string, "SIGABRT");
#ifdef SIGEMT
    } else if (signal == SIGEMT) {
        sprintf(string, "SIGEMT");
#endif
    } else if (signal == SIGFPE) {
        sprintf(string, "SIGFPE");
    } else if (signal == SIGKILL) {
        sprintf(string, "SIGKILL");
    } else if (signal == SIGBUS) {
        sprintf(string, "SIGBUS");
    } else if (signal == SIGSEGV) {
        sprintf(string, "SIGSEGV");
    } else if (signal == SIGSYS) {
        sprintf(string, "SIGSYS");
    } else if (signal == SIGPIPE) {
        sprintf(string, "SIGPIPE");
    } else if (signal == SIGALRM) {
        sprintf(string, "SIGALRM");
    } else if (signal == SIGTERM) {
        sprintf(string, "SIGTERM");
#ifdef SIGURG
    } else if (signal == SIGURG) {
        sprintf(string, "SIGURG");
#endif
    } else if (signal == SIGSTOP) {
        sprintf(string, "SIGSTOP");
    } else if (signal == SIGTSTP) {
        sprintf(string, "SIGTSTP");
    } else if (signal == SIGCONT) {
        sprintf(string, "SIGCONT");
    } else if (signal == SIGCHLD) {
        sprintf(string, "SIGCHLD");
#ifdef SIGCLD
    } else if (signal == SIGCLD) {
        sprintf(string, "SIGCLD");
#endif
    } else if (signal == SIGTTIN) {
        sprintf(string, "SIGTTIN");
    } else if (signal == SIGTTOU) {
        sprintf(string, "SIGTTOU");
#ifdef SIGIO
    } else if (signal == SIGIO) {
        sprintf(string, "SIGIO");
#endif
#ifdef SIGPOLL
    } else if (signal == SIGPOLL) {
        sprintf(string, "SIGPOLL");
#endif
    } else if (signal == SIGXCPU) {
        sprintf(string, "SIGXCPU");
    } else if (signal == SIGXFSZ) {
        sprintf(string, "SIGXFSZ");
    } else if (signal == SIGVTALRM) {
        sprintf(string, "SIGVTALRM");
    } else if (signal == SIGPROF) {
        sprintf(string, "SIGPROF");
    } else if (signal == SIGWINCH) {
        sprintf(string, "SIGWINCH");
#ifdef SIGLOST
    } else if (signal == SIGLOST) {
        sprintf(string, "SIGLOST");
#endif
    } else if (signal == SIGUSR1) {
        sprintf(string, "SIGUSR1");
    } else if (signal == SIGUSR2) {
        sprintf(string, "SIGUSR2");
    } else {
        sprintf(string, "signal %d", signal);
    }

    return string;
}
