/* @(#)util.h	2.12 01/31/97 */
/*======================================================================
 *
 *  include/util.h
 *
 *  Defines, data structures, and function prototypes for use 
 *  with the util library.
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#ifndef util_h_included
#define util_h_included

#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <string.h>

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

/*  Typical machine byte orders  */

#ifndef LTL_ENDIAN_ORDER
#define LTL_ENDIAN_ORDER 0x10325476  /* VAX, 80x86   */
#endif

#ifndef BIG_ENDIAN_ORDER
#define BIG_ENDIAN_ORDER 0x76543210  /* Sun, MC680x0 */
#endif

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef SEEK_SET
#define SEEK_SET 0
#endif

#ifndef SEEK_CUR
#define SEEK_CUR 1
#endif

#ifndef SEEK_END
#define SEEK_END 2
#endif

/* Macros */

#ifdef strerror
#define syserrmsg(code) strerror(code)
#else
char *syserrmsg(int);
#endif

#ifndef leap_year
#define leap_year(i) ((i % 4 == 0 && i % 100 != 0) || i % 400 == 0)
#endif

#ifndef daysize
#define daysize(i) (365 + leap_year(i))
#endif

#define year_secs(year) (long) util_ydhmsmtod((int)year, 0, 0, 0, 0, 0)

#ifdef MSDOS
#include <fcntl.h>
#include <io.h>
#define SETMODE(fd, mode) setmode((fd), (mode))
#else
#define SETMODE(fd, mode) 1
#ifndef O_TEXT
#define O_TEXT 0x4000
#endif
#ifndef O_BINARY
#define O_BINARY 0x8000
#endif
#endif /* ifdef MSDOS */

/* Various record locking macros */

#define util_rlock(fd, offset, whence, len) \
        util_lock(fd, F_SETLK, F_RDLCK, offset, whence, len)

#define util_wlock(fd, offset, whence, len) \
        util_lock(fd, F_SETLK, F_WRLCK, offset, whence, len)

#define util_rlockw(fd, offset, whence, len) \
        util_lock(fd, F_SETLKW, F_RDLCK, offset, whence, len)

#define util_wlockw(fd, offset, whence, len) \
        util_lock(fd, F_SETLKW, F_WRLCK, offset, whence, len)

#define util_unlock(fd, offset, whence, len) \
        util_lock(fd, F_SETLKW, F_UNLCK, offset, whence, len)

/*  Misc. useful constants  */

#define BYTES_PER_KBYTE 1024
#define BYTES_PER_MBYTE 1048576
#define BYTES_PER_GBYTE 1073741824

/* Typedefs */

typedef void    Sigfunc(int);

/*  Function prototypes  */

#ifdef __STDC__

/* Not yet converted to util_xxx form for one reason or another */
#ifdef strerror
#define syserrmsg(code) strerror(code)
#else
char *syserrmsg(int);
#endif
int isfloat(char *);
int isinteger(char *);
int word_order(void);
double attodt(char *);
char *dttostr(double, int);
char *lttostr(long, int);
void tsplit(double, int *, int *, int *, int *, int *, int *);
double ydhmsmtod(int, int, int, int, int, int);
int jdtomd(int, int, int *, int *);
int ymdtojd(int, int, int);
long today(void);
int parse(FILE *, char **, char *, int);

long util_atolk(
    char *
);

double util_attodt(
    char *
);

int util_bindmp(
    unsigned char *,
    long,
    long,
    char
);

int util_bground(
    int,
    int
);

int util_cat(
    char *,
    char *,
    char *
);

int util_connect(
    char *,
    char *,
    int,
    char *,
    int,
    int
);

int util_cfgpair(
    FILE *,
    int  *,
    char **,
    char **
);

long util_chksum(
    long *,
    long
);

void util_clip(
    long *,
    long,
    long
);

long util_cm6(
    long *,
    char *,
    long,
    long,
    int,
    int
);

long util_cm8(
    long *,
    char *,
    long,
    long,
    int,
    int
);

long util_csteim1(
    char *,
    long,
    long *,
    long,
    long *
);

long util_dcm6(
    char *,
    long *,
    long,
    long,
    int
);

long util_dcm8(
    char *,
    long *,
    long,
    long,
    int
);

int util_dsteim1(
    long *,
    long,
    char *,
    long,
    u_long order,
    long
);

char *util_dttostr(
    double,
    int
);

int util_email(
    char *,
    char *,
    char *
);

void util_fstdif(
    long *,
    long
);

int util_getline(
    FILE *,
    char *,
    int,
    char,
    int *
);

int util_hexdmp(
    unsigned char *,
    long,
    long,
    char
);

void util_iftovf(
    unsigned long *,
    long
);

int util_jdtomd(
    int,
    int,
    int *,
    int *
);

char *util_lcase(
    char *
);

int util_lenprt(
    FILE *,
    char *,
    int,
    char
);

int util_lock(
    int,
    int,
    int,
    off_t,
    int,
    off_t
);

void util_log(
    int, 
    char *, 
    ...
);

void util_logclose(
    void
);

int util_logopen(
    char *,
    int,
    int,
    int,
    char *,
    char *,
    ...
);

void util_incrloglevel(
    void
);

void util_rsetloglevel(
    void
);

long util_lcomp(
    char *,
    long *,
    long
);

long util_ldcmp(
    long *,
    char *,
    long
);

void util_lswap(
    long *,
    long
);

char *util_lttostr(
    long,
    int
);

int util_mkpath(
    char *,
    int
);

int util_move(
    char *,
    char *
);

int util_octdmp(
    unsigned char *,
    long,
    long,
    char
);

unsigned long util_order(
    void
);

int util_parse(
    char *,
    char **,
    char *,
    int,
    char
);

int util_powerof(
    long,
    int
);

int util_query(
    char *
);

long util_read(
    int,
    void *,
    long,
    int
);

void util_rmfdif(
    long *,
    long
);

long util_scomp(
    char *,
    short *,
    long
);

short util_sdcmp(
    short *,
    char *,
    long
);

void util_sockinfo(
    int,
    int
);

int util_sparse(
    char *,
    char **,
    char *,
    int
);

void util_sswap(
    short *,
    long
);

long util_today(
    void
);

void util_tsplit(
    double,
    int *,
    int *,
    int *,
    int *,
    int *,
    int *
);

char *util_ucase(
    char *
);

void util_vftoif(
    unsigned long *,
    long
);

long util_write(
    int,
    void *,
    long,
    int
);

double util_ydhmsmtod(
    int,
    int,
    int,
    int,
    int,
    int
);

int util_ymdtojd(
    int,
    int,
    int
);

#else

#endif /* ifdef __STDC__ */

#endif
