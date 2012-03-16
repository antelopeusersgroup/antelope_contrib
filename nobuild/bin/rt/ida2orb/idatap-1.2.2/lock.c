/* @(#)lock.c	2.2 01/28/97 */
/*======================================================================
 *
 *  lock.c
 *
 *  Record locking utilities.
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include "util.h"

int util_lock(fd, cmd, type, offset, whence, len)
int fd;
int cmd;
int type;
off_t offset;
int whence;
off_t len;
{
struct flock lock;

    lock.l_type   = type;
    lock.l_start  = offset;
    lock.l_whence = whence;
    lock.l_len    = len;

    return fcntl(fd, cmd, &lock);
}
