/* @(#)globals.c	1.10 03/18/97 */
/*======================================================================
 *
 * Declaration of xfer library globals
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include "xfer.h"

int xfer_errno    = 0;
int _xfer_timeout = 60;
unsigned long xfer_nsend = 0;
unsigned long xfer_nrecv = 0;

union xfer_buffer _Xfer_Buffer;
char *Xfer_Buffer = (char *) &_Xfer_Buffer;
int Xfer_Buflen   = sizeof(_Xfer_Buffer);
