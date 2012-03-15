/* %W% %G% */
/*======================================================================
 *
 *  Read/write xfer_packets.
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <stdio.h>
#include <errno.h>
#include <memory.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include "xfer.h"
#include "util.h"

static size_t slen = XFER_SNAMLEN + 1;
static size_t clen = XFER_CNAMLEN + 1;
static size_t ilen = XFER_INAMLEN + 1;
static size_t Dlen = sizeof(double);
static size_t Flen = sizeof(float);
static size_t Ilen = sizeof(int);
static size_t Llen = sizeof(long);
static size_t Slen = sizeof(short);
static size_t Ulen = sizeof(unsigned long);

int Xfer_WrtPacket(fp, packet)
FILE *fp;
struct xfer_packet *packet;
{
long dlen;

    dlen = packet->nsamp * sizeof(long);

    if (fwrite(packet->sname,     1, slen, fp) != slen) return  -1;
    if (fwrite(&packet->lat,      1, Flen, fp) != Flen) return  -2;
    if (fwrite(&packet->lon,      1, Flen, fp) != Flen) return  -3;
    if (fwrite(&packet->elev,     1, Flen, fp) != Flen) return  -4;
    if (fwrite(&packet->depth,    1, Flen, fp) != Flen) return  -5;
    if (fwrite(packet->cname,     1, clen, fp) != clen) return  -6;
    if (fwrite(packet->instype,   1, ilen, fp) != ilen) return  -7;
    if (fwrite(&packet->sint,     1, Flen, fp) != Flen) return  -8;
    if (fwrite(&packet->calib,    1, Flen, fp) != Flen) return  -9;
    if (fwrite(&packet->calper,   1, Flen, fp) != Flen) return -10;
    if (fwrite(&packet->vang,     1, Flen, fp) != Flen) return -11;
    if (fwrite(&packet->hang,     1, Flen, fp) != Flen) return -12;
    if (fwrite(&packet->beg,      1, Dlen, fp) != Dlen) return -13;
    if (fwrite(&packet->tear,     1, Ilen, fp) != Ilen) return -14;
    if (fwrite(&packet->nsamp,    1, Llen, fp) != Llen) return -15;
    if (fwrite(packet->data,      1, dlen, fp) != dlen) return -16;

    return 0;
}

int Xfer_RdPacket(fp, packet)
FILE *fp;
struct xfer_packet *packet;
{
long dlen;

    if (fread(packet->sname,     1, slen, fp) != slen) return  -1;
    if (fread(&packet->lat,      1, Flen, fp) != Flen) return  -2;
    if (fread(&packet->lon,      1, Flen, fp) != Flen) return  -3;
    if (fread(&packet->elev,     1, Flen, fp) != Flen) return  -4;
    if (fread(&packet->depth,    1, Flen, fp) != Flen) return  -5;
    if (fread(packet->cname,     1, clen, fp) != clen) return  -6;
    if (fread(packet->instype,   1, ilen, fp) != ilen) return  -7;
    if (fread(&packet->sint,     1, Flen, fp) != Flen) return  -8;
    if (fread(&packet->calib,    1, Flen, fp) != Flen) return  -9;
    if (fread(&packet->calper,   1, Flen, fp) != Flen) return -10;
    if (fread(&packet->vang,     1, Flen, fp) != Flen) return -11;
    if (fread(&packet->hang,     1, Flen, fp) != Flen) return -12;
    if (fread(&packet->beg,      1, Dlen, fp) != Dlen) return -13;
    if (fread(&packet->tear,     1, Ilen, fp) != Ilen) return -14;
    if (fread(&packet->nsamp,    1, Llen, fp) != Llen) return -15;
    if (packet->nsamp > XFER_MAXDAT) return -16;

    dlen = packet->nsamp * sizeof(long);

    if (fread(packet->dbuf, 1, dlen, fp) != dlen) return -17;
    packet->data = (long *) packet->dbuf;

    return 0;
}
