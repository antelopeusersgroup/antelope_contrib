/* @(#)toseed.c	1.3 11/07/97 */
/*======================================================================
 * 
 *  Demultiplex a FILE of xfer_packet data into miniSEED format.
 *
 *  Works by first unpacking all the data into files with temporary
 *  names determined by the system.
 *
 *  Data are written to disk as longs with byte order the same as the
 *  processing host.
 *
 *  These files are then mmap'd to arrays which are passed to the
 *  seed_mini() conversion routine.
 *
 *  The r.sta.chan.n files are deleted upon completion.
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <math.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include "util.h"
#include "seed.h"
#include "xfer.h"

#define DEMUX_BUFLEN 1024

struct file_info {
    FILE *fp;
    char *name;
    double sint;
    double prev_tofs;
    double tofs;
    struct seed_fsdh fsdh;
};

struct file_list {
    struct file_info info;
    struct file_list *next;
} head;

#ifndef MAP_FILE
#define MAP_FILE 0
#endif

long nfiles = 0;

static int info_cmp(const void *aptr, const void *bptr)
{
struct file_info *a;
struct file_info *b;
int cmpval;

    a = (struct file_info *) aptr;
    b = (struct file_info *) bptr;

    cmpval = strcasecmp(a->fsdh.staid, b->fsdh.staid);
    if (cmpval != 0) return cmpval;

    cmpval = strcasecmp(a->fsdh.chnid, b->fsdh.chnid);
    if (cmpval != 0) return cmpval;

    if (a->fsdh.start > b->fsdh.start) return 1;

    return -1;
}

static struct file_info *newfile(packet)
struct xfer_packet *packet;
{
struct file_list *crnt, *new;
static char *fid = "Xfer_ToSEED:newfile";

    crnt = &head;
    while (crnt->next != (struct file_list *) NULL) crnt = crnt->next;

    new = (struct file_list *) malloc(sizeof(struct file_list));
    if (new == (struct file_list *) NULL) {
        util_log(1, "%s: malloc: %s", fid, syserrmsg(errno));
        return (struct file_info *) NULL;
    }

    if ((new->info.name = tempnam(".", "2seed")) == (char *) NULL) {
        util_log(1, "%s: tempnam: %s", fid, syserrmsg(errno));
        return (struct file_info *) NULL;
    }

    memset(&new->info.fsdh, 0, sizeof(struct seed_fsdh));
    util_ucase(strcpy(new->info.fsdh.staid, packet->sname));
    util_ucase(strcpy(new->info.fsdh.chnid, packet->cname));
    new->info.fsdh.start = packet->beg;
    new->info.sint = packet->sint;
    seed_sintsplit(
        (double)packet->sint, &new->info.fsdh.srfact, &new->info.fsdh.srmult
    );
    new->info.fsdh.nsamp = 0;

    new->next = (struct file_list *) NULL;
    crnt->next = new;

    ++nfiles;

    util_log(2, "create %s:%s temp file %s (%d)",
        packet->sname, packet->cname, new->info.name, nfiles
    );
    if ((new->info.fp = fopen(new->info.name, "ab")) == (FILE *) NULL) {
        util_log(1, "%s: fopen: %s: %s",
            fid, new->info.name, syserrmsg(errno)
        );
        return (struct file_info *) NULL;
    }

    return &new->info;
}

static struct file_info *getinfo(packet)
struct xfer_packet *packet;
{
FILE *ifp;
int i, j, ndx;
struct file_list *crnt;
struct file_info *info;
static char *fid = "Xfer_ToSEED:info";

/* If we have a tear, then we need to get a new file */

    if (packet->tear) return newfile(packet);

/* Otherwise see if we have already encountered this sta/chn */

    info = (struct file_info *) NULL;
    crnt = head.next;
    while (crnt != (struct file_list *) NULL) {
        if (
            strcasecmp(packet->sname, crnt->info.fsdh.staid) == 0 &&
            strcasecmp(packet->cname, crnt->info.fsdh.chnid) == 0
        ) info = &crnt->info;
        crnt = crnt->next;
    }

/* and if we haven't, then get a new file */

    if (info == (struct file_info *) NULL) return newfile(packet);

/* Open the pre-exisiting file */

    if ((info->fp = fopen(info->name, "ab")) == (FILE *) NULL) {
        util_log(1, "%s: fopen: %s: %s", fid, info->name, syserrmsg(errno));
        return (struct file_info *) NULL;
    }

    return info;
}

static int Rwrtdat(info, packet, buffer, dupflag)
struct file_info *info;
struct xfer_packet *packet;
char *buffer;
int dupflag;
{
static char *fid = "Xfer_ToSEED:Rwrtdat";

    info->fsdh.nsamp += packet->nsamp;

    if (fwrite(
        packet->data, sizeof(long), packet->nsamp, info->fp
    ) != packet->nsamp) return -1;

    return 0;
}

int Xfer_ToSEED(ifp, oname, explen, network, dupflag)
FILE *ifp;
char *oname;
int  explen;
char *network;
int dupflag;
{
FILE *ofp;
int i, j, k, fd, flag, prot, status;
char *addr, *buffer;
off_t off;
size_t len;
long *input, remain, nc, used, seqno, reclen, buflen;
struct file_list *crnt, *prev;
struct file_info *info;
struct xfer_packet packet;
static char *fid = "Xfer_ToSEED";

/* Check input, and allocate work space */

    if (explen < 8 || explen > 16) {
        util_log(1, "%s: illegal explen (%d)", fid, explen);
        errno = EINVAL;
        return -1;
    }

    reclen = (long) pow(2.0, (double) explen);
    buflen = (reclen < DEMUX_BUFLEN) ? DEMUX_BUFLEN : reclen;

    if ((buffer = (char *) malloc(buflen)) == (char *) NULL) {
        util_log(1, "%s: malloc: %s", fid, syserrmsg(errno));
        return -2;
    }

    head.next = (struct file_list *) NULL;

/* Open output file */

    if (oname != (char *) NULL) {
        if ((ofp = fopen(oname, "w")) == (FILE *) NULL) {
            util_log(1, "%s: fopen: %s: %s", fid, oname, syserrmsg(errno));
            return -3;
        }
    } else {
        ofp = stdout;
    }

/* Demultiplex raw data to disk */

    util_log(2, "demultiplexing raw input to tmp files");
    while (Xfer_RdPacket(ifp, &packet) == 0) {
        if ((info = getinfo(&packet)) == (struct file_info *) NULL) {
            util_log(1, "%s: error: info failed", fid);
            return -4;
        }
        if (dupflag && info->prev_tofs == info->tofs) {
            util_log(2, "%s: dup %s:%s ignored",
                fid, info->fsdh.staid, info->fsdh.chnid
            );
        } else if ((status = Rwrtdat(info, &packet, buffer)) != 0) {
            util_log(1, "%s: toseed_Rwrtdat(%d): %s",
                fid, status, syserrmsg(errno)
            );
            return -5;
        }
        info->prev_tofs = info->tofs;
        fclose(info->fp);
    }

/* Convert from linked list to array */

    util_log(2, "creating sta:chn sorted array of input files");
    info = (struct file_info *) malloc(nfiles*sizeof(struct file_info));
    if (info == (struct file_info *) NULL) {
        util_log(1, "%s: malloc: %s", fid, syserrmsg(errno));
        return -6;
    }

    i = 0;
    crnt = head.next;
    while (crnt != (struct file_list *) NULL) {
        if (i == nfiles) {
            util_log(1, "%s: logic error 1", fid);
            return -7;
        }
        info[i++] = crnt->info;
        prev = crnt;
        crnt = crnt->next;
        free(prev);
    }

    if (i != nfiles) {
        util_log(1, "%s: logic error 2", fid);
        return -8;
    }

    qsort((void *) info, nfiles, sizeof(struct file_info), info_cmp);

/* Process through the accumulated list of demux files */

    seqno = 1;

    for (i = 0; i < nfiles; i++) {

        util_log(2, "converting %s:%s file %s",
            info[i].fsdh.staid, info[i].fsdh.chnid, info[i].name
        );

    /* open demux'd raw data file */

        if ((info[i].fp = fopen(info[i].name, "r")) == (FILE *) NULL) {
            util_log(1, "%s: %s: fopen: %s",
                fid, info[i].name, syserrmsg(errno)
            );
        }

    /* mmap raw data file to input array */

        addr = (char *) 0;
        fd = fileno(info[i].fp);
        if ((long) (len = lseek(fd, 0, SEEK_END)) < 0) {
            util_log(1, "%s: lseek: %s: %s",
                fid, info[i].name, syserrmsg(errno)
            );
            return -9;
        }
        prot = PROT_READ;
        flag = (int)    MAP_FILE | MAP_SHARED;
        off  = (off_t)  0;

        input = (long *) mmap(addr, len, prot, flag, fd, off);

        if (input == (long *) -1) {
            util_log(1, "%s: mmap: %s: %s",
                fid, info[i].name, syserrmsg(errno)
            );
            return -10;
        }

    /* Convert to Steim 1 compressed mini-SEED */

        info[i].fsdh.ioclck |=  SEED_FLAG_START;
        info[i].fsdh.ioclck &= ~SEED_FLAG_STOP;
        strcpy(info[i].fsdh.netid, network);

        remain = info[i].fsdh.nsamp;
        util_log(2, "compressing %ld samples into %ld byte records",
            remain, reclen
        );
        while (remain != 0) {

            info[i].fsdh.seqno = seqno++;

            util_log(3, "ioclck = 0x%0x", info[i].fsdh.ioclck);

            nc = seed_mini(input,remain,&info[i].fsdh,buffer,explen,&used);

            if (nc < 0) {
                util_log(1, "%s: seed_mini failure: %d", fid, nc);
                return -11;
            }
            if (fwrite(buffer, sizeof(char), reclen, ofp) != reclen) {
                util_log(1, "%s: fwrite: %s", fid, syserrmsg(errno));
                return -12;
            }

            input  += nc;
            remain -= nc;

            util_log(2, "%ld samples -> %ld bytes, %ld samples left",
                nc, used, remain
            );
            util_log(2, "Compression ratio = %.2f",
                (float) nc * 4 / (float) used
            );

            info[i].fsdh.ioclck &= ~SEED_FLAG_START;

            util_log(3, "update starttime: was %s",
                util_dttostr(info[i].fsdh.start, 0)
            );

            info[i].fsdh.start += (double) nc * info[i].sint;

            util_log(3, "new starttime:        %s",
                util_dttostr(info[i].fsdh.start, 0)
            );
            util_log(3, "nc = %ld, sint = %.3f, incre = %.3f",
                nc, info[i].sint, (double) nc * info[i].sint
            );
        }

    /* close and delete demux'd raw data file */

        fclose(info[i].fp);
        unlink(info[i].name);
        free(info[i].name);

        util_log(2, "close/free/delete %s", info[i].name);
    }

    if (ofp != stdout) {
        fclose(ofp);
    } else {
        fflush(ofp);
    }

/* Free up list and working buffer */

    free(info);
    free(buffer);

    return 0;
}
