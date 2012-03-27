/* @(#)togse.c	1.3 11/07/97 */
/*======================================================================
 *
 *  Given a FILE of xfer_packet data, demultiplex it to CM6 or CM8 format
 *  in the current directory.
 *
 *  Works by first unpacking all the data into files with names of the
 *
 *  form r.sta.chan[.n] where sta.chan are the (lower case) station and
 *  channel names.  If there are more than one output files for the
 *  same station and channel, then subsequent files are given the .n
 *  appendix, where n=1,2,3... etc.  Any pre-existing files in the
 *  current directory with the same names are silently overwritten.
 *
 *  These files are then mmap'd to arrays and the GSE2.0 checksum is
 *  computed and then compressed into files with names of the form
 *  c.sta.chan[.n] (using mmap), which observe the requested maximum line
 *  length.
 *
 *  Finally, the 
 *
 *  The [rc].sta.chan[.n] files are deleted upon completion.
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
#include "xfer.h"

struct gse_header {
    double tofs;                  /* time of first sample         */
    char sta[XFER_SNAMLEN+1];     /* station name                 */
    char chn[XFER_CNAMLEN+1];     /* channel name                 */
    char auxid[5];                /* aux ident code (unused here) */
    char datatype[4];             /* data type code               */
    long nsamp;                   /* number of samples            */
    float srate;                  /* nominal sample rate (Hz)     */
    float calib;                  /* calib, nm/counts             */
    float calper;                 /* calibration reference period */
    char instype[XFER_INAMLEN+1]; /* instrument type              */
    float hang;                   /* horizontal orientation angle */
    float vang;                   /* vertical orientation angle   */
    long checksum;                /* checksum                     */
};

#ifndef MAP_FILE
#define MAP_FILE 0
#endif

#define CMXBUFLEN 1024
static char gsebuf[CMXBUFLEN];

static int format;
static int linlen;
static int diff;
static int clip;
static int nsta = 0;

struct chn_hist {
    char name[XFER_CNAMLEN+1];
    char rawfile[XFER_SNAMLEN + XFER_CNAMLEN + 10];
    char cmpfile[XFER_SNAMLEN + XFER_CNAMLEN + 10];
    char outfile[XFER_SNAMLEN + XFER_CNAMLEN + 10];
    int nrec;
    int ident;
    FILE *raw;
    struct gse_header gse;
    double prev_tofs;
    struct {
        long *raw; /* mmap'd to raw disk file */
        char *cmp; /* mmap'd to compressed disk file */
    } data;
};

static struct sta_hist {
    char name[XFER_SNAMLEN+1];
    int nchn;
    struct chn_hist chn[XFER_MAXCHN];
} sta[XFER_MAXSTA];

static void inihdr(hdr, packet)
struct gse_header *hdr;
struct xfer_packet *packet;
{
int azm, inc;
static char *fid = "Xfer_ToGSE:inihdr";

    util_log(2, "create %s:%s GSE header", packet->sname, packet->cname);

/* Patch hang and vang if remote server got it wrong */

    if (packet->hang >= 0.0 && packet->vang >= 0.0) {
        util_log(1, "patching hang and vang");
        azm = (int) packet->hang;
        inc = (int) packet->vang;
        if (inc == 90 || inc == 89) {
            packet->vang = -1.0;
        } else if (inc == 0) {
            packet->hang = -1.0;
        }
    } else if (packet->hang < 0.0 && packet->vang < 0.0) {
        util_log(1, "guessing hang and vang");
        if (packet->cname[2] == 'z' || packet->cname[2] == 'Z') {
            packet->hang = -1.0;
            packet->vang =  0.0;
        } else if (packet->cname[2] == 'n' || packet->cname[2] == 'N') {
            packet->hang =  0.0;
            packet->vang = -1.0;
        } else if (packet->cname[2] == 'e' || packet->cname[2] == 'E') {
            packet->hang = 90.0;
            packet->vang = -1.0;
        }
    } else {
        util_log(1, "hang and vang are correct");
    }

    hdr->nsamp  = 0;
    hdr->srate  = 1.0 / packet->sint;
    hdr->tofs   = packet->beg;
    hdr->calib  = packet->calib;
    hdr->calper = packet->calper;
    hdr->hang   = packet->hang;
    hdr->vang   = packet->vang;
    util_ucase(strcpy(hdr->sta,     packet->sname));
    util_ucase(strcpy(hdr->chn,     packet->cname));
    util_ucase(strcpy(hdr->instype, packet->instype));
    strcpy(hdr->auxid, "    ");
    hdr->checksum = 0;
        
}
            
static int wrthdr(fp, hdr, linlen)
FILE *fp;
struct gse_header *hdr;
int linlen;
{
int yr, jd, mo, da, hr, mn, sc, ms;
static char *fid = "Xfer_ToGSE:wrthdr";

    util_log(2, "write %s:%s GSE header", hdr->sta, hdr->chn);

/*  Build the header string  */

    util_tsplit(hdr->tofs, &yr, &jd, &hr, &mn, &sc, &ms);
    util_jdtomd(yr, jd, &mo, &da);

    sprintf(gsebuf, "WID2");
    sprintf(gsebuf+strlen(gsebuf), " %04d/%02d/%02d", yr, mo, da);
    sprintf(gsebuf+strlen(gsebuf), " %02d:%02d:%02d.%03d", hr, mn, sc, ms);
    sprintf(gsebuf+strlen(gsebuf), " %-5s", hdr->sta);
    sprintf(gsebuf+strlen(gsebuf), " %-3s", hdr->chn);
    sprintf(gsebuf+strlen(gsebuf), " %-4s", hdr->auxid);
    if (format == 6) {
        sprintf(gsebuf+strlen(gsebuf), " CM6");
    } else {
        sprintf(gsebuf+strlen(gsebuf), " CM8");
    }
    sprintf(gsebuf+strlen(gsebuf), " %8ld",   hdr->nsamp);
    sprintf(gsebuf+strlen(gsebuf), " %11.6f", hdr->srate);
    sprintf(gsebuf+strlen(gsebuf), " %10.2e", hdr->calib);
    sprintf(gsebuf+strlen(gsebuf), " %7.3f",  hdr->calper);
    sprintf(gsebuf+strlen(gsebuf), " %-6s",   hdr->instype);
    sprintf(gsebuf+strlen(gsebuf), " %5.1f",  hdr->hang);
    sprintf(gsebuf+strlen(gsebuf), " %4.1f",  hdr->vang);
    sprintf(gsebuf+strlen(gsebuf), "\n");
    sprintf(gsebuf+strlen(gsebuf), "DAT2\n");

/*  Print the header  */

    if ((util_lenprt(fp, gsebuf, linlen, '\\')) != 0) {
        util_log(1, "error writing GSE header: util_prtline: %s",
            syserrmsg(errno)
        );
        return -1;
    }

    return 0;
}

static struct chn_hist *mkfile(packet, ident)
struct xfer_packet *packet;
int ident;
{
static struct chn_hist chn;
static char *fid = "Xfer_ToGSE:mkfile";

    strcpy(chn.name, packet->cname);
    chn.nrec = 0;
    if (ident) {
        sprintf(chn.rawfile, "r.%s.%s.%d",
            packet->sname, packet->cname, ident
        );
        sprintf(chn.cmpfile, "c.%s.%s.%d",
            packet->sname, packet->cname, ident
        );
        sprintf(chn.outfile,   "%s.%s.%d",
            packet->sname, packet->cname, ident
        );
    } else {
        sprintf(chn.rawfile, "r.%s.%s", packet->sname, packet->cname);
        sprintf(chn.cmpfile, "c.%s.%s", packet->sname, packet->cname);
        sprintf(chn.outfile,   "%s.%s", packet->sname, packet->cname);
    }
    util_lcase(chn.rawfile);
    util_lcase(chn.cmpfile);
    util_lcase(chn.outfile);
    util_log(2, "%s: create file %s", fid, chn.rawfile);
    if ((chn.raw = fopen(chn.rawfile, "w")) == NULL) {
        util_log(1, "%s: fopen: %s: %s",
            fid, chn.rawfile, syserrmsg(errno)
        );
        return NULL;
    }

    chn.ident = ident;

    inihdr(&chn.gse, packet);

    return &chn;
}

static int process(chn)
struct chn_hist *chn;
{
int i, fd, prot, flag;
char *addr;
off_t off;
size_t len;
long nbytes;
FILE *raw, *cmp, *out;
static char *fid = "Xfer_ToGSE:process";

/*  Open all files for this chn  */

    if ((raw = fopen(chn->rawfile, "r+")) == NULL) {
        util_log(1, "%s: fopen: %s: %s",
            fid, chn->rawfile, syserrmsg(errno)
        );
        return -1;
    }

    if ((cmp = fopen(chn->cmpfile, "w+")) == NULL) {
        util_log(1, "%s: fopen: %s: %s",
            fid, chn->cmpfile, syserrmsg(errno)
        );
        return -2;
    }

    if ((out = fopen(chn->outfile, "w")) == NULL) {
        util_log(1, "%s: fopen: %s: %s",
            fid, chn->cmpfile, syserrmsg(errno)
        );
        return -3;
    }

/* mmap raw data file to array  */

    addr = (char *) 0;
    fd = fileno(raw);
    if ((long) (len = lseek(fd, 0, SEEK_END)) < 0) {
        util_log(1, "%s: lseek: %s: %s",
            fid, chn->rawfile, syserrmsg(errno)
        );
        return -4;
    }
    prot = PROT_READ | PROT_WRITE;
    flag = (int)    MAP_FILE | MAP_SHARED;
    off  = (off_t)  0;

    chn->data.raw = (long *) mmap(addr, len, prot, flag, fd, off);

    if (chn->data.raw == (long *) -1) {
        util_log(1, "%s: mmap: %s: %s",
            fid, chn->rawfile, syserrmsg(errno)
        );
        return -5;
    }

/* create/size compressed data file, and mmap to array */

    fd = fileno(cmp);
    if (lseek(fd, len - 1, SEEK_SET) < 0) {
        util_log(1, "%s: lseek: %s: %s",
            fid, chn->rawfile, syserrmsg(errno)
        );
        return -6;
    }
    if (write(fd, chn->data.raw, 1) != 1) {
        util_log(1, "%s: lseek: %s: %s",
            fid, chn->rawfile, syserrmsg(errno)
        );
        return -7;
    }
    if (lseek(fd, 0, SEEK_SET) < 0) {
        util_log(1, "%s: lseek: %s: %s",
            fid, chn->rawfile, syserrmsg(errno)
        );
        return -8;
    }

    addr = (char *) 0;
    prot = PROT_READ | PROT_WRITE;
    flag = (int) MAP_FILE | MAP_SHARED;
    off  = (off_t)  0;

    chn->data.cmp = (char *) mmap(addr, len, prot, flag, fd, off);

    if (chn->data.cmp == (char *) -1) {
        util_log(1, "%s: mmap: %s: %s",
            fid, chn->cmpfile, syserrmsg(errno)
        );
        return -9;
    }

/* apply CMX compression */

    if (format == 6) {
        nbytes = util_cm6(
            chn->data.raw,chn->data.cmp,-chn->gse.nsamp,len,diff,clip
        );
        if (nbytes < 0) {
            util_log(1, "%s: util_cm6: %s: %s",
                fid, chn->cmpfile, syserrmsg(errno)
            );
            return -10;
        }
    } else {
        nbytes = util_cm8(
            chn->data.raw,chn->data.cmp,-chn->gse.nsamp,len,diff,clip
        );
        if (nbytes < 0) {
            util_log(1, "%s: util_cm8: %s: %s",
                fid, chn->cmpfile, syserrmsg(errno)
            );
            return -11;
        }
    }

/* compute GSE 2.0 checksum  */

    chn->gse.checksum = util_chksum(chn->data.raw, chn->gse.nsamp);

/* write CMX header */

    if (wrthdr(out, &chn->gse, linlen) != 0) return -12;

/* write compressed data */

    util_log(2, "write compressed data to %s", chn->outfile);
    if (format == 6) {
       for (i = 0; i < nbytes; i++) {
            if (i && i % linlen == 0) fprintf(out, "\n");
            fprintf(out, "%c", chn->data.cmp[i]);
        }
        fprintf(out, "\n");
    } else {
        if (fwrite(chn->data.cmp, sizeof(char), nbytes, out) != nbytes){
            util_log(1, "%s: fwrite: %s: %s",
                fid, chn->cmpfile, syserrmsg(errno)
            );
            return -13;
        }
    }

/* write the checksum */

    util_log(2, "write checksum to %s", chn->outfile);

    fprintf(out, "CHK2 %ld\n", chn->gse.checksum);

/* delete intermediate files */

    util_log(2, "close files and delete %s and %s",
        chn->rawfile, chn->cmpfile
    );

    fclose(raw);
    fclose(cmp);
    fclose(out);
    if (unlink(chn->rawfile) != 0) {
        util_log(1, "%s: warning: can't unlink `%s': %s",
            fid, chn->rawfile, syserrmsg(errno)
        );
    }
    if (unlink(chn->cmpfile) != 0) {
        util_log(1, "%s: warning: can't unlink `%s': %s",
            fid, chn->cmpfile, syserrmsg(errno)
        );
    }

    return 0;
}

static struct chn_hist *getchn(packet)
struct xfer_packet *packet;
{
FILE *fp;
int i, j, ndx;
struct chn_hist *chn;
static char *fid = "Xfer_ToGSE:getfp";

    for (i = 0; i < nsta; i++) {

        if (strcmp(sta[i].name, packet->sname) == 0) {

            for (j = 0; j < sta[i].nchn; j++) {
                chn = sta[i].chn + j;
                if (strcmp(chn->name, packet->cname) == 0) {
                    if (packet->tear) {
                        fclose(chn->raw);
                        if (process(chn) != 0) return NULL;
                        chn->raw = NULL;
                        chn = mkfile(packet, chn->ident + 1);
                        if (chn == NULL) return NULL;
                        sta[i].chn[j] = *chn;
                    }
                    return chn;
                }
            }

        /*  Must be a new channel for this station... add it to the list */

            if (sta[i].nchn == XFER_MAXCHN) {
                util_log(1, "%s: too many channels, increase XFER_MAXCHN",
                    fid
                );
                return NULL;
            }

            if ((chn = mkfile(packet, 0)) == NULL) return NULL;

            ndx = sta[i].nchn++;
            sta[i].chn[ndx] = *chn;
            return sta[i].chn + ndx;
        }
    }

/*  Must be a new station ... add it to the list  */

    if (nsta == XFER_MAXSTA) {
        util_log(1, "%s: too many stations, increase XFER_MAXSTA", fid);
        return NULL;
    }
   
    ndx = nsta++;
    strcpy(sta[ndx].name, packet->sname);
    sta[ndx].nchn = 1;
    if ((chn = mkfile(packet, 0)) == NULL) return NULL;
    sta[ndx].chn[0] = *chn;

    return sta[ndx].chn;
}

static int Rwrtdat(chn, packet)
struct chn_hist *chn;
struct xfer_packet *packet;
{
int i;
static char *fid = "Xfer_ToGSE:Rwrtdat";

    chn->gse.nsamp += packet->nsamp;

    if (fwrite(
        packet->data, sizeof(long), packet->nsamp, chn->raw
    ) != packet->nsamp) return -4;

    return 0;
}

int Xfer_ToGSE(fp, format_arg, linlen_arg, diff_arg, clip_arg, dupflag)
FILE *fp;
int format_arg;
int linlen_arg;
int diff_arg;
int clip_arg;
int dupflag;
{
FILE *cmp, *out;
int i, j, k, fd, flag, prot, status;
char *addr;
off_t off;
size_t len;
long nbytes;
struct chn_hist *chn;
struct xfer_packet packet;
static char *fid = "Xfer_ToGSE";

    if (format_arg == 6 || format_arg == 8) {
        format = format_arg;
    } else {
        errno = EINVAL;
        return -1;
    }

    linlen = linlen_arg;
    diff   = diff_arg;
    clip   = clip_arg;
        
/*  demultiplex raw data to disk  */

    nsta = 0;
    while (Xfer_RdPacket(fp, &packet) == 0) {
        if ((chn = getchn(&packet)) == NULL) {
            util_log(1, "%s: error: can't get output channel info", fid);
            return -2;
        }
        if (dupflag && chn->prev_tofs == packet.beg) {
            util_log(2, "%s: dup %s:%s packet ignored (tofs = %s)", fid,
                chn->gse.sta, chn->gse.chn, util_dttostr(chn->gse.tofs,0)
            );
        } else if ((status = Rwrtdat(chn, &packet)) != 0) {
            util_log(1, "%s: Rwrtdat(%d): %s",
                fid, status, syserrmsg(errno)
            );
            return -3;
        }
        chn->prev_tofs = packet.beg;
    }

/*  Close all raw files, and process  */

    for (i = 0; i < nsta; i++) {
        for (j = 0; j < sta[i].nchn; j++) {
            chn = sta[i].chn + j;
            if (chn->raw != NULL) {
                fclose(chn->raw);
                process(chn);
            }
        }
    }

    return 0;
}
