/* @(#)b1000.c	1.5 06/30/97 */
/*======================================================================
 *
 *  Load a blockette 1000 into memory, and vice versa.
 *
 *  Requires 2-byte shorts.
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <assert.h>
#include "seed.h"
#include "util.h"

void seed_load1000(dest, src, order)
char *dest;
struct seed_b1000 *src;
unsigned long order;
{
int swap;
short type   = 1000, stmp;
char  unused = 0;

    swap = (order != util_order());
    assert(sizeof(short) == 2);

    stmp = type; if (swap) util_sswap(&stmp, 1);
    memcpy(dest,     &stmp, 2);

    stmp = src->next; if (swap) util_sswap(&stmp, 1);
    memcpy(dest + 2, &stmp, 2);

    memcpy(dest + 4, &src->format, 1);
    memcpy(dest + 5, &src->order,  1);
    memcpy(dest + 6, &src->length, 1);
    memcpy(dest + 7, &unused,      1);
}

int seed_b1000(dest, src)
struct seed_b1000 *dest;
char *src;
{
int swap;
short type;

    assert(sizeof(short) == 2);

    memcpy(&type, src, 2);
    if (type == 1000) {
        swap = 0;
    } else {
        util_sswap(&type, 1);
        if (type == 1000) {
            swap = 1;
        } else {
            return -1;
        }
    }

    memcpy(&dest->next, src + 2, 2);
    if (swap) util_sswap(&dest->next, 1);

    memcpy(&dest->format, src + 4, 1);
    memcpy(&dest->order,  src + 5, 1);
    memcpy(&dest->length, src + 6, 1);

    return 0;
}

#ifdef DEBUG_TEST
#include <stdio.h>
#include <math.h>

main(int argc, char **argv)
{
struct seed_fsdh fsdh;
struct seed_b1000 dest;
int nread, type, next, count;
char *ptr, *string, src[1024];

    nread = fread(src, 1, 1024, stdin);
    if (nread < 48) {
        fprintf(stderr, "nread = %d is too small!\n", nread);
        exit(1);
    }

       if (seed_fsdh(&fsdh, src) != 0) {
           fprintf(stderr, "seed_fsdh failed\n");
           exit(1);
       }

    printf("FSDH\n");
    printf("seqno  = %ld\n", fsdh.seqno);
    printf("staid  = %s\n",  fsdh.staid);
    printf("locid  = %s\n",  fsdh.locid);
    printf("chnid  = %s\n",  fsdh.chnid);
    printf("netid  = %s\n",  fsdh.netid);
    printf("start  = %s\n",  util_dttostr(fsdh.start, 0));
    printf("nsamp  = %hd\n", fsdh.nsamp);
    printf("srfact = %hd\n", fsdh.srfact);
    printf("srmult = %hd\n", fsdh.srmult);
    printf("active = %d\n",  (int) fsdh.active);
    printf("ioclck = %d\n",  (int) fsdh.ioclck);
    printf("qual   = %d\n",  (int) fsdh.qual);
    printf("more   = %d\n",  (int) fsdh.more);
    printf("tcorr  = %ld\n", fsdh.tcorr);
    printf("bod    = %hd\n", fsdh.bod);
    printf("first  = %hd\n", fsdh.first);
    printf("order  = %s\n",  fsdh.order == LTL_ENDIAN_ORDER ? "little endian" : "big endian");
    printf("swap   = %d\n",  fsdh.swap);

    printf("\n");
    next = fsdh.first;
    do {
        ptr = src + next;
        type = seed_type(ptr, &next, fsdh.swap);
        printf("B%d\n", type);
        if (type == 1000) {
            if (seed_b1000(&dest, ptr) != 0) {
                fprintf(stderr, "seed_b1000 failed\n");
            } else {
                printf("next   = %hd ", dest.next);
                if (next != dest.next) {
                    printf("ERROR: next offset confusion!\n");
                } else {
                    printf("\n");
                }
                printf("format = %d ", (int) dest.format);
                switch (dest.format) {
                  case  1: string = "16 bit integers"; break;
                  case  2: string = "24 bit integers"; break;
                  case  3: string = "32 bit integers"; break;
                  case  4: string = "IEEE float"; break;
                  case  5: string = "IEEE double"; break;
                  case 10: string = "Steim 1 compressed"; break;
                  case 11: string = "Steim 2 compressed"; break;
                  case 12: string = "GEOSCOPE 1"; break;
                  case 13: string = "GEOSCOPE 1"; break;
                  case 14: string = "GEOSCOPE 1"; break;
                  case 15: string = "USNSN compressed"; break;
                  case 16: string = "CDSN 16 bit gain ranged"; break;
                  case 17: string = "Graefenberg 16 bit gain ranged"; break;
                  case 18: string = "IPG-Strasbourg 16 bit gain ranged"; break;
                  case 30: string = "SRO format"; break;
                  case 31: string = "HGLP format"; break;
                  case 32: string = "DWWSSN gain ranged"; break;
                  case 33: string = "RSTN 16 bit gain ranged"; break;
                  default: string = "unknown";
                }
                printf("(%s)\n", string);
                printf("order  = %d ", (int) dest.order);
                printf("(%s)\n", dest.order ? "big endian" : "little endian");
                printf("length = %d ", (int) dest.length);
                printf("(%d)\n", (int) pow(2.0, (double) dest.length));
            }
        } else {
            printf("blockette ignored\n");
        }
    } while (next != 0);
    exit(0);
}

#endif /* DEBUG_TEST */
