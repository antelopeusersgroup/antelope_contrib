/* @(#)swap.c	2.3 01/28/97 */
/*======================================================================
 *
 *  Byte-swapping utilities.
 *
 *  util_lswap:  byte swap an array of longs
 *  util_sswap:  byte swap an array of shorts
 *  util_iftovf: convert IEEE floats into VAX floats.
 *  util_vftoif: convert VAX floats into IEEE floats.
 *  util_order:  determine native bute order
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <memory.h>
#include "util.h"

#define MANTISSA_MASK ((unsigned long)(0x00FFFFFF))
#define MANTISSA_SIZE (24)

/**********************************************************************/

void util_lswap(input, number)
long *input;
long number;
{
short s_temp[2];
short temp;
long i;

    util_sswap((short *) input, number*2);
    for (i = 0; i < number; i++) {
        memcpy(s_temp, input + i, 4);
        temp      = s_temp[0];
        s_temp[0] = s_temp[1];
        s_temp[1] = temp;
        memcpy(input + i, s_temp, 4);
    }
}

/**********************************************************************/

void util_sswap(input, number)
short *input;
long number;
{
char byte[2];
char temp;
long i;

    for (i = 0; i < number; i++) {
        memcpy(byte, input + i, 2);
        temp = byte[0];
        byte[0] = byte[1];
        byte[1] = temp;
        memcpy(input + i, byte, 2);
    }
}

/**********************************************************************/

void util_iftovf(input, number)
unsigned long *input;     /*  pointer to data to be converted    */
long number;              /*  number of samples to be converted  */
{
static int native_order = -1;
unsigned long  mantissa, exponent;
long i;

    if (native_order == -1) native_order = util_order();
    if (native_order == LTL_ENDIAN_ORDER) util_lswap((long *) input, number);
    for (i = 0; i < number; i++) {
        mantissa = input[i] & MANTISSA_MASK;
        exponent = (((input[i] >> MANTISSA_SIZE) + 1) << MANTISSA_SIZE);
        input[i] = mantissa | exponent;
    }
    util_sswap((short *)input, number*2);
    if (native_order == LTL_ENDIAN_ORDER) util_lswap((long *)input, number);
}

/**********************************************************************/

void util_vftoif(input, number)
unsigned long *input;     /*  pointer to data to be converted    */
long number;              /*  number of samples to be converted  */
{
static int native_order = -1;
unsigned long  mantissa, exponent;
long i;

    if (native_order == -1) native_order = util_order();
    if (native_order == LTL_ENDIAN_ORDER) util_lswap((long *)input, number);
    util_sswap((short *)input, number*2);
    for (i = 0; i < number; i++) {
        if (input[i] != 0) {
            mantissa = input[i] & MANTISSA_MASK;
            exponent = (((input[i]>>MANTISSA_SIZE)-1)<<MANTISSA_SIZE);
            input[i] = mantissa | exponent;
        }
    }
    if (native_order == LTL_ENDIAN_ORDER) util_lswap((long *)input, number);
}

/**********************************************************************/

unsigned long util_order()
{
union {
    unsigned char character[4];
    unsigned long int integer;
} test4;
char wordorder[4+1];

/* Construct a 4-byte word of known contents - 0x76543210 */
/* Result will be 0x10325476 for little endian machines (eg Vax, PC) */
/*                0x76543210 for big    endian machines (eg Sun)     */
/*                0x54761032 for PDP-11's                            */
/* The include file "util.h" defines the constants BIG_ENDIAN_ORDER  */
/* and LTL_ENDIAN_ORDER to correspond to output of this routine.     */

    test4.character[0] = 0x76;
    test4.character[1] = 0x54;
    test4.character[2] = 0x32;
    test4.character[3] = 0x10;

    return test4.integer;
}
