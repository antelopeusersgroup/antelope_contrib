/* @(#)pow.c	1.1 01/13/97 */
/*======================================================================
 *
 *  If a given long integer is exactly a power of the given base, then
 *  return the power.  Otherwise, return -1.
 *
 *====================================================================*/
#include <math.h>
#include "util.h"

int util_powerof(value, base)
long value;
int base;
{
int power;

    power = rint(log((double) value) / log((double) base));
    if (value == (long) pow((double) base, (double) power)) {
        return power;
    } else {
        return -1;
    }
}

#ifdef DEBUG_TEST

main(argc, argv)
int argc;
char *argv[];
{
long value;
int base, power;

	if (argc != 3) {
		fprintf(stderr, "%usage: %s length base\n", argv[0]);
		exit(1);
	}

	value = atol(argv[1]);
	base  = atoi(argv[2]);

	if ((power = util_powerof(value, base)) < 0) {
		printf("%ld is NOT a power of %d\n", value, base);
	} else {
		printf("%ld = %d ** %d\n", value, base, power);
	}

	exit(0);
}

#endif /* DEBUG_TEST */
