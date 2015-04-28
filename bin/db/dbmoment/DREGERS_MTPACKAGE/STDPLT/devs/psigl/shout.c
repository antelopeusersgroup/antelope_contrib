/* All output is channeled through here. If you want to change the program
 * to output somewhere else (say directly to AppleTalk), this is the routine
 * to modify.
 *
 * Entry points:
 *
 *	OpenOutput() 		Call this first.
 *	OutputF(format, args)	Formatted output
 *	CloseOutput()		Clean up
 */

#include "const.h"
#include "types.h"

OpenOutput()
{
}

OutputF(format, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14)
char *format;
{
  printf(format, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14);
}

CloseOutput()
{
}
