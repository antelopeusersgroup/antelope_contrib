/* All input is done through here. Call OpenInput() before doing any input,
 * call CloseInput() after you're finished. CloseInput() checks to see if the
 * input is open, if not it returns without doing anything (as required by
 * the Bye() function.)
 */


#include <stdio.h>

#include "const.h"
#include "types.h"


PRIVATE int UngetBuffer;	/* One character pushback buffer */
PRIVATE int PushBacked;		/* A character was pushed back. */



OpenInput()	/* Call this first. */
{
  PushBacked = 0;
}


CloseInput()	/* Nothing has to be done. */
{
}


/* GetC(): reads one character or returns EOF.
 */
GetC()
{
  if (PushBacked) {
    PushBacked = 0;
    return UngetBuffer;
  }
  else
    return getc(stdin);	/* Don't use getchar(), Aztec's version is weird. */
}


/* UngetC(c): push back one character. OK to push back EOF.
 */
UngetC(c)
int c;
{
  if (PushBacked) 	/* Just in case. */
    Fatal("Bug! Tried to push back two characters.");
  PushBacked = 1;
  UngetBuffer = c;
}


/* GetH(): reads a 16 bit value. MSB first.
 */
GetH()
{
  int msb, lsb;
  if ( (msb = GetC()) == EOF || (lsb = GetC()) == EOF ) {
    Fatal("Unexpected end of file, 1, shinp");
  }
  return ((msb & 0xFF) << 8) + (lsb & 0xFF);
}


/* GetW(): reads a 32 bit value. MSB first.
 */
GetW()
{
  int word;
  word = getw (stdin);
  if ( ferror(stdin) ) {
    Fatal("Unexpected end of file, 1, shinp");
  }
  return (word);
}


/* GetPatRow(): reads one pattern row (a 32 bit value)
 */
PatRow GetPatRow()
{
  int hi, m1, m2, lo;
  if ((hi = GetC()) == EOF ||
      (m1 = GetC()) == EOF ||
      (m2 = GetC()) == EOF ||
      (lo = GetC()) == EOF) {
    Fatal("Unexpected end of file, 2, shinp");
  }
  hi &= 0xFF;
  m1 &= 0xFF;
  m2 &= 0xFF;
  lo &= 0xFF;
  return (hi << 24) | (m1 << 16) | (m2 << 8) | lo;
}
