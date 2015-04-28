/*   All error messages are posted here. Warning() and Fatal() both take format
 * strings and arguments. Warning() returns after printing the message,
 * Fatal() calls Bye() so it does not return. Error message string can contain
 * but doesn't have to end with a newline.
 */

#include <stdio.h>

Warning(format, i1, i2, i3, i4, i5, i6, i7, i8, i9)
char *format;
{
  fprintf(stderr, format, i1, i2, i3, i4, i5, i6, i7, i8, i9);
  fprintf(stderr, "\n");
}


Fatal(format, i1, i2, i3, i4, i5, i6, i7, i8, i9)
char *format;
{
  fprintf(stderr, format, i1, i2, i3, i4, i5, i6, i7, i8, i9);
  fprintf(stderr, "\n");
  Bye();
}
