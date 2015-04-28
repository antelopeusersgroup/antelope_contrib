/*   All error handling occurs here. There are two calls: Warning() and Fatal().
 * Both accept a format string that can contain up to four %s or %ld's. 
 * Fatal() does not return, it calls Bye() to clean things up and
 * exits to the Finder.
 *
 *   You may put newlines in the string. The string need not be terminated by
 * a newline.
 *
 *   Fatal() and Warning() can be safely called at any time. The shell version
 * just appends a newline and outputs to stderr.
 */

#include <stdio.h>

Warning(format, p1, p2, p3, p4)
char *format;
char *p1, *p2, *p3, *p4;
{
  char message[500];
  sprintf(message, format, p1, p2, p3, p4);
  fprintf(stderr, "%s\n", message);
}


Fatal(format, p1, p2, p3, p4)
char *format;
char *p1, *p2, *p3, *p4;
{
  char message[500];
  sprintf(message, format, p1, p2, p3, p4);
  fprintf(stderr, "%s\n", message);
  Bye();
}
