/* Bye() cleans up everything and exits to the Finder. It's written so that
 * it can be safely called from anywhere (for example, the CloseX() routines
 * work correctly even if X isn't open.)
 */

Bye(ReturnCode)
int ReturnCode;	   /* If running from Aztec shell, return this value. */
{
  CloseInput();
  EndCode();
  exit(ReturnCode);
}
