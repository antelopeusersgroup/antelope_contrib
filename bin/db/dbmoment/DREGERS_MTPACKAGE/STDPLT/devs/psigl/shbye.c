/*   Bye() does a controlled exit by closing the input and output (if they're open)
 * and exiting to the Finder. This is used for both for a premature exit because
 * of a fatal error and to clean things up at the end.
 *   Bye() is written so that it can be called safely at any time. For example,
 * the CloseX()  routines that it calls are written so that they behave correctly
 * even if X isn't open.
 */
Bye()
{
  CloseInput();
  CloseOutput();

  exit(0);		/* The shell version. */
}
