/* Common join point for PS and PSIGL. Unixmain() expects to see a program name
 * and run-time options in its argc and argv parameters. PSIGL's main() routine
 * reads run-time options from a file and sets argc and argv up so that unixmain()
 * is fooled into thinking it was called from the shell.
 */

#include <stdio.h>
#include "const.h"
#include "types.h"

unixmain(argc, argv)
int  argc;
char **argv;
{
  int c;

  BuildCmdTable();

  OpenInput();

  beginpar(argc, argv);
  getstdpar();
  getspcpar();
  endpar();

  StartCode();
  StartPage();
  while ((c = GetC()) != EOF) {
    while (c != EOF && !(c & 0x80))	/* Skip past ASCII characters. */
      c = GetC();

    if (c != EOF) {
      IMPORT struct Cmd CmdTable[];

      if (c < CMDTABLEBASE || c >= CMDTABLEBASE + CMDTABLESIZE ||
      	  CmdTable[c - CMDTABLEBASE].name == NULL) {
        if ((c & 0x7F) >= ' ' && (c & 0x7F) < '~')
          Warning("Illegal command %d = \"%c\"", c, c & 0x7F);
        else
          Warning("Illegal command %d", c);
      }
      else {
        CompileCmd(c, CmdTable[c - CMDTABLEBASE].name,
		      CmdTable[c - CMDTABLEBASE].format);
      }
    }
  }
  EndPage();
  EndCode();
  Bye();
}
