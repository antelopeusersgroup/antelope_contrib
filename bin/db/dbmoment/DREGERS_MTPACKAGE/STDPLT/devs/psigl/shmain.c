/* Pass control to unixmain() giving it a shell-like environment. Run-times
 * options like textscale and pagerot should be passed thru argc and argv.
 */

#include "const.h"
#include "types.h"

main(argc, argv)
int  argc;
char **argv;
{
  unixmain(argc, argv);
}
