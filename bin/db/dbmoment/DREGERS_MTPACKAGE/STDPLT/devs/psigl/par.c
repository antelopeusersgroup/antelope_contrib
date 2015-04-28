/* Parameter parsing entry points:
 *
 *	beginpar(argc, argv)		Call this first.
 *	int  argc;
 *	char argv;
 *
 *	getpar(key, type, &var)		Looks for a parameter key=value, parses
 *	char *key;			value according to type ("d", "s", "f" or
 *	char *type;			"b" for integer, string, flt and bool) and
 *					sticks it in var. If no such parameter
 *					exists, var is left unchanged and getpar()
 *					returns 0. The same key can appear more
 *					than once; the last one takes precedence.
 *
 *	endpar()			Call this last.
 */

#include "const.h"
#include "types.h"

char *malloc();

PRIVATE int	ArgC;	/* Private copy of ArgC */
PRIVATE char	**ArgV;	/* Malloced private copy of argv[] pointers */


/* Begin parameter parsing. Argv[0] is assumed to be a program name and isn't
 * used for option scanning. Argv[1] thru argv[argc - 1] should be strings
 * of the form option=value.
 */
beginpar(argc, argv)
int  argc;
char **argv;
{
  int i;
  ArgC = argc;
  if (!(ArgV = (char **)malloc(argc * sizeof(char *))))
    Fatal("Out of heap space.");
  for (i = 0; i < argc; i++)
    ArgV[i] = argv[i];
}


/*   Look for a parameter string key=value. Parse value according to type
 * ("d" = int, "s" = string, "f" = float, "b" = bool (0 or 1)) and put
 * result in dest. The same key can appear more than once, the last
 * value is used. If the key doesn't appear, *dest is left unchanged and
 * 0 is returned.
 *   Don't call getpar() twice with the same key. It removes options as it
 * finds them so the second call will report that the option doesn't exist.
 */
getpar(key, type, dest)
char *key;
char *type;
char *dest;	/* A blind pointer, actually */
{
  int i;
  int foundit;	/* return code */

  foundit = 0;
  for (i = 1; i < ArgC; i++) {
    if (ArgV[i] != NULL) {	/* Parameter may have already been used. */
      int keylen;
      keylen = strlen(key);
      if (strlen(ArgV[i]) > keylen && ArgV[i][keylen] == '=') {
        if (!strncmp(ArgV[i], key, keylen)) {	/* Found it. */
	  int ok;
	  switch(*type) {
	    case 'd': ok = sscanf(ArgV[i] + keylen + 1, "%d", dest);
	     	      break;
	    case 's': ok = 1;
	    	      strcpy(dest, ArgV[i] + keylen + 1);
	    	      break;
	    case 'f': ok = sscanf(ArgV[i] + keylen + 1, "%f", dest);
	    	      break;
	    case 'b': ok = sscanf(ArgV[i] + keylen + 1, "%d", dest);
	     	      break;
	    default: Fatal("Bug! GetPar(): key = %s, type = %s", key, type);
	    	     break;
	  }
	  if (ok)
	    foundit = 1;
	  else {
	    switch(*type) {
	      case 'd': Warning("Option %s: integer required.", ArgV[i]);
	      		break;
	      case 's': Warning("Option %s: string required.", ArgV[i]);
	      		break;
	      case 'f': Warning("Option %s: floating pt. required.", ArgV[i]);
	      		break;
	      case 'b': Warning("Option %s: boolean (0 or 1) required.", ArgV[i]);
	      		break;
	    }
	  }
	  ArgV[i] = NULL;	/* Remove from argument list */
	}
      }
    }
  }
  return foundit;
}


/* All parsing has been done. Report any remaining arguments as errors.
 */
endpar()
{
  int i;
  for (i = 1; i < ArgC; i++)
    if (ArgV[i] != NULL)
      Warning("Option not supported: %s", ArgV[i]);
  free(ArgV);
}
