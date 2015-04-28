#include <stdio.h>
int bw_only = 0;
#define DEFAULT_FDIR	"/usr/lib/vfont"
char fdir[256];
char *getenv();

get_spc_par()
   {
	char *p;
	getpar("bw_only","d",&bw_only);
	/*  If "iglfontdir" was specified on the command line,	*/
	/*  then use that font dir.				*/
	/*  else if a IGLFONTDIR environment variable is set,	*/
	/*  then use that font dir.				*/
	/*  else send to the default font dir.			*/
	p = getenv("IGLFONTDIR");
	if (p != NULL)
		strcpy (fdir, p);
	else
		strcpy (fdir, DEFAULT_FDIR);
	getpar("iglfontdir","s",fdir);
   }
