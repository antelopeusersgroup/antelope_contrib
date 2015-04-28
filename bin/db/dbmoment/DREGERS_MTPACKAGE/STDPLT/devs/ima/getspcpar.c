#include <stdio.h>
#include "devpar.h"
int headerpage	=0;
int ncopies	=1;
char printer[33];
char *getenv();

get_spc_par()
   {
	char *p;
	getpar("headerpage","d",&headerpage);
	getpar("copies","d",&ncopies);
	/*  If a printer was specified on the command line,	*/
	/*  then use that printer.				*/
	/*  else if a PRINTER environment variable is set,	*/
	/*  then use that printer.				*/
	/*  else send to the default printer.			*/
	p = getenv("PRINTER");
	if (p != NULL) 
		strcpy (printer, p);
	else
		strcpy (printer, DEFAULT_PRINTER);
	getpar("printer","s",printer);
   }

