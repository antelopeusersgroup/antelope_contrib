#include	<stdio.h>

geth(iop)
register FILE *iop;
   {
	register i;
	register char *p;
	short w;

	p= (char *) &w;
	for(i= sizeof(short); --i >= 0; )
		*p++ = getc(iop);
	if( feof(iop) ) return(EOF);
	return(w);
   }
