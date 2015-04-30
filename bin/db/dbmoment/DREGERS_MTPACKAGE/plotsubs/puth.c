#include	<stdio.h>

puth(w,iop)
short w;
register FILE *iop;
   {
	register i;
	register char *p;

	p= (char *) &w;
	for(i= sizeof(short); --i >= 0; )
		putc(*p++,iop);
	return( ferror(iop) );
   }
