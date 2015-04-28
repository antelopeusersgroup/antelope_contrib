#include	<stdio.h>
text(file)
FILE *file;
  {
	int c;
	fprintf(stdout,"text: ");
	while( (c=getc(file)) != NULL)
	   {
		if( (int)c == EOF) break;
		putc(c,stdout);
	   }
	fprintf(stdout,"$\n");
  }
