#include	<stdio.h>

defcolor(icol,rcol,gcol,bcol)
int icol,rcol,gcol,bcol;
   {
	fprintf(stdout,"defcolor %3d: (%3d,%3d,%3d)\n",icol,rcol,gcol,bcol);
   }

setcolor(icol)
int icol;
   {
	fprintf(stdout,"setcolor %3d\n",icol);
   }

defpattern(ipat,file)
int ipat;
FILE *file;
   {
	int i;
	for(i=0; i<32; i++) getw(file);
	fprintf(stdout,"def pattern %d\n",ipat);
   }

setfill(ipat)
int ipat;
   {
	fprintf(stdout,"set pattern %d\n",ipat);
   }
