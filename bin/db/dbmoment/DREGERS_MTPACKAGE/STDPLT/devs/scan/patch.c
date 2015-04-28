#include	<stdio.h>
#include	"../com/global.h"
#include	"devpar.h"  

int *patchmap[NXB*NYB];
int *releaselist[(NXB*NYB +PATCHINC-1)/PATCHINC];
int nreleaselist	=0;
int npatchavail		=0;
int *patchbuf;

initpatchmap()
   {
	register int i;
	/* free up memory from previous map */
	for(i=0; i<nreleaselist; i++) free(releaselist[i]);
	/* set all map entries to NULL */
	for(i=0; i<NXB*NYB; i++) patchmap[i]= NULL;
	nreleaselist= 0;
	npatchavail = 0;
   }

/* getpatch(x,y) returns a pointer to a block (patch) of 32 32-bit integers
   that contains the point (x,y). The pointer pointers to the beginning of
   the block. The routine allocates memory if that block has not used up to
   this point.
 */
int *getpatch(x,y)
register int x, y;
   {
	register int addr, *ptr, i;

	addr= (y>>5) +NYB*(x>>5);
	if( (ptr= patchmap[addr]) != NULL){
		return(ptr);
	} 
	if(npatchavail <= 0)
	   { 
		if( (patchbuf= (int *)malloc(4*32*PATCHINC)) == NULL )
			err(FATAL,"cannot allocate memory in getpatch\n");
		releaselist[nreleaselist++]= patchbuf;
		/* we zero memory because of an apparent bug in 4.2BSD */
		zap4(patchbuf,32*PATCHINC);
		npatchavail= PATCHINC;
	   }
	ptr= &patchbuf[32*(PATCHINC-npatchavail)];
	patchmap[addr]= ptr;
	npatchavail--;
	return(ptr);
   }
/* getpatchx(x,y) returns a pointer to a block (patch) of 32 32-bit integers
   that contains the point (x,y). The pointer points to the integer that 
   stores the requested x.
   The routine allocates memory if that block has not used up to
   this point.
 */
int *getpatchx(x,y)
register int x, y;
   {
	register int addr, *ptr, i;

	addr= (y>>5) +NYB*(x>>5);
	if( (ptr= patchmap[addr]) != NULL) return(ptr +(x%32));
	if(npatchavail <= 0)
	   {
		if( (patchbuf= (int *)malloc(4*32*PATCHINC)) == NULL )
			err(FATAL,"cannot allocate memory in getpatch\n");
		releaselist[nreleaselist++]= patchbuf;
		/* we zero memory because of an apparent bug in 4.2BSD */
		zap4(patchbuf,32*PATCHINC);
		npatchavail= PATCHINC;
	   }
	ptr= &patchbuf[32*(PATCHINC-npatchavail)];
	patchmap[addr]= ptr;
	npatchavail--;
	return(ptr +(x%32));
   }
