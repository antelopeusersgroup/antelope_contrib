#include	<stdio.h>

static int	bitsfixed	=0;

/* newplot: set or reset variables for a new plot */
newplot()
   {
	extern int xcur, ycur;
	extern int bit[], cbit[], lbits[], clbits[], rbits[], crbits[];

	initpatchmap();
	xcur= 0;
	ycur= 0;
	if(bitsfixed == 0) 	/* fix the bits (once) */
	   {
		bitfix(bit   ,32);
		bitfix(cbit  ,32);
		bitfix(lbits ,32);
		bitfix(clbits,32);
		bitfix(rbits ,32);
		bitfix(crbits,32);
		bitsfixed= 1;
	   } 
   }
