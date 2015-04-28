#include	<stdio.h>

/* newplot: set or reset variables for a new plot */
newplot()
   {
	extern int xcur, ycur;
	extern int xlast, ylast;
	xcur= 0;
	ycur= 0;
	xlast= -1;
	ylast= -1;
   }
