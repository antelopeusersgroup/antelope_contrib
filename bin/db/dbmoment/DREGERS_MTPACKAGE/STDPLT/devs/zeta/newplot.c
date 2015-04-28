#include <stdio.h>
#include "devpar.h"

/* newplot: set or reset variables for a new plot */
newplot()
   {
	extern int xcur, ycur;
	extern int xlast, ylast;
	extern int color;
	extern int xmax;
	extern int penstate;
	xcur= 0;
	ycur= 0;
	xlast= 0;
	ylast= 0;
	xmax=0;
	penstate=PEN_UP;
   }
