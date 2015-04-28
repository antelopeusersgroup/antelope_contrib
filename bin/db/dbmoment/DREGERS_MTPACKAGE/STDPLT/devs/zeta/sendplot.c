#include <stdio.h>
#include "devpar.h"

sendplot(plotnum, more)
int plotnum;
int more;
{
	extern int xmax;
	int ok;
	int newpagex; 

	/* We need to position ourselves to the end of this plot, 	*/
	/* so that we are positioned for the next one.  Since all 	*/
	/* moves are relative, we need to move the "next page" 		*/
	/* following any output that we have generated.			*/

	/* Assume:							*/
	/* We are at xlast, and have plotted upto xmax.			*/

	newpagex = ((xmax + PAGELEN - 1) / PAGELEN) * PAGELEN;
#ifdef DEBUG
	fprintf (stderr, "In sendplot, going to (%d,0)", newpagex);
#endif
	move (newpagex, 0);
	
	ok= 1;	
	return(ok);
}
