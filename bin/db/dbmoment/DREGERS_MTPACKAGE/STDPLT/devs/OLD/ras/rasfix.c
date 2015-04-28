#include	<sgtty.h>
#include	"devpar.h"
#include	<stdio.h>

struct sgttyb oldtty, newtty;

int plotout;

main()
   {

	if( (plotout= open(RASTTECH,1)) < 0)
		fprintf(stderr,"cannot open %s\n",RASTTECH);  
	gtty(plotout,&oldtty);
	newtty.sg_ispeed = oldtty.sg_ispeed;
	newtty.sg_ospeed = oldtty.sg_ospeed;
	newtty.sg_erase  = oldtty.sg_erase;
	newtty.sg_kill   = oldtty.sg_kill;
	newtty.sg_flags  = oldtty.sg_flags;
	newtty.sg_flags |= -RAW | LITOUT;	/* -raw litout seems to work */
	stty(plotout,&newtty);			/* might not be right */


	write(plotout,"\372",1); 	/* set in graphics mode */
	write(plotout,"\054\001",2);	/* reset = moddis 1 	*/
	write(plotout,"\377",1);	/* exit graphics mode	*/
	stty(plotout,&oldtty);
   }
