#include	<sgtty.h>
#include	"../com/global.h"
#include	"devpar.h"
#include	<stdio.h>

struct sgttyb oldtty, newtty;

int plotout;

opendev()
   {
	extern int do_erase, do_reset;
	if( (plotout= open(JUPITER,1)) < 0)
		err(FATAL,"cannot open %s\n",JUPITER);  
/*	if( (plotout= open("file",1)) < 0)
		err(FATAL,"cannot open %s\n","file");   */

	gtty(plotout,&oldtty);
	newtty.sg_ispeed = oldtty.sg_ispeed;
	newtty.sg_ospeed = oldtty.sg_ospeed;
	newtty.sg_erase  = oldtty.sg_erase;
	newtty.sg_kill   = oldtty.sg_kill;
	newtty.sg_flags  = oldtty.sg_flags;
	newtty.sg_flags |= CBREAK| RAW;
	stty(plotout,&newtty);


	if(do_reset)
	   {
		write(plotout,"\033\060",2);    /*ascii escape and 0*/
		sleep(2);			/*decimal 27 and 48*/
	   }



	/* set in 8-bit mode */
	write(plotout,"\033G1888N",7);
	if(do_erase) erase(); 
   }

closedev()
   {
	write(plotout,"\001",1);
	stty(plotout,&oldtty);
   }

erase()
   {
	write(plotout,"~",1);
	sleep(1);
   }
