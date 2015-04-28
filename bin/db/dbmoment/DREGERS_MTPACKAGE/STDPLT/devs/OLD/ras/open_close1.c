#include	<sgtty.h>
#include	"../com/global.h"
#include	"devpar.h"
#include	<stdio.h>


		/* interactive version of open_close */

struct sgttyb oldtty, newtty;

int plotout;

opendev()
   {
	extern int do_erase, do_reset, filethis;
	int i,xshift, yshift, xmax, ymax,min ;
	char c[2], negbit;
	negbit = '\200';	/* negative sign */
	xshift = -640;   	/* shifts necessary to make 0,0 */
	yshift = -512;		/* lower left hand corner 	*/
	xmax = 1279;
	ymax = 1023;
	min = 0;

if (!filethis)
	{
	if( (plotout= open(RASTTECH,1)) < 0)
		fprintf(stderr,"cannot open %s\n",RASTTECH);  
	}
else
	{
	if( (plotout= creat("storefile",0775)) < 0)
		fprintf(stderr,"cannot open %s\n","storefile"); 
	}
	gtty(plotout,&oldtty);
	newtty.sg_ispeed = oldtty.sg_ispeed;
	newtty.sg_ospeed = oldtty.sg_ospeed;
	newtty.sg_erase  = oldtty.sg_erase;
	newtty.sg_kill   = oldtty.sg_kill;
	newtty.sg_flags  = oldtty.sg_flags;
	newtty.sg_flags |= -RAW | LITOUT;	/* -raw litout seems to work */
	stty(plotout,&newtty);			/* might not be right */


	if(do_reset)
	   {
		write(plotout,"\375",1); 	/* cold start */
		sleep(12);		
	   }


	/* set in graphics mode */
	write(plotout,"\372",1);
	write(plotout,"\054\001",2);	/* reset terminal clear screen */
	write(plotout,"\067",1); 	/* shift coordinate origin */
	c[1] = xshift & 0xff;
	c[0] = (xshift >> 8 ) & 0xff;
	c[0] = c[0] | negbit;
	write(plotout,c,2);
	c[1] = yshift & 0xff;
	c[0] = (yshift >> 8 ) & 0xff;
	c[0] = c[0] | negbit;
	write(plotout,c,2);
	write(plotout,"\066",1); 	/* shift screen origin */
	c[1] = xshift & 0xff;
	c[0] = (xshift >> 8 ) & 0xff;
	c[0] = c[0] | negbit;
	write(plotout,c,2);
	c[1] = yshift & 0xff;
	c[0] = (yshift >> 8 ) & 0xff;
	c[0] = c[0] | negbit;
	write(plotout,c,2);
	write(plotout,"\072",1);	/* shift window */
	c[1] = min & 0xff;		/* 0, 0 assumed min */
	c[0] = min & 0xff;
	write(plotout,c,2);
	write(plotout,c,2);
	c[1] = xmax & 0xff;
	c[0] = (xmax >> 8 ) & 0xff;
	write(plotout,c,2);
	c[1] = ymax & 0xff;
	c[0] = (ymax >> 8 ) & 0xff ;
	write(plotout,c,2);
	write(plotout,"\037\001",2);	/* set prmfil on to fill boxes */
   }

closedev()
   {
	char anschar;

	fprintf(stderr,"?");	/* wait for prompt before resetting */
	ttyin();			/* this stuff is for ras1 */
	fprintf(stderr,"ok");
				/* ras1 cannot be run from graphics terminal*/ 
					/* or else it will hang!! */
	if (!filethis)
		write(plotout,"\054\001",2); 
					 /*reset=moddis=erases screen*/
	write(plotout,"\377",1);	/* exit graphics mode */
	stty(plotout,&oldtty);
   }

erase()
   {
	char anschar;

	fprintf(stderr,"?");   /* wait for prompt before resetting */
	ttyin();		/* this stuff is for ras1 */
	fprintf(stderr,"ok");  /* ras1 cannot be run from graphics terminal*/
				/* or else it will hang!! */
	setcolor(0);
	if (!filethis)
		write(plotout,"\007",1);   

	sleep(1);

	setcolor(1);
   }

