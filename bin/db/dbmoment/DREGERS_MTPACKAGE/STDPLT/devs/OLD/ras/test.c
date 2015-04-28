
#include	<sgtty.h>
#include	"devpar.h"
#include	<stdio.h>

struct sgttyb oldtty, newtty;

int plotout;

opendev()
   {
	int do_erase, do_reset;
	int i,xshift, yshift, xmax, ymax,min ;
	char c[2], negbit;
	char mychar;
	negbit = '\200';
	xshift = -640;   	/* shifts necessary to make 0,0 */
	yshift = -512;		/* lower left hand corner 	*/
	xmax = 1279;
	ymax = 1023;
	min = 0;

	/*if( (plotout= open(RASTTECH,2)) < 0)
		fprintf(stderr,"cannot open %s\n",RASTTECH);  */
	if( (plotout= creat("testfile",0775)) < 0)
		fprintf(stderr,"cannot open %s\n","testfile"); 

	gtty(plotout,&oldtty);
	newtty.sg_ispeed = oldtty.sg_ispeed;
	newtty.sg_ospeed = oldtty.sg_ospeed;
	newtty.sg_erase  = oldtty.sg_erase;
	newtty.sg_kill   = oldtty.sg_kill;
	newtty.sg_flags  = oldtty.sg_flags;
	newtty.sg_flags |= -RAW | LITOUT;	/* -raw litout seems to work */
	stty(plotout,&newtty);

	if(do_reset)
	   {
		write(plotout,"\375",1); 
		sleep(12);		
	   }



	/* set in graphics mode */
	write(plotout,"\372",1); 	/*  enter graphics mode */
	write(plotout,"\054\001",2); /* reset = moddis command */
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
	c[1] = min & 0xff;
	c[0] = min & 0xff;
	write(plotout,c,2);
	write(plotout,c,2);
	c[1] = xmax & 0xff;
	c[0] = (xmax >> 8 ) & 0xff;
	write(plotout,c,2);
	c[1] = ymax & 0xff;
	c[0] = (ymax >> 8 ) & 0xff ;
	write(plotout,c,2);
/* test circle */
	write(plotout,"\037\001",2);
	write(plotout,"\001",1);
	write(plotout,"\0\0\0\0",4);
	write(plotout,"\201\001\000\001\000",5);
	write(plotout,"\016\000\370",3);
	mychar = getc(stdin);
	sleep(3);
	erase();
	write(plotout,"\260\003",2); /* set color blue*/
	write(plotout,"\201\002\140\002\140",5);
	write(plotout,"\016\001\070",3);
	sleep(3);
erase();
/*	if(do_erase) erase();  */
	/* DO SET PRMFIL */
   }


closedev()
   {
	sleep(3);
	/*write(plotout,"\054\001",2);*/	/* reset screen */
	write(plotout,"\377",1);
	stty(plotout,&oldtty);
   }

erase()
   {
	fprintf(stderr,"?");
	ttyin();
	fprintf(stderr,"ok");
	write(plotout,"\260\000",2);	
	write(plotout,"\007",1);   /* flood */
	write(plotout,"\260\077",2);
	sleep(1);
   }

main()
{
	opendev();
	closedev();
}

