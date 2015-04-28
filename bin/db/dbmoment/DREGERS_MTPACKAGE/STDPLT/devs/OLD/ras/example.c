
#include	<sgtty.h>
#include	"devpar.h"
#include	<stdio.h>

struct sgttyb oldtty, newtty;

int plotout;

main()
   {
	int do_erase, do_reset;
	int i,xshift, yshift, xmax, ymax,min ;
	char c[2];
	ymax = 1023;
	xmax = 1279;

	if( (plotout= open(RASTTECH,2)) < 0)
		fprintf(stderr,"cannot open %s\n",RASTTECH); 

/*	if( (plotout= creat("testfile",0775)) < 0)
		fprintf(stderr,"cannot open %s\n","testfile");   */

	gtty(plotout,&oldtty);
	newtty.sg_ispeed = oldtty.sg_ispeed;
	newtty.sg_ospeed = oldtty.sg_ospeed;
	newtty.sg_erase  = oldtty.sg_erase;
	newtty.sg_kill   = oldtty.sg_kill;
	newtty.sg_flags  = oldtty.sg_flags;
	newtty.sg_flags |= -RAW | LITOUT;	/* -raw litout seems to work */
	stty(plotout,&newtty);



	/* set in graphics mode */
	write(plotout,"\372",1); 	/*  enter graphics mode */
	write(plotout,"\054\001",2);	 /* reset = moddis command */
	write(plotout,"\067",1); 	/* shift coordinate origin */
	write(plotout,"\375\200\376\000",4); /* -640, -512 as
						 16 bit unsigned numbers
						 note leftmost bit on
						 means negative. leftmost
						 bit off means positive
						 number */
	write(plotout,"\066\375\200\376\000",5);/* shift screen origin */
	write(plotout,"\072",1);	/* shift window */
	c[1] = '\000';
	c[0] = '\000';
	write(plotout,c,2);	/* x window min = 0 */
	write(plotout,c,2);     /* y window min = 0 */

/* now another way to figure octal equivalent  without calculations */
	c[1] = xmax & 0xff;	/* 0xff = 255 = 8 bits on 	  
			           c[1] will be lowest 8 bits  of x */
	c[0] = (xmax >> 8 ) & 0xff;  /* this shifts x over 8 bits --
					chops off lowest 8 bits.  Then
					the 0xff part takes the lowest
					8 bits of what is left, which
					is the bits originally in 9th -
					16th positions */
				/* so c[0] c[1] has the 16th - 1st bits
				   (or 15th - 0th bits if you like) of
				   x.  Note that if a number is negative
				   you must make sure the left most bit
			           is on.  (since usually ints are 
				   32 bits and we just took the bottom
				   16.  You can do this by doing 
			           c = c | '\200'  
				   Octal 200 is decimal 128 and for 
				   8 bits looks like 10000000 
				   | and & are bit operators and not
			           logical or, and */
	write(plotout,c,2);
	c[1] = ymax & 0xff;
	c[0] = (ymax >> 8 ) & 0xff ;
	write(plotout,c,2);
/* test circle */
	write(plotout,"\037\001",2); /* prmfil on  so circles are filled */
	write(plotout,"\001",1);	/* movabs */
	write(plotout,"\0\0\0\0",4); 	/* movabs to 0 0 */
	write(plotout,"\201\001\000\001\000",5); /* drwabs a line */
	write(plotout,"\016\000\370",3);	/* circle */
	write(plotout,"\260\003",2); /* set color blue*/
	write(plotout,"\001\000\140\003\140",5);	/* move */
		/* 003 140 in octal interpreted as 16 bit number is
		   0000 0011 0110 0000 .  Lefthand 8 bits are 003 and
		   righthand 8 bits are 140.  When you interpret  as
		   16 bit number you get 512 + 256 + 64 + 32 = 864. */
	write(plotout,"\016\001\070",3); /* another circle */
	sleep(6);
	write(plotout,"\054\001",2);	/* reset screen */
	write(plotout,"\377",1);	/* exit graphics */
	stty(plotout,&oldtty);
   }
