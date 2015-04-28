/*  Specials for Zeta driver:					*/
/*	pen:	integer (1)
/*		Set pen 0 of IGL to really be pen 1-8 of plotter.
/*		e.g. swap zeta pen 1 and the specified pen.
/*		NOTE: For this parameter, pens are zeta pens (1-8_.
/*	speed:	integer (1-20)
/*		Set plotter speed to speed in range 1-20.
/*	dfat:	boolean (default = false)
/*		Do fat lines, or ignore fat lines.
/*	lpr:	boolean (default = true)
/*		Send the output to lpr for automatic spooling.
*/

#include	"devpar.h"
#include	<stdio.h>

int headerpage	=0;			/* Header page?			*/
int ncopies	=1;			/* How many copies?		*/
int draw_fat    =DEFAULT_DRAW_FAT;	/* Should I do fat lines?	*/
int init_pen	=1;			/* default IGL pen.		*/
int speed	=0;			/* default speed - set no speed	*/
int auto_lpr	=1;			/* should I spool output?	*/

extern int penmap[];

get_spc_par()
   {
	int i;

	getpar("speed","d", &speed);
	getpar("pen","d", &init_pen);
	getpar("dfat","b", &draw_fat);
	getpar("lpr","b", &auto_lpr);

	/* Validate default pen.				*/
	if (init_pen >= 1 && init_pen <= NCOLORS) {
		/* swap IGL pen 0 be the specified init_pen.	*/
		i = penmap[0];
		penmap[0] = penmap[init_pen-1];
		penmap[init_pen-1] = i;
	}
	else {
		fprintf (stderr, "Invalid pen: must be between 1 & %d inclusive\n",
			NCOLORS);
		exit(1);
	}

	/* Validate pen speed.					*/
	if (speed < 0 || speed > 20) {
		fprintf (stderr, "Invalid speed: must be between 0 & 20 inclusive\n");
		exit(1);
	}
   }

