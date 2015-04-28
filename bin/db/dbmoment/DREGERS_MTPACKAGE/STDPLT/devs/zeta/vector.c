#include <stdio.h>
#include "devpar.h"

int penstate;

/* Vector commands for arguments with different number of CRB digits.	*/
static int vectable[4][4]={
	'F', 'G', 'H', 'I', 
	'K', 'L', 'M', 'N', 
	'P', 'Q', 'R', 'S', 
	'U', 'V', 'W', 'X' };

/* Mapping table from 5 bit values to CRB character.			*/
static int CRBtable[32] = {
	'0', '1', '2', '3', '4', '5', '6', '7', 
	'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 
	'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 
	'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', };

#ifdef BITRANGESIXTEEN
static int bitmask [4] = { 
	0x0000001F,			/* 5 bits	*/
	0x000003FF,			/* 10 bits	*/
	0x00007FFF, 			/* 15 bits	*/
	0x0000FFFF };			/* 16 bits 	*/
#else
static int bitmask [4] = { 
	0x0000001F,			/* 5 bits	*/
	0x000003FF,			/* 10 bits	*/
	0x00007FFF, 			/* 15 bits	*/
	0x000FFFFF };			/* 20 bits 	*/
#endif

vector(x1,y1,x2,y2)
int x1, y1, x2, y2;
   {
	extern int xlast, ylast;
	extern int xmax;
	/* device dependent routine to draw a line from (x1,y1) to (x2,y2).
	   The vector has already been clipped, and dash and fat are
	   taken care of by the calling program.
	 */
	/* We will ignore pen mode for the time being, since we can't do
	   anything with it. 
	*/
	if(x1 != xlast || y1 != ylast) move(x1,y1);
#ifdef DEBUG
	fprintf (stderr, "\nvector from (%d,%d) to (%d,%d)\n", x1, y1, x2, y2);
#endif
	draw(x2,y2);
   }

/* Move to new location.  Note that the location arguments are absolute	*/
/* but the plotter works in relative coordinates.			*/
move(x,y)
register int x, y;
   {
	extern int xlast, ylast;
	extern int xmax;
#ifdef DEBUG
	fprintf (stderr, "\nmove to (%d,%d)\n", x, y);
#endif
	relmove (x-xlast, y - ylast, PEN_UP);
   }

/* Draw from the current position to the new absolute position. 	*/
draw(x,y)
register int x, y;
   {
	extern int xlast, ylast;
	extern int xmax;

#ifdef DEBUG
	fprintf (stderr, "\ndraw to (%d,%d)\n", x, y);
#endif
	relmove (x - xlast, y - ylast, PEN_DOWN);
   }

/* Perform relative movement on the plotter. 				*/
relmove (dx, dy, penpos)
int dx, dy, penpos;
{
	extern int xlast, ylast;
	extern int xmax;
	int idx, idy;

	penpos = (penpos == PEN_UP) ? PEN_UP : PEN_DOWN;
	if ((penstate == PEN_DOWN) && (penpos == PEN_UP)) {
		/* pick up the pen. */
		fputs (PEN_UP_CMD, stdout);
		penstate = PEN_UP;
	}
	else if ((penstate == PEN_UP) && (penpos == PEN_DOWN)) {
		/* put pen down */
		fputs (PEN_DOWN_CMD, stdout);
		penstate = PEN_DOWN;
	}

#ifdef DEBUG
	fprintf (stderr, "\nrelmove to (%d,%d) with penpos %d\n", dx, dy, penpos);
#endif
	/* Due to the extended range in the X direction, we may have to	*/
	/* make multiple moves.						*/

	while (dx != 0 || dy != 0) {
		if (dx > MAXDX) idx = MAXDX;
		else if (dx < MINDX) idx = MINDX;
		else idx = dx;
		idy = dy;		
		veccmd (idx, dy);
		xlast += idx;
		ylast += idy;
		dx -= idx;
		dy -= idy;
	}
	if (penstate == PEN_DOWN) xmax = (xlast > xmax) ? xlast : xmax;
   }

veccmd (dx, dy)
int dx, dy;
{
	char str[10], *p;
	int i, j;

	/* Generate a vector command for (dx,dy).			*/
	p = &str[1];
	i = stocrb (p, dx);
	p+=i;
	j = stocrb (p, dy);
	p+=j;
	*p = '\0';
	str[0] = vectable [i-1] [j-1];
	fputs (str, stdout);
}

/* Convert an integer to signed CRB format.  Function returns CRB 	*/
/* string length as function value					*/
/* ASSUMPTIONS:  The machine is a 2's compliment architecture.		*/
int stocrb (p, val)
char *p;
int val;
{
	int range = 16;
	int n = 1;
	int tval;
	int i, r;

	/* Determine the number of CRB digits needed.			*/
	while ((val >= range) || (val < -range)) {
		range *= 32;
		n++;
	}
	val &= bitmask[n-1];
	i = n;
	while (i > 0) {
		r = val & bitmask[0];
		val >>= 5;
		p[--i] = CRBtable[r];
	}
	return(n);
}

/* Convert an integer to unsigned CRB format.  Function returns CRB 	*/
/* string length as function value					*/
/* ASSUMPTIONS:  The machine is a 2's compliment architecture.		*/
int utocrb (p, val)
char *p;
int val;
{
	int range = 32;
	int n = 1;
	int tval;
	int i, r;

	/* Determine the number of CRB digits needed.			*/
	while ((val >= range) || (val < -range)) {
		range *= 32;
		n++;
	}
	val &= bitmask[n-1];
	i = n;
	while (i > 0) {
		r = val & bitmask[0];
		val >>= 5;
		p[--i] = CRBtable[r];
	}
	return(n);
}

