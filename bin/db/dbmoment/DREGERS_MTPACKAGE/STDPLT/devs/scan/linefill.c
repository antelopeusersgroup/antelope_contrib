#include	<stdio.h>
#include	"../../h/igl.h"


/* fills a line in patchmap. y1 should be <= y2 */ 
/* NOTE that the parameters given to linefill should already be checked */
/* that they are within the plotting window. This should be done in boxfill */
/* polygon or other routine that calls linefill */
linefill(x,y1,y2)
int x, y1, y2;
   {
	int *blk, *getpatchx(); 
	int fillwhite = 0;
	int y1mod32, y2mod32,  pat, lpat, rpat, val, nfull, i,tempmode;
	extern int lbits[], rbits[], crbits[], clbits[], brushmode, brushpat;
	extern int ixwmin, ixwmax, iywmin, iywmax;
        extern int patterns[][32];
	y1mod32= y1%32;
	y2mod32= y2%32;


	pat = patterns[brushpat][x%NPATROWS];  
	blk= getpatchx(x,y1);

 	if (brushmode==FILL_BRWHITE){
		pat = 0;  		/* white fill */
		fillwhite = 1;
		brushmode = FILL_EQU;
	}

	if ( ((y2-y1)<32  && y2mod32>y1mod32) || (y1==y2))
	/*start and stop in same patch*/
	   {
		pat &= (rbits[y1mod32] & lbits[y2mod32]);
		switch(brushmode)
		   {
			case FILL_OR:
				*blk |= pat;
				break;
			case FILL_XOR:
				*blk ^= pat;
				break;
			case FILL_AND:
				*blk &= pat;
				break;
			case FILL_EQU:
				val= ((*blk & crbits[y1mod32]) | (*blk & clbits[y2mod32]));
				*blk = val | pat;
				if (fillwhite) {
					brushmode = FILL_BRWHITE;
					fillwhite = 0;				
				}
				break;
		   }
		return;
	   }

	lpat= pat & rbits[y1mod32];
	rpat= pat & lbits[y2mod32];

	/*  nfull is the number of times 32 can go into y2 - y1,
	    exclusive of the parts near y1 and near y2 that hang-over
	    into incomplete blocks of 32.  i.e., exclusive of
	    32-y1mod32  and  y2mod32 */
	nfull = y2 -y1 - y2mod32 -32 + y1mod32;
	nfull = (int) (nfull/32);
	switch(brushmode)
	   {
		case FILL_OR:
			*blk |= lpat;
			y1 += 32;     
			y1 = y1 - y1mod32;  /* start of next block */
			for(i=0; i<nfull; i++)
			   {
				blk= getpatchx(x,y1);
				*blk |= pat;
				y1 += 32;
			   }
			blk= getpatchx(x,y2);
			*blk |= rpat;
			break;
		case FILL_XOR:
			*blk ^= lpat;
			y1 += 32;
			y1 = y1 - y1mod32;
			for(i=0; i<nfull; i++)
			   {
				blk= getpatchx(x,y1);
				*blk ^= pat;
				y1 += 32;
			   }
			blk= getpatchx(x,y2);
			*blk ^= rpat;
			break;
		case FILL_AND:
			*blk &= lpat;
			y1 += 32;
			y1 = y1 - y1mod32;
			for(i=0; i<nfull; i++)
			   {
				blk= getpatchx(x,y1);
				*blk &= pat;
				y1 += 32;
			   }
			blk= getpatchx(x,y2);
			*blk &= rpat;
			break;
		case FILL_EQU:
			val= *blk & crbits[y1mod32];
			*blk = val | lpat;
			y1 += 32;
			y1 = y1 - y1mod32;
			for(i=0; i<nfull; i++)
			   {
				blk= getpatchx(x,y1);
				*blk = pat;
				y1 += 32;
			   }
			blk= getpatchx(x,y2);
			val= *blk & clbits[y2mod32];
			*blk = val | rpat;
			if (fillwhite)
			   {
				brushmode = FILL_BRWHITE;
				fillwhite = 0;				
			   }
			break;
	   }

   }


