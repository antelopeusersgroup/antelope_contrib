#include	<stdio.h>
#include	"../../h/igl.h"
#include	"../com/global.h"

extern int bit[], cbit[], lbits[], clbits[], rbits[], crbits[];

vector(x1,y1,x2,y2)
int x1,y1,x2,y2;
   {
	register int *blk, test, x, y;
	int dx, dy, dx2, dy2;
	int *getpatch(), *getpatchx();
	extern int penmode;

	if(y1 == y2)	/* special case -horizontal line */
	   {
		if(x1 > x2) { test= x1; x1= x2; x2= test; }
		
		blk= getpatchx(x1,y1);
		switch(penmode)
		   {
			case FILL_OR:
				test= bit[y1%32];
				for (x=x1; x<=x2; x++)
				   {
					if (x%32 == 0) blk = getpatch(x,y1);
					*blk++ |= test;
				   }
				break;
			case FILL_XOR:
				test= bit[y1%32];
				for(x=x1; x<=x2; x++)
				   {
					if(x%32 == 0) blk=getpatch(x,y1);
					*blk++ ^= test;
				   }
				break;
			case FILL_WHITE:
				test= cbit[y1%32];
				for(x=x1; x<=x2; x++)
				   {
					if(x%32 == 0) blk=getpatch(x,y1);
					*blk++ &= test;
				   }
				break;
		   }
		return;
	   }

	if(x1 == x2)	/* special case -vertical line */
	   {		/* TODO: use better algorithm for this case */
		if(y1 > y2) { test= y1; y1= y2; y2= test; }
		blk= getpatchx(x1,y1);
		switch(penmode)
		   {
			case FILL_OR:
				for(y=y1; y<=y2; y++)
				   {
					if(y%32 == 0) {
						blk=getpatchx(x1,y);
					}
					*blk |= bit[y%32];
				   }
				break;
			case FILL_XOR:
				for(y=y1; y<=y2; y++)
				   {
					if(y%32 == 0) {
						blk=getpatchx(x1,y);
					}
					*blk ^= bit[y%32];
				   }
				break;
			case FILL_WHITE:
				for(y=y1; y<=y2; y++)
				   {
					if(y%32 == 0) blk=getpatchx(x1,y);
					*blk &= cbit[y%32];
				   }
				break;
		   }
		return;
	   }

	/* general case: */
	dx= (x2>x1 ? x2-x1 : x1-x2);
	dy= (y2>y1 ? y2-y1 : y1-y2);
	dx2= 2*dx;
	dy2= 2*dy;
	if(dx >= dy)	/* plot y = f(x) */
	   {
		if(x2 < x1) { test=x1; x1=x2; x2=test;
			      test=y1; y1=y2; y2=test; }
		blk= getpatchx(x1,y1);
		test= dx;
		y= y1;
		if(y2 > y1)
		   {
			switch(penmode)
			   {
				case FILL_OR:
					for(x=x1; x<=x2; x++)
					   {
						if(x%32 == 0) blk=getpatch(x,y);
						*blk++ |= bit[y%32];
						if(test <= 0)
						   {
							y++;
							if(y%32 == 0)
								blk=getpatchx(x+1,y);
							test += dx2;
						   }
						test -= dy2;
					   }
					break;
				case FILL_XOR:
					for(x=x1; x<=x2; x++)
					   {
						if(x%32 == 0) blk=getpatch(x,y);
						*blk++ ^= bit[y%32];
						if(test <= 0)
						   {
							y++;
							if(y%32 == 0)
								blk=getpatchx(x+1,y);
							test += dx2;
						   }
						test -= dy2;
					   }
					break;
				case FILL_WHITE:
					for(x=x1; x<=x2; x++)
					   {
						if(x%32 == 0) blk=getpatch(x,y);
						*blk++ &= cbit[y%32];
						if(test <= 0)
						   {
							y++;
							if(y%32 == 0)
								blk=getpatchx(x,y);
							test += dx2;
						   }
						test -= dy2;
					   }
					break;
			   }
		   }
		 else
		   {
			switch(penmode)
			   {
				case FILL_OR:
					for(x=x1; x<=x2; x++)
					   {
						if(x%32 == 0) blk=getpatch(x,y);
						*blk++ |= bit[y%32];
						if(test <= 0)
						   {
							y--;
							if (y%32==31)
								blk=getpatchx(x+1,y);
							test += dx2;
						   }
						test -= dy2;
					   }
					break;
				case FILL_XOR:
					for(x=x1; x<=x2; x++)
					   {
						if(x%32 == 0) blk=getpatch(x,y);
						*blk++ ^= bit[y%32];
						if(test <= 0)
						   {
							y--;
							if(y%32 == 31)
								blk=getpatchx(x+1,y);
							test += dx2;
						   }
						test -= dy2;
					   }
					break;
				case FILL_WHITE:
					for(x=x1; x<=x2; x++)
					   {
						if(x%32 == 0) blk=getpatch(x,y);
						*blk++ &= cbit[y%32];
						if(test <= 0)
						   {
							y--;
							if(y%32 == 31)
								blk=getpatchx(x+1,y);
							test += dx2;
						   }
						test -= dy2;
					   }
					break;
			   }
		   }
	   }
	 else	/* plot x = f(y) */
	   {
		if(y2 < y1) { test=x1; x1=x2; x2=test;
			      test=y1; y1=y2; y2=test; }
		blk= getpatch(x1,y1);
		test= dy;
		x= x1;
		if(x2 > x1)
		   {
			switch(penmode)
			   {
				case FILL_OR:
					for(y=y1; y<=y2; y++)
					   {
						if(y%32 == 0) blk=getpatch(x,y);
						blk[x%32] |= bit[y%32];
						if(test <= 0)
						   {
							x++;
							if(x%32 == 0)
								blk=getpatch(x,y);
							test += dy2;
						   }
						test -= dx2;
					   }
					break;
				case FILL_XOR:
					for(y=y1; y<=y2; y++)
					   {
						if(y%32 == 0) blk=getpatch(x,y);
						blk[x%32] ^= bit[y%32];
						if(test <= 0)
						   {
							x++;
							if(x%32 == 0)
								blk=getpatch(x,y);
							test += dy2;
						   }
						test -= dx2;
					   }
					break;
				case FILL_WHITE:
					for(y=y1; y<=y2; y++)
					   {
						if(y%32 == 0) blk=getpatch(x,y);
						blk[x%32] &= cbit[y%32];
						if(test <= 0)
						   {
							x++;
							if(x%32 == 0)
								blk=getpatch(x,y);
							test += dy2;
						   }
						test -= dx2;
					   }
					break;
			   }
		   }
		 else
		   {
			switch(penmode)
			   {
				case FILL_OR:
					for(y=y1; y<=y2; y++)
					   {
						if(y%32 == 0) blk=getpatch(x,y);
						blk[x%32] |= bit[y%32];
						if(test <= 0)
						   {
							x--;
							if(x%32 == 31)
								blk=getpatch(x,y);
							test += dy2;
						   }
						test -= dx2;
					   }
					break;
				case FILL_XOR:
					for(y=y1; y<=y2; y++)
					   {
						if(y%32 == 0) blk=getpatch(x,y);
						blk[x%32] ^= bit[y%32];
						if(test <= 0)
						   {
							x--;
							if(x%32 == 31)
								blk=getpatch(x,y);
							test += dy2;
						   }
						test -= dx2;
					   }
					break;
				case FILL_WHITE:
					for(y=y1; y<=y2; y++)
					   {
						if(y%32 == 0) blk=getpatch(x,y);
						blk[x%32] &= cbit[y%32];
						if(test <= 0)
						   {
							x--;
							if(x%32 == 31)
								blk=getpatch(x,y);
							test += dy2;
						   }
						test -= dx2;
					   }
					break;
			   }
		   }
	   }
   }
