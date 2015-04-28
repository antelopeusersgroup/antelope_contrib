#include	<stdio.h>
#define N	100
float size	= 3.0;
struct poly { float x, y; };
main()
   {
	struct poly p[N];
	float frand();
	int i;

	setorig(size,size);
	setscl(size,size);
	for(i=0; i<N; i++)
	   {
		p[i].x = frand();
		p[i].y = frand();
	   }
	for(i=0; i<N; i++) uplot(p[i].x,p[i].y,i);
	uplot(p[0].x,p[0].y,1);
	upolyfill(p,N);
   }
static	long	frandx = 1;

/* sfrand(x) set the seed for the random number generator to x */
sfrand(x)
unsigned int x;
   {
	frandx = x;
   }

/* frand() returns a uniform distribution of random numbers
 * in the range -1.0 -> 1.0.
 */
double frand()
   {
	frandx = (frandx * 1103515245 + 12345) & 0x7fffffff;
	return( (double)(frandx)/1073741824.0 -1.0 );
   }
