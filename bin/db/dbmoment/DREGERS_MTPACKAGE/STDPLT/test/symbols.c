#define MAX_SYMBOLS	20
#define TEXT_SIZE	0.5
/*
 * test plot for all of the symbols
 */
#include	<stdio.h>
main(argc,argv)
short argc; char **argv;
   {
	int symbol();
	int i;
	double x,y;

	argc--; argv++;
	x = 1.0; y = 0.0;
	for (i = 0; i< MAX_SYMBOLS; i++) {
		x += 1.5;
		if (i%4 == 0) {
			y += 1.0;
			x = 1.0;
		}
		symbol (x,y,i,.5,0.);
	}
	settextsize(TEXT_SIZE);
	y+=TEXT_SIZE;
	x=1.;
	settextfont(0);
	text(x,y,"Symbol Test");
	settextsize(TEXT_SIZE);
	y+=TEXT_SIZE;
	x=1.;
	settextfont(1);
	text(x,y,"Symbol Test");
   }
