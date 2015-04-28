#include	<stdio.h>
main()
   {
	int i;
	setscl(1.0,1.0);
	setorig(1.0,1.0);
	setmin(1.0,1.0);
	uplot(1.0,1.0,0);
	uplot(4.0,4.0,1);
	for(i=0; i<4; i++) dumpframe(i);

	pushframe();
	setscl(1.0,1.0);
	setorig(10.0,10.0);
	setmin(1.0,1.0);
	uplot(1.0,1.0,0);
	uplot(4.0,4.0,1);
	for(i=0; i<4; i++) dumpframe(i);
	popframe();
	for(i=0; i<4; i++) dumpframe(i);

	uplot(1.0,1.0,0);
	uplot(4.0,4.0,1);

	text(1.0,1.0,"TEXT EXAMPLE");
	fprintf(stdout,"Random user text on stdout\n");
	defdash(4,0.1,0.1,0.1,0.1);
	defcolor(35,0.25,0.50,0.75);
	setcolor(35);
	pushglobal();
	setcolor(1);
	popglobal();
   }
