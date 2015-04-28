#include <stdio.h> /***********************/
do_dot(x,y,size,mode)
int x, y, mode;
float size;
   {
	extern int penfat;
	extern float pixinch;
	register int a, b;

	/*
	if(x)
	   {
		do_point(x,y);
		return;
	   }
	*/
	a = (int)(pixinch * size);
	b = (int)(0.5 * pixinch * size);

	do_line(x-b,y+a,x+b,y+a);
	do_line(x+b,y+a,x+a,y+b);
	do_line(x+a,y+b,x+a,y-b);
	do_line(x+a,y-b,x+b,y-a);
	do_line(x+b,y-a,x-b,y-a);
	do_line(x-b,y-a,x-a,y-b);
	do_line(x-a,y-b,x-a,y+b);
	do_line(x-a,y+b,x-b,y+a);

	if(mode > 0)
	   {
		do_line(x-a,y+b,x+a,y-b);
		do_line(x-a,y-b,x+a,y+b);
		do_line(x-b,y+a,x+b,y-a);
		do_line(x+b,y+a,x-b,y-a);
	   }
   }
