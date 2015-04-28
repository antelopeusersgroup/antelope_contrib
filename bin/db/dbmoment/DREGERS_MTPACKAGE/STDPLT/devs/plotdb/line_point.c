#include	<stdio.h>
do_line(x1,y1,x2,y2)
int x1,y1,x2,y2;
   {
	fprintf(stdout,"line: (%4d,%4d) -> (%4d,%4d)\n",x1,y1,x2,y2);
   }

do_point(x,y)
int x,y;
   {
	fprintf(stdout,"point: (%4d,%4d)\n",x,y);
   }
