#include	<stdio.h>
#define INCR	.1

main()
   {
	settextfont(0);
	showtext();
	erase();
	settextfont(1);
	showtext();
   }

showtext() 
  {
	float x, y, size;
	x = 1.0;
	y = 1.0;
	for (size = INCR; y+size< 8.5; size+=INCR) {
		settextsize(size);
		text(x, y,"Text size is: %.2f", size);
		y+=(size < .2 ? size*2 : size+.2);
	}
  }
