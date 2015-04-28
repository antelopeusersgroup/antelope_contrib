#include	<stdio.h>
FILE *f, *fopen();

main()
   {
	float x, y;
	float x1, y1, x2, y2;
	char c;

	f= fopen("monitor","w");
	box(0.5,0.5,7.5,7.5);
	/*
	plot(0.5,0.5,0);
	plot(7.5,0.5,1);
	plot(7.5,7.5,1);
	plot(0.5,7.5,1);
	plot(0.5,0.5,1);
	*/

	plot(5.0,5.0,0);

	while( (c = getbox(&x1,&y1,&x2,&y2) ) != '\0')
	   {
		/*
		fprintf(f,"c=%c x=%7.2f y=%7.2f\n",c,x,y);
		fflush(f);
		*/
		switch(c)
		   {
			case 'q': goto last;

			case 'd':
				box(x1,y1,x2,y2);
				break;
			
			case 'm':
				break;
		   }
	