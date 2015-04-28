#include 	<stdio.h>
#include	<math.h>

main()
{
	int icount;
	float x,y;
	float theta;

	icount = 0;
	theta = 0.0;

	while (theta < 6.283185)
          {
		x = cos(theta);
		y = sin(theta);
		x *= .5;
		y *= .5;
		x += .5;
		y += .5;


		fprintf(stdout,"%7.3f %7.3f",x,y);
		if ((icount%4)==0) fprintf(stdout,"\n");
		theta += .1;
		icount++;
	  }
	fprintf(stdout,"icount = %d\n",icount);
}
