#include	<stdio.h>

int nang	=90;
float rad	=5.0;
float x0	=1.0;
float y0	=1.0;

main(ac,av)
int ac; char **av;
   {
	int i;
	double dang, angle, cos(), sin();
	float xp, yp;
/*
	setpar(ac,av);
	getpar("nang","d",&nang);
	getpar("rad","f",&rad);
	getpar("x0","f",&x0);
	getpar("y0","f",&y0);
	endpar();
*/

	dang= 90.0/ (double)(nang-1);
	setorig(x0,y0);
	for(i=0; i<nang; i++)
	   {
		angle= (i*dang)*3.14159/180.0;
		xp= rad * sin(angle);
		yp= rad * cos(angle);
		uplot(0.0,0.0,0);
		uplot( xp, yp,1);
/*
fprintf(stderr,"%3d %7.2f %7.2f\n",i,xp,yp);
*/
	   }
   }
