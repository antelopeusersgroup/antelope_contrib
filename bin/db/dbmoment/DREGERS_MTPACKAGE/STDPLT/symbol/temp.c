#include	<stdio.h>
#define DTOR	0.0174532925
main(ac,av)
int ac; char **av;
   {
	int n, i;
	char *name;
	double c, s, sin(), cos(), ang, dang;
	ac--; av++;
	n= atol(av[0]);
	name= av[1];
	fprintf(stdout,"+ %s %2d 0 0\n",name,n);
	dang= (float)(360/n);
	for(i= 0, ang= 0.0; i< n; i++, ang += dang)
	   {
		c= 0.5* cos(ang *DTOR);
		s= 0.5* sin(ang *DTOR);
		fprintf(stdout,"\t%8.5f %8.5f\n",c,s);
	   }
   }
