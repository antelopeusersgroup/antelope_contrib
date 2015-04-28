#include	<stdio.h>
float x[1000];
main()
   {
	int i;
	double sin();
	for(i=0; i< 1000; i++) x[i]= sin( (float)(i) * 0.01 / 3.14159 );
	for(i=0; i<1000; i++) plot( (float)(i)*0.005, 3.0+x[i]);
   }
