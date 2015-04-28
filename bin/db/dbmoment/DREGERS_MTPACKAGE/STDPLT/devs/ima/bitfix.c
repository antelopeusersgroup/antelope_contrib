#include <stdio.h>  /*take this out later*/
bitfix(x,n)
register struct { char a, b, c, d; } *x;
register int n;
   {
	/*
	char t;
	while(n--)
	   {
		t= x->a; x->a= x->d; x->d= t;
		t= x->c; x->c= x->b; x->b= t;
		x++;
	   }
	*/
   }
