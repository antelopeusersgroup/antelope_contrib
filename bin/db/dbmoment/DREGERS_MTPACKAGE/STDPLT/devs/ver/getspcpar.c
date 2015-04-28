#include	<stdio.h>

int domap	=0;
extern float squirt;

get_spc_par()
   {
	getpar("domap","b",&domap);
	getpar("squirt","f",&squirt);
   }
