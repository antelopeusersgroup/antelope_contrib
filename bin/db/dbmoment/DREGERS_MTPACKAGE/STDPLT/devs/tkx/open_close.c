#include	"../com/global.h"
#include	"devpar.h"
#include	<stdio.h>

int plotout;

opendev()
   {
	extern int do_erase, do_reset;
	extern gsreqd;
	plotout= 1;
	gsreqd= 1;


	/*
	if(do_erase) erase(); 
	*/
	if(do_erase)
	   {
		write(plotout,"\033\014",2);
	   }
   }

closedev()
   {
	vector(0,0,0,0);
	write(plotout,"\037",1);
	/*
	stty(plotout,&oldtty);
	*/
   }

erase()
   {
   int gsreqd;
	/*
	write(plotout,"\033[2J",4);
	*/
	write(plotout,"\033\014",2);
	sleep(1);
	gsreqd= 1;
   }
