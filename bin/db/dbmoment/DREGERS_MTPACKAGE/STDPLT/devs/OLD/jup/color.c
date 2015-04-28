#include <stdio.h>  /*****TAKE OUT LATER****/

extern int plotout;  

setcolor(icol)
int icol;
   {
	char c[2];
	c[0]= 'C';
	c[1]= icol &0xff;
	write(plotout,c,2);  
   }

defcolor(icol,ir,ig,ib)
int icol,ir,ig,ib;
   {
	char c[6];
	c[0]= 'K';
	c[1]= icol & 0xff;
	c[2]= 1;
	c[3]= ir &0xff;
	c[4]= ig &0xff;
	c[5]= ib &0xff;
	write(plotout,c,6); 
   }
