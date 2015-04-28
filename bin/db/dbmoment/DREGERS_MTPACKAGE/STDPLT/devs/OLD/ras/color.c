extern int plotout;

setcolor(icol)
int icol;
   {
	char c[2];
	c[0]= '\260';
	c[1]= icol &0xff;
	write(plotout,c,2);
   }

defcolor(icol,ir,ig,ib)
int icol,ir,ig,ib;
   {
	char c[5];
	c[0]= '\034';
	c[1]= icol & 0xff;
	c[2]= ir &0xff;
	c[3]= ig &0xff;
	c[4]= ib &0xff;
	write(plotout,c,5);
   }
