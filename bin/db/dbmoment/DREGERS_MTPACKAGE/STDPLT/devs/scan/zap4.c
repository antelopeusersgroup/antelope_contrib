zap4(x,n)
register int *x, n;
   {
	register int cnt;

	cnt= n%8;
	while(cnt--) *x++ = 0;
	cnt= n/8;
	while(cnt--)
	   {
		*x++ = 0;
		*x++ = 0;
		*x++ = 0;
		*x++ = 0;
		*x++ = 0;
		*x++ = 0;
		*x++ = 0;
		if (cnt>1) *x++ = 0;
		else *x=0;
	   }
   }
