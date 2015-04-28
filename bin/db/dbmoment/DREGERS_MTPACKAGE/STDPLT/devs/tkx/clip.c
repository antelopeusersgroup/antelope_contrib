/* code for clip */
#define code(x,y) (x<ixwmin?1:(x>ixwmax?2:0))|(y<iywmin?4:(y>iywmax?8:0))

clip(x1,y1,x2,y2)	/* window the plot */
int *x1,*y1,*x2,*y2;
   {
	register int c1,c2,temp;
	register int swap;
	extern int ixwmin, iywmin, ixwmax, iywmax;

	c1=code(*x1,*y1);
	c2=code(*x2,*y2);
	swap=0;
	if(!(c1||c2)) return(0); /* line completely in bounds */
	while(c1|c2)
	   {
		if( c1&c2 )
			return(1);  /* line completely out of bounds */
		if(!c1)	/* interchange endpoints */
		   {
			temp= *x1;*x1= *x2;*x2=temp;
			temp= *y1;*y1= *y2;*y2=temp;
			temp=c1;c1=c2;c2=temp;
			swap= ~swap;
		   }
		if(c1<4)	/* move endpoint in x */
		   {
			temp=(c1&2?ixwmax:ixwmin);
			*y1= solve(temp,*x1,*y1,*x2,*y2);
			*x1=temp;
		   }
		  else		/* move endpoint in y */
		   {
			temp=(c1&8?iywmax:iywmin);
			*x1= solve(temp,*y1,*x1,*y2,*x2);
			*y1=temp;
		   }
		c1=code(*x1,*y1);
	   }
	if( swap )	/* put endpoints in order */
	   {
		temp= *x1; *x1= *x2; *x2=temp;
		temp= *y1; *y1= *y2; *y2=temp;
	   }
	return(0);
   }

solve(pnot,p1,q1,p2,q2)
register int pnot,p1,q1,p2,q2;
   {
	/* floating point version */
	double invslope;
	register int qnot;
	if(pnot==p1) return(q1);
	if(pnot==p2) return(q2);
	if(q1==q2) return(q1);
	invslope= (q1-q2)/( (double) (p1-p2));
	qnot= (pnot-p1)*invslope + (double) q1 + 0.5;
	return(qnot);
   }
