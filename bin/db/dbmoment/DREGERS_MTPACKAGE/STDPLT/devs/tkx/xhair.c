#include	<stdio.h>
#include	<sys/ttold.h>
int xhold[1024], yhold[1024];
main()
   {
	int x1,y1, x2,y2;
	int i, n;
	char c, xhair();
	/*
	setbuttons();
	*/
	x1= y1= 500;
	xhold[0]= x1;
	yhold[0]= y1;
	n=1;
	while((c=xhair(&x2,&y2)) != '\0')
	   {
		switch(c)
		   {
			default:
			case 'x':
				vector(x1,y1,x2,y2);
				break;
			case 'j':
				vector(x1,y1,x2,y2);
				for(i=0; i<n-1; i++)
					vector(xhold[i],yhold[i],x2,y2);
				break;
			case 'q':
				goto last;
		   }
		x1= x2;
		y1= y2;
		xhold[n]= x2;
		yhold[n]= y2;
		n++;
		if(n > 1000) break;
	   }
	unsettty();
	vector(10,10,10,10);
last:	fprintf(stdout,"done n=%d\n",n);
   }

setbuttons()
   {
	write(1,"\033P1;1;0|251/z\033\\",15);
	/*
	write(1,"\033P1;1;0%|26/a\033\\",15);
	*/
   }
settty()
   {
	struct sgttyb buf;
	ioctl(0,TIOCGETP,&buf);
	buf.sg_flags &= ~(O_ECHO);
	/*
	buf.sg_flags |= (O_RAW | O_CBREAK);
	*/
	ioctl(0,TIOCSETP,&buf);
   }
unsettty()
   {
	struct sgttyb buf;
	ioctl(0,TIOCGETP,&buf);
	buf.sg_flags |= (O_ECHO);
	/*
	buf.sg_flags &= ~(O_RAW | O_CBREAK);
	*/
	ioctl(0,TIOCSETP,&buf);
   }
char xhair(x,y)
int *x, *y;
   {
	int n;
	char c, cbuf[32];
	write(1,"\035\033\032",3);
	settty();
	n= read(0,cbuf,16);
	unsettty();
	c= '\0';
	if(n != 6) return(c);
	*x= ((cbuf[1] &0x1f) << 5) | (cbuf[2] &0x1f);
	*y= ((cbuf[3] &0x1f) << 5) | (cbuf[4] &0x1f);
	c = cbuf[0];
	return(c);
   }

#define GS	035
int plotout =1;
int xcur	=0;
int ycur	=0;
int	gsreqd	=0;
vector(x1,y1,x2,y2)
register int x1,y1,x2,y2;
   {
	char cbuf[16];
	register char *pc;
	register int n;
	extern int xcur, ycur, gsreqd;
	extern int plotout;
	static int xhold = -10000;
	static int yhold = -10000;

	/*
	fprintf(stderr,"x1=%3d y1=%3d , x2=%3d y2=%3d\n",x1,y1,x2,y2);
	fprintf(stderr,"xcur=%3d ycur=%3d\n",xcur,ycur);
	*/
	pc= cbuf;
	n= 0;
	gsreqd= 1;
	if(gsreqd || x1 != xhold || y1 != yhold) /* do a move */
	   {
		*pc++ = GS;
		*pc++ = (y1>>5)|040; 	/* y/32+32 hi y byte */
		*pc++ = (y1&037)|0140; 	/* y%32+96 low y byte */
		*pc++ = (x1>>5)|040; 	/* x/32+32 hi x byte */
		*pc++ = (x1&037)|0100; 	/* x%32+64 low x byte */
		n += 5;
		/*
		fprintf(stderr,"M x1=%3d y1=%3d ",x1,y1);
		*/
	   }
	/* do draw */
	*pc++ = (y2>>5)|040; 	/* y/32+32 hi y byte */
	*pc++ = (y2&037)|0140; 	/* y%32+96 low y byte */
	*pc++ = (x2>>5)|040; 	/* x/32+32 hi x byte */
	*pc++ = (x2&037)|0100; 	/* x%32+64 low x byte */
	n += 4;
	/*
	fprintf(stderr,"D x2=%3d y2=%3d\n",x2,y2);
	*/
	write(plotout,cbuf,n);
	gsreqd= 0;
	xcur= x2;
	ycur= y2;
	xhold= x2;
	yhold= y2;
   }
