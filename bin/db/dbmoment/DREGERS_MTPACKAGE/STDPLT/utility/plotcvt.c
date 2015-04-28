/*
 * converts old plot format to new format
 * Author- Rob Clayton, Seismo Lab, Caltech
 */

#include	<stdio.h>
#include	"../h/igl.h"
#define ENDRAS	-1	/* end of raster data flag */

char inbuf[BUFSIZ];	/* input buffer -used by getc/h */
int newplot;		/* new plot flag */
int fatbase,fat;	/* line thickness parameters */
int xwmax;
int xwmin;
int ywmax;
int ywmin;
int xnew,ynew;		/* new pen location */
int xold,yold;		/* old pen location */
int xorigin,yorigin;	/* origin for text mode */
int xorig,yorig;	/* global origin to be set by 'o' command */
int erase;		/* erase flag */
float scale	=1.0;	/* global default scale factor */
float xscale	=1.0;	/* global x-scale factor */
float yscale	=1.0;	/* global y-scale factor */
float xdscale	=1.0;	/* global x-scale factor */
float ydscale	=1.0;	/* global y-scale factor */

#define GETX	(geth(stdin)-xorig)*xscale*5.0
#define GETY	(geth(stdin)-yorig)*yscale*5.0

#define WARN	0
#define FATAL	1
#define PENDING	2
#define FINAL	3


main(argc,argv)
int argc; char **argv;
   {
	register int i, len; register char c; register short *ptr;
	char *prasbuf, *rasbuf, *malloc();
	FILE *rasfile, *fopen();
	int nx, ny;

	setpar(argc,argv);
	setbuf(stdin,inbuf);
	getpar("scale","f",&scale);
	getpar("xscale","f",&xscale);
	getpar("yscale","f",&yscale);
	xscale *= scale*xdscale;
	yscale *= scale*ydscale;
	getpar("fat","d",&fatbase);
	fat= fatbase;
	endpar();

	erase= -1; newplot= 1;

	while((c=getc(stdin))!= EOF)
	   {
		switch(c)		/* loop over plot commands */
		   {
			 case 'm':		/* move */
				xold= xorigin= GETX;
				yold= yorigin= GETY;
				output1(IGL_MOVE,xold,yold);
				break;
			 case 'd':		/* draw */
				xnew=GETX; ynew=GETY;
				output1(IGL_DRAW,xnew,ynew);
				xold=xnew; yold=ynew;
				break;
			 case 'p':		/* plot point */
				xnew=GETX; ynew=GETY;
				output1(IGL_POINT,xnew,ynew);
				xold=xnew; yold=ynew;
				break;
			 case 't':		/* text */
				/*text();*/
				break;
			 case 'r':		/* raster */
				/*raster(stdin);*/
				break;
			 case 'R':		/* raster file */
				if( (rasbuf=malloc(BUFSIZ))==NULL)
				     err(FATAL,"cannot allocate raster buffer");
				prasbuf= rasbuf;
				while( (*prasbuf=getc(stdin)) != EOF )
				   {
					if(*prasbuf=='\n' || *prasbuf=='\0')
						break;
					prasbuf++;
				   }
				*prasbuf= '\0';
				if((rasfile=fopen(rasbuf,"r"))==NULL)
					err(FATAL,"cannot open raster file %s",
						rasbuf);
				setbuf(rasfile,rasbuf);
				/*raster(rasfile);*/
				fclose(rasfile);
				free(rasbuf);
				break;
			 case 'e':		/* erase */
			 case 'b':		/* break */
				output2(IGL_ERASE);
				break;
			 case 'f':		/* fat */
				fat= fatbase+ geth(stdin);
				output3(IGL_SETFAT,fat);
				break;
			 case 'o':		/* set origin */
				xorig= geth(stdin);
				yorig= geth(stdin);
				xold=xorigin= xorig*xscale*5.0;
				yold=yorigin= yorig*yscale*5.0;
				break;
			 case 'w':		/* set plot window */
				xwmin=GETX; ywmin=GETY;
				xwmax=GETX; ywmax=GETY;
				output4(IGL_WINDOW,xwmin,ywmin,xwmax,ywmax);
				break;
			case 'a':		/* polygon shade */
				/*area(0);*/
				break;
			case 'B':		/* box fill */
				/*boxfill();*/
				break;
			case 'c':		/* colour */
				geth(stdin);
			case 'x':		/* display X-hairs */

			case 'n':		/* no op */
				break;
			default: 		/* error */
				err(PENDING,"invalid plot command %c",c);
				err(FINAL,"cannot recover"); /* error is fatal */
		   }
		erase = 0;
	   }
	newplot=0;
	erase=0;
   }

err(type,fmt,a1,a2,a3)
int type; char *fmt; double a1,a2,a3;
   {
	fprintf(stderr,"pen: ");
	switch(type)
	   {
		case WARN:
			fprintf(stderr,"(warning) ");
			fprintf(stderr,fmt,a1,a2,a3);
			fprintf(stderr,"\n");
			break;
		case FATAL:
		default:
			fprintf(stderr,"(fatal) ");
			fprintf(stderr,fmt,a1,a2,a3);
			fprintf(stderr,"\n");
			exit(-1);
		case PENDING:
			fprintf(stderr,"(fatal) ");
			fprintf(stderr,fmt,a1,a2,a3);
			break;
		case FINAL:
			fprintf(stderr,"\n");
			exit(-1);
	   }
   }
output1(c,x,y)
char c;
int x, y;
   {
	putc(c,stdout);
	puth(x,stdout);
	puth(y,stdout);
   }
output2(c)
char c;
   {
	putc(c,stdout);
   }
output3(c,n)
char c;
int n;
   {
	putc(c,stdout);
	putc(n,stdout);
   }
output4(c,x1,y1,x2,y2)
char c;
int x1, y1,x2,y2;
   {
	putc(c,stdout);
	puth(x1,stdout);
	puth(y1,stdout);
	puth(x2,stdout);
	puth(y2,stdout);
   }
