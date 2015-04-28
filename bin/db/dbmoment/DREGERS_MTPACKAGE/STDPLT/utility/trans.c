#include	<stdio.h>
#include	"../h/igl.h"

char line[1024];
main(ac,av)
int ac; char **av;
   {
	int c;
	char *pline, *p;
	int n, n1, n2,n3, n4, n5, n6, n7, npoly, ipoly, i;
	int x, y, x1, y1, x2, y2;
	FILE *in, *out;

	in= stdin;
	out= stdout;

#ifdef MKASCII
	while( (c= (int)getc(in)) != EOF)
	   {
		c &= 0xff;
		if( (c & M) == 0)	/* non-plot text */
		   {
			putc(c,stderr);
			while( (c=(int)getc(in)) != EOF)
			   {
				if( (c&M) != 0) break;
				putc(c,stderr);
			   }
			if(c == EOF) break;
			c &= 0xff;
		   }
#else
	while( (n= getline(line,in)) >= 0)
	   {
		if(n == 0) continue;
		c= line[0] & 0x7f;
		c |= M;
		pline= &line[1];
#endif

		switch(c)	/* master command switch */
		   {
			/* format #1 */
			case IGL_MOVE:
			case IGL_DRAW:
			case IGL_POINT:
#ifdef MKASCII
				getxy(&x,&y,in);
				fprintf(out,"%c %d %d\n",c,x,y);
#else
				sscanf(pline,"%d %d\n",&x,&y);
				putc(c|M,out);
				putxy(x,y,out);
#endif
				break;

			/* format #2 */
			case IGL_LINE:
			case IGL_BOX:
			case IGL_WINDOW:
			case IGL_BOXFILL:
#ifdef MKASCII
				getxy(&x1,&y1,in);
				getxy(&x2,&y2,in);
				fprintf(out,"%c %d %d %d %d\n",c,x1,y1,x2,y2);
#else
				sscanf(pline,"%d %d %d %d\n",&x1,&y1,&x2,&y2);
				putc(c|M,out);
				putxy(x1,y1,out);
				putxy(x2,y2,out);
#endif
				break;

			
			case IGL_UNWINDOW:
			case IGL_ERASE:
#ifdef MKASCII
				fprintf(out,"%c\n",c);
#else
				putc(c|M,out);
#endif
				break;

			case IGL_SETPEN:
			case IGL_SETPENMODE:
			case IGL_SETFAT:
			case IGL_SETPENCOLOR:
			case IGL_SETBRUSH:
			case IGL_SETBRUSHCOLOR:
			case IGL_SETPATTERN:
			case IGL_SETBRUSHMODE:
			case IGL_SETRASTMODE:
			case IGL_TEXTCENTER:
#ifdef MKASCII
				n= getc(in) & 0xff;
				fprintf(out,"%c %d\n",c,n);
#else
				sscanf(pline,"%d\n",&n);
				putc(c|M,out);
				putc(n&0xff,out);
#endif
				break;

			case IGL_DEFPEN:
#ifdef MKASCII
				n1= getc(in) & 0xff;
				n2= getc(in) & 0xff;
				n3= getc(in) & 0xff;
				n4= getc(in) & 0xff;
				n5= getc(in) & 0xff;
				n6= getc(in) & 0xff;
				n7= getc(in) & 0xff;
				fprintf(out,"%c %d %d %d %d %d %d %d\n",
					c,n1,n2,n3,n4,n5,n6,n7);
#else
				sscanf(pline,"%d %d %d %d %d %d %d\n",
					&n1,&n2,&n3,&n4,&n5,&n6,&n7);
				putc(c|M,out);
				putc(n1&0xff,out);
				putc(n2&0xff,out);
				putc(n3&0xff,out);
				putc(n4&0xff,out);
				putc(n5&0xff,out);
				putc(n6&0xff,out);
				putc(n7&0xff,out);
#endif
				break;

			case IGL_DEFCOLOR:
#ifdef MKASCII
				n1= getc(in) & 0xff;
				n2= getc(in) & 0xff;
				n3= getc(in) & 0xff;
				n4= getc(in) & 0xff;
				fprintf(out,"%c %d %d %d %d\n",c,n1,n2,n3,n4);
#else
				sscanf(pline,"%d %d %d %d\n",&n1,&n2,&n3,&n4);
				putc(c|M,out);
				putc(n1&0xff,out);
				putc(n2&0xff,out);
				putc(n3&0xff,out);
				putc(n4&0xff,out);
#endif
				break;
			
			case IGL_DEFDASH:
#ifdef MKASCII
				n1= getc(in) & 0xff;
				n2= geth(in);
				n3= geth(in);
				n4= geth(in);
				n5= geth(in);
				fprintf(out,"%c %d %d %d %d %d\n",
					c,n1,n2,n3,n4,n5);
#else
				sscanf(pline,"%d %d %d %d %d\n",
					&n1,&n2,&n3,&n4,&n5);
				putc(c|M,out);
				putc(n1&0xff,out);
				puth(n2,out);
				puth(n3,out);
				puth(n4,out);
				puth(n5,out);
#endif
				break;
			
			case IGL_DEFPATTERN:
#ifdef MKASCII
				n1= getc(in) & 0xff;
				fprintf(out,"%c %d\n",c,n1);
				for(i=0; i<32; i++)
				   {
					n1= getw(in);
					fprintf(out,"\t%x\n",n1);
				   }
#else
				sscanf(pline,"%d\n", &n1);
				putc(c|M,out);
				putc(n1&0xff,out);
				for(i=0; i<32; i++)
				   {
					fscanf(in,"%x",&n1);
					putw(n1,out);
				   }
#endif
				break;

			case IGL_DEFBRUSH:
#ifdef MKASCII
				n1= getc(in) & 0xff;
				n2= getc(in) & 0xff;
				n3= getc(in) & 0xff;
				n4= getc(in) & 0xff;
				n5= getc(in) & 0xff;
				fprintf(out,"%c %d %d %d %d %d\n",
					c,n1,n2,n3,n4,n5);
#else
				sscanf(pline,"%d %d %d %d %d\n",
					&n1,&n2,&n3,&n4,&n5);
				putc(c|M,out);
				putc(n1&0xff,out);
				putc(n2&0xff,out);
				putc(n3&0xff,out);
				putc(n4&0xff,out);
				putc(n5&0xff,out);
#endif
				break;



		/* areal-fill commands */
			case IGL_POLYFILL:
#ifdef MKASCII
				n= geth(in);
				fprintf(out,"%c %d\n",c,n);
				for(i=0; i<n; i++)
				   {
					getxy(&x,&y,in);
					fprintf(out,"\t%d %d\n",x,y);
				   }
#else
				sscanf(pline,"%d",&n);
				putc(c|M,out);
				puth(n,out);
				for(i=0; i<n; i++)
				   {
					fscanf(in,"%d %d",&x,&y);
					putxy(x,y,in);
				   }
#endif
				break;

			case IGL_POLYFILLN:
#ifdef MKASCII
				npoly= geth(in);
				fprintf(out,"%c %d\n",c,n);
				for(ipoly=0; ipoly<npoly; ipoly++)
				   {
					n= geth(in);
					fprintf(out,"\t%d\n",n);
					for(i=0; i<n; i++)
					   {
						getxy(&x,&y,in);
						fprintf(out,"\t%d %d\n",x,y);
					   }
				   }
#else
				sscanf(pline,"%d",&npoly);
				putc(c|M,out);
				puth(npoly,out);
				for(ipoly=0; ipoly<npoly; ipoly++)
				   {
					fscanf(in,"%d",&n);
					puth(n,out);
					for(i=0; i<n; i++)
					   {
						fscanf(in,"%d %d",&x,&y);
						putxy(x,y,in);
					   }
				   }
#endif
				break;

		/* text commands */
			case IGL_TEXT:
#ifdef MKASCII
				fprintf(out,"%c \n",c);
				while( (c=getc(in)) != NULL)
				   {
					if(c == '\n') *p++ = '\\';
					*p++= c;
				   }
				fprintf(out,"%s\n",line);
#else
				/* imbedded newlines not properly handled */
				putc(c|M,out);
				p= &line[2];
				while( (c= *p++) != '\n') putc(c,out);
				putc(NULL,out);
#endif
				break;

			case IGL_SETTEXTANGLE:
			case IGL_SETTEXTSIZE:
#ifdef MKASCII
				n= geth(in);
				fprintf(out,"%c %d\n",c,n);
#else
				sscanf(pline,"%d\n",&n);
				putc(c|M,out);
				puth(n,out);
#endif
				break;

		/* raster commands */
			/*
			case IGL_RASTER:
				raster(file);
				break;
			case IGL_RASTERFILE:
				cptr= buf;
				do *cptr = getc(file); while(*cptr++);
				temp= fopen(buf,"r");
				if(temp == NULL)
				   {
					err(WARN,"cannot open raster file %s\n",
						buf);
					break;
				   }
				raster(temp);
				fclose(temp);
				break;
			*/
				 
			
			default:
				fprintf(stderr,"*** ERROR ***\n");
				fprintf(stderr,"unknown plot command= %c =%o\n",
					c&0x7f, c&0xff);
				break;
		   }
	   }
   }


getxy(x,y,file)
register int *x, *y;
FILE *file;
   {
	register struct xypack { short xin, yin; } *xy;

	int xxyy;
	xxyy= getw(file);
	xy= (struct xypack *)(&xxyy);
	*x= (int)(xy->xin);
	*y= (int)(xy->yin);
   }

putxy(x,y,file)
register int x, y;
FILE *file;
   {
	puth(x,file);
	puth(y,file);
   }

getline(line,file)
register char *line;
FILE *file;
   {
	register int n;
	char c;

	n= 0;
	while( (c= getc(file)) != EOF)
	   {
		if(c == '\n')
		   {
			*line++= '\0';
			return(n);
		   }
		*line++= c;
		n++;
	   }
	return(EOF);
   }
