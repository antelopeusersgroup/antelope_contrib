#include	<stdio.h>
#include	"../../h/igl.h"
#include	"global.h"

char buff[1024];
int intbuff[32];
extern char *devname;
extern float pixinch, xscale, yscale;
extern int xmax, ymax, xpage, ypage, page, grid;
extern int ixwmin, iywmin, ixwmax, iywmax;
extern int xorigin, yorigin, colordev, fatbase;
extern int xcur, ycur, pencur, brushcur;
extern int pencolor, penmode, penfat, pendash;
extern float *dashptr, dashval;
extern int brushmode, brushcolor, brushpat;
extern int textfont, textcenter;
extern float textsize, textangle, textcosang, textsinang,symangle;
extern float textscale,symscale;
extern int rastfillmode;
extern struct pentypes pens[];
extern struct brushtypes brushs[];
extern int patterns[][32];
extern float dashs[][4];
extern char colors[][4];
extern char labelstore[];
extern int do_window;
int do_pause, pausetime;
int do_erase;
extern int labelix,labeliy;

readcom(file,level)
FILE *file;
int level;
   {
	int i,i2,i3;
	struct intpolygon *p, **verts;
	char buf[MAXTEXT+1];
	char *cptr;
	int c;
	int x, y, dx1, dx2, dy1, dy2, npts;
	int icol, rcol, gcol, bcol, ifill;
	int ifat, ipat, x1,x2,y1,y2;
	int delta;
	int cnt, len;
	int npoly, n, nv;
	int idash;
	float on1, off1, on2, off2;
	float tempsize,t2,t3,t4;
	int *nverts, ipen, ibrush, iang, isize;
	float angle, size;
	int isym;
	FILE *temp, *fopen();
	double cos(), sin();
	double PI=3.141592;


	if(level > MAXLEVEL)
		err(FATAL,"too many recursive include files, level=%d",level);

	while( (c= (int)getc(file)) != EOF)
	   {
		c &= 0xff;
		if( (c & M) == 0)	/* non-plot text */
		   {
			putc(c,stderr);
			while( (c=(int)getc(file)) != EOF)
			   {
				if( (c&M) != 0) break;
				putc(c,stderr);
			   }
			if(c == EOF) break;
			c &= 0xff;
		   }
		switch(c)	/* master command switch */
		   {

		/* vector commands */
			case IGL_MOVE:
				getxy(&xcur,&ycur,file);
				dashval= 0.0;
				break;

			case IGL_DRAW:
				getxy(&x,&y,file);
				if(pendash> 0) do_dash(xcur,ycur,x,y);
				 else	       do_line(xcur,ycur,x,y);
				xcur= x;
				ycur= y;
				break;

			case IGL_LINE:
				getxy(&x,&y,file);
				getxy(&xcur,&ycur,file);
				if(pendash > 0) do_dash(x,y,xcur,ycur);
				 else	        do_line(x,y,xcur,ycur);
				break;

			case IGL_POINT:
				getxy(&xcur,&ycur,file);
				do_point(xcur,ycur);
				break;
			
			case IGL_BOX:
				getxy(&xcur,&ycur,file);
				getxy(&x,&y,file);
				if(pendash > 0)
				   {
					do_dash(xcur,ycur,x,ycur);
					do_dash(x,ycur,x,y);
					do_dash(x,y,xcur,y);
					do_dash(xcur,y,xcur,ycur);
				   }
				 else
				   {
					do_line(xcur,ycur,x,ycur);
					do_line(x,ycur,x,y);
					do_line(x,y,xcur,y);
					do_line(xcur,y,xcur,ycur);
				   }
				break;
			
		/* window commands */
			case IGL_WINDOW:
				getxy(&x1,&y1,file);
				getxy(&x2,&y2,file);
				if (x1>x2) {
					i = x1;
					x1 = x2;
					x2 = i;
				}
				if (y1>y2) {
					i = y1;
					y1 = y2;
					y2 = i;
				}
				if (do_window)
			           {
					ixwmin= (x1<0 ? 0 : x1);
					iywmin= (y1<0 ? 0 : y1);
					ixwmax= (x2 > xmax ? xmax : x2);
					iywmax= (y2 > ymax ? ymax : y2);
				    }
				break;
			
			case IGL_UNWINDOW:
				ixwmin= 0;
				iywmin= 0;
				ixwmax= xmax;
				iywmax= ymax;
				break;

		/* pen-state commands */
			case IGL_SETPEN:
				ipen= getc(file) & 0xff;
				if(colordev)
					if(pens[ipen].pen_cflags == 0) break;
				if(!(colordev))
					if(pens[ipen].pen_bwflags == 0) break;
				pencolor= pens[ipen].pen_color;
				setcolor(pencolor);
				if(colordev) pendash= pens[ipen].pen_cdash;
				 else	     pendash= pens[ipen].pen_idash;
				dashptr= dashs[pendash];
				if(dashptr[0] <= 0.0) pendash= 0;
				dashval= 0.0;

				penmode= pens[ipen].pen_mode;
				if(colordev) penfat= pens[ipen].pen_fat;
				 else        penfat= pens[ipen].pen_cfat; 
				break;

			case IGL_DEFBWPEN:
				ipen= getc(file) & 0xff;
				pens[ipen].pen_fat   = getc(file) & 0xff;
				pens[ipen].pen_idash = getc(file) & 0xff;
				pens[ipen].pen_mode  = getc(file) & 0xff;
				pens[ipen].pen_bwflags = 1;
				break;

			case IGL_DEFCPEN:
				ipen= getc(file) & 0xff;
				pens[ipen].pen_cfat  = getc(file) & 0xff;
				pens[ipen].pen_cdash = getc(file) & 0xff;
				pens[ipen].pen_color = getc(file) & 0xff;
				pens[ipen].pen_cflags = 1;
				break;

			case IGL_SETPENMODE:
				penmode= getc(file) & 0x3;
				break;
			
			case IGL_SETFAT:
				ifat= getc(file) & 0xff;
				penfat= fatbase +ifat;
				if(penfat < 0) penfat=0;
				if(penfat > MAXFAT) penfat= MAXFAT;
				break;

			case IGL_SETDASH:
				pendash = getc(file) & 0xff;
				dashptr= dashs[pendash];
				break;
				

			case IGL_SETPENCOLOR:
				pencolor= getc(file) & 0xff;
				setcolor(pencolor);
				break;

			case IGL_DEFCOLOR:
				icol= getc(file) & 0xff;
				rcol= getc(file) & 0xff;
				gcol= getc(file) & 0xff;
				bcol= getc(file) & 0xff;
				colors[icol][0]= 1;
				colors[icol][1]= rcol;
				colors[icol][2]= gcol;
				colors[icol][3]= bcol;
				defcolor(icol,rcol,gcol,bcol);
				break;

			case IGL_DEFDASH:
				idash= getc(file) & 0xff;
				on1 = (float)(geth(file)) / FLOATNORM;
				off1= (float)(geth(file)) / FLOATNORM;
				on2 = (float)(geth(file)) / FLOATNORM;
				off2= (float)(geth(file)) / FLOATNORM;
				dashs[idash][0]= on1;
				dashs[idash][1]= on1 +off1;
				dashs[idash][2]= on1 +off1 +on2;
				dashs[idash][3]= on1 +off1 +on2 +off2;
				/* scale dash lengths to pixels */
				dashs[idash][0] *=  pixinch;
				dashs[idash][1] *=  pixinch;
				dashs[idash][2] *=  pixinch;
				dashs[idash][3] *=  pixinch;
				break;
			
		/* brush-state commands */
			case IGL_DEFPATTERN:
				ipat= getc(file) & 0xff;
				fread(intbuff,4,32,file); 
				for (i=0; i<32 ; i++)  
					patterns[ipat][i]= intbuff[i];
				break;


			case IGL_DEFBWBRUSH:
				ibrush= getc(file) & 0xff;
				brushs[ibrush].brush_ipat  = getc(file) & 0xff;
				brushs[ibrush].brush_mode  = getc(file) & 0xff;
				brushs[ibrush].brush_bwflags = 1;
				break;

			case IGL_DEFCBRUSH:
				ibrush= getc(file) & 0xff;
				brushs[ibrush].brush_color = getc(file) & 0xff;
				brushs[ibrush].brush_cflags = 1;
				break;

			case IGL_SETBRUSH:
				ibrush= getc(file) & 0xff;
				if(colordev)
				   if(brushs[ibrush].brush_cflags == 0) break;
				if(!(colordev))
				   if(brushs[ibrush].brush_bwflags == 0) break;
				brushcolor= brushs[ibrush].brush_color;
				brushpat  = brushs[ibrush].brush_ipat;
				brushmode = brushs[ibrush].brush_mode;
				break;

			case IGL_SETBRUSHCOLOR:
				brushcolor= getc(file) & 0xff;
				break;


			case IGL_SETPATTERN:
				ipat= getc(file) & 0xff;
				brushpat = ipat;
				break;

			case IGL_SETBRUSHMODE:
				brushmode= getc(file) & 0x7;
				break;

		/* raster commands */
			case IGL_RASTER:
				raster(file);
				break;
			case IGL_RASTERFILE:
				cptr= buf;
				i = 0;
				do {
					*cptr = getc(file); 
					i++;
				} while(*cptr++ && i<MAXTEXT+1);
				*cptr++= '\0';
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
			case IGL_SETRASTMODE:
				rastfillmode= getc(file) & 0x7;
				break;

		/* areal-fill commands */
			case IGL_POLYFILL:
				/* set this up to use a buffer */
				setcolor(brushcolor);
				npts= geth(file);
				len= (npts+1)*sizeof(struct intpolygon);
				p= (struct intpolygon *)malloc(len);
				if(p == NULL)
					err(FATAL,"cannot allocate memory for polyfill npts=%d",npts);
				for(i=0; i<npts; i++)
				   {
					getxy(&x,&y,file);
					p[i].ixv= x;
					p[i].iyv= y;
				   }
				polyfill(npts,p);  
				free(p);
				setcolor(pencolor);
				break;

			case IGL_POLYFILLN:
				setcolor(brushcolor);
				npoly= geth(file);
				nverts= (int *)malloc(npoly*4);
				verts= (struct intpolygon **)malloc(npoly*4);
				if (nverts == NULL || verts == NULL)
				 err(FATAL,"cannot allocate memory for polyfilln npoly=%d",npoly);

				for (n=0; n<npoly; n++)
				   { 
					nv= nverts[n]= geth(file);
					len= (nv+1)*sizeof(struct intpolygon);
					p= verts[n]= (struct intpolygon *)malloc(len);
					if(p == NULL)
						err(FATAL,"cannot allocate memory for polyfill nv=%d",nv);
					for(i=0; i<nv; i++)
					  {
						getxy(&x,&y,file);
						p[i].ixv= x;
						p[i].iyv= y;
					   }
				   }
				polyfilln(npoly,nverts,verts);  
				for(n=0; n<npoly; n++)
				   {
					free(verts[n]);
				   }
				free(nverts);
				free(verts);
				setcolor(pencolor);
				break;

			case IGL_BOXFILL:
				setcolor(brushcolor);
				getxy(&x1,&y1,file);
				getxy(&x2,&y2,file);
				boxfill(x1,y1,x2,y2);
				setcolor(pencolor);
				break;

		/* text commands */
			case IGL_TEXT:
				getxy(&xcur,&ycur,file);
				/* this is not very robust!!!! */
				cptr= buf;
				i=0;
				do {
					*cptr = getc(file);
					i++;
				}    while(*cptr++ && i<MAXTEXT+1);
				*cptr++= '\0';
				if(textfont > 0) rastext(xcur,ycur,buf);
				 else		 text(xcur,ycur,buf);
				break;


			case IGL_SETTEXTANGLE:
				textangle= (float)(geth(file))/ANGLENORM;
				textangle *= 360.0; 
				switch(page)
				 {
					case 0:
						break; /* no rotation */
					case 1:
						textangle += 90.0;
						if (textangle>=360.0)
						 	 textangle-=360.0;
						break;
					case 2:
						textangle += 180.0;
						if (textangle>=360.0)
						 	 textangle-=360.0;
						break;
					case 3:
						textangle += 270.0;
						if (textangle>=360.0)
						 	 textangle-=360.0;
						if (textangle>360.0)
						 	 textangle-=360.0;
						break;
				 } /*switch*/

				textangle*= 2*PI/360.0; /* change to radians */
				textcosang= cos(textangle);
				textsinang= sin(textangle);
				break;
				 
			case IGL_SETTEXTSIZE:
				textsize= (float)(geth(file))/FLOATNORM;
				textsize = textsize*textscale;
				break;
				 
			case IGL_TEXTCENTER:
				textcenter= getc(file) &0xff;
				break;


			case IGL_SETTEXTFONT:
				textfont= getc(file) &0xff;
				break;

		/*symbol commands*/
			case IGL_SYMBOL:
				isym = getc(file);
				getxy(&x,&y,file);
				size= (float)(geth(file))/FLOATNORM;
				angle= 360.0*(float)(geth(file))/ANGLENORM;
				do_symbol(x,y,isym,size,angle);
				break;

			case IGL_BDOT: /* special symbol for halftoning */
				getxy(&x,&y,file);
				size= (float)(geth(file))/FLOATNORM;
				/*
				fprintf(stderr,"x=%5d y=%5d size=%7.3f\n",x,y,size);
				*/
				do_dot(x,y,size,1);
				break;

			case IGL_WDOT: /* special symbol for halftoning */
				getxy(&x,&y,file);
				size= (float)(geth(file))/FLOATNORM;
				do_dot(x,y,size,0);
				break;

		/* misc. commands */
			case IGL_ERASE:
				if (do_pause) sleep(pausetime);
				erase();
				/* assume more to come */
				return(1);
				break;

			case IGL_PAUSE: 
				if (do_pause) sleep(pausetime);
				break;
				 
			/*
			case IGL_INCLUDE:
				cptr= buf;
				i=0;
				do {
					*cptr = getc(file);
					i++;
				}
					while(*cptr++ && i<MAXTEXT+1);
				*cptr++= '\0';
				temp= fopen(buf,"r");
				if(temp == NULL)
				   {
					err(WARN,"cannot open include file %s\n",
						buf);
					break;
				   }
				readcom(temp,level+1);
				fclose(temp);
				break;
				
			case IGL_INIT:
			*/
			case IGL_ENDPLOT:  /* end program */
				if (do_pause) sleep(pausetime);
				/*assume more to come*/
				if (do_erase) erase();
				return(1);
				break;
			
			case IGL_PLOTLABEL:
				/* this is not very robust!!!! */
				cptr= buf;
				i=0;
				do {
					*cptr = getc(file);
					i++;
				}	while(*cptr++ && i<MAXTEXT+1);
				*cptr++= '\0';
				sprintf(labelstore,"%s",buf);
				break;

			default:
				fprintf(stdout,"*** ERROR ***\n");
				fprintf(stdout,"unknown plot command= %c =%o\n",
					c&0x7f, c&0xff);
				break;
		   } /*switch*/
	   }  /*while*/
	return(0); /* if we reach here, that's all */
   } /*readcom*/


/*   This routine supplies all (x,y) coordinates to the drivers.  If we
     change to a greater number of bits per coordinate, we need only change
     this routine as far as the device drivers.  A variable number of bits
     could also be supported here.
*/
getxy(x,y,file)
register int *x, *y;
FILE *file;
   {
	extern int xorigin, yorigin, xpage, ypage;
	extern float xscale, yscale;
	extern int page;
	register struct xypack { short xin, yin; } *xy;

	int xxyy;
	xxyy= getw(file);
	xy= (struct xypack *)(&xxyy);
	/* need to fix these page rotations up. Hopefully correct now. */
	switch(page)
	   {
		case 0:
			*x= xorigin + xscale * ( (int)(xy->xin) );
			*y= yorigin + yscale * ( (int)(xy->yin) );
			return;
		case 1:
			*x= yorigin - yscale * ( (int)(xy->yin) );
			*y= xorigin + xscale * ( (int)(xy->xin) );
			return;
		case 2:
			*x= xorigin - xscale * ( (int)(xy->xin) );
			*y= yorigin - yscale * ( (int)(xy->yin) );
			return;
		case 3:
			*x= yorigin + yscale * ( (int)(xy->yin) );
			*y= xorigin - xscale * ( (int)(xy->xin) );
			return;
	   }
   }
