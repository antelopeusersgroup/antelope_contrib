#include	<stdio.h>
#include	"../../h/igl.h"
#include	"../com/global.h"
#include	"devpar.h"
#define SWAB	1 
#define TMPDIR	"/tmp"

sendplot(plotnum,more)
int plotnum,more;
   {
	register int i, ix, iy;
	int *ptr, fd, ixmax, ix32, iy32 ;
	char head[10], fname[32], line[128], dochead[128];
	extern int grid, penmode, xmax, ymax, ncopies, headerpage;
	extern int *patchmap[];
	extern int bit[];
	int l;

	/* do grid if required */
	if(grid)
	   {
		int oldpenmode;
		oldpenmode= penmode;
		penmode= FILL_OR;
		for(i= grid; i < xmax; i += grid)
			vector(i,0,i,ymax);
		for(i= grid; i < ymax; i += grid)
			vector(0,i,xmax,i);
		penmode= oldpenmode;
	   }
	/* find maximum size of plot */
	for(i= NXB*NYB-1; i >= 0; i--)
		if(patchmap[i] != NULL) break;
	if(i<0)
	   {
		err(WARN,"plot is empty\n");
		return(1);
	   }
	ixmax= (i +NYB-1)/NYB;

	/* create temporary file */
	sprintf(fname,"%s/ima%d.%d",TMPDIR,getpid(),plotnum);
	if( (fd= creat(fname,0666)) < 0)
		err(WARN,"cannot open tmp file %s\n",fname);

	/* send document header */
	sprintf(dochead, "@Document(language impress, jobheader %s)\0",
		(headerpage ? "on" : "off") );
	write(fd,dochead,strlen(dochead));
	/* set up constant part of header */
	head[0]= 135;	/* hmoveop */
	head[3]= 137;	/* vmoveop */
	head[6]= 235;	/* bitmapop */
	head[7]=   7;	/* mode */
	head[8]=   1;	/* hsize */
	head[9]=   1;	/* vsize */

	/* now send the active patches */
	for(ix=0; ix<ixmax; ix++)
	for(iy=0; iy<NYB; iy++)
	   {
		if( (ptr= patchmap[ix*NYB+iy]) == NULL) continue;
				
		ix32= ix*32;
		iy32= iy*32;
#ifdef SWAB
		head[1]= (iy32 >> 8) &0xff;
		head[2]= (iy32     ) &0xff;
		head[4]= (ix32 >> 8) &0xff;
		head[5]= (ix32     ) &0xff;
#else
		head[1]= (iy32     ) &0xff;
		head[2]= (iy32 >> 8) &0xff;
		head[4]= (ix32     ) &0xff;
		head[5]= (ix32 >> 8) &0xff;
#endif
		write(fd,head,10);
		write(fd,ptr,4*32);
	   }
	head[0]= 219;	/* endpage */
	write(fd,head,1);
	close(fd);
	/* issue system command to spool plot */
	/*
	fprintf(stdout,"fake exit from sendplot");
	if (1==1) return(1);
	*/
	sprintf(line,"/usr/local/ipr -c%d -r %s",ncopies,fname);
	system(line);
	return(1);
   }
