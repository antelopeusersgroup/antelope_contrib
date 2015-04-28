#include	<stdio.h>
#include	"../com/global.h"
#include	"devpar.h"
#include	<sys/vcmd.h>

int vplotmode[]	= { VPLOT,  0, 0 }; /* for Versatec plot mode */
int vprntmode[]	= { VPRINT, 0, 0 }; /* for Versatec print mode */

extern char labelbuf[];
extern int do_label;

float	squirt	=5.0; 

sendplot(plotnum,more)
int more;
   {
	register int i, *p, *blk;
	int ix, iy, ixmax, limit, vp;
	int *sb, *sb_base;
	int nattempt, leftover;
	extern int *patchmap[], grid;
	char line[NYB+4];
	extern int domap;

	/* find size of plot */
	for(i= NXB*NYB-1; i >= 0; i--)
		if(patchmap[i] != NULL) break;
	if(i<0)
	   {
		err(WARN,"plot is empty");
		return(1);
	   }
	ixmax= (i + NYB-1) / NYB;

	if(domap)
	   {
		/* fprintf(stdout,"patch map ixmax= %d\n",ixmax);*/
		for(ix=0; ix<ixmax; ix++)
		   {
			for(i=0; i<NYB; i++) line[i]= '.';
			for(i=0; i<NYB; i++)
				if(patchmap[ix*NYB+i] != NULL) line[i]= 'O';
			line[NYB]= '\0';
			/* fprintf(stdout,"%3d %s\n",ix,line);*/
		   }
		return(1);
	   }


	/* open plotter */
	nattempt= NOPENATTEMPT;
	while( (vp= open(PLOTTER,1)) < 0 && nattempt--)
		sleep(SLEEPTIME);
	if(vp < 0) err(FATAL,"cannot open plotter");

	/* set the plotter in print mode for label */
	/*
	if (do_label) {
		ioctl(vp,VSETSTATE,vprntmode);
		write(vp,labelbuf,strlen(labelbuf)); 
	}
	*/

	/* set the plotter in plot mode */
	ioctl(vp,VSETSTATE,vplotmode);

	if( (sb_base= (int *)malloc(4*32*NSCANBLK*NYB)) == NULL)
		err(FATAL,"cannot alloc scan memory");
	zap4(sb_base,32*NSCANBLK*NYB);
	
	sb= sb_base;
	for(ix=0; ix <= ixmax; ix++)
	   {
		/*fprintf(stderr,"doing %d\n",ix);*/
		for(iy= 0; iy<NYB; iy++)
		   {
			if( (blk=patchmap[ix*NYB+iy]) == NULL) continue;
			p= sb + iy;
			for(i=0; i<32; i++)
			   {
				*p = *blk++;
				p += NYB;
			   }
		   }
		sb += 32*NYB;
		if(((ix+1) % NSCANBLK) == 0)
		   {
			/* fprintf(stderr,"\ttriggered\n");*/
			if(grid) addgrid(sb_base,ix*32,NSCANBLK*32);
			write(vp,sb_base,4*32*NSCANBLK*NYB);
			sb= sb_base;
			zap4(sb_base,32*NYB*NSCANBLK);
		   }
	   }
	limit= ixmax + ((int)(squirt*PIXINCH) +32-1)/32;
	for(ix=ixmax+1; ix <= limit; ix++)
	   {
		if(((ix+1) % NSCANBLK) == 0)
		   {
			if(grid) addgrid(sb_base,ix*32,NSCANBLK*32);
			write(vp,sb_base,4*32*NSCANBLK*NYB);
			sb= sb_base;
			zap4(sb_base,32*NYB*NSCANBLK);
		   }
	   }
	/* dump left over part */
	if( (leftover= (limit % NSCANBLK)) )
	   {
		if(grid) addgrid(sb_base,32*(limit-leftover),32*leftover);
		write(vp,sb_base,4*32*NYB*leftover);
	   }
	/* fprintf(stderr,"leftover= %d\n",leftover);*/
	free(sb_base);
	close(vp);
	return(1);
   }

addgrid(scan,ix,nline)
int *scan, ix, nline;
   {
	int iy, pat, *pscan, k, last, ixst;
	extern int bit[];
	extern int grid;

	for(iy=0; iy< 32*NYB; iy += grid)
	   {
		pat= bit[iy%32];
		pscan= scan + (int) iy/32; 
		for(k=0; k<nline; k++)
		   {
			*pscan |= pat;
			pscan += NYB;
		   }
	   }
	last= ix + nline;
	ixst= ix + (ix%grid);
	for(ix=ixst; ix<last; ix += grid)
	   {
		pscan= scan + (ix-ixst)*NYB;
		for(k=0; k<NYB; k++) *pscan++ = 0xffffffff;
	   }
   }
