#include	<stdlib.h>
#include	<stdio.h>
#include	"../h/igl.h"
#include	"stdplt.h"

struct frameinfo frames[NFRAME]	=
   {
	ACTIVE,			/* flags */
	0.0, 0.0,		/* origin */
	0.0, 0.0,		/* min */
	1.0, 1.0,		/* scale */
	0,0,GEN_XMAX,GEN_YMAX,	/* window */
	0.0,0.0,		/* current position */
	NULL			/* last frame */
   };

struct frameinfo *cfr	= &frames[0];	/* current frame */

struct globalinfo glbase	= {
	NULL,		/* struct	globalinfo *gl_pop	*/
	0,		/* int		gl_flags		*/

	0,		/* int		gl_pen			*/
	1,		/* int		gl_pencolor		*/
	FILL_OR,	/* int		gl_penmode		*/
	0,		/* int		gl_fat			*/
	0,		/* int		gl_idash		*/

	0,		/* int		gl_brush		*/
	2,		/* int		gl_brushcolor		*/
	FILL_OR,	/* int		gl_brushmode		*/
	1,		/* int		gl_brushpat		*/

	FILL_OR,	/* int		gl_rastermode		*/

	0,		/* int		gl_textfont		*/
	0,		/* int		gl_textcenter		*/
	0.1,		/* float	gl_textsize		*/
	0.0,		/* float	gl_textangle		*/
	DEFAULT_INTBYTES,/* int		gl_intbytes		*/
	DEFAULT_INTMIN,	/* int		gl_intmin		*/
	DEFAULT_INTMAX,	/* int		gl_intmax		*/
   };
struct globalinfo *glcur	= &glbase;

extern float PIXINCH, INCHPIX;

/*
 * setframe(ifr) causes frames[ifr] to be the current frame.  If the new 
 *   frame isn't already active, it is made a copy of the old cfr.
 */
setframe(ifr)
register int ifr;
   {
	register struct frameinfo *newfr;

	if(ifr < 0 || ifr >= NFRAME)
	   {
		err(WARN,"attempt to change to invalid frame=%d\n",ifr);
		return(-1);
	   }
	newfr= &frames[ifr];
	fprintf(stderr,"new-old = %d\n",newfr - frames);
	if( !(newfr->fr_flags & ACTIVE) )  
		 framecopy(cfr,newfr,sizeof(struct frameinfo)); 
	
	newfr->fr_lastfr= cfr;
	newfr->fr_flags |= ACTIVE;
	/* should issue a raw move here!!!!! */
	/* try this */
	output2(IGL_MOVE,IXPOS,IYPOS);
	output10(IGL_WINDOW,IXWMIN,IYWMIN,IXWMAX,IYWMAX);
	cfr= newfr;
	return(ifr);
   }


/*
 * freeframe(ifr) makes frames[ifr] an inactive frame.  If ifr is the current
 *   frame, then the old current frame is made cfr.  frames[0] can't be freed.
 */
freeframe(ifr)
register int ifr;
   {
	register struct frameinfo *fr;

	if(ifr < 0 || ifr >= NFRAME)
	   {
		err(WARN,"attempt to free invalid frame=%d\n",ifr);
		return(-1);
	   }
	if(ifr == 0) return(0);
	fr= &frames[ifr];
	if(fr == cfr)  {
		cfr= cfr->fr_lastfr;
		setframe(frADDR(cfr));
	}
	fr->fr_flags = 0;
	return(frADDR(cfr));
   }


/*
 * pushframe() saves the current frame, allowing temporary modifications.
 */
pushframe()
   {
	register int ifr;
	register struct frameinfo *fr;
	for(ifr=1; ifr<NFRAME; ifr++)
	   {
		fr= &frames[ifr];
		if( !(fr->fr_flags & ACTIVE) )
		   {
			fprintf(stderr,"changing to ifr=%d\n",ifr);
			setframe(ifr);		/* use first inactive frame */
			return(ifr);
		   }
	   }
	fprintf(stderr,"stdplt: out of frames for push\n");
	return(-1);
   }


/*
 * popframe() ends use of a temporary frame
 */
popframe()
   {
	return( freeframe(frADDR(cfr)) );
   }


framecopy(in,out,n)
register char *in, *out;
register int n;
   {
	while(n--) *out++ = *in++;
   }

dumpframe(ifr)
int ifr;
   {
	struct frameinfo *f;
	if (ifr<0 || ifr>=NFRAME) {
		fprintf(stderr,"frame %d: invalid frame number\n",ifr);
		return(-1);
	}
	f= &frames[ifr];
	fprintf(stderr,"frame %2d is %s\n",ifr,
		(f->fr_flags & ACTIVE ? "active" : "undefined"));
	if((f->fr_flags & ACTIVE) == 0) return;
	fprintf(stderr,"\tflags = %x(hex)\n",f->fr_flags);
	fprintf(stderr,"\torigin= %7.3f %7.3f\n",f->fr_xorig,f->fr_yorig);
	fprintf(stderr,"\tscale = %7.3f %7.3f\n",f->fr_xscl ,f->fr_yscl );
	fprintf(stderr,"\tmin   = %7.3f %7.3f\n",f->fr_xmin ,f->fr_ymin );
	fprintf(stderr,"\tpos   = %7.3f %7.3f\n",f->fr_xpos ,f->fr_ypos );
	return (0);
   }




pushglobal()
   {
	/* need to fix this up */
	struct globalinfo *glbuf;
	glbuf= (struct globalinfo *)malloc(sizeof(struct globalinfo));
	if(glbuf == NULL)
	   {
		err(WARN,"out of memory for global push");
		return(-1);
	   }
	framecopy(glcur,glbuf,sizeof(struct globalinfo));
	glcur->gl_pop= glbuf;
	return(0);
   }


popglobal()
   {
	register struct globalinfo *glnew, *glold;

	if( (glnew= glcur->gl_pop) == NULL )
	   {
		err(WARN,"illegal pop of global");
		return(-1);
	   }
	glold= glcur;
	glcur= glnew;
	/* store the global state */
	if( PEN       	!= glold->gl_pen)	setpen(PEN);
	if( FAT       	!= glold->gl_fat)	setfat(FAT);
	if( PENCOLOR  	!= glold->gl_pencolor)	setpencolor(PENCOLOR);
	if( PENMODE   	!= glold->gl_penmode)	setpenmode(PENMODE);
	if( IDASH     	!= glold->gl_idash)	setdash(IDASH);

	if( BRUSH     	!= glold->gl_brush)	setbrush(BRUSH);
	if( BRUSHCOLOR 	!= glold->gl_brushcolor)setbrushcolor(BRUSHCOLOR);
	if( BRUSHPAT 	!= glold->gl_brushpat)	setbrushpat(BRUSHPAT);
	if( BRUSHMODE 	!= glold->gl_brushmode)	setbrushmode(BRUSHMODE);

	if( RASTERMODE 	!= glold->gl_rastermode)setrastmode(RASTERMODE);

	if( TEXTFONT 	!= glold->gl_textfont)	settextfont(TEXTFONT);
	if( TEXTSIZE 	!= glold->gl_textsize)	settextsize(TEXTSIZE);
	if( TEXTANGLE 	!= glold->gl_textangle)	settextangle(TEXTANGLE);
	if( TEXTCENTER 	!= glold->gl_textcenter)
			settextcenter(TEXTCENTER&07,(TEXTCENTER>>3)&07);
	if ( INTBYTES	!= glold->gl_intbytes)	setintbytes (INTBYTES);
	free(glold);
	return(0);
   }


   /*  This routine searches the stack of global frames to determine the 
       depth, and lists the state of global variables  */
dumpglobal()
{   struct globalinfo *tempgl;
    int i=1;

    tempgl = glcur;
    while ((*tempgl).gl_pop != NULL)
	 {
	   i++;
	   tempgl =(*tempgl).gl_pop;
         }
    fprintf(stderr,"global frames in stack =%d\n",i);
    fprintf(stderr,"global variables:\n");
    fprintf(stderr,"\tpen=%d\n",PEN);
    fprintf(stderr,"\tfat=%d\n",FAT);
    fprintf(stderr,"\tpencolor=%d\n",PENCOLOR);
    fprintf(stderr,"\tpenmode=%d\n",PENMODE);
    fprintf(stderr,"\tidash=%d\n",IDASH);
    fprintf(stderr,"\tbrush=%d\n",BRUSH);
    fprintf(stderr,"\tbrushcolor=%d\n",BRUSHCOLOR);
    fprintf(stderr,"\tbrushpat=%d\n",BRUSHPAT);
    fprintf(stderr,"\tbrushmode=%d\n",BRUSHMODE);
    fprintf(stderr,"\trastermode=-%d\n",RASTERMODE);
    fprintf(stderr,"\ttextfont=%d\n",TEXTFONT);
    fprintf(stderr,"\ttextcenter=%d\n",TEXTCENTER);
    fprintf(stderr,"\ttextsize=%7.3f\n",TEXTSIZE);
    fprintf(stderr,"\ttextangle=%7.3f\n",TEXTANGLE);
    return(0);

} /*dumpglobal*/    

