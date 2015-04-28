#define NO_OP	-1

/* flags for pens and brushs */
#define	DEFINED	01

/* global plot variables */

struct globalinfo
   {
	struct	globalinfo *gl_pop;	/* forward link to stacked data */
	int	gl_flags;

	int	gl_pen;
	int	gl_pencolor;
	int	gl_penmode;
	int	gl_fat;
	int	gl_idash;

	int	gl_brush;
	int	gl_brushcolor;
	int	gl_brushmode;
	int	gl_brushpat;

	int	gl_rastermode;

	int	gl_textfont;
	int	gl_textcenter;
	float	gl_textsize;
	float	gl_textangle;
   };
/* abbreviations: */

#define PEN		glcur->gl_pen
#define FAT		glcur->gl_fat
#define IDASH		glcur->gl_idash
#define PENCOLOR	glcur->gl_pencolor
#define PENMODE		glcur->gl_penmode

#define BRUSH		glcur->gl_brush
#define BRUSHCOLOR	glcur->gl_brushcolor
#define BRUSHPAT	glcur->gl_brushpat
#define BRUSHMODE	glcur->gl_brushmode

#define TEXTFONT	glcur->gl_textfont
#define TEXTSIZE	glcur->gl_textsize
#define TEXTANGLE	glcur->gl_textangle
#define TEXTCENTER	glcur->gl_textcenter

#define RASTERMODE	glcur->gl_rastermode

/* frames variables */
/* bit masks for fr_flags */
#define ACTIVE	0x01	/* frame has been set up */
#define DASH	0x02	/* dash lines are in effect */
struct frameinfo
   {
	int 	fr_flags;
	float	fr_xorig,	fr_yorig;	
	float	fr_xmin,	fr_ymin;	
	float	fr_xscl,	fr_yscl;	
	int	fr_ixwmin,	fr_iywmin;
	int	fr_ixwmax,	fr_iywmax;
	float	fr_xpos,	fr_ypos;	
	struct frameinfo *fr_lastfr;
   }; 

/* abbreviations */
#define frADDR(i)	( ((int) i - (int) frames) / sizeof(struct frameinfo) )
#define XSCL(x)		(cfr->fr_xorig+cfr->fr_xscl*((x)-cfr->fr_xmin))
#define YSCL(y)		(cfr->fr_yorig+cfr->fr_yscl*((y)-cfr->fr_ymin))
#define XPOS		cfr->fr_xpos
#define YPOS		cfr->fr_ypos
#define IXPOS		((int)(cfr->fr_xpos*PIXINCH))
#define IYPOS		((int)(cfr->fr_ypos*PIXINCH))
#define IXWMIN		cfr->fr_ixwmin
#define IXWMAX		cfr->fr_ixwmax
#define IYWMIN		cfr->fr_iywmin
#define IYWMAX		cfr->fr_iywmax

/* error control definitions */
#define WARN	1
#define CONT	2
#define FATAL	3
