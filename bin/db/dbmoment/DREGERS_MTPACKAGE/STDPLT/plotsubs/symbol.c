#include	<string.h>
#include	<stdio.h>
#include 	"../h/igl.h"
#include	"stdplt.h"
#include 	"symbolname.h"
#define	SYM_DRAW	128
#define SYM_FILL	256
#define SYM_MASK	0x3f
#define SYM_BAD		0xffffff00

extern float PIXINCH,INCHPIX;
extern struct frameinfo *frames, *cfr;



getsymbol(s)
register char *s;
   {
	register struct symbolname *p;
	for(p= symnames; p->symname[0] != '\0'; p++)
		if(strcmp(s,p->symname) == 0) return(p->symnum);
	return(-1);
   }

symbol(xinch,yinch,isym,size,angle)
float xinch,yinch,size,angle;
int isym;
  {
	int ix,iy,isize,iang;

	/* check for valid symbol number */
	if( (isym & SYM_BAD) != 0 )
	   {
		err(WARN,"bad symbol number %d",isym);
		return(-1);
	   }
	if ((isym & SYM_MASK) >= NSYMBOL)
	   {
		err(WARN,"no such symbol %d",isym);
		return(-1);
	   }
	ix = (int) (xinch*PIXINCH);
	iy = (int) (yinch*PIXINCH);

	isize= (int)(FLOATNORM * size);

	while(angle > 360.0) angle -= 360.0;
	while(angle <   0.0) angle += 360.0;
	iang= (int)(ANGLENORM * angle/360.0);

	output12(IGL_SYMBOL,isym,ix,iy,isize,iang);
	XPOS = xinch;
	YPOS = yinch;
	return(0);
   }


usymbol(xuser,yuser,isym,size,angle)
float xuser,yuser,size,angle;
int isym;
   {
	int ix,iy,isize,iang;
	float xinch,yinch;


	/* check for valid symbol number */
	if( (isym & SYM_BAD) != 0 )
	   {
		err(WARN,"bad symbol number %d",isym);
		return(-1);
	   }
	if ((isym & SYM_MASK) >= NSYMBOL)
	   {
		err(WARN,"no such symbol %d",isym);
		return(-1);
	   }

	xinch = XSCL(xuser);
	yinch = YSCL(yuser);
	ix = (int) (xinch*PIXINCH);
	iy = (int) (yinch*PIXINCH);

	isize= (int)(FLOATNORM * size);

	while(angle > 360.0) angle -= 360.0;
	while(angle <   0.0) angle += 360.0;
	iang= (int)(ANGLENORM * angle/360.0);

	output12(IGL_SYMBOL,isym,ix,iy,isize,iang);

	XPOS = xinch;
	YPOS = yinch;
   }

/* This routine is included to allow documentation programs to make
   complete tables of the symbols
 */
struct symbolname *getsymbolnames(nsymbol)
int *nsymbol;
   {
	struct symbolname *p;
	int max;

	max= 0;
	for(p=symnames; p->symname[0] != '\0'; p++)
		if(p->symnum > max) max= p->symnum;
	*nsymbol= max;
	return(symnames);
   }
