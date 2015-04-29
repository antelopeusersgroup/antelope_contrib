#include	<stdio.h>
#include	"igl.h"
#include	"stdplt.h"

char fr_line[1024];	/* buffer for text */

float	PIXINCH		=GEN_PIXINCH;
float	INCHPIX		=GEN_INCHPIX;
int	XMAX		=GEN_XMAX;
int	YMAX		=GEN_YMAX;
/*
FILE   *PLOTOUT		=stdout;
*/


char bwpens[MAXPENS];
char cpens[MAXPENS];
char bwbrushs[MAXBRUSH];
char cbrushs[MAXBRUSH];
char dashs[NDASH];
char colors[NCOLOR];
char patterns[NPATTERNS];

extern struct frameinfo *frames, *cfr;
extern struct globalinfo *glcur;

setfat(ifat)
int ifat;
   {
	if(ifat < 0) ifat= 0;
	if(ifat > MAXFAT)
	   {
		err(WARN,"fat > %d not allowed, using fat=%d\n",MAXFAT,MAXFAT);
		ifat= MAXFAT;
	   }
	output4(IGL_SETFAT,ifat);
	FAT= ifat;
   }

getfat()
   {
	return(FAT);
   }

/*
 * Setbrushpat sets the current fill pattern index
 */
setbrushpat(ipat)
int ipat;
   {
	ipat &= 0xff;
	BRUSHPAT= ipat;
	output4(IGL_SETPATTERN,ipat);
   }

getbrushpat()
   {
	return(BRUSHPAT);
   }

 /* fillmode is the mode used to fill polygons */
getfillmode()
   {
	return(BRUSHMODE);
   }

getpenmode()
   {
	return(PENMODE);
   }


defpen(ipen, ifat, idash, imode, icfat, icdash, icolor)
int ipen, idash, icdash, icolor, ifat, imode,icfat;
   {
	int i;

	if(ipen < 0)	/* find free pen */
	   {
		/* find first available, starting with last one */
		for(i= MAXPENS-1; i>0; i--)
			if(bwpens[i] == 0 && cpens[i] == 0) break;
		if(i == 0) /* cannot have this one */
		   {
			err(WARN,"no more pens available for defpen");
			return(0);
		   }
		ipen= i;
	   }
	if(ipen >= MAXPENS)
	   {
		err(WARN,"invalid pen requested in defpen");
		return(0);
	   }
	bwpens[ipen]= 1;
	cpens[ipen]= 1;
	idash %= NDASH;
	icdash %= NDASH;
	ifat %= MAXFAT;
	icfat %= MAXFAT;
	icolor %= NCOLOR;
	imode %= 0xff;
	output7(IGL_DEFBWPEN,ipen,ifat,idash,imode);
	output7(IGL_DEFCPEN,ipen,icfat,icdash, icolor);
	return(ipen);
   }

defbwpen(ipen, ifat, idash, imode)
int ipen, idash, ifat, imode;
   {
	int i;

	if(ipen < 0)	/* find free pen */
	   {
		/* find first available, starting with last one */
		for(i= MAXPENS-1; i>0; i--)
			if(bwpens[i] == 0) break;
		if(i == 0) /* cannot have this one */
		   {
			err(WARN,"no more pens available for defpen");
			return(0);
		   }
		ipen= i;
	   }
	if(ipen >= MAXPENS)
	   {
		err(WARN,"invalid pen requested in defpen");
		return(0);
	   }
	bwpens[ipen]= 1;
	idash %= NDASH;
	ifat %= MAXFAT;
	imode %= 0xff;
	output7(IGL_DEFBWPEN,ipen,ifat,idash,imode);
	return(ipen);
   }
defcpen(ipen, icfat, icdash, icolor)
int ipen,icfat,icdash,icolor; 
   {
	int i;

	if(ipen < 0)	/* find free pen */
	   {
		/* find first available, starting with last one */
		for(i= MAXPENS-1; i>0; i--)
			if(cpens[i] == 0) break;
		if(i == 0) /* cannot have this one */
		   {
			err(WARN,"no more pens available for defpen");
			return(0);
		   }
		ipen= i;
	   }
	if(ipen >= MAXPENS)
	   {
		err(WARN,"invalid pen requested in defpen");
		return(0);
	   }
	cpens[ipen]= 1;
	icdash %= NDASH;
	icfat %= MAXFAT;
	icolor %= NCOLOR;
	output7(IGL_DEFCPEN,ipen,icfat,icdash, icolor);
	return(ipen);
}

defbrush(ibrush, ipat, imode, icolor)
int ibrush, ipat, icolor, imode;
   {
	int i;

	if(ibrush < 0)	/* find free brush */
	   {
		/* find first available, starting with last one */
		for(i= MAXBRUSH-1; i>0; i--)
			if(bwbrushs[i] == 0 && cbrushs[i] == 0) break;
		if(i == 0) /* cannot have this one */
		   {
			err(WARN,"no more brushs available for defbrush");
			return(0);
		   }
		ibrush= i;
	   }
	if(ibrush >= MAXBRUSH)
	   {
		err(WARN,"invalid brush requested in defbrush");
		return(0);
	   }
	bwbrushs[ibrush]= 1;
	cbrushs[i] = 1;
	ipat %= NPATTERNS;
	icolor %= NCOLOR;
	imode %= 0xff;
	/*
	output7(IGL_DEFBRUSH,ibrush,icolor,ipat,imode);
	*/
	output11(IGL_DEFBWBRUSH,ibrush,ipat,imode);
	output15(IGL_DEFCBRUSH,ibrush,icolor);
	return(ibrush);
   }

defbwbrush(ibrush, ipat, imode)
int ibrush, ipat, imode;
   {
	int i;

	if(ibrush < 0)	/* find free brush */
	   {
		/* find first available, starting with last one */
		for(i= MAXBRUSH-1; i>0; i--)
			if(bwbrushs[i] == 0 ) break;
		if(i == 0) /* cannot have this one */
		   {
			err(WARN,"no more brushs available for defbrush");
			return(0);
		   }
		ibrush= i;
	   }
	if(ibrush >= MAXBRUSH)
	   {
		err(WARN,"invalid brush requested in defbrush");
		return(0);
	   }
	bwbrushs[ibrush]= 1;
	ipat %= NPATTERNS;
	imode %= 0xff;
	output11(IGL_DEFBWBRUSH,ibrush,ipat,imode);
	return(ibrush);
   }

defcbrush(ibrush, icolor)
int ibrush, icolor;
   {
	int i;

	if(ibrush < 0)	/* find free brush */
	   {
		/* find first available, starting with last one */
		for(i= MAXBRUSH-1; i>0; i--)
			if(cbrushs[i] == 0) break;
		if(i == 0) /* cannot have this one */
		   {
			err(WARN,"no more brushs available for defbrush");
			return(0);
		   }
		ibrush= i;
	   }
	if(ibrush >= MAXBRUSH)
	   {
		err(WARN,"invalid brush requested in defbrush");
		return(0);
	   }
	cbrushs[i] = 1;
	icolor %= NCOLOR;
	output15(IGL_DEFCBRUSH,ibrush,icolor);
	return(ibrush);
   }

defpattern(ipat, bits)
int ipat, bits[];
   {
	int i;

	if(ipat < 0)	/* find free pat */
	   {
		/* find first available, starting with last one */
		for(i= NPATTERNS-1; i>0; i--)
			if(patterns[i] == 0) break;
		if(i == 0) /* cannot have this one */
		   {
			err(WARN,"no more patterns available for defpattern");
			return(0);
		   }
		ipat= i;
	   }
	if(ipat >= NPATTERNS)
	   {
		err(WARN,"invalid pattern requested in defpattern");
		return(0);
	   }
	patterns[ipat]= 1;  
	ipat %= NPATTERNS;
	output5(IGL_DEFPATTERN,ipat,bits);
	return(ipat);
   }


defdash(idash,on1,off1,on2,off2)
int idash;
float on1, off1, on2, off2;
   {
	int i, ion1, ioff1, ion2, ioff2;

	if(idash < 0)	/* find free dash */
	   {
		/* find first available, starting with last one */
		for(i= NDASH-1; i>0; i--)
			if(dashs[i] == 0) break;
		if(i == 0) /* cannot have this one */
		   {
			err(WARN,"no more patterns available for defdash");
			return(0);
		   }
		idash= i;
	   }
	if(idash >= NDASH)
	   {
		err(WARN,"invalid dash requested in defdash");
		return(0);
	   }
	dashs[idash]= 1;
	idash %= NDASH;
	ion1 = (int)(on1*FLOATNORM);
	ioff1= (int)(off1*FLOATNORM);
	ion2 = (int)(on2*FLOATNORM);
	ioff2= (int)(off2*FLOATNORM);
	output12(IGL_DEFDASH,idash,ion1,ioff1,ion2,ioff2);
	return(idash);
   }

defcolor(icolor, r, g, b)
int icolor;
float  r, g, b;
   {
	int i, ir, ig, ib;

	if(icolor < 0)	/* find free color */
	   {
		/* find first available, starting with last one */
		for(i= NCOLOR-1; i>0; i--)
			if(colors[i] == 0) break;
		if(i == 0) /* cannot have this one */
		   {
			err(WARN,"no more patterns available for defcolor");
			return(0);
		   }
		icolor= i;
	   }
	if(icolor >= NCOLOR)
	   {
		err(WARN,"invalid color requested in defcolor");
		return(0);
	   }
	icolor %= NCOLOR;
	colors[icolor]= 1;
	ir= (int)(255.0 * r) & 0xff;
	ig= (int)(255.0 * g) & 0xff;
	ib= (int)(255.0 * b) & 0xff;
	output7(IGL_DEFCOLOR,icolor,ir,ig,ib);
	return(icolor);
   }

setpen(ipen)
int ipen;
   {
	int oldipen;
	oldipen= PEN;
	if(ipen < 0 || ipen >= MAXPENS)
	   {
		err(WARN,"invalid pen set");
		return(oldipen);
	   }
	   /*
	if(bwpens[ipen] == 0) err(WARN,"info: bwpen is not yet user-defined");
	if(cpens[ipen] == 0) err(WARN,"info: color pen is not yet user-defined");
	   */
	PEN= ipen;
	output4(IGL_SETPEN,ipen);
	return(oldipen);
   }

getpen()
   {
	return(PEN);
   }

setbrush(ibrush)
int ibrush;
   {
	int oldibrush;
	oldibrush= BRUSH;
	if(ibrush < 0 || ibrush >= MAXBRUSH)
	   {
		err(WARN,"invalid brush set");
		return(oldibrush);
	   }
	   /*
	if(bwpens[ibrush] == 0) err(WARN,"info: bw brush is not yet user-defined");
	if(cpens[ibrush] == 0) err(WARN,"info: color brush is not yet user-defined");
	   */
	BRUSH= ibrush;
	output4(IGL_SETBRUSH,ibrush);
	return(oldibrush);
   }

getbrush()
   {
	return(BRUSH);
   }

setdash(idash)
int idash;
   {
	int oldidash;
	oldidash= IDASH;
	if(idash < 0 || idash >= NDASH)
	   {
		err(WARN,"invalid dash set");
		return(oldidash);
	   }
	   /*
	if(dashs[idash] == 0) err(WARN,"info: dash is not yet user-defined");
	    */
	IDASH= idash;
	output4(IGL_SETDASH,idash);
	return(oldidash);
   }

getdash()
   {
	return(IDASH);
   }

setcolor(icolor)
int icolor;
   {
	int oldicolor;
	oldicolor= PENCOLOR;
	if(icolor < 0 || icolor >= NCOLOR)
	   {
		err(WARN,"invalid color set");
		return(oldicolor);
	   }
	   /*
	if(colors[icolor] == 0) err(WARN,"info: color is not yet user-defined");
	    */
	PENCOLOR= icolor;
	BRUSHCOLOR= icolor;
	output4(IGL_SETPENCOLOR,icolor);
	output4(IGL_SETBRUSHCOLOR,icolor);
	return(oldicolor);
   }

setpencolor(icolor)
int icolor;
   {
	int oldicolor;
	oldicolor= PENCOLOR;
	if(icolor < 0 || icolor >= NCOLOR)
	   {
		err(WARN,"invalid color set");
		return(oldicolor);
	   }
	   /*
	if(colors[icolor] == 0) err(WARN,"info: color is not yet user-defined");
	    */
	PENCOLOR= icolor;
	output4(IGL_SETPENCOLOR,icolor);
	return(oldicolor);
   }

setbrushcolor(icolor)
int icolor;
   {
	int oldicolor;
	oldicolor= BRUSHCOLOR;
	oldicolor= PENCOLOR;
	if(icolor < 0 || icolor >= NCOLOR)
	   {
		err(WARN,"invalid color set");
		return(oldicolor);
	   }
	BRUSHCOLOR= icolor;
	output4(IGL_SETBRUSHCOLOR,icolor);
	return(oldicolor);
   }

getpencolor()
   {
	return(PENCOLOR);
   }

getcolor()
   {
	return(PENCOLOR);
   }

getbrushcolor()
   {
	return(BRUSHCOLOR);
   }


/* * Set fill mode changes the fill action to OR, XOR, EQU, AND * must use
 setbrushmode or setpenmode to set the mode * */
setpenmode(imode)
int imode;
   {
	int oldimode;
	oldimode= PENMODE;
	PENMODE= imode;
	output4(IGL_SETPENMODE,imode);
	return(oldimode);
   }

setbrushmode(imode)
int imode;
   {
	int oldimode;
	oldimode= BRUSHMODE;
	BRUSHMODE= imode;
	output4(IGL_SETBRUSHMODE,imode);
	return(oldimode);
   }

getbrushmode()
   {
	return(BRUSHMODE);
   }

setrastmode(imode)
int imode;
   {
	int oldimode;
	oldimode= RASTERMODE;
	RASTERMODE= imode;
	output4(IGL_SETRASTMODE,imode);
	return(oldimode);
   }

setintbytes(nbytes)
int nbytes;
   {
	int i;
	/* This ASSUMES 8 bit bytes and 2's compliment representation.	*/
	/* Compute limits for this number of bytes. */
	INTMAX = 127;
	INTMIN = -128;
	for (i = 1; i < nbytes; i++) {
		INTMAX = (INTMAX << 8) | 255;
		INTMIN = (INTMIN << 8);
	}
	INTBYTES = nbytes;
	output4(IGL_SETINTBYTES, INTBYTES);
   }

