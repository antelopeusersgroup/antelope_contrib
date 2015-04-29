#include <stdlib.h>
#include <string.h>
#include <stdio.h>

extern lnblnk_();

/************************************************************************/
/*
	Routines that require a DIFFERENT calling sequence.
	Due to the differences in C and FORTRAN, the following
	routines have a DIFFERENT calling sequence in FORTRAN that
	they do in C.
*/

/*::	DIFFERENT CALLING CONVENTION.	::*/
/*::	polyfilln
	polyfilln (npoly, npts, ind, vertices)
		where:
			npoly:		number of polygons.
			npts(npoly):	array containing # points per polygon.
			ind(npoly):	array containing index in vertices
					of the start of the polygon.
			vertices(2,n)	array of (x,y) vertices.
::*/
polyfilln_ (npoly, npts, ind, vertices)
int *npoly, *npts, *ind;
float *vertices;
{
	float **pvert, **p;
	int i;
	if ( (pvert = (float**)malloc(*npoly*sizeof(float))) == NULL ) {
		fprintf (stderr, "unable to malloc in polyfilln_\n");
		exit(1);
	}
	p = pvert;
	for (i=0; i<*npoly; i++) {
		*p++ = vertices+(2*(ind[i]-1));
	}
	i = polyfilln (*npoly, npts, pvert);
	free(pvert);
	return( i );
}

/*::	DIFFERENT CALLING CONVENTION.	::*/
/*::	upolyfilln
	polyfilln (npoly, npts, ind, vertices)
		where:
			npoly:		number of polygons.
			npts(npoly):	array containing # points per polygon.
			ind(npoly):	array containing index in vertices
					of the start of the polygon.
			vertices(2,n)	array of (x,y) vertices.
::*/
upolyfilln_ (npoly, npts, ind, vertices)
int *npoly, *npts, *ind;
float *vertices;
{
	float **pvert, **p;
	int i;
	if ( (pvert = (float**)malloc(*npoly*sizeof(float))) == NULL ) {
		fprintf (stderr, "unable to malloc in polyfilln_\n");
		exit(1);
	}
	p = pvert;
	for (i=0; i<*npoly; i++) {
		*p++ = vertices+(2*(ind[i]-1));
	}
	i = upolyfilln (*npoly, npts, pvert);
	free(pvert);
	return( i );
}

/*::	RESTRICTED CALLING CONVENTION.		::*/
/*::	Print a simple text string ONLY.	::*/
/*::	NO formatting done.			::*/
text_ (px,py,str,islen)
float *px,*py;
char *str;
int *islen;
{
	int i;
	char *p;
	i = lnblnk_ (str,islen);
	if ((p = (char *) malloc (i+1)) == NULL) {
		fprintf (stderr, "unable to malloc in text_\n");
		exit(1);
	}
	strncpy (p,str,i);
	*(p+i) = '\0';
	text(*px,*py,p);
	free(p);
}

/*::	RESTRICTED CALLING CONVENTION.		::*/
/*::	Print a simple text string ONLY.	::*/
/*::	NO formatting done.			::*/
utext_ (px,py,str,islen)
float *px,*py;
char *str;
int *islen;
{
	int i;
	char *p;
	i = lnblnk_ (str,islen);
	if ((p = (char *) malloc (i+1)) == NULL) {
		fprintf (stderr, "unable to malloc in utext_\n");
		exit(1);
	}
	strncpy (p,str,i);
	*(p+i) = '\0';
	utext(*px,*py,p);
	free(p);
}

/*::	RESTRICTED CALLING CONVENTION.		::*/
/*::	Print a simple text string ONLY.	::*/
/*::	NO formatting done.			::*/
plotlabel_ (str,islen)
char *str;
int *islen;
{
	int i;
	char *p;
	i = lnblnk_ (str,islen);
	if ((p = (char *) malloc (i+1)) == NULL) {
		fprintf (stderr, "unable to malloc in plotlabel_\n");
		exit(1);
	}
	strncpy (p,str,i);
	*(p+i) = '\0';
	plotlabel(p);
	free(p);
}

/************************************************************************/
/*
	Identical FORTRAN interludes to C routines.	
	FORTRAN interludes use standard FORTRAN conventions.
	Arguments should be as follows:
	C expects:	FORTRAN uses: 	Conversion in interulude:
	------------------------------------------------------------
	int		INTEGER		int *	->	int
	float		REAL		float *	->	float
	float *		REAL		no conversion required
	char *		CHARACTER	copy and null terminate
	int[]		INTEGER array	no conversion required
	float[]		REAL array	no conversion required
	return( int )	INTEGER func	no conversion required
*/	

defbwbrush_ (pibrush,pipat,pimode)
float *pibrush,*pipat,*pimode;
{	return( defbwbrush(*pibrush,*pipat,*pimode) );	}

defcbrush_ (pibrush,picolor)
float *pibrush,*picolor;
{	return( defcbrush(*pibrush,*picolor) );	}

defbrush_ (pibrush,pipat,pimode,picolor)
float *pibrush,*pipat,*pimode,*picolor;
{	return( defbrush(*pibrush,*pipat,*pimode,*picolor) );	}

setbrush_ (pibrush)
int *pibrush;
{	return( setbrush(*pibrush) );	}

setbrushpat_ (pipat)
int *pipat;
{	return( setbrushpat(*pipat) );	}

setbrushmode_ (pimode)
int *pimode;
{	return( setbrushmode(*pimode) );}

setbrushcolor_ (picolor)
int *picolor;
{	return( setbrushcolor(*picolor) );}

getbrush_ ()
{	return( getbrush() );		}

getbrushpat_ ()
{	return( getbrushpat() );	}

getfillmode_ ()
{	return( getfillmode() );	}

getbrushmode_ ()
{	return( getbrushmode() );	}

getbrushcolor_ ()
{	return( getbrushcolor() );	}

defcolor_ (picolor,pr,pg,pb)
int *picolor,*pr,*pg,*pb;
{	return( defcolor(*picolor,*pr,*pg,*pb) );	}

getpencolor_ ()
{	return( getpencolor() );	}

getcolor_ ()
{	return( getcolor() );		}

defdash_ (pidash,pon1,poff1,pon2,poff2)
int *pidash;
float *pon1,*poff1,*pon2,*poff2;
{	return( defdash(*pidash,*pon1,*poff1,*pon2,*poff2) );	}

setdash_ (pidash)
int *pidash;
{	return( setdash(*pidash) );	}

getdash_ ()
{	return( getdash() );		}

defpattern_ (ipat,bits)
int *ipat,*bits;
{	return( defpattern(*ipat,bits) );		}

boxfill_ (px1, py1, px2, py2)
float *px1,*py1,*px2,*py2;
{	boxfill (*px1,*py1,*px2,*py2);	}

uboxfill_ (px1, py1, px2, py2)
float *px1,*py1,*px2,*py2;
{	boxfill (*px1,*py1,*px2,*py2);	}

polyfill_ (vl,np)
float *vl;
int *np;
{	return( polyfill(vl,*np) );	}

upolyfill_ (vl,np)
float *vl;
int *np;
{	return( upolyfill(vl,*np) );	}

setframe_(ifr)
int *ifr;
{	return( setframe(*ifr) );	}

freeframe_(ifr)
int *ifr;
{	return( freeframe(*ifr) );	}

pushframe_()
{	return( pushframe() );		}

popframe_()
{	return( popframe() );		}

dumpframe_(ifr)
int *ifr;
{	return( dumpframe(*ifr) );	}

pushglobal_()
{	return( pushglobal() );		}

popglobal_()
{	return( popglobal() );		}

dumpglobal_()
{	return( dumpglobal() );		}

plot_ (px,py,pipen)
float *px,*py;
int *pipen;
{	plot(*px,*py,*pipen);		}

uplot_ (px,py,pipen)
float *px,*py;
int *pipen;
{	uplot(*px,*py,*pipen);		}

line_ (px1,py1,px2,py2)
float *px1,*py1,*px2,*py2;
{	line(*px1,*py1,*px2,*py2);	}

uline_ (px1,py1,px2,py2)
float *px1,*py1,*px2,*py2;
{	uline (*px1,*py1,*px2,*py2);	}

point_ (px,py)
float *px,*py;
{	point (*px,*py);		}

upoint_ (px,py)
float *px,*py;
{	upoint (*px,*py);		}

box_ (px1,py1,px2,py2)
float *px1,*py1,*px2,*py2;
{	box (*px1,*py1,*px2,*py2);	}

ubox_ (px1,py1,px2,py2)
float *px1,*py1,*px2,*py2;
{	ubox (*px1,*py1,*px2,*py2);	}

erase_()
{	erase();		}

endplot_()
{	endplot();		}

purge_()
{	purge();		}

pause_()
{	pause();		}

where_ (px,py)
float *px,*py;
{	where(px,py);			}

uwhere_ (px,py)
float *px,*py;
{	uwhere(px,py);			}

defbwpen_ (pipen,pifat,pidash,pimode)
float *pipen,*pifat,*pidash,*pimode;
{	return( defbwpen(*pipen,*pifat,*pidash,*pimode) );	}

defcpen_ (pipen,pifat,pidash,picolor)
float *pipen,*pifat,*pidash,*picolor;
{	return( defcpen(*pipen,*pifat,*pidash,*picolor) );	}

defpen_ (pipen,pifat,pidash,pimode,picfat,picdash,picolor)
float *pipen,*pifat,*pidash,*pimode,*picfat,*picdash,*picolor;
{	return( defpen(*pipen,*pifat,*pidash,*pimode,*picfat,*picdash,*picolor) ); }

setpen_ (pipen)
int *pipen;
{	return( setpen(*pipen) );	}

setpencolor_ (picolor)
int *picolor;
{	return( setpencolor(*picolor) );}

setcolor_ (picolor)
int *picolor;
{	return( setcolor(*picolor) );	}

setfat_ (pifat)
int *pifat;
{	return( setfat(*pifat) );	}

setpenmode_ (pimode)
int *pimode;
{	return( setpenmode(*pimode) );	}

getpen_ ()
{	return( getpen() );		}

getfat_ ()
{	return( getfat() );		}

getpenmode_ ()
{	return( getpenmode() );		}

setorig_ (px,py)
float *px,*py;
{	setorig(*px,*py);		}

setscl_ (px,py)
float *px,*py;
{	setscl(*px,*py);		}

setmin_ (px,py)
float *px,*py;
{	setmin(*px,*py);		}

getorig_ (px,py)
float *px,*py;
{	getorig(px,py);			}

getscl_ (px,py)
float *px,*py;
{	getscl(px,py);			}

getmin_ (px,py)
float *px,*py;
{	getmin(px,py);			}

symbol_ (px,py,pisym,psize,pangle)
int *pisym;
float *px,*py,*psize,*pangle;
{	return( symbol(*px,*py,*pisym,*psize,*pangle) );	}

usymbol_ (px,py,pisym,psize,pangle)
int *pisym;
float *px,*py,*psize,*pangle;
{	return( usymbol(*px,*py,*pisym,*psize,*pangle) );	}

getsymbol_ (symbolname,islen)
char *symbolname;
int *islen;
{
	int i;
	char *p;
	i = lnblnk_ (symbolname,islen);
	if ((p = malloc (i+1)) == NULL) {
		fprintf (stderr, "unable to malloc in getsymbol_\n");
		exit(1);
	}
	strncpy (p,symbolname,i);
	*(p+i) = '\0';
	i = getsymbol(p);
	free(p);
	return( i );
}

settextangle_ (pangle)
float *pangle;
{	settextangle(*pangle);		}

settextsize_ (psize)
float *psize;
{	settextsize(*psize);		}

settextfont_ (pfont)
int *pfont;
{	settextfont(*pfont);		}

settextcenter_ (ph,pv)
float *ph,*pv;
{	settextcenter(*ph,*pv);		}

window_ (px1, py1, px2, py2)
float *px1,*py1,*px2,*py2;
{	window (*px1,*py1,*px2,*py2);	}

uwindow_ (px1, py1, px2, py2)
float *px1,*py1,*px2,*py2;
{	uwindow (*px1,*py1,*px2,*py2);	}

unwindow_()
{	unwindow();			}


/*
setpattern_ (pipat)
int *pipat;
{	setpattern(*pipat);		}
*/

