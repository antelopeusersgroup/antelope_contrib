#include	<stdio.h>
#include	"../../h/igl.h"
#include	"../com/global.h"
#include	"devpar.h"

char *devname;			/* program name for error reporting */

/* device specifications */
float	pixinch		=PIXINCH;
float	inchpix		=INCHPIX;
int	xmax		=XMAX;
int	ymax		=YMAX;
int	xpagelen	=XPAGELEN;
int	ypagelen	=YPAGELEN;
int	colordev	=COLORDEV;	/* 0 ==> BW device, 1 ==> color */
int     labeldev        =LABELDEV; 
char    devid           =DEVID; 

/* window parameters */
int	ixwmin		=0;
int	iywmin		=0;
int	ixwmax		=XMAX;
int	iywmax		=YMAX;

/* scaling, translation, and rotation parameters */
int	page		=0;
int	xorigin		=0;
int	yorigin		=0;
float	xscale		=1.0;
float	yscale		=1.0;
float   textscale	=1.0;
float   symscale	=1.0;
float	pixpoint	=.48; 	/* empirically derived number of
				   points per pixel for raster text */
				

/* symbol fill mode -- hollow or filled */
int	symfill		=1;
float	symsinang	=0.0;
float	symcosang	=1.0;
/* windowing */
int do_window = 1;
/* label */
int do_label = 1;

/* pen state */
int	xcur		=0;
int	ycur		=0;
int	xlast		= -1;
int	ylast		= -1;
int	pencur		=0;
int	pencolor	=1;
int	penmode		=0;
int	penfat		=0;
int	pendash		=0;
float	*dashptr	=NULL;
float	dashval		=0.0;

/* brush state */
int	brushcur	=0;
int	brushmode	=0;
int	brushcolor	=1;
int	brushpat	=0;

/* text */
float	textsize	=0.2;
int	textfont	=0;
int	textcenter	=0;
float	textangle	=0.0;
float	textcosang	=1.0;
float	textsinang	=0.0;

/* misc. run time selectable parameters */
int	grid		=0;
int	fatbase		=0;

/* raster parameters */
int rastfillmode	=0;

/* label parameters  */
float labelx = .3;    /* position of label */
float labely = .3;

/* initial table of pens */
struct pentypes  pens[MAXPENS] = {
/*	     bwflag   cflag   color    fat    cfat     idash   cdash    mode */
/*  0 */	1,	1,      1,	0,    2,	0,	0,	0,
/*  1 */	1,	1,      2,	2,    2,	0,	0,	0,
/*  2 */	1,	1,      3,	4,    2,	0,	0,	0,
/*  3 */	1,	1,      4,	6,    2,	0,	0,	0,
/*  4 */	1,	1,      5,	0,    2,	1,	0,	0,
/*  5 */	1,	1,      6,	0,    2,	2,	0,	0,
/*  6 */	1,	1,      7,	0,    2,	3,	0,	0,
/*  7 */	1,	1,      8,	0,    2,	4,	0,	0	};

/* initial table of brushs */
struct brushtypes  brushs[MAXBRUSH] = {
/*	     bwflag   cflag color    ipat    mode */
/*  0 */	1,	1,    1,	1,	0,
/*  1 */	1,	1,    2,	2,	0,
/*  2 */	1,	1,    3,	3,	0,
/*  3 */	1,	1,    4,	4,	0,
/*  4 */	1,	1,    5,	5,	0,
/*  5 */	1,	1,    6,	6,	0,
/*  6 */	1,	1,    7,	7,	0,
/*  7 */	1,	1,    8,	8,	0	};

/* initial table of dashs */
#define P PIXINCH
float dashs[NDASH][4] = {
/*  0 */ 1.00*P, 1.00*P, 2.00*P, 2.00*P, /* solid */
/*  1 */ 0.05*P, 0.10*P, 0.15*P, 0.20*P,
/*  2 */ 0.10*P, 0.20*P, 0.30*P, 0.40*P,
/*  3 */ 0.15*P, 0.30*P, 0.45*P, 0.60*P,
/*  4 */ 0.05*P, 0.15*P, 0.17*P, 0.27*P,
/*  5 */ 0.10*P, 0.15*P, 0.17*P, 0.27*P,
/*  6 */ 0.10*P, 0.20*P, 0.30*P, 0.32*P,
/*  7 */ 0.20*P, 0.30*P, 0.35*P, 0.40*P	};

/* initial table of colors */
char colors[NCOLOR][4] = {
/*	  defined     red   green    blue */
/*   0 */	1,	0,	0,	0,          /* black  */
/*   1 */	1,	255,	0,	0,          /* red    */
/*   2 */	1,	0,	255,	0,          /* green  */
/*   3 */	1,	255,	255,	0,          /* yellow */
/*   4 */	1,	0,	0,	255,        /* blue   */
/*   5 */	1,	255,	0,	255,        /* magenta*/
/*   6 */	1,	0,	255,	255,        /* cyan   */
/*   7 */	1,	255,	255,	255	};  /* white  */

#include "patterns.h"
