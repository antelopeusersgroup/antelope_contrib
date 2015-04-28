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
float	symsinangle	=0.0;
float	symcosangle	=1.0;
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
/*   0 */	1,	0,	0,	0,         
/*   1 */	1,	255,	0,	0,          
/*   2 */	1,	0,	255,	0,
/*   3 */	1,	255,	255,	0,          /* yellow */
/*   4 */	1,	0,	0,	255,        /* blue   */
/*   5 */	1,	255,	0,	255,        /* magenta*/
/*   6 */	1,	0,	255,	255,        /* cyan   */
/*   7 */	1,	255,	255,	255	};  /* white  */


/* initial table of patterns - should add some more   */
int patterns[NPATTERNS][32] = {
/* black 0  */
	  0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 
	  0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 
	  0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 
	  0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 
	  0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 
	  0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 
	  0xffffffff, 0xffffffff, 
/*dark shade 1  */
	0x55555555, 0, 0xaaaaaaaa, 0, 0x55555555, 0, 0xaaaaaaaa, 0,
	0x55555555, 0, 0xaaaaaaaa, 0, 0x55555555, 0, 0xaaaaaaaa, 0,
	0x55555555, 0, 0xaaaaaaaa, 0, 0x55555555, 0, 0xaaaaaaaa, 0,
	0x55555555, 0, 0xaaaaaaaa, 0, 0x55555555, 0, 0xaaaaaaaa, 0,
/* dark another shade 2 */
	  0x1f1f1f1f, 0, 0x44444444, 0, 0x1f1f1f1f, 0, 0x44444444, 0,
	  0x1f1f1f1f, 0, 0x44444444, 0, 0x1f1f1f1f, 0, 0x44444444, 0,
	  0x1f1f1f1f, 0, 0x44444444, 0, 0x1f1f1f1f, 0, 0x44444444, 0,
	  0x1f1f1f1f, 0, 0x44444444, 0, 0x1f1f1f1f, 0, 0x44444444, 0,
/*medium shade  3 */
	0x01010101, 0, 0x10101010, 0, 0x01010101, 0, 0x10101010, 0,
	0x01010101, 0, 0x10101010, 0, 0x01010101, 0, 0x10101010, 0,
	0x01010101, 0, 0x10101010, 0, 0x01010101, 0, 0x10101010, 0,
	0x01010101, 0, 0x10101010, 0, 0x01010101, 0, 0x10101010, 0,
/* light shade  4 */
	  0x01010101, 0, 0, 0, 0x10101010, 0, 0, 0, 
	  0x01010101, 0, 0, 0, 0x10101010, 0, 0, 0, 
	  0x01010101, 0, 0, 0, 0x10101010, 0, 0, 0, 
	  0x01010101, 0, 0, 0, 0x10101010, 0, 0, 0, 
/*crosshatch for debug 5 */
        0xfffffffd, 0xfffffffd, 0xfffffffd, 0xfffffffd, 0xfffffffd, 0xfffffffd,
        0xfffffffd, 0xfffffffd, 0xfffffffd, 0xfffffffd, 0xfffffffd, 0xfffffffd,
        0xfffffffd, 0xfffffffd, 0xfffffffd, 0xfffffffd, 0xfffffffd, 0xfffffffd,
        0xfffffffd, 0xfffffffd, 0xfffffffd, 0xfffffffd, 0xfffffffd, 0xfffffffd,
        0xfffffffd, 0xfffffffd, 0xfffffffd, 0xfffffffd, 0xfffffffd, 0xfffffffd,
        0xfffffffd, 0, 
/* 6 brick */
	0xffffffff, 0x80000000, 0x80000000, 0x80000000,
	0x80000000, 0x80000000, 0x80000000, 0x80000000,
	0x80000000, 0x80000000, 0x80000000, 0x80000000,
	0x80000000, 0x80000000, 0x80000000, 0x80000000,
	0xffffffff, 0x00008000, 0x00008000, 0x00008000,
	0x00008000, 0x00008000, 0x00008000, 0x00008000,
	0x00008000, 0x00008000, 0x00008000, 0x00008000,
	0x00008000, 0x00008000, 0x00008000, 0x00008000,

/* 7  brick */ 
	0x0000ffff, 0x01000100, 0x01000100, 0x01000100,
	0x01000100, 0x01000100, 0x01000100, 0x01000100,
	0x01000100, 0x01000100, 0x01000100, 0x01000100,
	0x01000100, 0x01000100, 0x01000100, 0x01000100,
	0xffff0000, 0x01000100, 0x01000100, 0x01000100,
	0x01000100, 0x01000100, 0x01000100, 0x01000100,
	0x01000100, 0x01000100, 0x01000100, 0x01000100,
	0x01000100, 0x01000100, 0x01000100, 0x01000100,

/* upward pointing chevron 8  */
	0x20202020, 0x10101010, 0x08080808, 0x04040404,
	0x08080808, 0x10101010, 0x20202020, 0,
	0x20202020, 0x10101010, 0x08080808, 0x04040404,
	0x08080808, 0x10101010, 0x20202020, 0,
	0x20202020, 0x10101010, 0x08080808, 0x04040404,
	0x08080808, 0x10101010, 0x20202020, 0,
	0x20202020, 0x10101010, 0x08080808, 0x04040404,
	0x08080808, 0x10101010, 0x20202020, 0,
/* skinny diagonal up 9 */
	0x80808080,0x40404040,0x20202020,0x10101010,   
	0x08080808,0x04040404,0x02020202,0x01010101,
	0x80808080,0x40404040,0x20202020,0x10101010,
	0x08080808,0x04040404,0x02020202,0x01010101,
	0x80808080,0x40404040,0x20202020,0x10101010,
	0x08080808,0x04040404,0x02020202,0x01010101,
	0x80808080,0x40404040,0x20202020,0x10101010,
	0x08080808,0x04040404,0x02020202,0x01010101,
/* skinny diagonal down 10 */
	  0x01010101,0x02020202,0x04040404,0x08080808,
	  0x10101010,0x20202020,0x40404040,0x80808080,
	  0x01010101,0x02020202,0x04040404,0x08080808,
	  0x10101010,0x20202020,0x40404040,0x80808080,
	  0x01010101,0x02020202,0x04040404,0x08080808,
	  0x10101010,0x20202020,0x40404040,0x80808080,
	  0x01010101,0x02020202,0x04040404,0x08080808,
	  0x10101010,0x20202020,0x40404040,0x80808080,
/* diagonal up 11 */
	0x80008000,0x40004000,0x20002000,0x10001000,   
	0x08000800,0x04000400,0x02000200,0x01000100,
	0x00800080,0x00400040,0x00200020,0x00100010,
	0x00080008,0x00040004,0x00020002,0x00010001,
	0x80008000,0x40004000,0x20002000,0x10001000,
	0x08000800,0x04000400,0x02000200,0x01000100,
	0x00800080,0x00400040,0x00200020,0x00100010,
	0x00080008,0x00040004,0x00020002,0x00010001,
/* diagonal down 12 */
	  0x00010001,0x00020002,0x00040004,0x00080008,
	  0x00100010,0x00200020,0x00400040,0x00800080,
	  0x01000100,0x02000200,0x04000400,0x08000800, 
	  0x10001000,0x20002000,0x40004000,0x80008000,
	  0x00010001,0x00020002,0x00040004,0x00080008,
	  0x00100010,0x00200020,0x00400040,0x00800080,
	  0x01000100,0x02000200,0x04000400,0x08000800, 
	  0x10001000,0x20002000,0x40004000,0x80008000,
/* splatters 13 */
	0x80000000,0x40000000,0x20000000,0x10000000,
	0x08000000,0x04000000,0x02000000,0x01000000,
	0x00800000,0x00400000,0x00200000,0x00100000,
	0x00080000,0x00040000,0x00020000,0x00010000,
	0x00008000,0x00004000,0x00002000,0x00001000,
	0x00000800,0x00000400,0x00000200,0x00000100,
	0x00000080,0x00000040,0x00000020,0x00000010,
	0x00000008,0x00000004,0x00000002,0x000000001,
/* vertical line 14 */
	   0xffffffff, 0xffffffff, 0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,
/* horizontal line 15 */
	0x00000002, 0x00000002, 0x00000002, 0x00000002,
	0x00000002, 0x00000002, 0x00000002, 0x00000002,
	0x00000002, 0x00000002, 0x00000002, 0x00000002,
	0x00000002, 0x00000002, 0x00000002, 0x00000002,
	0x00000002, 0x00000002, 0x00000002, 0x00000002,
	0x00000002, 0x00000002, 0x00000002, 0x00000002,
	0x00000002, 0x00000002, 0x00000002, 0x00000002,
	0x00000002, 0x00000002, 0x00000002, 0x00000002,
	   };
