/*			SGP - Standard Graphics Package
 *	  		    (c) Robert W. Clayton
 *	   		     All rights reserved
 *
 * NAME(S):	igl.h
 *
 * CLASS:	format specification for I.G.L
 *
 * COMMENTS:	This is where the Intermediate Graphics Language (IGL) is
 *		defined. Changes here affect the  user plotsubs, and all
 *		the device drivers. At a minimum the device driver common
 *		routine 'readcom' should be changed.
 *
 * HISTORY:	RWC created 01/11/85.
 *		RWC modified 23/03/86.
 */

/* Intermediate Graphics Language  (IGL) */

#define M	(0x80)			/* command mask */
					/* FMT */
/* vector commands */
#define IGL_MOVE		'm'|M	/*  1	move x16 y16 */
#define IGL_DRAW		'd'|M	/*  1	draw x16 y16 */
#define IGL_LINE		'l'|M	/*  2	line xa16 ya16 xb16 yb16 */
#define IGL_POINT		'p'|M	/*  1 */
#define IGL_BOX			'b'|M	/*  2 */
#define IGL_SETFAT		'f'|M	/*  4 */
#define IGL_SETPENCOLOR		'c'|M	/*  4 */
#define IGL_SETPENMODE		'X'|M	/*  4 */
#define IGL_SETPEN		'q'|M	/*  4 */
#define IGL_SETDASH		'E'|M	/*  4 */

/* area fill commands */
#define IGL_POLYFILL		'a'|M	/*  ? */
#define IGL_POLYFILLN		'n'|M	/*  ? */
#define IGL_BOXFILL		'B'|M	/*  2 */
#define IGL_SETPATTERN		'F'|M	/*  4 */
#define IGL_SETBRUSHMODE	'k'|M	/*  4 */
#define IGL_SETBRUSHCOLOR	'C'|M	/*  4 */
#define IGL_SETBRUSH		'Q'|M	/*  4 */

/* define tables commands */
#define IGL_DEFPEN		's'|M	/* 11 */
#define IGL_DEFBWPEN        	'g'|M   /*  7 */
#define IGL_DEFCPEN             'h'|M   /*  7 */
#define IGL_DEFBRUSH		'S'|M	/*  7 */
#define IGL_DEFBWBRUSH		'i'|M   /* 15 */
#define IGL_DEFCBRUSH		'o'|M   /* 16 */ 
#define IGL_DEFCOLOR		'K'|M	/*  7 */
#define IGL_DEFPATTERN		'j'|M	/*  5 */
#define IGL_DEFDASH		'G'|M	/* 14 */

/* raster commands */
#define IGL_RASTER		'r'|M	/*  ? */
#define IGL_RASTERFILE		'R'|M	/*  3 */
#define IGL_SETRASTMODE		'L'|M	/*  4 */

/* text commands */
#define IGL_TEXT		't'|M	/*  3 */
#define IGL_SETTEXTANGLE	'T'|M	/*  8 */
#define IGL_SETTEXTSIZE		'U'|M	/*  8 */
#define IGL_SETTEXTFONT		'Y'|M	/*  4 */
#define IGL_TEXTCENTER		'v'|M	/*  4 */

/* symbols */
#define IGL_SYMBOL		'u'|M	/* 12 */
#define IGL_LOADSYMS		'N'|M	/*    */
#define IGL_SETSYMANGLE		'V'|M	/*  8 */
#define IGL_SETSYMSIZE		'H'|M	/*  8 */

/* plot window commands */
#define IGL_WINDOW		'w'|M	/*  2 */
#define IGL_UNWINDOW		'W'|M	/* 11 */

/* misc. commands */
#define IGL_PURGE		'P'|M	/* 11 */
#define IGL_ORIGIN		'O'|M	/*  1 */
#define IGL_INCLUDE		'I'|M	/*  3 */
#define IGL_ERASE		'e'|M	/* 11 */

/* used letters:
	a b c d e f g h i j k l m n o p q r s t u v w x y z
	+ + + + + + + + + + + + + + + + + + + + + + +       

	A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
	  + +   + + + + +   + +   + + + + + + + + + + + +  

	0 1 2 3 4 5 6 7 8 9 ! @ # $ % ^ & * ( ) _ - + = ~ `

	| \ ] [ : ; " ' < > , . ? / } {
	                   

 */

/* Command formats:
 *
 *	fmt01: c x16 y16		  5 bytes
 *	fmt02: c x16 y16 x16 y16	  9 bytes
 *	fmt03: c string\0		 -  bytes
 *	fmt04: c n8			  2 bytes
 *	fmt05: c 32*n32			129 bytes
 *	fmt06: c n16 n*bytes		 -  bytes
 *	fmt07: c n8 n8 n8 n8		  5 bytes
 *	fmt08: c n16			  3 bytes
 *	fmt09: c n16 4*n*bytes		 -  bytes
 *	fmt10: c n16 n16 n16 n16	  9 bytes
 *	fmt11: c 			  1 bytes
 *	fmt12: c n8 n16 n16		  6 bytes
 *	fmt13: c n8 n8 n8 n8 n8 n8	  7 bytes
 *	fmt14: c n8 n16 n16 n16 n16	 10 bytes
 */

/* conversion factors */
#define ANGLENORM	16384.0	/* 2**14 */
#define FLOATNORM	1000.0	/* normalization for floats to shorts */

/* fill and raster modes: */
#define FILL_OR		0
#define FILL_XOR	1
#define FILL_EQU	2
#define FILL_WHITE	2
#define FILL_AND	3

/* Specifications of generic plottting device */

#define GEN_PIXINCH	1000.0
#define GEN_INCHPIX	0.001
#define GEN_XMAX	32767
#define GEN_YMAX	32767

/* Limits */
#define NFRAME		 10	/* maximum number of frames */
#define MAXLEVEL	 10	/* maximum number levels of include files */
#define MAXPENS		 64	/* maximum number of pens */
#define MAXFAT		 64	/* maximum thickness of lines */
#define MAXBRUSH	 64	/* maximum number of brushs */
#define NCOLOR		256	/* maximum number of colors */
#define NDASH		 16	/* maximum number of dashs */
#define NPATTERNS	 16
#define MAXVERT	       4096     /* number of verts for polygon */
#define NPATROWS	 32     /*no. of rows(integers) in pattern table (32)*/
#define NSYMBOLS	 10      /* number of symbols shapes */
#define NSYMROWS	  8     /* number of rows(integers)in symbol patterns*/
#define NSYMMODES	  6     /* number of symbol modes (dark, light, etc.)*/
				/* or colors for color devs */
