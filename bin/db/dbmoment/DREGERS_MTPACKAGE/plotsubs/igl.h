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
 *		DSN modified 06/12/90.
 *			Added IGL_SETINTBYTES
 */

/* Intermediate Graphics Language  (IGL) */

#define M	(0x80)			/* command mask */
					/* FMT */
/* vector commands */
#define IGL_MOVE		'm'|M	/*  2 */ 
#define IGL_DRAW		'd'|M	/*  2 */
#define IGL_LINE		'l'|M	/* 10 */
#define IGL_POINT		'p'|M	/*  2 */ 
#define IGL_BOX			'b'|M	/* 10 */
#define IGL_SETFAT		'f'|M	/*  4 */
#define IGL_SETPENCOLOR		'c'|M	/*  4 */
#define IGL_SETPENMODE		'X'|M	/*  4 */
#define IGL_SETPEN		'q'|M	/*  4 */
#define IGL_SETDASH		'E'|M	/*  4 */

/* area fill commands */
#define IGL_POLYFILL		'a'|M	/*  9 */
#define IGL_POLYFILLN		'n'|M	/*  8 and 13 */
#define IGL_BOXFILL		'B'|M	/* 10 */
#define IGL_SETPATTERN		'F'|M	/*  4 */
#define IGL_SETBRUSHMODE	'k'|M	/*  4 */
#define IGL_SETBRUSHCOLOR	'C'|M	/*  4 */
#define IGL_SETBRUSH		'Q'|M	/*  4 */

/* define tables commands */
#define IGL_DEFBWPEN        	'g'|M   /*  7 */
#define IGL_DEFCPEN             'h'|M   /*  7 */
#define IGL_DEFBWBRUSH		'i'|M   /* 11 */
#define IGL_DEFCBRUSH		'o'|M   /* 15 */ 
#define IGL_DEFCOLOR		'K'|M	/*  7 */
#define IGL_DEFPATTERN		'j'|M	/*  5 */
#define IGL_DEFDASH		'G'|M	/* 12 */

/* raster commands */
#define IGL_RASTER		'r'|M	/*  ? */
#define IGL_RASTERFILE		'R'|M	/*  3? */
#define IGL_SETRASTMODE		'L'|M	/*  4 */

/* text commands */
#define IGL_TEXT		't'|M	/* 14 */
#define IGL_SETTEXTANGLE	'T'|M	/*  8 */
#define IGL_SETTEXTSIZE		'U'|M	/*  8 */
#define IGL_SETTEXTFONT		'Y'|M	/*  4 */
#define IGL_TEXTCENTER		'v'|M	/*  4 */

/* symbols */
#define IGL_SYMBOL		'u'|M	/* 12 */
#define IGL_LOADSYMS		'N'|M	/*  1 */
#define IGL_SETSYMANGLE		'V'|M	/*  8 */
#define IGL_SETSYMSIZE		'H'|M	/*  8 */
#define IGL_DEFSYMCOLOR         's'|M   /*  7 */
#define IGL_DEFSYMPATTERN       'S'|M   /* 16 */
#define IGL_BDOT		'z'|M	/* 16 */
#define IGL_WDOT		'Z'|M	/* 16 */

/* plot window commands */
#define IGL_WINDOW		'w'|M	/* 10 */
#define IGL_UNWINDOW		'W'|M	/*  1 */

/* misc. commands */
#define IGL_PURGE		'P'|M	/* purge */
#define IGL_ORIGIN		'O'|M	/*  1? */
#define IGL_INCLUDE		'I'|M	/*  3? */
#define IGL_ERASE		'e'|M	/*  1 */
#define IGL_PAUSE		'J'|M   /*  1 */
#define IGL_ENDPLOT		'D'|M   /*  1 */
#define IGL_PLOTLABEL		'y'|M   /*  3 */
#define IGL_SETINTBYTES		'x'|M	/*  4 */

/* used letters:
	a b c d e f g h i j k l m n o p q r s t u v w x y z
	+ + + + + + + + + + + + + + + + + + + + + + +     + 

	A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
	  + + + + + + + + + + +   + + + + + + + + + + + + +

	0 1 2 3 4 5 6 7 8 9 ! @ # $ % ^ & * ( ) _ - + = ~ `

	| \ ] [ : ; " ' < > , . ? / } {
	                   

 */

/* Command formats:
 *	c:	command			  1 byte
 *	b:	byte (numeric)		  1 byte
 *	n:	integer numeric		  INTBYTE bytes
 *	ix:	integer x value		  INTBYTES bytes
 *	iy:	integer y value		  INTBYTES bytes
 *
 *	purge: c			  1 byte + fflush
 *	fmt01: c			  1 byte 
 *	fmt02: c ix iy                    1 byte + (2*INTBYTES) bytes
 *	fmt03: c string\0		 -  bytes
 *	fmt04: c b			  2 bytes
 *	fmt05: c b 32*n32		130 bytes
 *	fmt06: c n n*bytes		 -  bytes
 *	fmt07: c b b b b		  5 bytes
 *	fmt08: c n			  1 byte + INTBYTE bytes
 *	fmt09: c n * (ix iy)		  1 byte + INTBYTE + 2n*INTBYTES
 *	fmt10: c ix1 iy1 ix2 iy2	  1 byte + (4*INTBYTES) bytes
 *	fmt11: c b b b			  4 bytes
 *	fmt12: c b ix1 iy1 ix2 iy2	  2 bytes + (4*INTBYTES) bytes
 *	fmt13: n * (ix iy)		  2 bytes + (2n*INTBYTES) bytes
 *	fmt14: c ix iy str\0              1 byte + (2*INTBYTES) bytes + str
 *      fmt15: c b B			  3 bytes 
 *      fmt16: c ix1 iy1 n		  1 bytes + (2*INTBYTES) + INTBYTES bytes
 */

/* conversion factors */
#define ANGLENORM	16384.0	/* 2**14 */
#define FLOATNORM	1000.0	/* normalization for floats to shorts */

/* fill and raster modes: */
#define FILL_OR		0
#define FILL_XOR	1
#define FILL_WHITE	2    	/* white fill for penmode */
#define FILL_EQU	2       /* write over mode for brushmode */
#define FILL_BRWHITE	3       /* white fill for brushmode */
#define FILL_AND	4       /* not sure what this does  */

/* Specifications of generic plottting device */

#define GEN_PIXINCH	1000.0
#define GEN_INCHPIX	0.001
#define GEN_XMAX	32767
#define GEN_YMAX	32767

/* Limits */
#define NFRAME		 10	/* maximum number of frames */
#define MAXLEVEL	 10	/* maximum number levels of include files */
#define MAXPENS		256	/* maximum number of pens */
#define MAXFAT		 64	/* maximum thickness of lines */
#define MAXBRUSH	256	/* maximum number of brushs */
#define NCOLOR		256	/* maximum number of colors */
#define NDASH		 16	/* maximum number of dashs */
#define NPATTERNS	128     /* maximum number of patterns */
#define MAXVERT	       4096     /* number of verts for polygon */
#define POLYBUFSIZE	128	
#define NPATROWS	 32     /*no. of rows(integers) in pattern table (32)*/
#define NSYMBOLS	 32      /* number of symbols shapes */
#define MAXSYMVERTS	125	/* max number of verts in symbol */
#define MAXTEXT		128     /* max text in one call to text or label */
#define DEFAULT_INTBYTES  2	/* # of bytes for integer values */
#define DEFAULT_INTMIN	-32768	/* default minimum integer value	*/
#define DEFAULT_INTMAX	32767	/* default minimum integer value	*/
