/*  Helpful constants. Used in every file.
 */

#ifndef NULL
#define NULL 0L
#endif


/* These labels aid in understanding variable scope.
 */
#define EXPORT		/* Global is allocated and visible to all */
#define IMPORT extern	/* Global is visible but allocated elsewhere */
#define PRIVATE static	/* Variable is local to this source file */


/* Legal penmodes.
 */
#define PENBLACK	0
#define PENXOR		1	/* LaserWriter substitutes PENBLACK */
#define PENWHITE	2


/* Legal brushmodes.
 */
#define BRUSHOR		0
#define BRUSHXOR	1	/* LaserWriter substitutes PENOR */
#define BRUSHEQU	2
#define BRUSHWHITE	3


/* Brush pattern info.
 */
#define PATSIZE	32 /* pixels */


/* Parameters for command table keyed by IGL code.
 */
#define CMDTABLEBASE 0xA0	/* First entry has this code */
#define CMDTABLESIZE 96		/* Number of entries. */


/* Other useful constants.
 */
#define PI 3.141592
