/* $Name $Revision$ $Date$ */
/*======================================================================
 *
 *  include/util.h
 *
 *  Include file of defines, data structures, and macros
 *  for use with the util library
 *
 *====================================================================*/
#ifndef util_h_included
#define util_h_included
#include <stdio.h>
#include <math.h>

/* Misc. constants */

#ifndef SUN_ORDER
#define SUN_ORDER (1)
#endif
#ifndef VAX_ORDER
#define VAX_ORDER (2)
#endif
#ifndef PDP_ORDER
#define PDP_ORDER (3)
#endif

#ifndef SEEK_SET
#define SEEK_SET 0
#endif

#ifndef SEEK_CUR
#define SEEK_CUR 1
#endif

#ifndef SEEK_END
#define SEEK_END 2
#endif

#ifndef TRUE
#define TRUE (1)
#endif

#ifndef FALSE
#define FALSE (0)
#endif

/* Macros */

#define blank_fill(fa,n) ( memset ((fa),' ',(n)) )
#define fromDB(x) (pow(10.0, x/20.0))
#define toDB(x)   (20.0 * log10(x))
#ifndef leap_year
#define leap_year(i) ((i % 4 == 0 && i % 100 != 0) || i % 400 == 0)
#endif
#ifndef dysize
#define dysize(i) (365 + leap_year(i))
#endif

typedef struct myentr {
  int num;
  char name[1024];
} MYENT;
 
/* Non integer function declarations */

extern char *ucase();
extern char *lcase();
extern char *strdup();
extern FILE *openf();
extern char *abs_path();
extern char *sccs_str();
extern double attodt();
extern int cmptbl();

/*  Comparison function declarations  */

extern int intcmp();
extern int longcmp();
extern int shortcmp();
extern int floatcmp();
extern int doublecmp();
extern int charcmp();
extern int stringcmp();

extern double fixtime();
extern int    network();
extern char   *rdhdr1();
extern void   shrt_despike();
 
#endif

/* $Id$ */
