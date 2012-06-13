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
#include <sys/types.h>
#include <fcntl.h>

/* Macros */

#define blank_fill(fa,n) ( memset ((fa),' ',(n)) )

/* True/false definitions */
 
#ifndef TRUE
#define FALSE 0
#define TRUE  1
#endif
 
typedef struct myentr {
  int num;
  char name[1024];
} MYENT;

/* Variable to handle execution when process gets SIGNALs  */

int Run;

/* Non integer function declarations */

extern void sig_hdlr();
extern int direntcmp();
extern char *getenv();
extern char *sccs_str();
extern char *lcase();
extern char *ucase();

#endif
