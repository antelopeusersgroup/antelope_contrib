/* $Name $Revision$ $Date$  */
/* general purpose macros to trim strings for C and pad them for fortran*/

#ifndef TRIM
#define TRIM(s,l) {int i;for(i=l-1;i>0,s[i-1]==' ';i--);s[i]='\0';}
#endif
#ifndef FPAD
#define FPAD(s,l) {int i;for(i=strlen(s);i<l;s[i++]=' ');}
#endif
#ifndef BFIL
#define BFIL(s,l) {int i;for(i=0;i<l;s[i++]=' ');}
#endif


/* $Id$ */
