#include	<stdio.h>
#include	"../h/igl.h"
#include	"stdplt.h"

/* WARNING: Assume 32 bit word. */
/* Insure that we don't lose precision. */
/*::
#define	CHECKINT(x,y)	if (x < INTMIN | x > INTMAX | y < INTMIN | y > INTMAX) \
		setintbytes (2*INTBYTES);
::*/
#define	CHECKINT(x,y)	if ( INTBYTES == 2 ) \
		setintbytes (2*INTBYTES);

extern struct frameinfo *frames, *cfr;
extern struct globalinfo *glcur;
/*
extern FILE *PLOTOUT;
02/24/2001dsd changed all references to PLOTOUT to stdout 
table.c FILE *PLOTOUT=stdout commented out per gcc compile error
*/

/* Basic output commands for IGL, all I/O file handling concentrated here */

/* There is one output?() for each command format. */

purge()
   {
     putc(IGL_PURGE,stdout);
     fflush(stdout);
   }

output1(c)
char c;
   {
	putc(c,stdout);
   }

output2(c,x,y)
char c;
int x, y;
   {
	CHECKINT(x,y);
	putc(c,stdout);
	putxy (x,y);
   }

output3(c,str)
char c, *str;
   {
	putc(c,stdout);
	do putc(*str,stdout); while(*str++);
   }

output4(c,n)
char c;
int n;
   {
	putc(c,stdout);
	putc(n,stdout);
   }

output5(c,n,ptr)
char c;
int n, *ptr;
   {
	putc(c,stdout);
	putc(n,stdout);
	fwrite(ptr,4,32,stdout);
   }
/* nothing currently uses this, but does it output xy pairs? */
output6(c,n,ptr,len)
char c;
int n, len;
short *ptr;
   {
	putc(c,stdout);
	puti(n,stdout);
	fwrite(ptr,1,len,stdout);
   }

output7(c,n1,n2,n3,n4)
char c;
int n1,n2,n3,n4;
   {
	putc(c,stdout);
	putc(n1,stdout);
	putc(n2,stdout);
	putc(n3,stdout);
	putc(n4,stdout);
   }
output8(c, n)
char c;
int n;
   {
	CHECKINT(35,35);		/* Force int precision */
	putc(c, stdout);
	puti(n, stdout);
   }

output9(c, p, n)
char c;
int *p;
int n;
   {
	int i;
	int *px, *py, *tp;
	tp = p;
	for (i = 0; i< n; i++) {
		px = tp++; py = tp++;
		CHECKINT (*px, *py);
	}
	putc(c, stdout);
	puti(n, stdout);
	tp = p;
	for (i = 0; i< n; i++) {
		px = tp++; py = tp++;
		putxy (*px, *py);
	}
   }

output10(c,x1,y1,x2,y2)
char c;
int x1,y1,x2,y2;
   {
	CHECKINT(x1, y1);
	CHECKINT(x1, y2);
	putc(c,stdout);
	putxy (x1, y1);
	putxy (x2, y2);
   }

output11(c,n1,n2,n3)
char c;
int n1,n2,n3;
   {
	putc(c,stdout);
	putc(n1,stdout);
	putc(n2,stdout);
	putc(n3,stdout);
   }

output12(c,b1,x1,y1,x2,y2)
char c;
int b1;
int x1,y1,x2,y2;
   {
	CHECKINT(x1, y1);
	CHECKINT(x2, y2);
	putc(c,stdout);
	putc(b1,stdout);
	putxy (x1, y1);
	putxy (x2, y2);
   }

 
output13(p,n)
int *p;
int n;
   {  
	int i;
	int *px, *py, *tp;
	tp = p;
	for (i = 0; i< n; i++) {
		px = tp++; py = tp++;
		CHECKINT (*px, *py);
	}
	puti(n,stdout);
	tp = p;
	for (i = 0; i< n; i++) {
		px = tp++; py = tp++;
		putxy (*px, *py);
	}
   }

output14(c,x1,y1,str)
char c, *str;
int x1,y1;
   {
	CHECKINT(x1,y1);
	putc(c,stdout);
	putxy(x1,y1);
	do putc(*str,stdout); while(*str++);
   }

output15(c,n1,n2)
char c;
int n1,n2;
   {
	putc(c,stdout);
	putc(n1,stdout);
	putc(n2,stdout);
   }

output16(c,x1,y1,n3)
char c;
int x1,y1,n3;
   {
	CHECKINT(x1,y1);
	putc(c,stdout);
	putxy(x1,y1);
	puti(n3,stdout);
   }

putxy (x,y)
int x, y;
{
	switch (INTBYTES) {
	case 2:
		puth (x,stdout);
		puth (y,stdout);
		break;
	case 4:
		putw (x,stdout);
		putw (y,stdout);
		break;
	default:
		err (FATAL,"INTBYTES = %d", INTBYTES);
		break;
	}
}

/* output an integer */
puti (n, file)
int n;
FILE *file;
{
	switch (INTBYTES) {
	case 2:
		puth (n, file);
		break;
	case 4:
		putw (n, file);
		break;
	default:
		err (FATAL,"INTBYTES = %d", INTBYTES);
		break;
	}
}
