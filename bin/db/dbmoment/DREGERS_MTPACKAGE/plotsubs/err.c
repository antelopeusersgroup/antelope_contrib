#include	<stdlib.h>
#include	<stdio.h>
#include	"igl.h"
#include	"stdplt.h"

#define ARGS	a1,a2,a3,a4,a5,a6

err(type,fmt,ARGS)
int type,ARGS;
char *fmt;
   {
	switch(type)
	   {
		case WARN:	/* warning, not fatal */
			fprintf(stderr,"stdplt: ");
			fprintf(stderr,fmt,ARGS);
			break;
		case CONT:	/* continuation, not fatal */
			fprintf(stderr,"\t");
			fprintf(stderr,fmt,ARGS);
			break;
		case FATAL:	/*  fatal */
			fprintf(stderr,"stdplt: ");
			fprintf(stderr,fmt,ARGS);
			fprintf(stderr,"stdplt: FATAL error\n");
			exit(-1);
			break;
	   }
	fprintf(stderr,"\n");
   }
