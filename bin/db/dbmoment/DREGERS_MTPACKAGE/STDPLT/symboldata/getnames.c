#include	<stdio.h>
#include  	<ctype.h>
#include 	"sym.h"
#include        "../h/igl.h"
#define BUFSIZE 100

char    *namebuf;
char    intbuf[4];
struct symnames *symnames;

symload(symptr)
struct symnames *symptr[];
   {
	int buf[100];
	int fn, i, np, len, nsym, count, *ihold;
	char *holdbuf,fname[100];
	char *c;

	symptr[0] = NULL;

	sprintf(fname,"%s%s",SYMDIR,NAMEFILE);
	if( (fn=open(fname,0)) < 0) {
		fprintf(stderr,"warning cannot open symbol file=%s\n",fname);
		fprintf(stderr,"no symbols\n");
		return(-1);
	}

	read(fn, &nsym,4);


	len= sizeof(struct symnames);
	np= (nsym > NSYMBOLS ? NSYMBOLS : nsym);
	symnames =(struct symnames  *)malloc(NSYMBOLS*len);
	namebuf =  (char *)malloc(BUFSIZE);

	if(symnames == NULL || namebuf == NULL) {
		fprintf(stderr,"cannot alloc memory for symbols\n");
		fprintf(stderr,"no symbols\n");
		return(NULL);
	}

	/* get names and indexes into table*/
	c = namebuf;
	count = 0;
	for (i=0; i<np; i++) {
		symnames[i].name = c; 
		if (count > BUFSIZE) {
			namebuf = (char *)malloc(BUFSIZE);
			if(namebuf == NULL) {
				fprintf(stderr,"cannot alloc memory for symbols\n");
				return(NULL);
			}
			c = namebuf;
			count = 0;
		}
		read(fn,c,1);
		count++;
		while (*c!=' ' && *c!='\n' && *c!='\t') {
			c++;
			if (count > BUFSIZE) {
				namebuf = (char *)malloc(BUFSIZE);
				if(namebuf == NULL) {
					fprintf(stderr,"cannot alloc memory for symbols\n");
					return(NULL);
				}
				c = namebuf;
				count = 0;
			}
			read(fn,c,1);
			count++;
		} /*while*/
		read(fn,c,1);
		holdbuf = intbuf;
		intbuf[0] = *c;
		holdbuf++;
		*c = '\0';
		c++;
		read(fn,holdbuf,1); holdbuf++;
		read(fn,holdbuf,1); holdbuf++;
		read(fn,holdbuf,1);
		ihold = (int *) intbuf;
		symnames[i].index = *ihold;
	} /*for*/
	for (i=nsym; i<NSYMBOLS; i++) {
		symnames[i].name = symnames[0].name;
		symnames[i].index = symnames[0].index;
	}
	symptr[0] = symnames;
	return(0);
   }


