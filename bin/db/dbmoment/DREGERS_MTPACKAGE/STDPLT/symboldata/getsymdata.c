#include	<stdio.h>
#include  	<ctype.h>
#include 	"../devs/com/global.h"
#include	"sym.h" 
#include        "../h/igl.h"




struct symboltable 
   {
      int length;
      float centerx;
      float centery;
      int index;
   };

int	symlen;
float	symcosang, symsinang, symsize,symangle;
int	symmode;
int  	symindex; 
extern float pixinch;
/*float pixinch = 100;*/
struct symlist *symlist;
struct symlist *currentsym; 
symload()
   {
	struct symboltable *symtab;
	float *symdata;
	short *shortdata;
	struct intpolygon *curdata;
	int fd,i, np, nverts, len1, len2, len3,len4,lfloat, nsym, npoints;
	int len5;
	int longest;
	float fhold;
	int buf[100];
	char fname[100];
	float fbuf[100]; 
	extern int page;
	/*int page = 0;*/

	sprintf(fname,"%s%s",SYMDIR,SYMFILE);
	if( (fd=open(fname,0)) < 0)
		error("cannot open symbol file=%s\n",SYMDIR);

	read(fd, &nsym,4);
	read(fd, &nverts, 4);

	lfloat = sizeof(float);
	len1= sizeof(struct symboltable);
	len2 = sizeof(float);
	len3= sizeof(struct symlist);
	len4 = sizeof(short);
	len5 = sizeof(struct intpolygon);
	symtab= (struct symboltable *)malloc(NSYMBOLS*len1);
	np= (nsym > NSYMBOLS ? NSYMBOLS : nsym);
	shortdata = (short *)malloc(nverts*2*len4);
	symdata = (float *)malloc(nverts*2*len2);
	symlist=(struct symlist  *)malloc(NSYMBOLS*len3);
	currentsym = (struct symlist *)malloc(len3); /*************/
	if(symtab == NULL || symdata == NULL)
		error("cannot alloc memory for symbols\n");

	/* get lengths and indexes into table*/
	for (i=0; i<np; i++) {
		read(fd,&symtab[i].length,4);
		read(fd,&symtab[i].centerx,4);
		read(fd,&symtab[i].centery,4);
		read(fd,&symtab[i].index,4);
	} /*for*/

	lseek(fd, (long) lfloat, 0);  
	lseek(fd, (long) lfloat, 1);
	lseek(fd, (long) (nsym*len1),1);  /*read lengths and indices*/

	read(fd,symdata,4*nverts);

	for (i=0; i<nverts; i++) {
		fhold = pixinch*symdata[i];
		shortdata[i] = (short) fhold;
	}

	free(symdata);


	longest = symtab[0].length;
	/* now build the table we actually use */
	for(i=0; i<np; i++)
	   {
		if (symtab[i].length > longest) longest= symtab[i].length;
		symlist[i].length = symtab[i].length;
		symlist[i].centerx = (int) pixinch*symtab[i].centerx;
		symlist[i].centery = (int) pixinch*symtab[i].centery;
		symlist[i].vertices = (struct intpolygon *)&shortdata[symtab[i].index];
	   }

	/* default undefined symbols to the first one */
	for(i=nsym; i<NSYMBOLS; i++)
	   {
		symlist[i].length = symlist[0].length;
		symlist[i].centerx = symlist[0].centerx;
		symlist[i].centery = symlist[0].centery;
		symlist[i].vertices = symlist[0].vertices;
           }		

	/* default the current symbol to the first */
	symsize = .25;
	symmode = 1;
	currentsym->length = symlist[0].length;
	fhold = (float)symlist[0].centerx;
	fhold = symsize*fhold;
	currentsym->centerx = (int) fhold; 
	fhold = (float)symlist[0].centery;
	fhold = symsize*fhold;
	currentsym->centery = (int) fhold;
	curdata = (struct intpolygon *)malloc(longest*len5);
	for (i=0; i < symlist[0].length; i++) {
		curdata[i].ixv = (short) symsize*symlist[0].vertices[i].ixv;
		curdata[i].iyv = (short) symsize*symlist[0].vertices[i].iyv;
	}
	currentsym->vertices = &curdata[0];
	/*currentsym->vertices = (struct intpolygon *)&curdata[0];*/
	symindex = 0;
	symangle = 0.0;
	symcosang= 0.0;
	symsinang= 0.0;
	switch(page)
	   {
		case 0:	symcosang=  1.0; break;
		case 1:	symsinang=  1.0; symangle = 90.0; break;
		case 2:	symcosang= -1.0; symangle = 180.0; break;
		case 3:	symsinang= -1.0; symangle = 270.0; break;
	   }
	free(symtab);
	/*dumpsymbols();*/
	return(0);
   }


dumpsymbols()
{
	int i,j;
	struct intpolygon *vertptr; 
	for (i=0; i<NSYMBOLS; i++) {
		vertptr = symlist[i].vertices;
		fprintf(stderr,"\nsymbol %d:\n",i);
		fprintf(stderr,"\tlength %d\n",symlist[i].length);
		fprintf(stderr,"\tcenterx = %d\t",symlist[i].centerx);
		fprintf(stderr,"\tcentery = %d\n",symlist[i].centery);
		for (j=0; j<symlist[i].length;j++) {
			fprintf(stderr,"\tx = %d",symlist[i].vertices[j].ixv);
			fprintf(stderr,"\ty = %d\n",symlist[i].vertices[j].iyv);
		}
	}
}
error(s1,s2)
char *s1,*s2;
{
fprintf(stderr,"err");
}
