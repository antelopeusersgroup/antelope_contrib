#include	<stdio.h>
#define MAXLEN	256

extern int Xscale();
extern int Xplot();
struct coms
   {
	char comname[16];
	int hash;
	int (* comsub)();
	int comarg;
   } coms[] =
   {
	"setscl",	0,	Xscale,	0,
	"setorig",	0,	Xscale,	1,
	"setmin",	0,	Xscale,	2,
	"plot",		0,	Xplot,	0,
	"move",		0,	Xplot,	1,
	"draw",		0,	Xplot,	2,
	"uplot",	0,	Xplot,	3,
	"umove",	0,	Xplot,	4,
	"udraw",	0,	Xplot,	5,
	NULL,		0,	NULL,	0
   };

char *av[20];
char argbuf[1024];
int ac;
main()
   {
	int k, i;
	char line[MAXLEN];
	genhash();
	while( fgets(line,MAXLEN,stdin) != NULL)
	   {
		ac= getav(av,line);
		if( (k= lookup(*av)) < 0)
		   {
			fprintf(stderr,"unknown command: ");
			for(i=0;i<ac;i++) fprintf(stderr," %s",av[i]);
		   }

		coms[k].comsub(ac,av,coms[k].comarg);
	   }
   }

getav(av,line)
char **av; char *line;
   {
	register char *pl, *aptr;
	int ac;

	aptr= argbuf;
	pl= line;
	ac=0;
	do
	   {
		while(*pl == ' ' || *pl == '\t') pl++;
		av[ac]= aptr;
		while(*pl != '\0' && *pl != ' ' && *pl != '\t') *aptr++ = *pl++;
		*aptr++ = '\0';
		ac++;
	   }	while(*pl);
	return(ac);
   }

lookup(name)
char *name;
   {
	int h, k;
	register struct coms *cptr;
	h= computehash(name);
	for(k=0, cptr= coms; cptr->comname[0] != '\0'; cptr++, k++)
	   {
		if( h != cptr->hash) continue;
		if( strcmp(name,cptr->comname) ) continue;
		return(k);
	   }
	return(-1);
   }
genhash()
   {
	register struct coms *cptr;

	for(cptr= coms; cptr->comname[0] != '\0'; cptr++)
	   {
		cptr->hash= computehash(cptr->comname);
	   }
   }
computehash(s)
register char *s;
   {
	register int h;
	h= s[0];
	if(s[1]) h |= (s[1])<<8;	else return(h);
	if(s[2]) h |= (s[2])<<16;	else return(h);
	if(s[3]) h |= (s[3])<<24;
	return(h);
   }
float getval(s)
char *s;
   {
	float f;
	double atof();
	f=atof(s);
	return(f);
   }

Xscale(ac,av,flag)
int ac, flag;
char **av;
   {
	float x, y;
	float getval();
	x= getval(av[1]);
	y= getval(av[2]);
	switch(flag)
	   {
		case 0: setscl(x,y);	break;
		case 1: setorig(x,y);	break;
		case 2: setmin(x,y);	break;
	   }
   }
Xplot(ac,av,flag)
int ac, flag;
char **av;
   {
	float x, y;
	float getval();
	int ipen;
	x= getval(av[1]);
	y= getval(av[2]);
	if(ac >=3) ipen= atol(av[3]);
	switch(flag)
	   {
		case 0: plot(x,y,ipen);	break;
		case 1: plot(x,y,0);	break;
		case 2: plot(x,y,1);	break;
		case 3: uplot(x,y,ipen);	break;
		case 4: uplot(x,y,0);	break;
		case 5: uplot(x,y,1);	break;
	   }
   }
