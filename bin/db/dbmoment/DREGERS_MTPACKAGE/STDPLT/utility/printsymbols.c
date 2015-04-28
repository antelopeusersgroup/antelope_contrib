#include	<stdio.h>

struct sn
   {
	char *symname;
	int   symnum;
   };
main()
   {
	struct sn *p, *getsymbolnames();
	int n;

	p= getsymbolnames(&n);
	fprintf(stdout,"nsymbol= %d\n",n);
	while(p->symname[0] != '\0')
	   {
		fprintf(stdout,"\t%10s %2d\n",p->symname,p->symnum);
		p++;
	   }
   }
