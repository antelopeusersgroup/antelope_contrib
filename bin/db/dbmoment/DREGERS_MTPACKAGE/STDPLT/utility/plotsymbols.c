#include	<stdio.h>

struct sn
   {
	char *symname;
	int   symnum;
   };
float hgt	=1.0;
float ang	=0.0;
main(ac,av)
int ac; char **av;
   {
	struct sn *p, *getsymbolnames();
	int n, i;
	float x, y;

	setpar(ac,av);
	getpar("hgt","f",&hgt);
	getpar("ang","f",&ang);
	endpar();

	p= getsymbolnames(&n);
	text(0.5,0.5,"nsymbol= %d",n);
	x= 1.0; y= 1.0;
	for(i=0; i<=n; i++)
	   {
		if(y >= 8.0)
		   {
			x += 1.5;
			y= 1.0;
		   }
		symbol(x,y,i+128,hgt,ang);
		y += 1.0;
	   }
	/*
	fprintf(stdout,"nsymbol= %d\n",n);
	while(p->symname[0] != '\0')
	   {
		fprintf(stdout,"\t%10s %2d\n",p->symname,p->symnum);
		p++;
	   }
	*/
   }
