#include	<stdio.h>
#include	"../h/chars.h"


#define DROPBIT 02000
short *symadd;
char *symbase;

main()
   {

	int i;
	int add,drop,xp,yp;
	char c,xyw,*symptr;
	int h, v;
	int cnt;
	symadd= ascii.saddr;
	symbase= ascii.svec;
	fprintf(stdout,"#\n# Old ugly characters - Roman\n#\n");


	for(i=0; i<96; i++)
	   {
		c= ' ' + i;
		add= symadd[(int)(c-040)];
		symptr= symbase +(add&01777);
		if(*symptr & 010) continue;
		drop= (add&DROPBIT ? 2 : 0);
		fprintf(stdout,"+ \"%c\" R  0 0  8\n",c);
		/*drop= 0;*/
		fprintf(stdout,"\t");
		cnt= 0;
		do
		   {
			if(cnt > 0 && (cnt % 8) == 0) fprintf(stdout,"\n\t");
			xyw= *symptr++;
			h= (((xyw&0160)>>4)); 
			/*v= (((xyw&07) - drop));*/
			v= (xyw&07) - drop;
			if( !(xyw&0200) )
				fprintf(stdout,"d %2d %2d ",h,v);
			 else	fprintf(stdout,"m %2d %2d ",h,v);
			cnt++;
		   } while( !(xyw&010) );
		fprintf(stdout,"\n");
	   }
	symadd= greek.saddr;
	symbase= greek.svec;
	fprintf(stdout,"#\n# Old ugly characters - Special\n#\n");

	for(i=0; i<96; i++)
	   {
		c= ' ' + i;
		add= symadd[(int)(c-040)];
		symptr= symbase +(add&01777);
		if(*symptr & 010) continue;
		drop= (add&DROPBIT ? 2 : 0);
		fprintf(stdout,"+ \"%c\" S  0 0 8\n",c);
		/*drop= 0;*/
		fprintf(stdout,"\t");
		cnt= 0;
		do
		   {
			if(cnt > 0 && (cnt % 8) == 0) fprintf(stdout,"\n\t");
			xyw= *symptr++;
			h= (((xyw&0160)>>4)); 
			v= (((xyw&07) - drop));
			if( !(xyw&0200) )
				fprintf(stdout,"d %2d %2d ",h,v);
			 else	fprintf(stdout,"m %2d %2d ",h,v);
			cnt++;
		   } while( !(xyw&010) );
		fprintf(stdout,"\n");
	   }
   }
