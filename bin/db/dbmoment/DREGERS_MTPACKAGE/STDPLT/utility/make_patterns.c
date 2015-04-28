#include	<stdio.h>

#define TABLEFILE	"pattern.tab"
#define BITFILE		"pattern.bin"
#define NLET	16
struct tablename
   {
	char name[NLET];
	int index;
   }	table[200];

int patbits[100][32];
int ntable	=0;
int npat	=0;

char line[128];
main()
   {
	int i, nbits, nlines, term, n, nlet;
	int *patptr;
	char *pn, *p, *pname;
	int bitout, tableout;
	char bits[32];

	nbits= nlines= 0;
	patptr= patbits[0];

	while( (n=getline(line)) >= 0)
	   {
		term= (n==0 || line[0] == '%' || line[0] == '#');
		if(!term)
		   {
			fprintf(stderr,"add bit line nlines=%d\n",nlines);
			nbits= 0;
			while( line[nbits] != '\0')
			   {
				bits[nbits]= (line[nbits] == 'O'
					|| line[nbits] == 'X' ? 1 : 0 );
				nbits++;
			   }
			/* repeat out to 32 bits */
			for(i=nbits; i<32; i++) bits[i]= bits[i%nbits];
			patptr[nlines]= setbits(bits);
			nlines++;
			continue;
		   }
		if(nlines != 0)
		   {
			/* repeat lines to 32 */
			for(i=nlines; i<32; i++) patptr[i]= patptr[i%nlines];
			fprintf(stderr,"add pat line npat=%d\n",npat);
			npat++;
			patptr= patbits[npat];
			nlines=0;
			continue;
		   }
		fprintf(stderr,"n=%2d %s\n",n,line);
		if(n == 0 || line[0] == '#') continue;
		/* get names for next pattern */
		pn= line+1;
		while(*pn != '\0')
		   {
			while(*pn == ' ' || *pn == '\t') pn++;
			if(*pn == '\0') break;
			p= pn+1;
			while(*p != '\0' && *p != ' ' && *p != '\t') p++;
			pname= table[ntable].name;
			nlet= 0;
			while(pn < p)
			   {
				*pname++ = *pn++;
				nlet++;
			   }
			*pname = '\0';
			nlet++;
			if(nlet > NLET)
			   {
				fprintf(stderr,"JACKPOT, %s is longer than %d letters\n",table[ntable].name,NLET);
				exit(-1);
			   }
			fprintf(stderr,"add table line ntable=%d\n",ntable);
			table[ntable].index= npat;
			ntable++;
		   }
	   }
	if(nlines)
	   {
		/* repeat lines to 32 */
		for(i=nlines; i<32; i++) patptr[i]= patptr[i%nlines];
		fprintf(stderr,"add pat line npat=%d\n",npat);
		npat++;
	   }
	if( (bitout=creat(BITFILE,0664)) < 0)
	   {
		fprintf(stderr,"cannot creat %s\n",BITFILE);
		exit(-1);
	   }
	if( (tableout=creat(TABLEFILE,0664)) < 0)
	   {
		fprintf(stderr,"cannot creat %s\n",TABLEFILE);
		exit(-1);
	   }
	write(bitout,patbits,4*32*npat);
	write(tableout,&ntable,4);
	write(tableout,&npat,4);
	write(tableout,table,ntable* sizeof(struct tablename));
	fprintf(stderr,"npat= %d ntable=%d\n",npat,ntable);
   }

setbits(bits)
char *bits;
   {
	int i, bit, val;
	val= 0;
	bit= 1;
	for(i=31; i>=0; i--)
	   {
		if(bits[i]) val |= bit;
		bit <<= 1;
	   }
	return(val);
   }

getline(b)
char *b;
   {
	int n;
	char c;
	n= 0;
	while( (c= getc(stdin)) != EOF)
	   {
		if(c == '\n')
		   {
			*b ='\0';
			return(n);
		   }
		*b++ = c;
		n++;
	   }
	return(-1);
   }
