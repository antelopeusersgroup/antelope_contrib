#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#define TNAME "textfile"
#define DNAME "namefile"
#define SYMNAME "symfile"
#define TMPNAME "temp"
#define permission 0775

/* this program data.c creates the data files that are read by the stdplt and
   the com programs to process symbol commands. 
   It reads and writes from files that are in the same directory as this
   program. It is independent of any other programs and must be run (once)
   to create the data files.  The file names must match those that are in 
   sym.h in order for the stdplt programs to work.
   It reads from textfile and writes out to namefile which contains
   information about valid symbols names, and writes out to symfile
   which contains information such as the number of pairs of vertices (length),
   the center point of the symbol, list of all the vertices and the
   index of each symbol into that list.
   The format for the textfile should be
   once--
	<total number of symbols in list>       (string repr. integer)
	<total number of pairs of vertices>	(string repr. integer)
   for each symbol
	<name of symbol>			(character only string)
	<number of pairs of vertices in symbol> (string repr. integer)
	<x center point>			(string repr. float --
							i.e. with decimal point)
	<y center point>			(string repr. float)
	<index into list of vertices>		(string repr. integer) 
	<list of x,y vertices for this symbol>  (strings repr. floats)
	<vertices for this symbol>		(strings repr. floats)
   The total number of symbols, and name and an index for each symbol is
   written out to namefile.  The index is just an integer 0,1,2,3.. that
   is the position of the symbol in textfile, and is not really  necessary.
   The namefile is used by the stdplt program if a symbol is required.

   The total number of symbols, total number of pairs of vertices,
   lengths, x and y center points,
   and indexes into a list of all the vertices for each symbol,
   and then a list of all the vertices is written out to symfile. (In an 
   intermediary step the list of vertices is written out to a temporary file
   which is located in the SYMDIR directory.)
   The index into the list of vertices could be computed in this program from 
   the lengths of each symbol, but is only done so as a check and must be 
   included in the textfile. Symfile is used in the com program. */


/* if given ptr is blank, skipblanks will skip to next non-blank character
   otherwise does nothing */
skipblanks(file,ptr)
int file;
char *ptr;
{
	int n;
	while (*ptr == ' ' || *ptr == '\n' || *ptr == '\t') {
	  	 n = read(file,ptr,1);
		 if (n <= 0) return(n); 
	}
	return(1);
}

/*given that the entering ptr is a digit, getinteger gets the value of the 
  next integer to be found in the file -- that is the next concurrent bunch
  of digits including the first value of ptr otherwise num is 0*/ 
getinteger(file,ptr,num)
char *ptr;
int *num,file;
{
	int n;
	*num = 0;
	while (*ptr>='0' && *ptr<='9') {
		*num = 10* *num + *ptr - '0';
	        n = read(file, ptr, 1); 
		if (n <= 0) return(n); 
	}
	return(1);
} /*getinteger*/


/*given that the entering ptr is a digit, getinteger gets the value of the 
  next decimal to be found in the file -- that is the next concurrent bunch
  of digits including the first value of ptr, each successive one divided
  by 10*/ 
getdecimal(file,ptr,fnum)
char *ptr;
int file;
float *fnum;
{
	int n,num;
	float fake,x,dn,hold;
	num = 0;
	dn = 1.0;
	while (*ptr>='0' && *ptr<='9') {
		dn *= .1;
		num = 10* num + *ptr - '0';
	        n = read(file, ptr, 1); 
		if (n <= 0) return(n); 
	}
	*fnum = num*dn;
	return(1);
} /*getdecimal*/


main(ac,av)
int ac; char **av;
{
	int numptr[8888],n;
	int length;
	int i,j,f1,f2,f3,f4,num,symnum,nverts;
	int vertindex = 0;
	float fnum,dec;
	int debug; 
	char ptr[1],line[132],c;

	setpar(ac,av);
	getpar("debug","d",&debug); /*for error checking of results*/


	if ((f1 = open(TNAME,0)) == -1)
		error("can't open %s", TNAME);
	if ((f2 = creat(DNAME,permission)) == -1)
		error("can't create %s", DNAME);
	if ((f3 = creat(SYMNAME,permission)) == -1)
		error("can't create %s", SYMNAME);
	if ((f4 = creat(TMPNAME,permission)) == -1)
		error("can't create %s", TMPNAME);


	/*get symnum*/
	n = read(f1,ptr,1);
	n = skipblanks(f1,ptr);
	if (n <= 0) error("nothing in file %s", TNAME);
	if  (isdigit(*ptr) <= 0)
		error("first entry not a number in %s",TNAME);
	n = getinteger(f1,ptr,&symnum);
	if (n <= 0) error("only one num in %s",TNAME);
	write(f2,&symnum,4);    /*write out number of symbols to both files*/
	write(f3,&symnum,4);
	if(debug) fprintf(stdout,"symnum=%d\n",symnum); 

	n = skipblanks(f1,ptr);
	if (n <= 0 && symnum!=0) error("need nverts in file %s", TNAME);
	if  (isdigit(*ptr) <= 0)
		error("need nverts in %s",TNAME);
	n = getinteger(f1,ptr,&nverts);
	if (n <= 0 && symnum != 0) error("need nverts/symnum must be int in %s",TNAME);
	write(f3,&nverts,4);
	if(debug) fprintf(stdout,"nverts=%d\n",nverts); 


	for (i=0; i<symnum ; i++) {
		n = skipblanks(f1,ptr);
		if (n <= 0) error("too few symbols in %s", TNAME);

		/* get name */
		if (isalpha(*ptr) <= 0)
			error("nverts must be int/missing name in %s",TNAME); 
		while (*ptr != ' ' && *ptr != '\n' && *ptr != '\t') {
			write(f2,ptr,1);
			n = read(f1,ptr,1);
			if (n <= 0) error("missing data in %s",TNAME);
		}
		c = ' ';
		write(f2,&c,1);
		write(f2,&i,4);   /*write out index of name*/
		
		/* now output length */
		n = skipblanks(f1,ptr);
		if (n <= 0) error("missing data in %s", TNAME);
		if (isdigit(*ptr) <= 0) 
			error("missing length in %s",TNAME);
		n = getinteger(f1,ptr,&num);
		if (n <= 0) error("missing data in %s",TNAME);
		write(f3,&num,4); 
		if(debug) fprintf(stdout,"length %d\n",num); 
		length = num;

		/* now output center x*/
		n = skipblanks(f1,ptr);
		if (n <= 0) error("length must be int/need center in %s", TNAME);
		if (isdigit(*ptr) <= 0 && *ptr != '.') 
			error("missing center %s",TNAME);
		n =  getinteger(f1,ptr,&num);
		fnum = (float) num;
		if (n <= 0) error("missing center in %s",TNAME);
		if (*ptr == '.')  {
			n = read(f1,ptr,1);
			if (n<= 0) error("missing center in %s",TNAME);
			n = getdecimal(f1,ptr,&dec);
		}
		if (n <= 0) error("missing center in %s",TNAME);
		fnum += dec;
		if(debug) fprintf(stdout,"centerx is %7.3f\n",fnum); 
		write(f3,&fnum,4); 
		/* now output center y*/
		n = skipblanks(f1,ptr);
		if (n <= 0) error("missing data in %s", TNAME);
		if (isdigit(*ptr) <= 0 && *ptr != '.') 
			error("missing center %s",TNAME);
		n =  getinteger(f1,ptr,&num);
		fnum = (float) num;
		if (n <= 0) error("missing center in %s",TNAME);
		if (*ptr == '.')  {
			n = read(f1,ptr,1);
			if (n<= 0) error("missing center in %s",TNAME);
			n = getdecimal(f1,ptr,&dec);
		}
		if (n <= 0) error("missing center in %s",TNAME);
		fnum += dec;
		if(debug) fprintf(stdout,"centery is %7.3f\n",fnum); 
		write(f3,&fnum,4); 

		/* now output index */
		/*NOTE: INDEX MUST BE AN INTEGER IN TEXTFILE */
		/* otherwise the vertices will be messed up */
/*************************
		n = skipblanks(f1,ptr);
		if (n <= 0) error("missing data in %s", TNAME);
		if (isdigit(*ptr) <= 0) 
			error("missing index  in %s",TNAME);
		n = getinteger(f1,ptr,&num);
		if (n <= 0) error("missing data in %s",TNAME);
		if(debug) fprintf(stdout,"index is %d\n",num); 
		if (num != vertindex) {
         	  fprintf(stdout,"warning-- index into vert list suspicious");
		  fprintf(stdout," symbol number %d", i);
		}
**********************************/
		write(f3,&vertindex,4); 
		vertindex += 2*length; /*vertindex is twice number of pairs*/
		if (vertindex > nverts) 
			fprintf(stdout,"warning-- nverts is less than number of verts\n");

	    /* do vertices */
	    for (j=0; j<length; j++) {
		n = skipblanks(f1,ptr);
		if (n <= 0) error("missing data in %s", TNAME);
		if (isdigit(*ptr) <= 0 && *ptr != '.') 
			error("missing some vertices %s",TNAME);
		n = getinteger(f1,ptr,&num);
		fnum = (float) num;
		if (n <= 0) error("missing some vertices in %s",TNAME);
		if (*ptr == '.')  {
			n = read(f1,ptr,1);
			if (n<= 0) error("bad float format in verts in %s",TNAME);
			n = getdecimal(f1,ptr,&dec);
		} 
		if (n <= 0) error("missing some vertices in %s",TNAME);
		fnum += dec;
		write(f4,&fnum,4); 
	  	if(debug) fprintf(stdout,"vert1 = %7.3fvert1\t",fnum);
	  	n = skipblanks(f1,ptr); 
		if (n <= 0) error("missing data in %s", TNAME);
		if (isdigit(*ptr) <= 0 && *ptr != '.') 
			error("missing some vertices in %s",TNAME);
		n =  getinteger(f1,ptr,&num);
		fnum = (float) num;
		if (n <= 0) error("missing some vertices in %s",TNAME);
		if (*ptr == '.') { 
			n = read(f1,ptr,1);
			if (n<= 0) error("missing some verts in %s",TNAME);
			n = getdecimal(f1,ptr,&dec);
		}
		if (n <= 0) error("missing some vertices in %s",TNAME);
		fnum += dec;
		write(f4,&fnum,4); 
	        if(debug) fprintf(stdout,"vert2 = %7.3fvert2\n",fnum);
	  } /*for j*/
	} /*for i*/
		
	/* now write all the vertices from f4 to f3 */
	/*lseek(f4,0L,0); */ /*for some reason this lseek doesn't work*/
			     /*so close and reopen f4 instead*/
	close(f4);
	if ((f4 = open(TMPNAME,0)) == -1)
		error("can't open %s", TMPNAME);
	while ((n = read(f4,ptr,1)) > 0) {
		write(f3,ptr,1);
	}
	close(f1);
	close(f2);
	close(f3);
	close(f4);
	/* remove f4 */
	sprintf(line,"rm %s",TMPNAME);
	system(line); 
} /*main*/


error(s1,s2)
char *s1,*s2;
{
	fprintf(stderr,"%s %s",s1,s2);
	fprintf(stderr,"---exiting");
	fprintf(stderr,"\n");
	exit(1);
}
