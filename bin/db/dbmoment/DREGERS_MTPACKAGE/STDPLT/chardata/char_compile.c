#include	<stdio.h>
#define MAX	  100
#define MAXDATA	100000
#define COMMENT	'#'
#define LINEMAX 256

#define forever for(;;)

#define uchar	unsigned char
#define ushort	unsigned short

struct charglyph {
	uchar	npoints;	/* # of points in character */
	uchar	hup;		/* largest vertical motion up */
	uchar	hdn;		/* largest vertical motion down */
	uchar	width;		/* width of character */
	ushort	index;		/* offset of character in stroke data */
};

struct stroke {
	uchar	x;
	char	y;
};

struct charglyph cg[96];

ushort sdata[MAXDATA];
struct stroke *data;

char cg_name[]="00_cg", data_name[]="00_data";

int	nchar	=0;
int	ndata	=0;
int	linenum	=0;
char line[LINEMAX], *args[LINEMAX];

char u_case[]="ABCDEFGHIJKLMNOPQRSTUVWXYZ";
char l_case[]="abcdefghijklmnopqrstuvwxyz";

void document(), zap(), err(), printstruct();

main(ac,av)
int ac; char **av;
{
	int i, n, nargs, np;
	int x, y, width, flag, ichar;
	char dochar, ch0, ch1;
	FILE *fd, *fopen();
	int hup, hdn;

	if (ac < 2) document(av[0]);      /* requires one argument */
	ch0 = av[1][0];
	if (ch0 != 'R' && ch0 != 'I' && ch0 != 'B' && ch0 != 'S') {
		if (ch0 != 'r' && ch0 != 'i' && ch0 != 'b' && ch0 != 's')
			document(av[0]);
		else ch0 = u_case[ch0 - 'a'];
	}
	ch1 = av[1][1];
	if (ch1 != 's' && ch1 != 'c' && ch1 != 'x') {
		if (ch1 != 'S' && ch1 != 'C' && ch1 != 'X')
			document(av[0]);
		else ch1 = l_case[ch1 - 'A'];
	}

	data = (struct stroke *) &sdata[0];
	linenum = 0;

	/* get an input line to start */
	n = getline(stdin,line);
	linenum++;

	zap(cg,96*sizeof(struct charglyph));

	while (n != EOF) {

		/* blank line or comment line */
		if (n == 0 || line[0] == COMMENT) {
			n = getline(stdin,line);
			linenum++;
			continue;
		}

		/* at this point, must start data entry with + line */
		/* format is '+' followed by at least two fields:
		   first, the character (ascii) being described and then,
		   the width of the character, other fields are comments */
		if (line[0] != '+') err("expecting a '+' field");

		/* parse input line, nargs is the number of fields */
		nargs = setpointers(&line[1],args);
		if (nargs < 2) err("expecting 2 arguments in '+' field");

		/* get char being described, and its width */
		dochar = args[0][0];
		/* ' ' is 040 (32), printing chars go to 0176 (126) */
		ichar = dochar - ' ';
		if (ichar <= 0 || ichar >= 95) err("illegal ichar=%d",ichar);
		width = atoi(args[1]);
		cg[ichar].width = width;

		/* set offset in strokes data to next stroke entry */
		cg[ichar].index = ndata;

		hup = hdn = 0;
		np = 0;
		forever {

			/* get a line of strokes */
			n = getline(stdin,line);
			linenum++;

			/* EOF, let while loop handle it */
			if (n == EOF) break;

			/* blank line or comment line */
			if (n == 0 || line[0] == COMMENT) {
				n = getline(stdin,line);
				linenum++;
				continue;
			}

			/* start of next data entry */
			if (line[0] == '+') break;

			/* parse input line, nargs is the number of fields */
			nargs = setpointers(line,args);

			/* stroke descriptions are m or d (move or draw)
			   followed by 2 integers for x - y coordinates */
			if (nargs % 3) err("bad strokes description");

			for (i=0; i<nargs; i += 3) {
				switch(args[i][0]) {
				   case 'm': flag = 1; break;
				   case 'd': flag = 0; break;
				   default:
				      err("specify m or d, not %c",args[i][0]);
				}
				/* NOTE: x may be < 0 */
				x = (atoi(args[i+1]) << 1);
				y = atoi(args[i+2]);
				if (y > hup) hup = y;
				if (y < hdn) hdn = y;
				data[ndata].x = (x | flag) & 0xff;
				data[ndata].y = y;
				ndata++;
				np++;
			}
			if (np == 0) err("no points specified");
		}
		cg[ichar].npoints = np;
		cg[ichar].hup = hup;
		cg[ichar].hdn = -hdn;
		nchar++;
	}

	fprintf(stderr,"%d out of a possible 94 characters have been described\n",nchar);

	/* open file to write include file */
	fd = stdout; /* stdout for now */

	/* define structures, ch0=R ch1=s is flag (so this is only done once
	   for a file containing multiple fonts */
	if (ch0 == 'R' && ch1 == 's') {
		fprintf(fd,"struct charglyph {\n");
		fprintf(fd,"\tshort\tnpoints;\n");
		fprintf(fd,"\tshort\thshift;\n");
		fprintf(fd,"\tshort\tvshift;\n");
		fprintf(fd,"\tshort\twidth;\n");
		fprintf(fd,"\tint\tindex;\n");
		fprintf(fd,"};\n");
		fprintf(fd,"\nstruct IglFont {\n");
		fprintf(fd,"\tstruct charglyph *cg;\n");
		fprintf(fd,"\tshort *data;\n");
		fprintf(fd,"} FontList[12];\n");
	}

	/* write static data */
	printstruct(fd,cg,data,ndata,ch0,ch1,nchar);
}

void
printstruct(fd,cg,data,ndata,ch0,ch1,nc)
FILE *fd;
struct charglyph *cg;
ushort *data;
int ndata, nc;
char ch0, ch1;
{
	int i, j;

	cg_name[0] = data_name[0] = ch0;
	cg_name[1] = data_name[1] = ch1;
	if (nc) {
		fprintf(fd,"\nstruct charglyph %s[] = {\n",cg_name);
		for (i = 0; i < 94; i += 2) {
			fprintf(fd," %3d, %2d, %2d, %2d, 0x%04x,     ",
				cg[i].npoints,
				cg[i].hup,
				cg[i].hdn,
				cg[i].width,
				cg[i].index);
			fprintf(fd," %3d, %2d, %2d, %2d, 0x%04x,\n",
				cg[i+1].npoints,
				cg[i+1].hup,
				cg[i+1].hdn,
				cg[i+1].width,
				cg[i+1].index);
		}
		fprintf(fd," %3d, %2d, %2d, %2d, 0x%04x,     ",
			cg[94].npoints,
			cg[94].hup,
			cg[94].hdn,
			cg[94].width,
			cg[94].index);
		fprintf(fd," %3d, %2d, %2d, %2d, 0x%04x\n",
			cg[95].npoints,
			cg[95].hup,
			cg[95].hdn,
			cg[95].width,
			cg[95].index);
		fprintf(fd,"};\n\n");
		fprintf(fd,"short %s[] = {\n",data_name);
		for (i = 0; i < ndata / 10; i++) {
			for (j=0; j<10; j++) fprintf(fd," 0x%04x,",*data++);
			fprintf(fd,"\n");
		}
		if (ndata % 10) {
			for (j=0; j<(ndata%10-1); j++)
				fprintf(fd," 0x%04x,",*data++);
			fprintf(fd," 0x%04x\n};\n",*data++);
		}
		else fprintf(fd,"};\n");
	}
	else {
		fprintf(fd,"struct charglyph **%s = NULL;\n",cg_name);
		fprintf(fd,"short *%s = NULL;\n",data_name);
	}
}

getline(fd,line)
FILE *fd;
char *line;
{
	int n;

	if (fgets(line,LINEMAX,fd) == NULL) return(EOF);
	for (n = 0; *line != '\n'; line++) n++;
	*line = '\0';
	return (n);
}

void
err(mess,a1,a2,a3,a4,a5)
char *mess;
int a1,a2,a3,a4,a5;
{
	fprintf(stderr,"error near line %d\n",linenum);
	fprintf(stderr,mess,a1,a2,a3,a4,a5);
	fprintf(stderr,"\n");
	exit(-1);
}

setpointers(list, ptrs)
char *list, *ptrs[];
{
	int n;

	n=0;
	while ( *list != '\0' ) {
		while (*list == ' ' || *list == '\t') list++;
		if (*list == '\0') break;
		ptrs[n] = list;
		n++;
		while (*list != ' ' && *list != '\t' && *list != '\0') list++;
		if (*list == '\0') break;
		*list++ = '\0';
	}
	return(n);
}

void
zap(c,n)
char *c;
int n;
{
	while (n--) *c++ = 0;
}

void
document(name)
char *name;
{
	fprintf(stderr,"%s: compiles character description files into C-language headers\n",name);
	fprintf(stderr,"    for static initialization of vector character information\n");
	fprintf(stderr,"Usage:  %s [R,I,B,S][s,c,x] < infile >> chardata.h\n",name);
	fprintf(stderr,"  where the single argument must have two characters\n");
	fprintf(stderr,"  the first is the font-designator, Roman Italic Bold Symbols\n");
	fprintf(stderr,"  the second is the complexity, simple complex extra\n");
	fprintf(stderr,"  (%s Rs  also writes structure declaration)\n",name);
	exit(1);
}
