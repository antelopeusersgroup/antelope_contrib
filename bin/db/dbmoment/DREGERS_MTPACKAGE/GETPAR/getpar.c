/* copyright (c) Robert W. Clayton
 *		 Seismological Laboratory
 *		 Caltech
 *		 Pasadena, CA 91125
 *
 * Getpar routines:
 *
 * Externally visable routines:
 *
 *		setpar(argc,argv)
 *		getpar(name,type,valptr)
 *		mstpar(name,type,valptr)
 *		endpar()
 *
 * To get C-version:
 *		cc -c getpar.c
 *
 * To get F77-version:
 *		cp getpar.c fgetpar.c
 *		cc -c -DFORTRAN fgetpar.c
 *		rm fgetpar.c
 *
 * To get the environment processing stuff add the flag
 *-DENVIRONMENT to each of the cc's above.
 */
#include	<stdio.h>
#include    <string.h>
#include    <stdlib.h> 

#define MAXLINE		1024	/* max length of line in par file */
#define MAXNAME		64	/* max length of name */
#define MAXVALUE	1024	/* max length of value */
#define MAXFILENAME	64	/* max length of par file name */
#define MAXVECTOR	10	/* max # of elements for unspecified vectors */
#define GETPAR_ERROR	100	/* exit status for getpar error */
#define GETPAR_STOP	101	/* exit status for STOP or mstpar */
#define MAXPARLEVEL	4	/* max recurrsion level for par files */

#ifdef FORTRAN
#define GETPAR	getpar_
#define MSTPAR	mstpar_
#define ENDPAR	endpar_
#else
#define GETPAR	getpar
#define MSTPAR	mstpar
#define ENDPAR	endpar
#endif

#define INIT	 1	/* bits for FLAGS (ext_par.argflags) */
#define STOP	 2
#define LIST	 4
#define END_PAR	 8
#define VERBOSE	16

#define LISTINC		32	/* increment size for arglist */
#define BUFINC		1024	/* increment size for argbuf */

struct arglist		/* structure of list set up by setpar */
   {
	char *argname;
	char *argval;
	int hash;
   };
struct ext_par		/* global variables for getpar */
   {
	char *progname;
	int argflags;
	struct arglist *arglist;
	struct arglist *arghead;
	char *argbuf;
	int nlist;
	int nbuf;
	int listmax;
	int bufmax;
	FILE *listout;
   }	ext_par;

/* abbreviations: */
#define AL 		struct arglist
#define PROGNAME	ext_par.progname
#define FLAGS		ext_par.argflags
#define ARGLIST		ext_par.arglist
#define ARGHEAD		ext_par.arghead
#define ARGBUF		ext_par.argbuf
#define NLIST		ext_par.nlist
#define NBUF		ext_par.nbuf
#define LISTMAX		ext_par.listmax
#define BUFMAX		ext_par.bufmax
#define LISTFILE	ext_par.listout

#ifdef FORTRAN
setpar_()
#else
setpar(ac,av)		/* set up arglist & process INPUT command */
int ac; char **av;
#endif
   {
	register char *pl, *pn, *pv;
	char  t, name[MAXNAME], value[MAXVALUE];
	FILE *file, *gp_create_dump();
	int i, addflags, nevlist, testav, testae;
	struct arglist *alptr;
#ifdef FORTRAN
	int ac; char **av;
	extern int xargc; extern char **xargv;
	ac= xargc; av= xargv;
#endif

	PROGNAME= *av;
	FLAGS= INIT;
	LISTFILE= stderr;

	ARGLIST= NULL;
	ARGBUF = NULL;
	NLIST= NBUF= LISTMAX= BUFMAX= 0;
#ifdef ENVIRONMENT
	gp_do_environment(ac,av);
#endif
	nevlist= NLIST;
	while(--ac>0)
	   {
		av++;
		pl= *av;
		while(*pl == ' ' || *pl == '\t') pl++;
		/* get name */
		pn= name;
		while(*pl != '=' && *pl != '\0') *pn++ = *pl++;
		*pn++ = '\0';
		/* get value */
		if(*pl == '=') pl++;
		pv= value;
		if(*pl == '"' || *pl == '\'')
		   {
			t= *pl++;
			while(*pl != '\0')
			   {
				if(*pl == t)
				   {
					if(pl[-1] != '\\') break;
					pv[-1]= t;
					pl++;
				   }
				 else	*pv++ = *pl++;
			   }
		   }
		 else	while(*pl) *pv++ = *pl++;
		*pv= '\0';
		if(name[0] == '-') gp_add_entry("SWITCH",&name[1]);
		 else		gp_add_entry(name,value);
		if(strcmp("par",name)==0) /* par file */
			gp_do_par_file(value,1);
	   }
	/* do not internally call getpar before this point because
	   ARGHEAD is not set. The search will have no stopping point */
	ARGHEAD= ARGLIST;
#ifdef ENVIRONMENT
	*value= '\0';
	if(GETPAR("NOENV","s",value)) ARGHEAD= ARGLIST+ nevlist;
#endif
	addflags= 0;
	*value= '\0';
	if(GETPAR("STOP","s",value)) addflags |= STOP;
	*value= '\0';
	if(GETPAR("VERBOSE","s",value)) addflags |= VERBOSE;
	*value= '\0';
	if(GETPAR("LIST","s",value))
	   {
		addflags |= LIST;
		LISTFILE =gp_create_dump(value,"list");
	   }
	*value= '\0';
	if(GETPAR("INPUT","s",value))
	   {
		file =gp_create_dump(value,"list input");
		fprintf(file,"%s: getpar input listing\n",PROGNAME);
		for(i=0, alptr=ARGLIST; i<NLIST; i++, alptr++)
		   {
			fprintf(file,"%3d: %16s = %s\n",
				i,alptr->argname,alptr->argval);
		   }
		gp_close_dump(file);
	   }
	FLAGS |= addflags;
   }

gp_add_entry(name,value)	/* add an entry to arglist, expanding memory */
register char *name, *value;	/* if necessary */
   {
	struct arglist *alptr;
	int len;
	register char *ptr;

	/* check arglist memory */
	if(NLIST >= LISTMAX)
	   {
		LISTMAX += LISTINC;
		if(ARGLIST == NULL)
			ARGLIST= (AL *)malloc(LISTMAX * sizeof(AL));
		 else	ARGLIST= (AL *)realloc(ARGLIST,LISTMAX * sizeof(AL));
	   }
	/* check argbuf memory */
	len= strlen(name) + strlen(value) + 2; /* +2 for terminating nulls */
	if(NBUF+len >= BUFMAX)
	   {
		BUFMAX += BUFINC;
		if(ARGBUF == NULL)
			ARGBUF= (char *)malloc(BUFMAX);
		 else	ARGBUF= (char *)realloc(ARGBUF,BUFMAX);
	   }
	if(ARGBUF == NULL || ARGLIST == NULL)
		gp_getpar_err("setpar","cannot allocate memory");

	/* add name */
	alptr= ARGLIST + NLIST;
	alptr->hash= gp_compute_hash(name);
	ptr= alptr->argname= ARGBUF + NBUF;
	do *ptr++ = *name; while(*name++);

	/* add value */
	NBUF += len;
	alptr->argval= ptr;
	do *ptr++ = *value; while(*value++);
	NLIST++;
   }
#ifdef ENVIRONMENT
gp_do_environment(ac,av)
int ac; char **av;
   {
	char **ae;
    int c; // Added by Juan Reyes
	register char *pl, *pn, *pv;
	char name[MAXNAME], value[MAXVALUE], t;

	/* The environ pointer ae, is assumed to have a specific relation
	   to the arg pointer av. This may not be portable. */
	ae= av +(ac+1);
    c = strlen(ae); // Added by Juan Reyes
	if(ae == NULL) return NULL;

	//while(*ae != NULL)
	while( c > -1 )  // Added by Juan Reyes
	   {
        c--;    // Added by Juan Reyes
		pl= *ae++;
		while(*pl == ' ' || *pl == '\t') pl++;
		/* get name */
		pn= name;
		while(*pl != '=' && *pl != '\0') *pn++ = *pl++;
		*pn = '\0';
		if(strcmp("NOENV",pn) == 0) return NULL;

		/* get value */
		if(*pl == '=') pl++;
		pv= value;
		if(*pl == '"' || *pl == '\'')
		   {
			t= *pl++;
			while(*pl != '\0')
			   {
				if(*pl == t)
				   {
					if(pl[-1] != '\\') break;
					pv[-1]= t;
					pl++;
				   }
				 else	*pv++ = *pl++;
			   }
		   }
		 else	while(*pl) *pv++ = *pl++;
		*pv= '\0';
		gp_add_entry(name,value);
	   }
   }
#endif

ENDPAR()	/* free arglist & argbuf memory, & process STOP command */
   {
	if(ARGLIST != NULL) free(ARGLIST);
	if(ARGBUF  != NULL) free(ARGBUF);
	ARGBUF=  NULL;
	ARGLIST= NULL;
	if(FLAGS & STOP)
	   {
		fprintf(stderr,"%s[endpar]: stop due to STOP in input\n",
			PROGNAME);
		exit(GETPAR_STOP);
	   }
	FLAGS= END_PAR;	/* this stops further getpar calls */
   }

#ifdef FORTRAN
mstpar_(name,type,val,dum1,dum2)
int dum1, dum2;	/* dum1 & dum2 are extra args that fortran puts in */
#else
mstpar(name,type,val)
#endif
char *name, *type;
int *val;
   {
	int cnt;
	char *typemess;

	if( (cnt= GETPAR(name,type,val)) > 0) return(cnt);

	/* The following line corrects a common input error */
	if(type[1]=='v') { type[1]= type[0]; type[0]='v'; }

	switch(*type)
	   {
		case 'd': typemess= "an integer";	break;
		case 'f': typemess= "a float";		break;
		case 'F': typemess= "a double";		break;
		case 's': typemess= "a string";		break;
		case 'b': typemess= "a boolean";	break;
		case 'v': switch(type[1])
			   {
				case 'd': typemess= "an integer vector"; break;
				case 'f': typemess= "a float vector"; 	 break;
				case 'F': typemess= "a double vector";	 break;
				default : typemess= "unknow vectorn (error)";
					break;
			   }
			  break;
		default : typemess= "unknown (error)";	break;
	   }
	gp_getpar_err("mstpar","must specify value for '%s', expecting %s",
		name,typemess);
   }

#ifdef FORTRAN
getpar_(name,type,val,dum1,dum2)
int dum1, dum2;	/* dum1 & dum2 are extra args that fortran puts in */
#else
getpar(name,type,val)
#endif
char *name, *type;
int *val;
   {
	register char *sptr;
	register struct arglist *alptr;
	register int i;
	double atof(), *dbl;
	float *flt;
	int h, hno, hyes, found;
	char line[MAXLINE], *str, *noname;

	if(FLAGS & END_PAR)
		gp_getpar_err("getpar","called after endpar");
	if( (FLAGS & INIT) == 0)
		gp_getpar_err("getpar","not initialized with setpar");
	if(FLAGS & VERBOSE)
		fprintf(stderr,"getpar: looking for %s\n",name);

	/* The following line corrects a common input error */
	if(type[1]=='v') { type[1]= type[0]; type[0]='v'; }


	if(*type == 'b') goto boolean;

	h= gp_compute_hash(name);
	found=0;
	/* search list backwards, stopping at first find */
	for(alptr= ARGLIST +(NLIST-1); alptr >= ARGHEAD; alptr--)
	   {
		if(alptr->hash != h) continue;
		if(strcmp(alptr->argname,name) != 0) continue;
		str= alptr->argval;
		switch(*type)
		   {
			case 'd':
				*val= atoi(str);
				found=1;
				break;
			case 'f':
				flt= (float *) val;
				*flt= atof(str);
				found=1;
				break;
			case 'F':
				dbl= (double *) val;
				*dbl= atof(str);
				found=1;
				break;
			case 's':
				sptr= (char *) val;
				while(*str) *sptr++ = *str++;
				*sptr= '\0';
				found=1;
				break;
			case 'v':
				found= gp_getvector(str,type,val);
				break;
			default:
				gp_getpar_err("getpar",
					"unknown conversion type %s",type);
				break;
		   }
		break;
	   }
	goto list;
boolean:
	noname= line;
	sprintf(noname,"no%s",name);
	hno = gp_compute_hash(noname);
	hyes= gp_compute_hash(  name);
	found=0;
	/* search list backwards, stopping at first find */
	for(alptr= ARGLIST +(NLIST-1); alptr >= ARGHEAD; alptr--)
	   {
		if(alptr->hash != hno && alptr->hash != hyes) continue;
		if(strcmp(alptr->argname,  name)== 0)
		   {
			if(alptr->argval[0] == '\0') *val= 1;
			 else *val= atol(alptr->argval);
			found++;
			break;
		   }
		if(strcmp(alptr->argname,noname)== 0)
		   {	*val= 0; found++; break; }
	   }
   list:
	if(FLAGS & LIST)
	   {
		switch(*type)
		   {
			case 'd': sprintf(line,"(int) = %d",*val); break;
			case 'f': flt= (float *)val;
				  sprintf(line,"(flt) = %14.6e",*flt); break;
			case 'F': dbl= (double *)val;
				  sprintf(line,"(dbl) = %14.6e",*dbl); break;
			case 's': sprintf(line,"(str) = %s",val); break;
			case 'b': sprintf(line,"(boo) = %d",*val); break;
			case 'v': switch(type[1])
				   {
					/* should list these out */
					case 'd': sprintf(line,"(int vec)");
						break;
					case 'f': sprintf(line,"(flt vec)");
						break;
					case 'F': sprintf(line,"(dbl vec)");
						break;
					default : sprintf(line," vec type error");
						break;
				   }
				  break;
			default : sprintf(line," type error"); break;
		   }
		fprintf(LISTFILE,"%16s (%s) %s \n",name,
			(found ? "set":"def"),line);
	   }
	return(found);
   }
FILE *gp_create_dump(fname,filetype)
char *fname;
char *filetype;
   {
	FILE *temp;

	if(*fname == '\0') return(stderr);
	if(strcmp(fname,"stderr") == 0) return(stderr);
	if(strcmp(fname,"stdout") == 0) return(stdout);
	if( (temp= fopen(fname,"w")) != NULL) return(temp);
	fprintf(stderr,"%s[setpar]: cannot create %s file %s\n",
		PROGNAME,filetype,fname);
	return(stderr);
   }

gp_close_dump(file)
FILE *file;
   {
	if(file == stderr || file == stdout) return NULL;
	fclose(file);
   }

gp_compute_hash(s)
register char *s;
   {
	register int h;
	h= s[0];
	if(s[1]) h |= (s[1])<<8;	else return(h);
	if(s[2]) h |= (s[2])<<16;	else return(h);
	if(s[3]) h |= (s[3])<<24;
	return(h);
   }

gp_do_par_file(fname,level)
char *fname;
int level;
   {
	register char *pl, *pn, *pv;
	char t1, t2, line[MAXLINE], name[MAXNAME], value[MAXVALUE];
	FILE *file, *fopen();

	if(level > MAXPARLEVEL)
		gp_getpar_err("setpar","%d (too many) recursive par file",level);
		
	if( (file=fopen(fname,"r"))==NULL)
		gp_getpar_err("setpar","cannot open par file %s",fname);

	while( fgets(line,MAXLINE,file) != NULL )
	   {
		pl= line;
		/* loop over entries on each line */
	loop:	while(*pl==' ' || *pl=='\t') pl++;
		if(*pl=='\0'|| *pl=='\n') continue;
		if(*pl=='#') continue; /* comments on rest of line */

		/* get name */
		pn= name;
		while(*pl != '=' && *pl != '\0' && *pl != ' '
			&& *pl != '\t') *pn++ = *pl++;
		*pn = '\0';
		if(*pl == '=') pl++;

		/* get value */
		*value= '\0';
		pv= value;
		if(*pl=='"' || *pl=='\'')	{ t1= t2= *pl++; }
		 else				{ t1= ' '; t2= '\t'; }
		while(*pl!=t1 && *pl!=t2 &&
			*pl!='\0' && *pl!='\n') *pv++= *pl++;
		*pv= '\0';
		if(*pl=='"' || *pl=='\'') pl++;
		gp_add_entry(name,value);
		if(strcmp("par",name) == 0)
			gp_do_par_file(value,level+1);
		goto loop;
	   }
	fclose(file);
   }

gp_getpar_err(subname,mess,a1,a2,a3,a4)
char *subname, *mess;
int a1, a2, a3, a4;
   {
	fprintf(stderr,"\n***** ERROR in %s[%s] *****\n\t",
		(PROGNAME == NULL ? "(unknown)" : PROGNAME),subname);
	fprintf(stderr,mess,a1,a2,a3,a4);
	fprintf(stderr,"\n");
	exit(GETPAR_ERROR);
   }
gp_getvector(list,type,val)
char *list, *type;
int *val;
   {
	register char *p;
	register int index, cnt;
	char *valptr;
	int limit;
	int ival, *iptr;
	float fval, *fptr;
	double dval, *dptr, atof();

	limit= MAXVECTOR;
	if(type[2] == '(' || type[2] == '[') limit= atol(&type[3]);
	if(limit <= 0)
		gp_getpar_err("getpar","bad limit=%d specified",limit);
	index= 0;
	p= list;
	while(*p != '\0'  && index < limit)
	   {
		cnt=1;
	 backup: /* return to here if we find a repetition factor */
		while(*p == ' ' || *p == '\t') p++;
		if(*p == '\0') return(index);
		valptr= p;
		while( *p != ',' && *p != '*' && *p != 'x' && *p != 'X' &&
			*p != '\0') p++;
		if(*p == '*' || *p == 'x' || *p == 'X')
		   {
			cnt= atol(valptr);
			if(cnt <= 0)
				gp_getpar_err("getpar",
					"bad repetition factor=%d specified",
					 cnt);
			if(index+cnt > limit) cnt= limit - index;
			p++;
			goto backup;
		   }
		switch(type[1])
		   {
			case 'd':
				iptr= (int *) val;
				ival= atol(valptr);
				while(cnt--) iptr[index++] = ival;
				break;
			case 'f':
				fptr= (float *) val;
				fval= atof(valptr);
				while(cnt--) fptr[index++] = fval;
				break;
			case 'F':
				dptr= (double *) val;
				dval= atof(valptr);
				while(cnt--) dptr[index++] = dval;
				break;
			default:
				gp_getpar_err("getpar",
					"bad vector type=%c specified",type[1]);
				break;
		   }
		if(*p != '\0') p++;
	   }
	return(index);
   }
