#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

/* copyright (c) Robert W. Clayton
 *		 Seismological Laboratory
 *		 Caltech
 *		 Pasadena, CA 91125
 *
 * Getarg routines:
 *
 * Externally visable routines:
 *
 *		setarg(argc,argv)
 *		getarg(name,type,valptr)
 *		endarg()
 *
 * To get C-version:
 *		cc -c getarg.c
 *
 * To get F77-version:
 *		cp getarg.c fgetarg.c
 *		cc -c -DFORTRAN fgetarg.c
 *		rm fgetarg.c
 */
#include	<stdio.h>

#define MAXNAME		64	/* max length of name */
#define MAXVALUE	1024	/* max length of value */
#define MAXVECTOR	10	/* max # of elements for unspecified vectors */
#define GETARG_ERROR	102	/* exit status for getarg error */

#define INIT	 1	/* bits for FLAGS (ext_arg.argflags) */
#define END_PAR	 2

#define LISTINC		32	/* increment size for arglist */
#define BUFINC		1024	/* increment size for argbuf */

struct arglist		/* structure of list set up by setarg */
   {
	char *argname;
	char *argval;
	int hash;
   };
struct ext_arg		/* global variables for getarg */
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
   }	ext_arg;

/* abbreviations: */
#define AL 		struct arglist
#define PROGNAME	ext_arg.progname
#define FLAGS		ext_arg.argflags
#define ARGLIST		ext_arg.arglist
#define ARGHEAD		ext_arg.arghead
#define ARGBUF		ext_arg.argbuf
#define NLIST		ext_arg.nlist
#define NBUF		ext_arg.nbuf
#define LISTMAX		ext_arg.listmax
#define BUFMAX		ext_arg.bufmax

#ifdef FORTRAN
setarg_(list,subname,dum1,dum2)
int dum1, dum2;
#else
setarg(list,subname)
#endif
char *subname, *list;
   {
	register char *pl, *pn, *pv;
	char t1, t2, name[MAXNAME], value[MAXVALUE];

	PROGNAME= subname;
	FLAGS= INIT;

	ARGLIST= NULL;
	ARGBUF = NULL;
	NLIST= NBUF= LISTMAX= BUFMAX= 0;

	if(list == NULL) return NULL;
	pl= list;
	/* loop over entries on each line */
	while(*pl != '\0')
	   {
		while(*pl==' ' || *pl=='\t') pl++;
		if(*pl=='\0'|| *pl=='\n') continue;

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
		ga_add_entry(name,value);
	   }
   }

ga_add_entry(name,value)	/* add an entry to arglist, expanding memory */
register char *name, *value;	/* if necessary */
   {
	struct arglist *alptr;
	int len;
	register char *ptr;

	/*fprintf(stderr,"getarg: adding %s (%s)\n",name,value);*/
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
		ga_getarg_err("setarg","cannot allocate memory");

	/* add name */
	alptr= ARGLIST + NLIST;
	alptr->hash= ga_compute_hash(name);
	ptr= alptr->argname= ARGBUF + NBUF;
	do *ptr++ = *name; while(*name++);

	/* add value */
	NBUF += len;
	alptr->argval= ptr;
	do *ptr++ = *value; while(*value++);
	NLIST++;
   }

#ifdef FORTRAN
endarg_()
#else
endarg() /* free arglist & argbuf memory, & process STOP command */
#endif
   {
	if(ARGLIST != NULL) free(ARGLIST);
	if(ARGBUF  != NULL) free(ARGBUF);
	ARGBUF=  NULL;
	ARGLIST= NULL;
	FLAGS= END_PAR;	/* this stops further getarg calls */
   }

#ifdef FORTRAN
getarg_(name,type,val,dum1,dum2)
int dum1, dum2;	/* dum1 & dum2 are extra args that fortran puts in */
#else
getarg(name,type,val)
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
	char noname[MAXNAME+2], *str;

	/*fprintf(stderr,"looking for %s, type=%s\n",name,type);*/
	if(FLAGS & END_PAR)
		ga_getarg_err("getarg","called after endarg");
	if( (FLAGS & INIT) == 0)
		ga_getarg_err("getarg","not initialized with setarg");

	if(NLIST == 0 || ARGLIST == NULL) return(0);
	/* The following line corrects a common input error */
	if(type[1]=='v') { type[1]= type[0]; type[0]='v'; }


	if(*type == 'b') goto boolean;

	h= ga_compute_hash(name);
	found=0;
	/* search list backwards, stopping at first find */
	for(alptr= ARGLIST +(NLIST-1); alptr >= ARGLIST; alptr--)
	   {
		/*fprintf(stderr,"getarg: checking %s\n",alptr->argname);*/
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
				found= ga_getvector(str,type,val);
				break;
			default:
				ga_getarg_err("getarg",
					"unknown conversion type %s",type);
				break;
		   }
		break;
	   }
	return(found);
boolean:
	sprintf(noname,"no%s",name);
	hno = ga_compute_hash(noname);
	hyes= ga_compute_hash(  name);
	found=0;
	/* search list backwards, stopping at first find */
	for(alptr= ARGLIST +(NLIST-1); alptr >= ARGLIST; alptr--)
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
	return(found);
   }

ga_compute_hash(s)
register char *s;
   {
	register int h;
	h= s[0];
	if(s[1]) h |= (s[1])<<8;	else return(h);
	if(s[2]) h |= (s[2])<<16;	else return(h);
	if(s[3]) h |= (s[3])<<24;
	return(h);
   }

ga_getvector(list,type,val)
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
		ga_getarg_err("getarg","bad limit=%d specified",limit);
	/*fprintf(stderr,"limit=%d\n",limit);*/
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
				ga_getarg_err("getarg",
					"bad repetition factor=%d specified",
					 cnt);
			if(index+cnt > limit) cnt= limit - index;
			p++;
			goto backup;
		   }
		/*fprintf(stderr,"index=%d cnt=%d p=%s$\n",index,cnt,p);*/
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
				ga_getarg_err("getarg",
					"bad vector type=%c specified",type[1]);
				break;
		   }
		if(*p != '\0') p++;
	   }
	return(index);
   }
ga_getarg_err(subname,mess,a1,a2,a3,a4)
char *subname, *mess;
int a1, a2, a3, a4;
   {
	fprintf(stderr,"\n***** ERROR in %s[%s] *****\n\t",
		(PROGNAME == NULL ? "(unknown)" : PROGNAME),subname);
	fprintf(stderr,mess,a1,a2,a3,a4);
	fprintf(stderr,"\n");
	exit(GETARG_ERROR);
   }
