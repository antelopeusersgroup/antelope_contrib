/* This library is designed to parse a pf stream for use mainly in data-driven, parallel
processing algorithms.  A pf stream is a stream of parameter file type blocks of 
metadata separated by a keyword defined in pfstream.h.  That is, each block of
ascii characters between the keyword sentinel is assumed to consist of keywords and
numbers as used in antelope parameter files.  

The format has two modes.  A single object mode will call pfcompile on each block of
text bounded by the input separator keyword.  In "ensemble" mode, the block of text 
is assumed to be in this structure:
ensemble &Arr{
---- constant parameters for this ensemble----
000001 &Arr {
--parameter for object 1----
}
000002 &Arr {
--parameters for object 2---
}
repeat for n objects

}

This file contains basic input and output functions for a pfstream.

Author:  GAry L. Pavlis
Written:  September 2002
*/
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include "stock.h"
#include "coords.h"
#include "pf.h"
#include "elog.h"
#include "brttutil.h"
#include "pfstream.h"

/* This is a low level read routine that sucks in data from file fname 
until the  end of block line (symbol ENDPF_SENTINEL defined in pfstream.h)
is read.  It then closes the file to allow another process to access the
file under an assuption that fname is a fifo.  Finally, 
pfcompile is then called and the resulting pfobject is returned.

The algorithm is more complicated than one might think it needs to be because
the size of the input block of text is not predictable.  With the structure
of parameter file descriptions it is necessary to suck in the whole file
before trying to interpret it.  As a result the algorithm uses an initial
buffer size which grows as needed whenever the previous read gets close to 
a high water mark.  There may be a more elegant way to do this with some
alternative function, but I don't know about it.  

RETURNS:
normal return is a valid pointer to a pf.  A NULL pointer indicates 
a failure in pfcompile.  The function will die if memory allocation fails.  

Author:  Gary L. Pavlis
Written:  September 2002
*/
#define BUFSIZE0 10000
#define DELTA_BUF 5000  /* realloc when this close to end of buffer*/
#define MAXLINE 512   
/* low level read from fp using stdio routine fgets.   Reads a pfstream
one line at a time pushing data into a large string buffer.  Then call
pfcompile to encapsulate the entire input into a single pf */
Pf *pfstream_read(FILE *fp)
{
	char *buffer;
	char line[MAXLINE];
	int current_buffer_size=BUFSIZE0;
	int fd;
	int space_in_buffer=BUFSIZE0;
	int high_water_mark=0;
	Pf *pf=NULL;
	int linecount=0,ncread,ierr;

	buffer=(char *)malloc(BUFSIZE0);
	if(buffer==NULL) elog_die(0,"pfstream_read:  cannot malloc input buffer\n");
	buffer[0]='\0';

	while(fgets(line,MAXLINE,fp)!=NULL)
	{
		++linecount;
		if(strstr(line,ENDPF_SENTINEL)!=NULL )
		{
			break;
		}
		if(strstr(line,END_OF_DATA_SENTINEL)!=NULL) return(NULL);

		strcat(buffer,line);
		high_water_mark=strlen(buffer);
		if(high_water_mark>(current_buffer_size-MAXLINE))
		{
			/* This assumes realloc does not destroy the 
			current contents that includes the data already read */
			current_buffer_size+=DELTA_BUF;
			buffer=(char *)realloc(buffer,current_buffer_size);
			if(buffer==NULL)
			  elog_die(0,"pfstream_read:  realloc of %d byte buffer failed\n",
							current_buffer_size);
		}
	}
	if(linecount<=0) return(NULL);
	ierr=pfcompile(buffer,&pf);
	if(ierr!=0) 
	{
		elog_complain(1,"pfstream_read:  pfcompile failed\n");
		pf=NULL;
	}
	free(buffer);
	return(pf);
}
/*  The next block of stuff are Pf_ensemble routines */


/* This is a C constructor for a Pf_ensemble object with hanging 
pointers.  It is called by the ensemble input routine below */

Pf_ensemble *create_Pf_ensemble(int nmembers,int ngroups)
{
	Pf_ensemble *pfe;
	int i;

	allot(Pf_ensemble *,pfe,1);
	/* I intentionally set these null to start */
	pfe->ensemble_keys=NULL;
	pfe->group_keys=NULL;
	if(nmembers>0)
	{
		allot(Pf **,pfe->pf,nmembers);
		pfe->nmembers=nmembers;
		/* explicit initialization a good idea here */
		for(i=0;i<nmembers;++i) pfe->pf[i]=NULL;
	}
	else
		pfe->nmembers=0;
	if(ngroups>0)
	{
		allot(int *,pfe->group_start,ngroups);
		allot(int *,pfe->group_end,ngroups);
		pfe->ngroups=ngroups;
	}
	else
	{
		pfe->group_start=NULL;
		pfe->group_end=NULL;
		pfe->ngroups=0;
	}
	return(pfe);
}
/* destructor */
void free_Pf_ensemble(Pf_ensemble *pfe)
{
	int i;
	if(pfe->nmembers > 0)
	{
		for(i=0;i<(pfe->nmembers);++i)
			if(pfe->pf[i] != NULL) pffree(pfe->pf[i]);
		if(pfe->pf != NULL) free(pfe->pf);
	}
	if(pfe->ngroups>0)
	{
		if(pfe->group_start != NULL) free(pfe->group_start);
		if(pfe->group_end != NULL) free(pfe->group_end);
	}
	/* This is dangerous because reading these from a pf file
	normally does not duplicate the tbl entries read from
	the parent file.  This assumes these tbl's were created
	by a copy operation like that below */
	if(pfe->ensemble_keys!=NULL) freetbl(pfe->ensemble_keys,free);
	if(pfe->group_keys!=NULL) freetbl(pfe->group_keys,free);
	free(pfe);
}

		
/* This routine takes apart an ensemble pf description and returns a vector of
pf objects for the components of the ensemble.  (see above).  It should always
be called after a read function (pfstream_read above, pfread, or extracting pf's
from an orb) to take the parameter ensemble apart.  

The algorithm has some built in assumptions that the pieces of the ensemble
are tagged by 6 digit numbers that start at 000000.  6 digits are surely 
overkill, but given the way memory is going it may not be so silly.  The are
taken apart in the order Arr returns them in to build Tbl containing pointers
to pf objects of the components of the ensemble.  That is, the ith pf object
from this ensemble can be extracted from the output tbl, t, with a call like this:
pf=(Pf *)gettbl(t,i)

This structure, although complex, was done by design as it allows a complete
ensemble to be encapsulated in a single pf object.  This simplifies and 
abstracts the process of reading from a fifo or from an orb.  i.e. calls
to functions that do quite different things can yield the same result.
This could be overloaded in C++ if I wanted to go that route. 

Multiple ensembles can be enscapsulated in one block of data passed
through a pfstream.  Each ensemble (table) is preceded by a keyword
passed as the argument "tag".  This allows multiple tables to be 
passed down the same input stream which could, in principle, be
reconstituted into a relational form with something like datascope
using internal, temporary databases.  This is not implemented here though.

Note that the input Pf object, pf, is only accessed to take apart the 
components of the "ensemble &Arr{...}" component of Pf.  If there are other parameters
in pf outside the range of this object the caller needs to handle these
separately.  Further note that if multiple tables are embeddged in on
pfstream this function has to be called separately for each table
defined by "tag".

Normal returns is a pointer to the Pf_ensemble object.  A NULL pointer
is returned if cracking the complex pf defined by the ensemble failed.

IT is worth clarifying here the gross structure of a multiple table
group of data received through a stream.  Suppose we had an origin 
and assoc table grouping passed as one block.  The controlling structure
 of the Pf would be:

origin &Arr{
ensemble_keys &Tbl{
orid
}
ensemble &Arr{
000001 &Arr{
data
}
000002 &Arr{
more data
}
...
}
assoc &Arr{
ensemble_keys &Tbl{
arid
orid
}
ensemble &Arr{
000001 &Arr{
data
}
000002 &Arr{
more data
}
...
}

}


Author:  Gary L. Pavlis
Date:  September 2002
*/
Pf_ensemble *pfget_Pf_ensemble(Pf *pfin,char *tag)
{
	Pf_ensemble *pfe;
	Tbl *ttmp;
	int nmembers,ngroups;
	Tbl *t=newtbl(0);
	Pf *pferaw,*pf,*pf_ens_arr;
	Tbl *list_keys;
	int i;


	pferaw=NULL;

	/*This extracts the data enclosed by "tag &Arr {" to "}" */
	if(pfget(pfin,tag,(void **)&pferaw) != PFARR) return(NULL);
	/* Nested Arr's, here we read the "ensemble" group embedded
	within this table.  I do this now to avoiding having
	to free up junk below if this step fails.  In the steps
	below note carefully the use of pferaw and pf_ens_arr 
	which are different levels of the heirarch of the pf.*/
	if(pfget(pferaw,"ensemble",(void **)&pf_ens_arr) != PFARR) return(NULL);

	/* First parse the grouping and attribute keys if present.
	We duplicate the elements of this tbl so 
	we can release the space of the parent strings */
	ttmp=pfget_tbl(pferaw,"group_keys");
	if(ttmp!=NULL)
	{
		pfe->group_keys=duptbl(ttmp,strdup);
		freetbl(ttmp,0);
	}
	/* group_records is a Tbl containing secondary grouping of 
	the ensemble.  If it is empty, we assume no grouping */
	ttmp = pfget_tbl(pferaw,"group_records");
	if(ttmp==NULL)
		ngroups=0;
	else
		ngroups=maxtbl(ttmp);

	/* this loop takes apart the pieces of the ensemble tagged by unspecified keywords.*/
	/* output will be in the order determined by the tags (normally numbers like 000000, 000001, etc.*/
	list_keys=pfkeys(pf_ens_arr);
	/* We now know the size of the Pf_ensemble and can create one */
	nmembers=maxtbl(list_keys);
	pfe=create_Pf_ensemble(nmembers,ngroups);
	/* First we build the group array vectors */
	if(ngroups>0)
	{
		for(i=0;i<maxtbl(ttmp);++i)
		{
			int is,ie;
			char *line;
			line=gettbl(ttmp,i);
			sscanf(line,"%d%d",&is,&ie);
			pfe->group_start[i]=is;
			pfe->group_end[i]=ie;
                        /* This is paranoia, but it has been executed */
                        if(ie>nmembers)
                        {
                            if(is<(nmembers-1) && i==(ngroups-1))
                            {
                                elog_notify(0,"Warning:  inconsistent group_records\nTruncated final group from record %d to %d\n",
                                        ie,nmembers-1);
                                pfe->group_end[i]=ie;
                            }
                            else
                            {
                                elog_die(0,"Illegal input on pfstream:  group_record Tbl is corrupted\nFor group %d end was listed as %d but there are only %d members in the ensemble\n",
                                    i,ie,nmembers);
                            }
			}
		}
		freetbl(ttmp,0);
	}
	/* now the big work -- filling the Pf vector */
	for(i=0;i<maxtbl(list_keys);++i)
	{
		char *key;

		pf=NULL;
		key = gettbl(list_keys,i);
		if(pfget(pf_ens_arr,key,(void **)&pf) != PFARR)
			elog_complain(0,"syntax error parsing ensemble for key %s\nData for this member of this ensemble will be ignored\n", key);
		else
		{
			/* We want to duplicate this pf so we can 
			release the input string storage */
			pfe->pf[i]=pfdup(pf);
		}
	}
	freetbl(list_keys,0);
	return(pfe);
}

/* Now we have the inverse functions.  The above were readers while the following 
are all writers */


/* this is the reciprocal of pfget_Pf_ensemble.  Basically it compiles
the pieces of a Pf_ensemble into a single pf object with a keyword
that identifies the block defined by the input variable tag.  

This is a large memory algorithm that works in a brute force way.  pf2string is
used to expand each of the components, the components are combined by string
manipulation, and the results are converted by to a pf by pfcompile.  
Note use of variable arguments to allow tables with multiple tags to be
pushed into the same pf object.

Arguments:
ntags - dummy required to implement variable arguments.  Standard
	C's implementation of variable args does not allow 
	a null start argument.  This defines the number of triplet 
	arguments that are expected to follow.  Since we are stuck
	with it anyway this avoids things like terminating 0 arg used
	in routines like dbgetv.

This function uses a variable argument list made up of triplets
in the following order:  [tag, ensemble_pf, pfe] where
	tag - name used to tag the output Arr.
	ensemble_pf - (optional) set of parameters to dump at head
		of output stream that is build here.  That is these
		are global parameters.  This should be set to NULL
		before calling this function if no global parameters
		are required.
		Note that if the grouping features is used this block
		must contain those parameters.
	pfe - Pf_ensemble that is to be compiled into single output pf

	[tag2, pfe2, ... ] repeat for additional tables (variable arg)

Author:  Gary L. Pavlis
Date:  September 2002
*/

Pf *build_ensemble(int ntags,...)
{
	va_list args;
	Pf_ensemble *pfe;
	Pf *ensemble_pf;
	char *tag;
	char **blocks;
	char *pfimage;
	int sum_block_sizes,pfimage_size=0;
	char *hdrblock;
	int i;
	Pf *pfresult=NULL;
	int npf;  /* useful shorthand */
	int itag;

	va_start(args,ntags);
	for(itag=0;itag<ntags;++itag)
	{
		tag=va_arg(args,char *);
		ensemble_pf=va_arg(args,Pf *);
		pfe=va_arg(args,Pf_ensemble *);

		npf=pfe->nmembers;
		allot(char **,blocks,npf);
		for(i=0,sum_block_sizes=0;i<npf;++i)
		{
			blocks[i]=pf2string(pfe->pf[i]);
			sum_block_sizes+=strlen(blocks[i]);
		}
		if(ensemble_pf==NULL)
			hdrblock=strdup("");
		else
		{
			hdrblock=pf2string(ensemble_pf);
			pfimage_size += strlen(hdrblock);
		}
		/* this is a generous estimate of the size of the full pf image*/
		pfimage_size += sum_block_sizes + npf*20+150;
		if(itag==0) 
		{
			pfimage = (char *) malloc(pfimage_size);
			pfimage[0]='\0';
		}
		else
			/* realloc preserves contents of previous */
			pfimage=realloc(pfimage,pfimage_size);
			
		if(pfimage==NULL) elog_die(0,"Cannot malloc %d bytes to build output ensemble pf image\n",
						pfimage_size);
	
		/*Now we use string functions to assemble this mess */
		strcat(pfimage,tag);
		strcat(pfimage," &Arr{\n");
		strcat(pfimage,hdrblock);
		strcat(pfimage,"ensemble &Arr{\n");
		for(i=0;i<npf;++i)
		{
			char key[16];
			sprintf(key,"%0.5d ",i);
			strcat(pfimage,key);
			/*This assumes the "blocks" are PFARR's because
			then the contents will begin with &Arr{ and end
			with a "}" */
			strcat(pfimage,blocks[i]);
		}
		strcat(pfimage,"}\n}\n");
	
		for(i=0;i<npf;++i) free(blocks[i]);
		free(blocks);
		free(hdrblock);
	}
	pfcompile(pfimage,&pfresult);
	free(pfimage);
	va_end(args);
	return(pfresult);
}
/* This collection of functions put a single attribute of a 
particular type to all members of a pfensemble.  If I did
this in C++ it would be a classic reason for function
overloading.  Here I just have to do this the hard way.
Author:  G Pavlis
Written:  October 2002
*/
void pfensemble_put_double(Pf_ensemble *pfe,char *name,double value)
{
	int i;

	for(i=0;i<(pfe->nmembers);++i)
		pfput_double(pfe->pf[i],name,value);
}
void pfensemble_put_time(Pf_ensemble *pfe,char *name,double value)
{
	int i;

	for(i=0;i<(pfe->nmembers);++i)
		pfput_time(pfe->pf[i],name,value);
}
void pfensemble_put_int(Pf_ensemble *pfe,char *name,int value)
{
	int i;

	for(i=0;i<(pfe->nmembers);++i)
		pfput_int(pfe->pf[i],name,value);
}
void pfensemble_put_string(Pf_ensemble *pfe,char *name,char *value)
{
	int i;

	for(i=0;i<(pfe->nmembers);++i)
		pfput_string(pfe->pf[i],name,value);
}
/* Simple little function that builds the group_records &Tbl 
as a Pf from the contents of the Pfensemble structure.  This is
necessary on outputs to define tables build with one row per
group.
*/
Pf *pfensemble_convert_group(Pf_ensemble *pfe)
{
	Tbl *t;
	int i;
	char buf[50];
	char *str;
	Pf *pf;

	pf = pfnew(PFFILE);
	t=newtbl(0);
	for(i=0;i<pfe->ngroups;++i)
	{
		sprintf(buf,"%d %d",pfe->group_start[i],
					pfe->group_end[i]);
		str=strdup(buf);
		pushtbl(t,str);
	}
	pfput_tbl(pf,"group_records",t);
	/* FUTURE PROBLEM LIKELY HERE:  currently pfput_tbl 
	has a memory management irregularity that creates this
	odd combo.  A new tbl is created and stored in the pf, but
	the leaves of the pf list are not duplicated.  Thus we 
	remove the tbl yet do not free the leaves.  A memory 
	leak will occur here if this is repaired as I have suggested
	to Dan Quinlan. */
	freetbl(t,0);
	return(pf);
}	
		

