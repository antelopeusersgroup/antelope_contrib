#include <stdio.h>
/* This file currently has an include for pthread.h so we don't require it */
#include "brttutil.h"
#include "pfstream.h"
/* This is the arg struct passed to both read and write threads */
typedef struct pfsc {
	FILE *fp;
	Pmtfifo *mtf;
} Pfstream_control;
/* These are read and write functions that define control by one 
thread that does in and out respectively.  i.e. they are passed to
pthread_create. */


/* This is the read function.  It simply reads data from the input
stream creating raw pf's that encapsulate all the data.  These
are simply pushed onto to mtfifo. 

The routine provides a redundant signal for end of data by
pushing a FINISHED boolean onto the pf set true.  This provides
an mechanism to terminate cleanly on eoi.  Other with a thread
reading and pushing I don't see how else to tell downstream
processors that this function is not just waiting for data
and the queue is empty.*/


void * pfstream_read_data(void *arg)
{
	Pfstream_control *pfsc;
	Pf *pf;  /* Data read in as a single, raw pf object*/
	pfsc = (Pfstream_control *)arg;

	while( (pf=pfstream_read(pfsc->fp)) != NULL)
	{
		pfput_boolean(pf,"FINISHED",0);
		pmtfifo_push(pfsc->mtf,(void *)pf);
	}
	pf=pfnew(PFFILE);
	pfput_boolean(pf,"FINISHED",1);
	pmtfifo_push(pfsc->mtf,(void *)pf);
	fclose(pfsc->fp);

	return(NULL);
}
/* This is the opposite of the previous */
void *pfstream_write_data(void *arg)
{
	Pfstream_control *pfsc=(Pfstream_control *)arg;
	Pf *pf,*pftest;
	int fini;
	int itest;  

	while(1)
	{
		pmtfifo_pop(pfsc->mtf,(void **)(&pf));
		/* this is a safe test if FINISHED is not set.  
		Should not require user to have to do this */
		itest = pfget(pf,"FINISHED",(void **)&pftest);
		if(itest==PFINVALID)
		{
			fini=0;
			pfput_boolean(pf,"FINISHED",0);
		}
		else
			fini=pfget_boolean(pf,"FINISHED");
		if(fini) 
		{
			fprintf(pfsc->fp,"%s\n",END_OF_DATA_SENTINEL);
			fflush(pfsc->fp);
			sleep(20);
			fclose(pfsc->fp);
			break;
		}
		pfout(pfsc->fp,pf);
		fprintf(pfsc->fp,"%s\n", ENDPF_SENTINEL);
		fflush(pfsc->fp);
		/* We have release the memory for pf here as the caller
		is pushing this away a forgetting about it */
		pffree(pf);
	}
	return(NULL);
}

/* Creates a new thread to read pfstream data from fname.  Returns
a handle that defines the interface into the pfstream.  This is implementd
here by Brtt's pmtfifo.  

This function mostly just creates the data objects used for the interface
and then sets up and creates the read thread passed to pthread_create.  

Author: Gary Pavlis
Written:  october 2002
*/
#define PFSTREAM_MAXQUEUE 10  /* size of stack in pmtfifo queue */
Pfstream_handle *pfstream_start_read_thread(char *fname)
{
 	Pfstream_handle *pfh;  /* Handle downstream functions can use to 
			access data from the pfstream */
	Pfstream_control *pfsc;  /* control structure passed to new thread */

	/*These need to be created fromt he free store.  If we 
	used a local variable in this function these would disappear when
	this routine went out of scope screwing up the thread it was passed
	to, and the return handle would get dropped.*/
	allot(Pfstream_handle *,pfh,1);
	allot(Pfstream_control *, pfsc, 1);
	
	/* brttutil routine to create a multithreaded, posix compatible thread */
	pfh->mtf = pmtfifo_create(PFSTREAM_MAXQUEUE,1,0); 
	if(pfh->mtf == NULL) 
	{
		elog_log(1,"pfstream_start_read_thead:  Could not create mtfifo\n");
		free(pfh);
		free(pfsc);
		return(NULL);
	}
	pfsc->mtf=pfh->mtf;

	/* open the stream and put the fp into the control structure */
	pfsc->fp = fopen(fname,"r");
	if(pfsc->fp==NULL)
	{
		elog_log(1,"pfstream_start_read_thread:  Cannot open input stream %s\n",
			fname);
		pmtfifo_destroy(pfh->mtf,free);
		free(pfh);
		free(pfsc);
		return(NULL);
	}
	/* This launches the i/o thread. It is assumed that all it does is
	read data and push objects onto the Pmtfifo for downstream processors
	to pop off and use. */
	if(pthread_create(&(pfh->thread_id),NULL,
		pfstream_read_data,(void *)pfsc)!=0)
	{
		elog_log(1,"pfstream_start_read_thread: cannot create read thread\n");
		pmtfifo_destroy(pfh->mtf,free);
		fclose(pfsc->fp);
		free(pfh);
                free(pfsc);
                return(NULL);
        }
	return(pfh);
}

/* Parallel to above, but for output */

Pfstream_handle *pfstream_start_write_thread(char *fname)
{
 	Pfstream_handle *pfh;  /* Handle downstream functions can use to 
			access data from the pfstream */
	Pfstream_control *pfsc;  /* control structure passed to new thread */

	/*These need to be created fromt he free store.  If we 
	used a local variable in this function these would disappear when
	this routine went out of scope screwing up the thread it was passed
	to, and the return handle would get dropped.*/
	allot(Pfstream_handle *,pfh,1);
	allot(Pfstream_control *, pfsc, 1);
	
	/* brttutil routine to create a multithreaded, posix compatible thread */
	pfh->mtf = pmtfifo_create(PFSTREAM_MAXQUEUE,1,0); 
	if(pfh->mtf == NULL) 
	{
		elog_log(1,"pfstream_start_write_thead:  Could not create mtfifo\n");
		free(pfh);
		free(pfsc);
		return(NULL);
	}
	pfsc->mtf=pfh->mtf;

	/* open the stream and put the fp into the control structure */
	pfsc->fp = fopen(fname,"r+");
	if(pfsc->fp==NULL)
	{
		elog_log(1,"pfstream_start_write_thread:  Cannot open output stream %s\n",
			fname);
		pmtfifo_destroy(pfh->mtf,free);
		free(pfh);
		free(pfsc);
		return(NULL);
	}
	/* This launches the write thread.  This is assumed to do the 
	opposite of the read thread.  That is, processing modules will
	push data onto this mtfifo and the write routine handled by
	this thread will pop them off and send the contents down the 
	stream now open as fp */
	if(pthread_create(&(pfh->thread_id),NULL,pfstream_write_data,(void *)pfsc)!=0)
	{
		elog_log(1,"pfstream_start_write_thread: cannot create write thread\n");
		pmtfifo_destroy(pfh->mtf,free);
		fclose(pfsc->fp);
		free(pfh);
                free(pfsc);
                return(NULL);
        }
	return(pfh);
}


/* This is the routine that uses data pushed onto the queue by the 
read thread.  All it does is call pmtfifo and handle error conditions.
pfh is the control structure returned by the read thread we use
to access the data contained in the fifo.

Returns a NULL to signal end of data.  This can happen one of two ways.
If there is an error in pmtfifo_pop, complain is called and we return NULL.
This was done to allow a graceful exit from the caller.  The warning message
may be too subtle, but I see now simple way to do this with plain C.  This
is a case where I can see reasons for C++'s throw having a big advantage.
The function will also return NULL when it sees the end of data.  
That is caught here by the FINISHED boolean in the pf.  
*/
Pf *pfstream_get_next_ensemble(Pfstream_handle *pfh)
{
	Pf *pf;  /* Pf popped from Pmtfifo */

	if(pmtfifo_pop(pfh->mtf,(void **)(&pf)) < 0)
	{
		elog_complain(1,"pfstream_get_next_ensemble:  Attempt to fetch data from input fifo failed\nData stream was probably trunctated\nTrying to clean up before exit\n");
		return(NULL);
	}
	if(pfget_boolean(pf,"FINISHED"))
	{
		pffree(pf);
		return(NULL);
	}
	return(pf);
}

/* The opposite is equally simple.  We assume the output has already been 
encapsulated into a single pf.  Returns 0 on success, -1 if problems 
occur */

int pfstream_put_ensemble(Pfstream_handle *pfh, Pf *pf)
{
	int iret;

	iret = pmtfifo_push(pfh->mtf,(void *)pf);
	if(iret<0)
	{
		elog_complain(0,"pfstream_put_ensemble: Attempt to push data to output fifo failed\n");
		return(-1);
	}
	return(0);
}
