
/*
 *   THIS FILE IS UNDER RCS - DO NOT MODIFY UNLESS YOU HAVE
 *   CHECKED IT OUT USING THE COMMAND CHECKOUT.
 *
 *    $Id$
 *
 *    Revision history:
 *     $Log$
 *     Revision 1.4  2003/06/01 08:25:40  lindquis
 *     Upgrade Iceworm libraries to Earthworm6.2. Add some rudimentary man
 *     pages. Preparation for the rewritten ew2orb.
 *
 *     Revision 1.2  2001/04/12 03:57:44  lombard
 *     Added functions dumpqueue, undumpqueue and peekNextElement
 *     to allow saving queue to disk and reloading.
 *
 *     Revision 1.1  2000/02/14 18:51:48  lucky
 *     Initial revision
 *
 *
 */

/* 
This is son of in 'queue_max_size.c', which is son of queue, which Will got from
Kelley and Pohl. These were clever linked list schemes which allocated as needed.
This was too scary: the system could run out of memory and fail after running for
a long time.
	So, out with the cleverness, and in with brute force: this version pre-
allocates the max amount of memory it will ever use at startup, and does the
buffering in an array of message slots. The buffering is circular, as before:
When we run out of message slots, we overwrite the oldest message.
alex 2/8/98
*/

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <earthworm.h>
#include <time.h>
#include <mem_circ_queue.h>

/* Internal Function Prototypes */
static void inc_circular( QUEUE* q, Q_POS * pQPos);
static void dec_circular( QUEUE* q, Q_POS * pQPos);

/****************************************************************
*			initqueue				*
*  allocates the three queue regions				*
*   	returns:						*
*		-1 if we couldn't get the memory		*
*		-2 if the amount implied is bigger than		*
*		   a long					*
*****************************************************************/
int initqueue( QUEUE* q, unsigned long maxElements, 
               unsigned long elementMaxSize )
{
  unsigned long 	temp;
  QUEUE_ENTRY* 	pQE;
  unsigned long StartingOffset, malloc_size;

  /* First allocate the QUEUE structure
   *************************************/
  /* this contains the pointers to the start of the message descriptor array
     and the message storage area */
  if (q == (QUEUE*)NULL) return(-1);

  /* save some arguments to local variables
   *****************************************/
  q->MyMaxElements = maxElements;
  q->MyMaxSize = elementMaxSize;
  q->NumOfElements = 0;

  /* Is the total number of bytes within an unsigned long?
   * I really hope so, because otherwise this is
   * going to be a 4GB queue.
   ********************************************/
  temp =  q->MyMaxElements * q->MyMaxSize;
  temp = temp/maxElements;
  if (temp != elementMaxSize) return (-2);  
  
  /* Allocate the array of queue entries
   ***************************************************/
  malloc_size= maxElements * (sizeof(QUEUE_ENTRY) + elementMaxSize);
  q->pQE = (QUEUE_ENTRY*)malloc(malloc_size);
  if (q->pQE == (QUEUE_ENTRY*)NULL) return(-1);
  
  /* Set DATA pointers for individual elements
   **********************************************************************/
  pQE= q->pQE;   /* element of the QUEUE structure which points to the array of buffer descriptors */
  
  StartingOffset=((unsigned long) pQE) + (maxElements * sizeof(QUEUE_ENTRY));
   
  for (temp=0; temp< maxElements; temp++)
  {
    /*  For each element, set the data pointer */
    pQE->d = (char *)(StartingOffset + (elementMaxSize * temp));
    pQE++;
  }

  /* set in and out pointers to indicate empty queue
   *************************************************/
  q->first=q->last=0;
  /* in  = out = first descriptor */

  return(0);
}

/***********************************************************************
 *                              dequeue                                *
 *                                                                     *
 *  Copies oldest message and its logo into caller's space.            *
 *  Returns 0 if no error.                                             *
 *  Returns -1 if the queue is empty.                                  *
 ***********************************************************************/
int dequeue( QUEUE *q, DATA x, long* size, MSG_LOGO* userLogoPtr )
{
  QUEUE_ENTRY * pFQE; /* Pointer to first(oldest) queue entry */

  if (!(q->NumOfElements) ) /* then the queue is empty */
    return( -1 );

  /* set shortcut for first queue element */
  pFQE=&(q->pQE[q->first]);
  
  /* copy text of message to caller's memory 
   ******************************************/
  memcpy( x, pFQE->d, (size_t)(pFQE->length) );
  
  /* copy the logo
   ***************/
  userLogoPtr->type   = (pFQE->queueLogo).type;
  userLogoPtr->mod    = (pFQE->queueLogo).mod ;
  userLogoPtr->instid = (pFQE->queueLogo).instid;
  
  *size  = pFQE->length;  /* message length */
  
  /* move the out pointer
   **********************/
  inc_circular(q, &(q->first) );   
  
  q->NumOfElements--;  /* we're dequeueing an element, so there is one less */
  
  return( 0 );
}


/***********************************************************************
 *                              enqueue                                *
 *                                                                     *
 *  moves message into circular buffer.                                *
 *  Copies string x to the new element.                                *
 *  Returns 0 if no error.                                             *
 *  Returns -1 message too big                                         *
 *  Returns -3 if we clobbered an unsent message (stepped on our tail) *
 ***********************************************************************/
int enqueue( QUEUE *q, DATA x, long size, MSG_LOGO userLogo )
{
  int ourRet;
  QUEUE_ENTRY * pLQE;  /* pointer to last(youngest) entry in queue */
  
  ourRet=0 ; /* presume all will go well */
  
  /* if message is larger than max, do nothing and return error
   ************************************************************/
  if (size > q->MyMaxSize) return(-1); 
  
  /* set shortcut for last queue element */
  pLQE=&((q->pQE)[q->last]);
  
  /* copy message text
   *******************/
  memcpy( pLQE->d, x, size);
  
  /* copy logo and size
   *********************/
  (pLQE->queueLogo).type   = userLogo.type;
  (pLQE->queueLogo).mod    = userLogo.mod;
  (pLQE->queueLogo).instid = userLogo.instid;
  (pLQE)->length = size;
  

  /* move to next buffer
   *********************/
  inc_circular(q, &(q->last) );   
  if( q->NumOfElements == q->MyMaxElements ) /* then the queue is full */
  { /* in that case we just overwrote the contents of the first(oldest)
       message in the queue, so 'drive' the outpointer forward one */
    inc_circular(q, &(q->first) );
    q->NumOfElements--;  /* we're overwriting an element, so there is one less */
    ourRet = -3;   /* meaning we're over-writing messages */
  }

  q->NumOfElements++;  /* we're adding an element, so there is one more */

  return( ourRet );
}

/* to move supplied pointer to the next buffer descriptor structure 
 * - in a circular way */
static void inc_circular( QUEUE* q, Q_POS * pQPos)
{
  (*pQPos)++;
  if(*pQPos >= q->MyMaxElements)
    *pQPos=0;
}

/* to move supplied pointer to the prev buffer descriptor structure 
 * - in a circular way */
static void dec_circular( QUEUE* q, Q_POS * pQPos)
{
  (*pQPos)--;
  if((*pQPos) <  0)
    *pQPos= (q->MyMaxElements)-1;
}


int getNumOfElementsInQueue( QUEUE * q)
{
  return(q->NumOfElements);
}

Q_POS getNext(QUEUE * q, int QueuePosition)
{
  Q_POS temp;
  
  temp=QueuePosition;
  if(temp < 0 || temp >= q->MyMaxElements)
    return(-1);
  else
  {
    inc_circular(q,&temp);
    return(temp);
  }
}


Q_POS getPrev(QUEUE * q, int QueuePosition)
{
  Q_POS temp;
  
  temp=QueuePosition;
  if(temp < 0 || temp >= q->MyMaxElements)
    return(-1);
  else
  {
    dec_circular(q,&temp);
    return(temp);
  }
}

Q_POS getPosFirst(QUEUE * q)
{
  return(q->first);
}


Q_POS getPosLast(QUEUE * q)
{
  return(q->last);
}

/*
 * dumpqueue: dump the contents of a queue to a file to be used for restarts
 *     returns: 0 on success
 *             -1 on errors opening or writing file; file is deleted
 */
int dumpqueue( QUEUE *q, char *filename)
{
  QUEUE_ENTRY *pQE;
  FILE *fp;
  long i;
  Q_POS p;
  int ret = 0;
  time_t timestamp;
  
  if ( (fp = fopen(filename, "wb")) == (FILE *)NULL)
    return( -1 );
  
  timestamp = time(0);
  if (fwrite(&timestamp, sizeof(time_t), 1, fp) < 1)
  {
    ret = -1;
    goto Cleanup;
  }
  if (fwrite(&q->NumOfElements, sizeof(long), 1, fp) < 1)
  {
    ret = -1;
    goto Cleanup;
  }
  /* Loop through all the queue entries */
  p = q->first;
  for (i = 0l; i < q->NumOfElements; i++, inc_circular(q, &p))
  {
    pQE = &(q->pQE[p]);
    if (fwrite(&pQE->queueLogo, sizeof(MSG_LOGO), 1, fp) < 1)
    {
      ret = -1;
      goto Cleanup;
    }
    if (fwrite(&pQE->length, sizeof(long), 1, fp) < 1)
    {
      ret = -1;
      goto Cleanup;
    }
    if ((long)fwrite(pQE->d, sizeof(char), pQE->length, fp) < pQE->length)
    {
      ret = -1;
      goto Cleanup;
    }
  }
  if (fwrite(&timestamp, sizeof(time_t), 1, fp) < 1)
  {
    ret = -1;
    goto Cleanup;
  }
  
 Cleanup:
  fclose(fp);
  if (ret < 0)
    unlink(filename);

  return( ret );
}

    
/*
 * undumpqueue: read a queue file previously written by dumpqueue().
 *              Contents of file will be inserted into the specified queue.
 *              The queue is assumed to be empty on entry to this function.
 *   Returns: 0 on success
 *           +1 if dump file does not exist
 *           -1 on errors opening or reading file
 *           -2 if there are more entries in the file than will fit in queue
 *           -3 if file entry is larger than will fit in queue
 *           -4 if timestamps at each end of file don't match
 */
int undumpqueue( QUEUE *q, char *filename)
{
  QUEUE_ENTRY *pQE;
  FILE *fp;
  long i, num_entries;
  int ret = 0;
  time_t timestamp1, timestamp2;
  
  if ( (fp = fopen(filename, "rb")) == (FILE *)NULL)
  {
    if (errno == ENOENT)
      return( +1);
    else
      return( -1 );
  }
  
  if (fread(&timestamp1, sizeof(time_t), 1, fp) < 1)
  {
    ret = -1;
    goto Cleanup;
  }

  if (fread(&num_entries, sizeof(long), 1, fp) < 1)
  {
    ret = -1;
    goto Cleanup;
  }
  if (num_entries > q->MyMaxElements)
  {
    ret = -2;
    goto Cleanup;
  }
  
  for (i = 0l; i < num_entries; i++)
  {
    pQE = &(q->pQE[q->last]);
    if (fread(&pQE->queueLogo, sizeof(MSG_LOGO), 1, fp) < 1)
    {
      ret = -1;
      goto Cleanup;
    }
    if (fread(&pQE->length, sizeof(long), 1, fp) < 1)
    {
      ret = -1;
      goto Cleanup;
    }
    if (pQE->length > q->MyMaxSize)
    {
      ret = -3;
      goto Cleanup;
    }
    if ((long)fread(pQE->d, sizeof(char), pQE->length, fp) < pQE->length)
    {
      ret = -1;
      goto Cleanup;
    }
    inc_circular(q, &q->last);
    q->NumOfElements++;
  }
  
  if (fread(&timestamp2, sizeof(time_t), 1, fp) < 1)
  {
    ret = -1;
    goto Cleanup;
  }
  if (timestamp2 != timestamp1)
  { /* Timestamps don't match; can't trust file, so reset the queue to empty */
    q->last = q->first = 0;
    q->NumOfElements = 0;
    ret = -4;
  }
  
 Cleanup:
  fclose(fp);

  return( ret );
}

/*
 * peekNextElement: return a pointer to the next element without changing it.
 *    Returns: pointer to data on success
 *             NULL if there are no elements in the queue
 */
DATA peekNextElement( QUEUE *q )
{
  if (q->NumOfElements > 0)
    return( q->pQE[q->first].d );
  else
    return( NULL );
}
