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

#ifdef _OS2
#define INCL_DOSMEMMGR
#define INCL_DOSSEMAPHORES
#include <os2.h>
#endif

#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include "earthworm.h"
#include "transport.h"
#include "mem_circ_queue.h"

void inc_circular( QUEUE* q, Q_POS * pQPos);   /* local routine to do circular increment */
void dec_circular( QUEUE* q, Q_POS * pQPos);   /* local routine to do circular decrement */

/****************************************************************
*			initqueue				*
*  allocates the three queue regions				*
*   	returns:						*
*		-1 if we couldn't get the memory		*
*		-2 if the amount implied is bigger than		*
*		   a long					*
*****************************************************************/
int initqueue( QUEUE* q, unsigned long maxElements, unsigned long elementMaxSize )
{
unsigned long 		temp;
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

   /* Is the total number of bytes within a long
     I really hope so, because otherwise this is
     going to be a 4GB queue.
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
   {
      return( -1 );
   }


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

void inc_circular( QUEUE* q, Q_POS * pQPos)
/* to move supplied pointer to then next buffer descriptor structure - in a circular way */

{
   (*pQPos)++;
   if(*pQPos >= q->MyMaxElements)
   {
     *pQPos=0;
   }
}

void dec_circular( QUEUE* q, Q_POS * pQPos)
/* to move supplied pointer to then prev buffer descriptor structure - in a circular way */

{
   (*pQPos)--;
   if((*pQPos) <  0)
   {
     *pQPos= (q->MyMaxElements)-1;
   }
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
  {
    return(-1);
  }
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
  {
    return(-1);
  }
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


