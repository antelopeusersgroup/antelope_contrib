
/*
 *   THIS FILE IS UNDER RCS - DO NOT MODIFY UNLESS YOU HAVE
 *   CHECKED IT OUT USING THE COMMAND CHECKOUT.
 *
 *    $Id$
 *
 *    Revision history:
 *     $Log$
 *     Revision 1.2  2003/06/01 08:25:40  lindquis
 *     Upgrade Iceworm libraries to Earthworm6.2. Add some rudimentary man
 *     pages. Preparation for the rewritten ew2orb.
 *
 *     Revision 1.5  2001/04/12 03:59:07  lombard
 *     changed "peak" to "peek"
 *
 *     Revision 1.4  2001/04/12 03:09:16  lombard
 *     Added functions for saving queue to disk and recovering.
 *
 *     Revision 1.3  2001/01/14 22:16:07  lombard
 *     spelling!
 *
 *     Revision 1.2  2001/01/14 22:14:44  lombard
 *     added include file <transport.h>, required for use with mem_circ_queue.
 *
 *     Revision 1.1  2000/02/14 20:05:54  lucky
 *     Initial revision
 *
 *
 */


/* circular memory queue which allocates all its memory at startup */

#ifndef MEM_CIRC_QUEUE_H
#define MEM_CIRC_QUEUE_H

#include <transport.h>

/* The scheme involves three glops of memory: 
	a small structure QUEUE - one for each queue, defined below;
	an array of message buffer descriptors  (QUEUE_ENTRY) and 
	a large region of message buffers where we'll store the messages 
        (each pointed to by d in the QUEUE_ENTRY structures).
*/

typedef char*		DATA;
typedef int     Q_POS;


typedef struct 
{
   long               length;
   MSG_LOGO           queueLogo;
   DATA               d;
} QUEUE_ENTRY;

typedef  struct
{
     QUEUE_ENTRY * pQE;  /* pointer to start of queue element array */
     Q_POS     first;       /* position of the first(oldest) element of the queue */
     Q_POS     last;        /* position of the last(youngest) element of the queue */
     long      MyMaxSize;		/* largest message we'll ever see */
     long      MyMaxElements;	/* number of message buffers in ring */
     long      NumOfElements;
   
}  QUEUE;


int initqueue( QUEUE* q, unsigned long maxElements, unsigned long elementMaxSize );
int dequeue( QUEUE *q, DATA x, long* size, MSG_LOGO* userLogoPtr );
int enqueue( QUEUE *q, DATA x, long size, MSG_LOGO userLogo );
int getNumOfElementsInQueue( QUEUE * q);
int dumpqueue( QUEUE *p, char *filename);
int undumpqueue( QUEUE *q, char *filename);
Q_POS getNext(QUEUE * q, int QueuePosition);
Q_POS getPrev(QUEUE * q, int QueuePosition);
Q_POS getPosFirst(QUEUE * q);
Q_POS getPosLast(QUEUE * q);
DATA peekNextElement( QUEUE *q );

#endif
