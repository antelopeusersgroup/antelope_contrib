
/* circular memory queue which allocates all its memory at startup */

#ifndef MEM_CIRC_QUEUE_H
#define MEM_CIRC_QUEUE_H


/* The scheme involves three glops of memory: 
	a small structure QUEUE - one for each queue, defined below;
	an array of message buffer descriptors  (BUF_DESC) and 
	a large region of message buffers where we'll store the messages 
	  (pointed to by pmsg_buffs in the QUEUE structure).
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
Q_POS getNext(QUEUE * q, int QueuePosition);
Q_POS getPrev(QUEUE * q, int QueuePosition);
Q_POS getPosFirst(QUEUE * q);
Q_POS getPosLast(QUEUE * q);

#endif
