/* definitions and documentation needed by Ringfile Replicator Protocol (RRP) 
	author D.C. Ketchum May 2007
*/

#ifndef __ring_reader_h
#define __ring_reader_h

#define MAXSEQNUM 2000000000

/* function prototypes */
int rrp_init(char *ringfilename, int modulus, char *statusext);
int rrp_get_next(char *buf, int *shutdown); /* return next mini-seed block */
int rrp_shutdown();                  /* user call on process exit for clean up */
int rrp_get_nextout();               /* return next sequence # to process */
                                     /* block to be returned rrp_get_next*/

int rrp_get_lastseq();               /* latest block seq # put in ring buffer */
int rrp_get_size();                  /* size of the ring in blocks */
int rrp_write_stat();                /* writes out the current status now (interal use only)*/
int rrp_read_ring_hdr();             /* read in ring hdr (internal use only) */

#endif
