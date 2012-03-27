/************************************************************************
*
*  
*  sig_hdlr.c
*
*  Get sigan "kill -16" to re-read parameter file and
*  reinitialize structures 
*
*
*
*  Author: Marina Harkins-Glushko
*  	    UCSD, IGPP
*	    glushko@ucsd.edu
***********************************************************************/
#include <signal.h>
#include "_pkt2.h"
 
extern Arr *Packets;
extern Arr *StaCh;
extern Arr *StaID;
 
void sig_hdlr(signo)
int signo;
 {
 
     elog_complain( 0, "sig_hdlr(): Got signal %d.\n", signo);
     elog_complain( 0, "Re-reading parameter file to get new settings..." );
 
     Packets == NULL; init_packets();
     StaID == NULL; init_StaID();
     StaCh == NULL; init_StaCh();
     
     elog_complain( 0, "Done.\n");
     signal(SIGUSR1, sig_hdlr );
   return;
 }
               

