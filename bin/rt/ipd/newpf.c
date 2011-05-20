/************************************************************************
*
*  
*  new_pf.c
*  reread parameter file and re-initialize PF structures.

*  Author: Marina Harkins-Glushko
*  	    UCSD, IGPP
*	    glushko@ucsd.edu
***********************************************************************/
#include <signal.h>
#include "_pkt.h"
 
extern Arr *Packets;
extern Arr *StaCh;
extern Arr *StaID;
extern char *pffile;
 
void newpf( char *pfile)
 {
 
     elog_complain( 0, "Re-reading parameter file to get new settings..." );
     initpf( pffile ); 
     Packets == NULL; init_packets();
     StaID == NULL; init_StaID();
     StaCh == NULL; init_StaCh();
     
     elog_complain( 0, "Done.\n");
   return;
 }
               

