/************************************************************************
*
*  
*  sig_hdlr.c
*
*
***********************************************************************/
#include <signal.h>
#include "stock.h"
 
extern in_dbase[];   
 
void sig_hdlr(signo)
int signo;
 {
 
      if( unlink( in_dbase ) < 0 )
           elog_die( 1, "can't remove %s\n", in_dbase);
	    
       return;
 }
               

