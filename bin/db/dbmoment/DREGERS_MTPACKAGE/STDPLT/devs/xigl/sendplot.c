#include <xview/xview.h>
#include <xview/canvas.h>

#include "xigl.h"

extern Menu_item NextPlot_Item ;

int
sendplot(plotnum,more)
int plotnum, more;
   {
	debug ("entering sendplot...\n");
	if (more == 0) menu_set(NextPlot_Item,
				MENU_INACTIVE,		TRUE,
				0);
	window_fit (canvas) ;
	window_fit (screen_frame) ;
/*::
	if (setjmp(env) == 0) {
::*/
		debug ("calling xv_main_loop...\n");	
		xv_main_loop(screen_frame) ;
/*::
	}
	else {
		debug ("longjmp to sendplot\n");
	}
	debug ("leaving sendplot...\n");
::*/
	return(0);
   }
