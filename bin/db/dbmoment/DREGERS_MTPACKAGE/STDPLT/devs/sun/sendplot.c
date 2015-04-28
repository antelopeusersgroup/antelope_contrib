#include <suntool/sunview.h>
#include <suntool/canvas.h>

extern Frame 	screen_frame ;
extern Canvas	canvas ;
extern Pixwin	*pw ;
extern Menu_item NextPlot_Item ;

int
sendplot(plotnum,more)
int plotnum, more;
   {
	pw_unlock(pw);
	pw_batch_off(pw);
	window_set(canvas,
		WIN_CONSUME_PICK_EVENT,			LOC_DRAG,
		0);
	if (more == 0) menu_set(NextPlot_Item,
				MENU_INACTIVE,		TRUE,
				0);
	window_fit (canvas) ;
	window_fit (screen_frame) ;
	window_main_loop (screen_frame) ;
	return(0);
   }
