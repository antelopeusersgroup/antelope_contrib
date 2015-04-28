#include	"../com/global.h"
#include	<stdio.h>
#include	"devpar.h"

#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <suntool/scrollbar.h>

Frame 	screen_frame ;
Canvas	canvas ;
Pixwin	*pw ;
Icon igl_icon;
Rect igl_canvas_rect;
Scrollbar sb;

static short igl_icon_image[] = {
#include "igltool.icon"
};
mpr_static(igltool_icon, 64, 64, 1, igl_icon_image);

int pencolor;

/* device specifications */
extern float	pixinch	;
extern float	inchpix	;
extern int	xmax	;
extern int	ymax	;
extern int	xpagelen;
extern int	ypagelen;
extern int	colordev;

extern int	bw_only;

opendev()
   {
	igl_icon = icon_create(ICON_IMAGE, &igltool_icon, 0);
	screen_frame = window_create (NULL, FRAME,
				WIN_WIDTH, 		xpagelen,
				WIN_HEIGHT, 		ypagelen,
				WIN_X,			0,
				WIN_Y,			0,
				FRAME_ICON,		igl_icon,
				0) ;
	sb = scrollbar_create(0);
	canvas = window_create (screen_frame, CANVAS, 
				CANVAS_AUTO_SHRINK, 		FALSE,
				CANVAS_WIDTH, 		xmax,
				CANVAS_HEIGHT, 		ymax,
				WIN_VERTICAL_SCROLLBAR, sb,
				WIN_HORIZONTAL_SCROLLBAR, scrollbar_create(0),
				0) ;
	pw = canvas_pixwin (canvas) ;

	if (pw->pw_pixrect->pr_depth > 1) { /* color monitor */
		pixinch	= PIXINCH2;
		inchpix	= INCHPIX2;
		xpagelen= XPAGELEN2;
		ypagelen= YPAGELEN2;

		window_set(screen_frame, FRAME_NO_CONFIRM, TRUE, 0);
		window_destroy(screen_frame);
		screen_frame = window_create (NULL, FRAME,
				WIN_WIDTH, 		xpagelen,
				WIN_HEIGHT, 		ypagelen,
				WIN_X,			0,
				WIN_Y,			0,
				FRAME_ICON,		igl_icon,
				0) ;
		sb = scrollbar_create(0);
		canvas = window_create (screen_frame, CANVAS, 
				CANVAS_AUTO_SHRINK, 		FALSE,
				CANVAS_WIDTH, 		xmax,
				CANVAS_HEIGHT, 		ymax,
				WIN_VERTICAL_SCROLLBAR, sb,
				WIN_HORIZONTAL_SCROLLBAR, scrollbar_create(0),
				0) ;
		pw = canvas_pixwin (canvas) ;

		if (!bw_only) {
			colordev = 1;
			pencolor = 0;
			pw_setcmsname(pw, "igl_color");
		}
	}
	scrollbar_scroll_to(sb, ypagelen),
	igl_canvas_rect.r_left	= 0;
	igl_canvas_rect.r_top	= 0;
	igl_canvas_rect.r_width	= xpagelen;
	igl_canvas_rect.r_height= ypagelen;
	pw_batch_on(pw);
	pw_lock(pw,igl_canvas_rect);
   }

closedev()
   {
	pw_unlock(pw);
	pw_batch_off(pw);
	window_fit (canvas) ;
	window_fit (screen_frame) ;
	window_main_loop (screen_frame) ;
   }

erase()
   {
   }
