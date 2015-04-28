/*::
10/08/90	doug@seismo.gps.caltech.edu
	Removed lock calles (moved to ifdef code) in an attempt to avoid 
	hanging in the menus under 4.1.
::*/

#include	"../com/global.h"
#include	<stdio.h>
#include	"devpar.h"

#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <suntool/scrollbar.h>
#include <suntool/alert.h>

Frame 	screen_frame ;
Canvas	canvas ;
Pixwin	*pw ;
Icon igl_icon;
Rect igl_canvas_rect;
Scrollbar sb1, sb2;
Menu igl_menu;
Menu_item NextPlot_Item;
Cursor cursor;

static short igl_icon_image[] = {
#include "igltool.icon"
};
mpr_static(igltool_icon, 64, 64, 1, igl_icon_image);

int pencolor;
unsigned char save_red,   save_green,   save_blue,
              same_red=0, same_green=0, same_blue=1;

/* device specifications */
extern float	pixinch	;
extern float	inchpix	;
extern int	xmax	;
extern int	ymax	;
extern int	xpagelen;
extern int	ypagelen;
extern float	xscale  ;
extern float	yscale  ;
extern int	xorigin ;
extern int	yorigin ;
extern int	colordev;

extern int	bw_only;

static int	visible = 1;
caddr_t		invisible_proc(), visible_proc(),
		next_plot_proc(), quit_proc();
void		igl_event_proc();

int screen_width;
int screen_height;

opendev()
   {
	if (get_fb_dim (&screen_width, &screen_height) != 0) {	
		screen_width = SCREEN_WIDTH;
		screen_height = SCREEN_HEIGHT;
	}
	pixinch  *= (float) screen_width / (float) SCREEN_WIDTH;
	inchpix   = 1.0 / pixinch;
	xpagelen *= pixinch / PIXINCH;
	ypagelen *= pixinch / PIXINCH;
	xorigin  *= pixinch / PIXINCH;
	yorigin  *= pixinch / PIXINCH;
	xscale   *= pixinch / PIXINCH;
	yscale   *= pixinch / PIXINCH;
	xmax     *= pixinch / PIXINCH;
	ymax     *= pixinch / PIXINCH;

	igl_icon = icon_create(ICON_IMAGE, &igltool_icon, 0);
	screen_frame = window_create (NULL, FRAME,
				WIN_WIDTH, 		screen_width,
				WIN_HEIGHT, 		screen_height,
				WIN_X,			0,
				WIN_Y,			0,
				FRAME_ICON,		igl_icon,
				0) ;
	sb1 = scrollbar_create(0);
	sb2 = scrollbar_create(0);
	canvas = window_create (screen_frame, CANVAS, 
				CANVAS_AUTO_SHRINK, 	FALSE,
				CANVAS_WIDTH, 		xpagelen,
				CANVAS_HEIGHT, 		ypagelen,
				WIN_VERTICAL_SCROLLBAR, sb1,
				WIN_HORIZONTAL_SCROLLBAR, sb2,
				WIN_EVENT_PROC,		igl_event_proc,
				0) ;
	pw = canvas_pixwin (canvas) ;

	if (pw->pw_pixrect->pr_depth > 1) { /* color monitor */
		if (!bw_only) {
			colordev = 1;
			pencolor = 1;
			pw_setcmsname(pw, "igl_color");
		}
	}
	scrollbar_scroll_to(sb1, ypagelen);
	igl_menu = menu_create(
		MENU_ACTION_ITEM,	"visible",	visible_proc,
		MENU_ACTION_ITEM,	"invisible",	invisible_proc,
		MENU_ACTION_ITEM,	"next plot",	next_plot_proc,
		MENU_ACTION_ITEM,	"quit",		quit_proc,
		0);
	NextPlot_Item = menu_get(igl_menu, MENU_NTH_ITEM, 3);
	igl_canvas_rect.r_left	= 0;
	igl_canvas_rect.r_top	= 0;
	igl_canvas_rect.r_width	= xpagelen;
	igl_canvas_rect.r_height= ypagelen;
#ifdef DOLOCK
	pw_batch_on(pw);
	pw_lock(pw,igl_canvas_rect);
#endif
   }

closedev()
   {
	window_destroy(screen_frame);
   }

erase() {}

void
igl_event_proc(win,event,arg)
Window win;
Event *event;
caddr_t arg;
   {
	static long last_x, last_y;

	switch (event_action(event)) {

	case LOC_DRAG:
		if (window_get(canvas,WIN_EVENT_STATE,MS_LEFT)) {
			scrollbar_scroll_to(sb1, last_y - event_y(event) +
				scrollbar_get(sb1,SCROLL_VIEW_START));
			scrollbar_scroll_to(sb2, last_x - event_x(event) +
				scrollbar_get(sb2,SCROLL_VIEW_START));
		}
		break;

	case MS_LEFT:
		last_x = event_x(event);
		last_y = event_y(event);
		break;

	case MS_MIDDLE:
		window_destroy(screen_frame);
		break;

	case MS_RIGHT:
		menu_show(igl_menu,win,canvas_window_event(canvas,event),0);
		break;
	}
   }

caddr_t
invisible_proc(men,menitem)
Menu men;
Menu_item menitem;
{
	if ( !visible ) return(0);
	if (colordev) {
		pw_getcolormap(pw,255,1,&save_red,&save_green,&save_blue);
		pw_putcolormap(pw,255,1,&same_red,&same_green,&same_blue);
	}
	scrollbar_set(sb1,
		SCROLL_BAR_DISPLAY_LEVEL,	SCROLL_NEVER,
		SCROLL_BUBBLE_DISPLAY_LEVEL,	SCROLL_NEVER,
		0);
	scrollbar_set(sb2,
		SCROLL_BAR_DISPLAY_LEVEL,	SCROLL_NEVER,
		SCROLL_BUBBLE_DISPLAY_LEVEL,	SCROLL_NEVER,
		0);
	scrollbar_paint_clear(sb1);
	scrollbar_paint_clear(sb2);
	cursor = window_get(canvas,WIN_CURSOR);
	cursor_set(cursor,CURSOR_SHOW_CURSOR,FALSE,0);
	window_set(canvas,WIN_CURSOR,cursor,0);
	visible = 0;
	return(0);
}

caddr_t
visible_proc(men,menitem)
Menu men;
Menu_item menitem;
{
	if ( visible ) return(0);
	if (colordev)
		pw_putcolormap(pw,255,1,&save_red,&save_green,&save_blue);
	scrollbar_set(sb1,
		SCROLL_BAR_DISPLAY_LEVEL,	SCROLL_ALWAYS,
		SCROLL_BUBBLE_DISPLAY_LEVEL,	SCROLL_ALWAYS,
		0);
	scrollbar_set(sb2,
		SCROLL_BAR_DISPLAY_LEVEL,	SCROLL_ALWAYS,
		SCROLL_BUBBLE_DISPLAY_LEVEL,	SCROLL_ALWAYS,
		0);
	scrollbar_paint_clear(sb1);
	scrollbar_paint_clear(sb2);
	cursor = window_get(canvas,WIN_CURSOR);
	cursor_set(cursor,CURSOR_SHOW_CURSOR,TRUE,0);
	window_set(canvas,WIN_CURSOR,cursor,0);
	visible = 1;
	return(0);
}

caddr_t
quit_proc(men,menitem)
Menu men;
Menu_item menitem;
{
	window_destroy(screen_frame);
	return(0);
}

caddr_t
next_plot_proc(men,menitem)
Menu men;
Menu_item menitem;
{
	int more, answer;

#ifdef DOLOCK
	pw_batch_on(pw);
	pw_lock(pw,igl_canvas_rect); 
#endif
	/* get confirmation */
	answer = alert_prompt(screen_frame,(Event *) NULL,
		ALERT_MESSAGE_STRINGS,
			"This will destroy current plot.",
			"Do you want to proceed?",
			0,
		ALERT_BUTTON_YES,	"Do next plot",
		ALERT_BUTTON_NO,	"cancel",
		0);
	if (answer == ALERT_NO) return(0);
	/* clear background */
	pw_writebackground(pw,0,0,xpagelen,ypagelen,PIX_SRC | PIX_COLOR(0));
	/* draw plot */
	more = readcom(stdin,1);
	if (more == 0) menu_set(menitem,
				MENU_INACTIVE,		TRUE,
				0);
#ifdef DOLOCK
	pw_unlock(pw); 
	pw_batch_off(pw);
#endif
	return(0);
}
