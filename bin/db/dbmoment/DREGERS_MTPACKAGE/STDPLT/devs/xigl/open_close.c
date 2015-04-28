#include	"../com/global.h"
#include	<stdio.h>
#include	"devpar.h"

#include <xview/scrollbar.h>
#include <xview/notice.h>
#include <xview/cursor.h>
#include <xview/icon.h>
#include <xview/window.h>
#include <xview/xv_xrect.h>
#include <xview/screen.h>

#define	DEFINE
#include "xigl.h"

/* XView datatypes.							*/

Icon		igl_icon;	/* icon for program			*/
Scrollbar	sb1, sb2;	/* horizontal and vertical scrollbars	*/
Xv_Cursor	cursor;		/* cursor				*/


short igl_icon_bits[] = {
#include "xigl.icon"
};

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

int screen_width;
int screen_height;

debug_event (event)
Event *event;
{
	fprintf (stderr, "event = %d\n", event->ie_code);
}

void
canvas_repaint_proc (canvas, paint_window, display, canvas_xid, xrects)
Canvas canvas;
Xv_Window paint_window;
Display *display;
Window canvas_xid;
Xv_xrectlist *xrects;
    {
	debug ("entering canvas_repaint_proc...\n");
	if (XV_OK != xv_set (cms, CMS_X_COLORS, xcolor) ) {
		fprintf (stderr, "error settting colormap\n");
	}
	XCopyArea (display, xid, canvas_xid, gc, 
		   0, 0,
		   xpagelen, ypagelen,
		   0, 0);
	debug ("leaving canvas_repaint_proc...\n");
    }

void
canvas_resize_proc (canvas, width, height)
Canvas canvas;
int width;
int height;
    {
	debug ("entering canvas_resize_proc...\n");
	screen_width = width;
	screen_height = height;
	debug ("leaving canvas_resize_proc...\n");
    }

void
igl_event_proc(win,event,arg)

Xv_Window win;
Event *event;
Notify_arg arg;
   {
	static long last_x, last_y;
	int action;
	int action_event;

	debug ("entering igl_event_proc...\n");
	action_event = event_action(event);
	action = event->action;

	fprintf (stderr, "event_action is: %d\n", action_event);

	if (event_is_ascii(event)) {
		debug ("ascii event\n");
	}
	else {	
		switch (action_event) {

		case ACTION_SELECT:	/* "select" (left) button	*/
		debug ("ACTION_SELECT\n");
/*::
		last_x = event_x(event);
		last_y = event_y(event);
::*/
		break;
		case ACTION_ADJUST:	/* "adjust" (middle) button	*/
		debug ("ACTION_ADJUST\n");
/*::
		xv_destroy_safe(screen_frame);
::*/
		break;

		case ACTION_MENU:	/* "menu" (right) button	*/
		debug ("ACTION_MENU\n");
		menu_show(igl_menu, win, event, NULL);
		break;

		default:
		fprintf (stderr, "no match on action: %d\n", action);
		}

		switch (action) {
/*::
		case LOC_DRAG:
		debug ("ACTION_DRAG\n");
		if (xv_get(canvas,WIN_EVENT_STATE,MS_LEFT)) {
			xv_set(sb1, last_y - event_y(event) +
				scrollbar_get(sb1,SCROLL_VIEW_START));
			scrollbar_scroll_to(sb2, last_x - event_x(event) +
				scrollbar_get(sb2,SCROLL_VIEW_START));

		}
::*/
		}
	}
	debug ("leaving igl_event_proc...\n");
   }

void
invisible_proc (menu, item)
Menu menu;
Menu_item item;
{
	debug ("invisible button\n");
	xv_set (screen_frame, XV_X, -50, XV_Y, -50, NULL);
}

void
visible_proc (menu, item)
Menu menu;
Menu_item item;
{
	debug ("visible button\n");
	xv_set (screen_frame, XV_X, 0, XV_Y, 0, NULL);
}

void
quit_proc (menu, item)
Menu menu;
Menu_item item;
{
	
	xv_destroy_safe(screen_frame);
}

void
next_plot_proc (menu, item)
Menu menu;
Menu_item item;
{
	int more;
	int saved_color;

	debug ("entering next plot proc...\n");
	saved_color = gc_val.foreground;

	gc_val.foreground = gc_val.background;
	XChangeGC (display, gc, GCForeground, &gc_val);
	XFillRectangle (display, xid , gc, 0, 0, xpagelen, ypagelen);
	gc_val.foreground = saved_color;
	XChangeGC (display, gc, GCForeground, &gc_val);

	more = readcom(stdin,1);
	if (more == 0) menu_set(item,
				MENU_INACTIVE,		TRUE,
				NULL);
	if (XV_OK != xv_set (cms, CMS_X_COLORS, xcolor) ) {
		fprintf (stderr, "error settting colormap\n");
	}
	XCopyArea (display, xid, canvas_xid, gc, 
		   0, 0,
		   xpagelen, ypagelen,
		   0, 0);
/*::
	debug ("longjump from next plot proc...\n");
	longjmp(env, 1);
::*/
	debug ("entering next plot proc...\n");
}

opendev()
   {
	int		screen_number;
	Xv_Screen	xv_screen;
	int saved_color;

	/* Create base frame, canvas, and scrollbars.			*/

	igl_icon_image = (Server_image)
		xv_create (NULL, SERVER_IMAGE,
			   XV_WIDTH,	64,
			   XV_HEIGHT,	64,
			   SERVER_IMAGE_BITS,	igl_icon_bits,
			   NULL);
	igl_icon = (Icon)xv_create(NULL, ICON, 
				   ICON_IMAGE, igl_icon_image, 
				   NULL);

	screen_frame = xv_create (NULL, FRAME,
				  FRAME_ICON,		igl_icon, 
				  XV_X,			0,
				  XV_Y,			0,
				  XV_WIDTH,	100,
				  XV_HEIGHT,	200,
				  WIN_DYNAMIC_VISUAL,	TRUE,
				  NULL) ;


	display = (Display *) xv_get (screen_frame, XV_DISPLAY);
	xv_screen =  (Xv_Screen) XV_SCREEN_FROM_WINDOW (screen_frame);
	screen_number =(int) xv_get (xv_screen, SCREEN_NUMBER);
	xscreen = XScreenOfDisplay (display, screen_number);

	/* Get desired size of window.				*/
	if (get_fb_dim (xscreen, &screen_width, &screen_height) != 0) {	
		screen_width = SCREEN_WIDTH;
		screen_height = SCREEN_HEIGHT;
	}

	if (XV_OK != xv_set (screen_frame,
			     XV_WIDTH,	screen_width,
			     XV_HEIGHT,	screen_height,
			     NULL))
	fprintf (stderr, "unable to set width and height\n");

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


	/* Get information to create graphics context and colormap.	*/

	depth = XDefaultDepthOfScreen (xscreen);
	if ( (depth > 1) && (!bw_only) ) {
		colordev = 1;
		pencolor = 1;
		ncolors = 1 << depth;
		ncolors = (ncolors > 256) ? 256 : ncolors;
		cms = (Cms)xv_create( NULL, CMS,
				     CMS_SIZE, ncolors,
				     CMS_TYPE, XV_DYNAMIC_CMS,
				     NULL);
	}
	else {
		colordev = 0;
		pencolor = 1;
		ncolors = 2;
	}

	/* Create icon for program.					*/

	canvas = xv_create (screen_frame, CANVAS, 
			    CANVAS_REPAINT_PROC,	canvas_repaint_proc,
			    CANVAS_RESIZE_PROC,		canvas_resize_proc,
			    CANVAS_X_PAINT_WINDOW,	TRUE,
			    CANVAS_AUTO_SHRINK, 	FALSE,
			    CANVAS_WIDTH, 		xpagelen,
			    CANVAS_HEIGHT, 		ypagelen,
			    WIN_DYNAMIC_VISUAL,		TRUE,
			    WIN_CMS,			cms,
			    NULL) ;

	xv_set (canvas_paint_window(canvas),
		WIN_EVENT_PROC,		igl_event_proc,
		WIN_CONSUME_EVENTS,		
			WIN_ASCII_EVENTS,
			WIN_MOUSE_BUTTONS,
			NULL,
		NULL);

	sb1 = xv_create(canvas, SCROLLBAR, 
			SCROLLBAR_DIRECTION, SCROLLBAR_VERTICAL,
			NULL);
	sb2 = xv_create(canvas, SCROLLBAR, 
			SCROLLBAR_DIRECTION, SCROLLBAR_HORIZONTAL,
			NULL);

	/* Create a pixmap into which we can draw. 			*/
	/* We will draw into this pixmap, and then copy it to the	*/
	/* canvas when it becomes exposed.  Otherwise, we have to	*/
	/* be able to recreate the picture when window damage occurs.	*/


	canvas_xid = (Window) xv_get(canvas_paint_window(canvas), XV_XID);
	xid = XCreatePixmap (display, canvas_xid, 
				     xpagelen, ypagelen, depth);


	/* Create graphics context for our writeable pixmap.		*/

/*::
	gc_val.foreground = ncolors-1;
	gc_val.background = 0;
::*/
	gc_val.foreground = 0;
	gc_val.background = ncolors-1;
	gc = XCreateGC (display, xid, GCForeground | GCBackground,
			&gc_val);

	/* Initialize the pixmap to the background color.		*/

	gc_val.foreground = gc_val.background;
	XChangeGC (display, gc, GCForeground, &gc_val);
	XFillRectangle (display, xid , gc, 0, 0, xpagelen, ypagelen);
	gc_val.foreground = saved_color;
	XChangeGC (display, gc, GCForeground, &gc_val);

	/* Create basic IGL menu.					*/

	igl_menu = menu_create(
		MENU_ACTION_ITEM,	"visible",	visible_proc,
		MENU_ACTION_ITEM,	"invisible",	invisible_proc,
		MENU_ACTION_ITEM,	"next plot",	next_plot_proc,
		MENU_ACTION_ITEM,	"quit",		quit_proc,
		0);

	NextPlot_Item = menu_get(igl_menu, MENU_NTH_ITEM, 3);

	debug ("leaving opendev...\n");
   }

closedev()
   {
	
	debug ("entering closedev...\n");
	xv_destroy_safe(screen_frame);
	debug ("leaving closedev...\n");
   }

erase() {}

