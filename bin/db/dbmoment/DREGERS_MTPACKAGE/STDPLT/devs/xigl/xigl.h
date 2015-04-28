#define		debug(str)	fprintf (stderr, str);

#ifdef	DEFINE
#define	DECLARE
#else
#define	DECLARE		extern
#endif

/* X information							*/
#include <X11/Xlib.h>
DECLARE	Display		*display;	/* X display			*/
DECLARE	Screen		*xscreen;	/* X screen			*/
DECLARE	int		depth;		/* depth of screen		*/
DECLARE	Window		canvas_xid;	/* xid of canvas paint window	*/
DECLARE	Pixmap		xid;		/* xid of our pixmap		*/
DECLARE	GC		gc;		/* graphics context		*/
DECLARE	XGCValues	gc_val;		/* graphics context values	*/
DECLARE	Server_image	igl_icon_image;	/* image of icon		*/
DECLARE	Icon		igl_icon;	/* icon for program		*/
DECLARE	XColor		xcolor[256];	/* X colormap structure		*/

/* XView information.							*/
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/cms.h>
DECLARE	Frame		screen_frame ;	/* base frame for program	*/
DECLARE	Canvas		canvas ;	/* canvas for graphics image	*/
DECLARE	Menu		igl_menu;	/* main program menu		*/
DECLARE	Menu_item	NextPlot_Item;	/* menu item for next plot	*/
DECLARE	Cms		cms;		/* color map			*/

/* Local information.							*/

#include <setjmp.h>
DECLARE	int		ncolors;	/* # of colors for canvas	*/

struct PATTERN_INFO {
	int	pixmap_is_current;	/* is pixmap up-to-date?	*/
	Pixmap	pixmap;			/* pointer to pattern pixmap	*/
};
DECLARE	jmp_buf		env;		/* setjmp/longjmp environment	*/

#define	NPATTERNS	256
DECLARE struct PATTERN_INFO	pattern_info[NPATTERNS];
