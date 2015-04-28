#include <fcntl.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <X11/Xlib.h>

#define	X_FRAME_ALLOWANCE	10
#define	Y_FRAME_ALLOWANCE	30

get_fb_dim (screen, p_screen_width, p_screen_height)
	Screen *screen;
	int *p_screen_width;
	int *p_screen_height;
{
	int status = 0;

	/* Determine the dimensions of the frame buffer. */
	*p_screen_width = XWidthOfScreen(screen) - X_FRAME_ALLOWANCE;
	*p_screen_height = XHeightOfScreen(screen) - Y_FRAME_ALLOWANCE;

	return(status);
}
