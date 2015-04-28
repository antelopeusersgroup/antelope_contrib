#include <fcntl.h>
#include <sys/ioctl.h>
#include <sun/fbio.h>
#include <errno.h>

get_fb_dim (p_screen_width, p_screen_height)
	int *p_screen_width;
	int *p_screen_height;
{
	struct fbgattr	attr_buff;
	int fd;
	int status = 0;

	/* Determine the dimensions of the frame buffer. */
	errno = 0;
	if ((fd = open("/dev/fb", O_RDONLY)) < 0)
		return(errno);
	errno = 0;
	if (ioctl(fd, FBIOGATTR, &attr_buff) < 0)
		if (ioctl(fd, FBIOGTYPE, &attr_buff.fbtype) < 0)
			status = errno;
	(void) close(fd);

	*p_screen_width = attr_buff.fbtype.fb_width;
	*p_screen_height = attr_buff.fbtype.fb_height;
	return(status);
}
