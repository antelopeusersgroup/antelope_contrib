#include <stdlib.h>
#include <stdio.h>
#include <strings.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

main() {
	Display *display;
	char	*display_name = NULL;
	char	*s;
	int	this_screen; 
	int	next_screen; 
	int	screen_count;

	/* Suppress error messages about inability to connect to display */
	fclose( stderr );

	display = XOpenDisplay( display_name );

	if( display == (Display *) NULL ) {
		exit( 0 );
	}

	screen_count = ScreenCount( display );
	this_screen = DefaultScreen( display );

	next_screen = ( this_screen + 1 ) % screen_count;

	s = strdup( DisplayString( display ) );
	s[strlen(s)-1] = '\0';
	sprintf( s, "%s%d", s, next_screen );

	printf( "-display %s", s );

	XCloseDisplay( display );
}
