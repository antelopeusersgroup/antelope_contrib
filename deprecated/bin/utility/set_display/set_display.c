#include <stdlib.h>
#include <stdio.h>
#include <strings.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include "elog.h"

static void 
usage()
{
	elog_die( 1, "Usage: set_display [-o] [-a #]\n" );
}

int
main( int argc, char **argv )
{
	Display *display;
	char	*display_name = NULL;
	char	*s;
	int	this_screen; 
	int	next_screen; 
	int	use_screen; 
	int	screen_count;
	int	c;
	int	avoid = -1;
	int	opposing = 0;

	while( ( c = getopt( argc, argv, "a:o" ) ) != -1 ) {
		switch( c ) {
		case 'a':
			avoid = atoi( optarg );
			break;
		case 'o':
			opposing++;
			break;
		case '?': 
			usage();
			break;
		}
	}

	if( argc - optind > 0 ) {
		usage();	
	}

	/* Suppress error messages about inability to connect to display */
	fclose( stderr );

	display = XOpenDisplay( display_name );

	if( display == (Display *) NULL ) {
		exit( 0 );
	}

	screen_count = ScreenCount( display );
	this_screen = DefaultScreen( display );

	next_screen = ( this_screen + 1 ) % screen_count;

	if( avoid == next_screen ) {

		use_screen = this_screen;

	} else if( avoid == this_screen  ) {

		use_screen = next_screen;

	} else if( opposing ) {

		use_screen = next_screen;

	} else {

		use_screen = this_screen;
	}

	s = strdup( DisplayString( display ) );
	s[strlen(s)-1] = '\0';
	sprintf( s, "%s%d", s, use_screen );

	printf( "-display %s", s );

	XCloseDisplay( display );

	return 0;
}
