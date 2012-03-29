/*
 *   Copyright (c) 2006 Lindquist Consulting, Inc.
 *   All rights reserved. 
 *                                                                     
 *   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
 *
 *   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
 *   KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 *   WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR 
 *   PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
 *   OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
 *   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 *   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
 *   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * 
 *   This software may be used freely in any way as long as 
 *   the copyright statement above is not removed. 
 *
 */

#include "dbgme.h"

int
delegate_to_perl( Dbptr db, Pf *delegate_pf )
{
	char	scriptfile[FILENAME_MAX];
	char	fifofile[FILENAME_MAX];
	char	pffile[FILENAME_MAX];
	char	*commands;
	char	*mypath;
	char	*cwd;
	FILE	*fp;
	Tbl	*keys;
	char	*key;
	Pf	*pfpart;
	pid_t	pid;
	int	status;
	int	i;

	if( ( mypath = datafile( "PATH", "perl" ) ) == 0 ) {

		elog_complain( 0, "Couldn't find perl on path.\n" );

		return -1;

	} else if( access( mypath, R_OK|X_OK ) ) {

		elog_complain( 0, "Couldn't find perl on path.\n" );
		
		return -1;
	}

	if( ( commands = pfget_string( delegate_pf, "commands" ) ) == NULL ) {

		elog_complain( 0, "No commands &Literal in parameter file\n" );

		return -1;
	}

	cwd = getcwd( NULL, FILENAME_MAX );

	sprintf( scriptfile, "/tmp/dbgme_script_%d_%d.pl", getuid(), getpid() );
	sprintf( fifofile, "/tmp/dbgme_fifo_%d_%d.fifoview", getuid(), getpid() );
	sprintf( pffile, "/tmp/dbgme_pf_%d_%d.pf", getuid(), getpid() );

	mkfifo( fifofile, 0775 );

	fp = fopen( pffile, "w" ); 
	keys = pfkeys( delegate_pf );
	for( i = 0; i < maxtbl( keys ); i++ ) {

		key = gettbl( keys, i );

		if( ! strcmp( key, "commands" ) ) {

			continue;
		}

		pfresolve( delegate_pf, key, 0, &pfpart );

		fprintf( fp, "%s ", key );
		pfout( fp, pfpart ); 
	}
	fclose( fp ); 
	
	fp = fopen( scriptfile, "w" );
	fprintf( fp, "@db = dbopen_table( \"%s\", \"r\" );\n", fifofile );
	fprintf( fp, "$pf = \"%s\";\n", pffile );
	fprintf( fp, "%s\n", commands );
	fprintf( fp, "exit\n" );
	fclose( fp );

	if( ( pid = fork() ) == 0 ) {

		if( Verbose ) {

			fprintf( stderr, "Running Perl delegate\n" );

		} else {

			fflush( stderr );
			fclose( stdout );
		}

		execlp( "perl", "perl", "-e", scriptfile, NULL );

	} else {

		fp = fopen( fifofile, "w" );
		dbwrite_view( db, fp );
		fclose( fp );

		wait( &status );
	}

	if( Verbose ) {

		fprintf( stderr, "Perl exit status is %d\n", status );
	}

	free( cwd );

	unlink( scriptfile );
	unlink( fifofile );
	unlink( pffile );

	if( status != 0 ) {

		return -1;

	} else {

		return 0;
	}
}
