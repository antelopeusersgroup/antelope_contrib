/* Copyright (c) 2004 Boulder Real Time Technologies, Inc. */
/* All rights reserved. */
/*                                                                     
/* Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. */
/*
/* This software may be used freely in any way as long as */
/* the copyright statement above is not removed. */

#include "dbgme.h"

int
delegate_to_matlab( Dbptr db, Pf *delegate_pf )
{
	char	scriptfile[FILENAME_MAX];
	char	fifofile[FILENAME_MAX];
	char	pffile[FILENAME_MAX];
	char	cmd[STRSZ];
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

	if( ( mypath = datafile( "PATH", "matlab" ) ) == 0 ) {

		elog_complain( 0, "Couldn't find matlab on path.\n" );

		return -1;

	} else if( access( mypath, R_OK|X_OK ) ) {

		elog_complain( 0, "Couldn't find matlab on path.\n" );
		
		return -1;
	}

	if( ( commands = pfget_string( delegate_pf, "commands" ) ) == NULL ) {

		elog_complain( 0, "No commands &Literal in parameter file\n" );

		return -1;
	}

	cwd = getcwd( NULL, FILENAME_MAX );

	sprintf( scriptfile, "/tmp/dbgme_script_%d_%d.m", getuid(), getpid() );
	sprintf( fifofile, "/tmp/dbgme_fifo_%d_%d", getuid(), getpid() );
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
	fprintf( fp, "addpath('%s')\n", cwd );
	fprintf( fp, "db = dbread_view( '%s' );\n", fifofile );
	fprintf( fp, "pf = dbpf( '%s' );\n", pffile );
	fprintf( fp, "%s\n", commands );
	fprintf( fp, "exit\n" );
	fclose( fp );

	sprintf( cmd, 
		"try, "
			"run %s, "
		"catch, "
			"lasterr, "
			"elog_clear_register('print'), "
			"unix(['kill -9 ' num2str(getpid)]), "
		"end", 
		scriptfile );

	if( ( pid = fork() ) == 0 ) {

		if( Verbose ) {

			fprintf( stderr, "Running Matlab delegate\n" );

		} else {

			fflush( stderr );
			fclose( stdout );
		}

		execlp( "matlab", "matlab", "-nodisplay", "-nojvm",
			"-r", cmd, NULL );

	} else {

		fp = fopen( fifofile, "w" );
		dbwrite_view( db, fp );
		fclose( fp );

		wait( &status );
	}

	if( Verbose ) {

		fprintf( stderr, "Matlab exit status is %d\n", status );
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
