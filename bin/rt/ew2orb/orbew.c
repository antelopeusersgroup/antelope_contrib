/*
 * orbew.c
 *
 * Copyright (c) 2003-2005 Lindquist Consulting, Inc.
 * All rights reserved. 
 *                                                                     
 * Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
 * 
 * This software may be used freely in any way as long as 
 * the copyright statement above is not removed. 
 *
 */

#include <stdlib.h>
#include "orbew.h"

char	*Default_TYPE_HEARTBEAT = "TYPE_HEARTBEAT";
char	*Default_TYPE_TRACEBUF = "TYPE_TRACEBUF";
char	*Default_TYPE_TRACEBUF2 = "TYPE_TRACEBUF2";
char	*Default_TYPE_TRACE_COMP_UA = "TYPE_TRACE_COMP_UA";

char	Program_loglevel[STRSZ] = DEFAULT_LOGLEVEL;

Earthworm_Info Ewinfo;
Flagdef Flags;

void 
pfreplace( Pf *sourcepf, Pf *destpf, 
	   char *sourcekey, char *destkey, 
	   char *type )
{
	Pf	*pfnew;
	Pf	*pfold;
	int 	rc;

	rc = pfresolve( sourcepf, sourcekey, 0, &pfnew );

	if( rc < 0 || rc == PFINVALID ) {

		return;
	}

	pfold = pfdel( destpf, destkey );

	if( pfold ) {

		pffree( pfold );
	}

	pfput( destpf, destkey, pfdup( pfnew ), PFPF );

	return;
}

void
refresh_earthworm_info() 
{
	Arr	*anarr;
	int	anint;
	char	*akey;
	char	*previous;
	char	setkey[STRSZ];
	Tbl	*keys;
	int	ikey;
	int	rc;

	mutex_lock( &Ewinfo.ew_mutex );

	if( ( rc = pfupdate( Ewinfo.pfname, &Ewinfo.pf ) ) < 0 ) {

		elog_complain( 1, "pfupdate of parameter-file '%s' failed\n",
			  Ewinfo.pfname );
	} 

	if( ( rc > 0 ) && ( Ewinfo.inst_names != 0 ) ) {
		
		freearr( Ewinfo.inst_names, free );
		Ewinfo.inst_names = 0;

		freearr( Ewinfo.inst_ids, 0 );
		Ewinfo.inst_ids = 0;

		freearr( Ewinfo.mod_names, free );
		Ewinfo.mod_names = 0;

		freearr( Ewinfo.mod_ids, 0 );
		Ewinfo.mod_ids = 0;

		freearr( Ewinfo.type_names, free );
		Ewinfo.type_names = 0;

		freearr( Ewinfo.type_ids, 0 );
		Ewinfo.type_ids = 0;
	}

	if( Ewinfo.inst_names == 0 ) {

		Ewinfo.inst_names = newarr( 0 );
		Ewinfo.inst_ids = newarr( 0 );
	
		Ewinfo.mod_names = newarr( 0 );
		Ewinfo.mod_ids = newarr( 0 );

		Ewinfo.type_names = newarr( 0 );
		Ewinfo.type_ids = newarr( 0 );

		sprintf( setkey, "%03d", DEFAULT_TYPE_HEARTBEAT );
		setarr( Ewinfo.type_names, setkey, 
					strdup( Default_TYPE_HEARTBEAT ) );
		setarr( Ewinfo.type_ids, Default_TYPE_HEARTBEAT, 
				 	(void *) DEFAULT_TYPE_HEARTBEAT );

		sprintf( setkey, "%03d", DEFAULT_TYPE_TRACEBUF );
		setarr( Ewinfo.type_names, setkey, 
					strdup( Default_TYPE_TRACEBUF ) );
		setarr( Ewinfo.type_ids, Default_TYPE_TRACEBUF, 
				 	(void *) DEFAULT_TYPE_TRACEBUF );

		sprintf( setkey, "%03d", DEFAULT_TYPE_TRACEBUF2 );
		setarr( Ewinfo.type_names, setkey, 
					strdup( Default_TYPE_TRACEBUF2 ) );
		setarr( Ewinfo.type_ids, Default_TYPE_TRACEBUF2, 
				 	(void *) DEFAULT_TYPE_TRACEBUF2 );

		sprintf( setkey, "%03d", DEFAULT_TYPE_TRACE_COMP_UA );
		setarr( Ewinfo.type_names, setkey, 
					strdup( Default_TYPE_TRACE_COMP_UA ) );
		setarr( Ewinfo.type_ids, Default_TYPE_HEARTBEAT, 
				 	(void *) DEFAULT_TYPE_TRACE_COMP_UA );
	}

	if( rc > 0 && Ewinfo.pf != 0 ) {

		if( ( ( anarr = 
			pfget_arr( Ewinfo.pf, "Installations" ) ) != NULL ) ) {

			keys = keysarr( anarr );

			for( ikey = 0; ikey < maxtbl( keys ); ikey++ ) {
				
				akey = gettbl( keys, ikey );

				anint = atoi( getarr( anarr, akey ) );

				sprintf( setkey, "%03d", anint );

				previous = getarr( Ewinfo.inst_names, setkey );

				if( previous != NULL ) {
					
					free( previous );
				}

				setarr( Ewinfo.inst_names, setkey, strdup( akey ) );
				setarr( Ewinfo.inst_ids, akey, (void *) anint );
			}

			freetbl( keys, 0 );

			freearr( anarr, 0 ); 
		}

		if( ( ( anarr = 
			pfget_arr( Ewinfo.pf, "Modules" ) ) != NULL ) ) {

			keys = keysarr( anarr );

			for( ikey = 0; ikey < maxtbl( keys ); ikey++ ) {
				
				akey = gettbl( keys, ikey );

				anint = atoi( getarr( anarr, akey ) );

				sprintf( setkey, "%03d", anint );

				previous = getarr( Ewinfo.mod_names, setkey );

				if( previous != NULL ) {
					
					free( previous );
				}

				setarr( Ewinfo.mod_names, setkey, strdup( akey ) );
				setarr( Ewinfo.mod_ids, akey, (void *) anint );
			}

			freetbl( keys, 0 );

			freearr( anarr, 0 ); 
		}

		if( ( ( anarr = 
			pfget_arr( Ewinfo.pf, "Messages" ) ) != NULL ) ) {

			keys = keysarr( anarr );

			for( ikey = 0; ikey < maxtbl( keys ); ikey++ ) {
				
				akey = gettbl( keys, ikey );

				anint = atoi( getarr( anarr, akey ) );

				sprintf( setkey, "%03d", anint );

				previous = getarr( Ewinfo.type_names, setkey );

				if( previous != NULL ) {
					
					free( previous );
				}

				setarr( Ewinfo.type_names, setkey, strdup( akey ) );
				setarr( Ewinfo.type_ids, akey, (void *) anint );
			}

			freetbl( keys, 0 );

			freearr( anarr, 0 ); 
		}
	}

	mutex_unlock( &Ewinfo.ew_mutex );

	return;
}

void
set_program_loglevel( Pf *pf )
{
	char	*ll;
	enum Loglevel loglevel;
	enum Loglevel old;

	old = translate_loglevel( Program_loglevel );

	if( ( ll = pfget_string( pf, "program_loglevel" ) ) == NULL ) {

		strcpy( Program_loglevel, DEFAULT_LOGLEVEL );

	} else {
		
		strcpy( Program_loglevel, ll );
	}

	loglevel = translate_loglevel( Program_loglevel );

	if( loglevel == QUIET ) {

		if( loglevel != old ) {

			elog_complain( 0, "entering 'quiet' mode\n" );
		}

		Flags.verbose = 0;
		Flags.VeryVerbose = 0;

	} else if( loglevel == VERBOSE ) {

		if( loglevel != old ) {

			elog_complain( 0, "entering 'verbose' mode\n" );
		}

		Flags.verbose = 1;
		Flags.VeryVerbose = 0;

	} else if( loglevel == VERYVERBOSE ) {

		if( loglevel != old ) {

			elog_complain( 0, "entering 'veryverbose' mode\n" );
		}

		Flags.verbose = 1;
		Flags.VeryVerbose = 1;
	}

	return;
}

enum Loglevel
translate_loglevel( char *loglevel )
{
	if( ! strcmp( loglevel, "quiet" ) ) {

		return QUIET;

	} else if( ! strcmp( loglevel, "verbose" ) ) {

		return VERBOSE;

	} else if( ! strcmp( loglevel, "veryverbose" ) ) {

		return VERYVERBOSE;

	} else {

		elog_complain( 0, 
			"Unknown loglevel '%s' from parameter file; "
			"setting to 'verbose'\n", 
			loglevel );

		return VERBOSE;
	}
}

void
ewlogo_tologo( char *inststr, char *modstr, char *typestr,
	       int *inst, int *mod, int *type )
{
	mutex_lock( &Ewinfo.ew_mutex );

	*inst = *mod = *type = 0;

	if( ! strcmp( inststr, "INST_WILDCARD" ) ) {

		*inst = INST_WILDCARD;

	} else if( ( *inst = (int) getarr( Ewinfo.inst_ids, inststr ) ) != 0 ) {
		
		; /* Success */

	}  else {
	
		elog_complain( 0, "Failed to translate '%s'; "
			     "please update %s.pf\n", 
			     inststr, DEFAULT_EARTHWORM_PFNAME );
	}

	if( ! strcmp( modstr, "MOD_WILDCARD" ) ) {

		*mod = MOD_WILDCARD;

	} else if( ( *mod = (int) getarr( Ewinfo.mod_ids, modstr ) ) != 0 ) {
		
		; /* Success */

	}  else {
	
		elog_complain( 0, "Failed to translate '%s'; "
			     "please update %s.pf\n", 
			     modstr, DEFAULT_EARTHWORM_PFNAME );
	}

	if( ! strcmp( typestr, "TYPE_WILDCARD" ) ) {

		*type = TYPE_WILDCARD;

	} else if( ( *type = (int) getarr( Ewinfo.type_ids, typestr ) ) != 0 ) {
		
		; /* Success */

	}  else {
	
		elog_complain( 0, "Failed to translate '%s'; "
			     "please update %s.pf\n", 
			     typestr, DEFAULT_EARTHWORM_PFNAME );
	}

	mutex_unlock( &Ewinfo.ew_mutex );

	return;
}

void
ewlogo_tostrings( int inst, int mod, int type, 
		 char *inststr, char *modstr, char *typestr )
{
	char	*istring;
	char	*mstring;
	char	*tstring;
	char	instkey[STRSZ];
	char	modkey[STRSZ];
	char	typekey[STRSZ];

	sprintf( instkey, "%03d", inst );
	sprintf( modkey, "%03d", mod );
	sprintf( typekey, "%03d", type );

	mutex_lock( &Ewinfo.ew_mutex );

	if( ( istring = getarr( Ewinfo.inst_names, instkey ) ) != NULL ) {
			
		strcpy( inststr, istring );

	} else {
			
		sprintf( inststr, "INST_%03d", inst );
	}

	if( ( mstring = getarr( Ewinfo.mod_names, modkey ) ) != NULL ) {
		
		strcpy( modstr, mstring );

	} else {
		
		sprintf( modstr, "MOD_%03d", mod );
	}

	if( ( tstring = getarr( Ewinfo.type_names, typekey ) ) != NULL ) {
		
		strcpy( typestr, tstring );

	} else {
		
		sprintf( typestr, "TYPE_%03d", type );
	}

	mutex_unlock( &Ewinfo.ew_mutex );

	return;
}

Tbl *
healthy_morphlist( Tbl *morphlist ) 
{
	Tbl	*new_morphlist;
	Tbl	*indices;
	int	i;
	char	*cp;
	char	*entry;

	if( morphlist == (Tbl *) NULL ) {
		return morphlist;
	} 

	new_morphlist = duptbl( morphlist, (void *(*)()) strdup );

	indices = greptbl( "^[/\"\'].*", new_morphlist );
	
	if( maxtbl( indices ) > 0 ) {

		for( i=maxtbl(indices)-1; i>=0; i-- ) {

			entry = gettbl( new_morphlist, (int) gettbl( indices, i ) );

			memmove( entry, entry + 1, strlen( entry ) );
			cp = entry + strlen( entry );
			while( cp > entry ) {
				if( *(cp-1) != '\\' && 
				    (*cp == '/' || *cp == '"' || *cp == '\'') ) {
					*cp = ' ';
				}
				cp--;	
			}

			strtrim( entry );
		}	
	}

	freetbl( indices, 0 );

	return new_morphlist;
}
