/*
 *   Copyright (c) 2007-2008 Lindquist Consulting, Inc.
 *   All rights reserved. 
 *                                                                     
 *   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
 *
 *   This software is licensed under the New BSD license: 
 *
 *   Redistribution and use in source and binary forms,
 *   with or without modification, are permitted provided
 *   that the following conditions are met:
 *   
 *   * Redistributions of source code must retain the above
 *   copyright notice, this list of conditions and the
 *   following disclaimer.
 *   
 *   * Redistributions in binary form must reproduce the
 *   above copyright notice, this list of conditions and
 *   the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 *   
 *   * Neither the name of Lindquist Consulting, Inc. nor
 *   the names of its contributors may be used to endorse
 *   or promote products derived from this software without
 *   specific prior written permission.

 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 *   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 *   WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *   PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
 *   THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY
 *   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 *   USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 *   IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
 *   USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *   POSSIBILITY OF SUCH DAMAGE.
 *
 */

#include <stdlib.h>
#include <string.h>
#include "Python.h"
#include "stock.h"
#include "pfxml.h"

#ifdef __APPLE__

/* The scaffold below works around a problem on Darwin */

char **environ;
char *__progname = "Python";

/* Include these for 4.11 libdeviants workaround on Darwin */

#include "deviants.h"
#include "sysdata.h"

extern void proc2pidstat( void *kinfo, void *process );

#endif

char	**_stock_argv = NULL;
int	_stock_argc = 0;

static PyObject *python_elog_init( PyObject *self, PyObject *args );
static PyObject *python_elog_notify( PyObject *self, PyObject *args );
static PyObject *python_elog_complain( PyObject *self, PyObject *args );
static PyObject *python_elog_die( PyObject *self, PyObject *args );
static PyObject *python_pfget_string( PyObject *self, PyObject *args );
static PyObject *python_pfget_int( PyObject *self, PyObject *args );
static PyObject *python_pfget_double( PyObject *self, PyObject *args );
static PyObject *python_pfget_size( PyObject *self, PyObject *args );
static PyObject *python_pfget_boolean( PyObject *self, PyObject *args );
static PyObject *python_pfget_time( PyObject *self, PyObject *args );
static PyObject *python_pfget_arr( PyObject *self, PyObject *args );
static PyObject *python_pfget_tbl( PyObject *self, PyObject *args );
static PyObject *python_pfget( PyObject *self, PyObject *args );
static PyObject *python_pfupdate( PyObject *self, PyObject *args );
static PyObject *python_pffiles( PyObject *self, PyObject *args );
static PyObject *python_pf2string( PyObject *self, PyObject *args );
static PyObject *python_pf2xml( PyObject *self, PyObject *args );
static PyObject *python_strtime( PyObject *self, PyObject *args );
static PyObject *python_strtdelta( PyObject *self, PyObject *args );
static PyObject *python_strydtime( PyObject *self, PyObject *args );
static PyObject *python_strtime( PyObject *self, PyObject *args );
static PyObject *python_strdate( PyObject *self, PyObject *args );
static PyObject *python_strlocaltime( PyObject *self, PyObject *args );
static PyObject *python_strlocalydtime( PyObject *self, PyObject *args );
static PyObject *python_strlocaldate( PyObject *self, PyObject *args );
static PyObject *python_str2epoch( PyObject *self, PyObject *args );
static PyObject *python_epoch2str( PyObject *self, PyObject *args );
static PyObject *python_epoch( PyObject *self, PyObject *args );
static PyObject *python_yearday( PyObject *self, PyObject *args );
static PyObject *python_now( PyObject *self, PyObject *args );
static PyObject *pf2PyObject( Pf *pfvalue );
static PyObject *string2PyObject( char *s );
static PyObject *strtbl2PyObject( Tbl *atbl );
static int parse_from_Boolean( PyObject *obj, void *addr );
static int parse_to_strtbl( PyObject *obj, void *addr );
PyMODINIT_FUNC init_stock( void );

static struct PyMethodDef stock_methods[] = {
	{ "_elog_init",   	python_elog_init,   	METH_VARARGS, "Initialize the Antelope error log" },
	{ "_elog_notify",   	python_elog_notify,   	METH_VARARGS, "Put a notification message on the Antelope error log" },
	{ "_elog_complain",   	python_elog_complain,  	METH_VARARGS, "Put a warning message on the Antelope error log" },
	{ "_elog_die",   	python_elog_die,   	METH_VARARGS, "Put a fatal message on the Antelope error log and exit" },
	{ "_pfget_string",   	python_pfget_string,   	METH_VARARGS, "Get a string value from a parameter file" },
	{ "_pfget_int",   	python_pfget_int,   	METH_VARARGS, "Get an integer value from a parameter file" },
	{ "_pfget_double",   	python_pfget_double,   	METH_VARARGS, "Get a double value from a parameter file" },
	{ "_pfget_size",   	python_pfget_size,   	METH_VARARGS, "Get a size value from a parameter file" },
	{ "_pfget_boolean",   	python_pfget_boolean,   METH_VARARGS, "Get a boolean value from a parameter file" },
	{ "_pfget_time",   	python_pfget_time,   	METH_VARARGS, "Get a time value from a parameter file" },
	{ "_pfget_arr",   	python_pfget_arr,   	METH_VARARGS, "Get an array value from a parameter file" },
	{ "_pfget_tbl",   	python_pfget_tbl,   	METH_VARARGS, "Get a table value from a parameter file" },
	{ "_pfget",   		python_pfget,   	METH_VARARGS, "Get a value from a parameter file" },
	{ "_pfupdate", 		python_pfupdate,   	METH_VARARGS, "Reread and update a parameter file" },
	{ "_pffiles", 		python_pffiles,   	METH_VARARGS, "Return a list of parameter path names" },
	{ "_pf2string",		python_pf2string,   	METH_VARARGS, "Convert a parameter-file to a string" },
	{ "_pf2xml",		python_pf2xml,   	METH_VARARGS, "Convert a parameter-file to an xml string" },
	{ "_strtdelta",   	python_strtdelta,   	METH_VARARGS, "Convert a time-difference to a string" },
	{ "_strydtime",   	python_strydtime,   	METH_VARARGS, "Convert an epoch time to a string date and time including julian day" },
	{ "_strdate",   	python_strdate,   	METH_VARARGS, "Convert an epoch time to a string date" },
	{ "_strlocaltime",   	python_strlocaltime,   	METH_VARARGS, "Convert an epoch time to a local date and time string" },
	{ "_strlocalydtime",   	python_strlocalydtime,   	METH_VARARGS, "Convert an epoch time to a local date and time string with julian day" },
	{ "_strlocaldate",   	python_strlocaldate,   	METH_VARARGS, "Convert an epoch time to a string date in local time zone" },
	{ "_strtime",   	python_strtime,   	METH_VARARGS, "Compute a string representation of epoch time" },
	{ "_str2epoch",   	python_str2epoch,   	METH_VARARGS, "Compute an epoch time from a string" },
	{ "_epoch2str",   	python_epoch2str,   	METH_VARARGS, "Convert an epoch time to a string" },
	{ "_epoch",	   	python_epoch,   	METH_VARARGS, "Convert a year-day value to an epoch time" },
	{ "_yearday",   	python_yearday,   	METH_VARARGS, "Convert epoch time to a year-day value" },
	{ "_now",   		python_now,   		METH_VARARGS, "Return epoch time for local system clock" },
	{ NULL, NULL, 0, NULL }
};

#ifdef __APPLE__

void 
proc2pidstat ( void *kinfo, void *process) {
	/* Resolve Antelope 4.11 problem with unresolved symbol in libdeviants under Darwin */
	return;
}

#endif

static int
parse_from_Boolean( PyObject *obj, void *addr )
{
	int	*value = (int *) addr;

	if( obj == Py_False ) {

		*value = 0;

		return 1;

	} else if( obj == Py_True ) {

		*value = -1;

		return 1;

	} else {

		PyErr_SetString( PyExc_TypeError, 
			"Attempt to coerce non-Boolean value into Boolean" );
	}

	return 0;
}

static int
parse_to_strtbl( PyObject *obj, void *addr )
{
	Tbl	**atbl = (Tbl **) addr;
	PyObject *seqobj;
	int	nitems = 0;
	int	iitem;
	char	*astring;
	char	errmsg[STRSZ];

	if( obj == Py_None ) {

		*atbl = NULL;

		return 1;
	} 

	if( PyString_Check( obj ) ) {

		*atbl = strtbl( strdup( PyString_AsString( obj ) ), NULL );

		return 1;
	}

	if( ! PySequence_Check( obj ) ) {

		PyErr_SetString( PyExc_TypeError, 
			"Attempt to convert sequence to table of strings failed: input argument is not a sequence" );

		*atbl = NULL;

		return 0;
	}

	nitems = PySequence_Size( obj );

	*atbl = newtbl( nitems );

	for( iitem = 0; iitem < nitems; iitem++ ) {
		
		seqobj = PySequence_GetItem( obj, iitem );

		if( ! seqobj ) {

			freetbl( *atbl, free );

			*atbl = NULL;

			sprintf( errmsg, 
				"Attempt to convert sequence to table of strings failed: "
				"failed to extract item %d (counting from 0)", iitem );

			PyErr_SetString( PyExc_TypeError, errmsg );

			return 0;
		}

		if( ! PyString_Check( seqobj ) ) {

			freetbl( *atbl, free );

			*atbl = NULL;

			sprintf( errmsg, 
				"Attempt to convert sequence to table of strings failed: "
				"item %d (counting from 0) is not a string", iitem );

			PyErr_SetString( PyExc_TypeError, errmsg );

			return 0;
		}

		if( ( astring = PyString_AsString( seqobj ) ) == NULL ) {

			freetbl( *atbl, free );

			*atbl = NULL;

			sprintf( errmsg, 
				"Attempt to convert sequence to table of strings failed: "
				"conversion of item %d (counting from 0) to string failed", iitem );

			PyErr_SetString( PyExc_TypeError, errmsg );

			return 0;
		}

		pushtbl( *atbl, strdup( astring ) );
	}

	return 1;
}

static PyObject *
strtbl2PyObject( Tbl *atbl ) 
{
	PyObject *obj;
	int	i;

	if( atbl == NULL ) {

		return NULL;
	} 

	obj = PyTuple_New( maxtbl( atbl ) );

	for( i = 0; i < maxtbl( atbl ); i++ ) {

		PyTuple_SetItem( obj, i, PyString_FromString( gettbl( atbl, i ) ) );
	}

	return obj;
}

static PyObject *
string2PyObject( char *s )
{
	PyObject *obj;
	int	tf;

	if( strmatches( s, "^[-+]{0,1}[[:digit:]]{1,}$", 0 ) == 1 ) {

		obj = Py_BuildValue( "i", atoi( s ) );

	} else if( ( tf = yesno( s ) ) <= 0 ) {

		/* Test for true/false after testing for integers 
		   since not every 0 or 1 is a boolean, yet 0 and 1 
		   will almost always work as booleans */

		if( tf ) {

			Py_INCREF( Py_True );

			obj = Py_True;

		} else {

			Py_INCREF( Py_False );

			obj = Py_False;
		}
	
	} else if( strmatches( s, "^[-+[:digit:].eE]{1,}$", 0 ) == 1 ) {

		obj = Py_BuildValue( "d", atof( s ) );

	} else if( strmatches( s, "^[[:digit:]]{1,}[GMkmu]$", 0 ) == 1 ) {

		obj = Py_BuildValue( "d", sz2dbl( s ) );

	} else {

		obj = Py_BuildValue( "s", s );
	}

	return obj;
}

static PyObject *
pf2PyObject( Pf *pf )
{
	PyObject *obj;
	Pf	*pfvalue;
	Tbl	*keys;
	char	*key;
	int	ivalue;

	switch( pf->type ) {
	case PFSTRING:
		
		obj = string2PyObject( pfexpand( pf ) );

		break;

	case PFTBL:

		obj = PyTuple_New( pfmaxtbl( pf ) );

		for( ivalue = 0; ivalue < pfmaxtbl( pf ); ivalue++ ) {

			pfvalue = (Pf *) gettbl( pf->value.tbl, ivalue );

			PyTuple_SetItem( obj, ivalue, pf2PyObject( pfvalue ) );
		}

		break;

	case PFFILE:
	case PFARR:

		keys = keysarr( pf->value.arr );

		obj = PyDict_New();

		for( ivalue = 0; ivalue < maxtbl( keys ); ivalue++ ) {

			key = gettbl( keys, ivalue );

			pfvalue = (Pf *) getarr( pf->value.arr, key );

			PyDict_SetItem( obj, Py_BuildValue( "s", key ), pf2PyObject( pfvalue ) );
		}

		break;

	case PFINVALID:
	default:

		obj = (PyObject *) NULL;

		break;
	}

	return obj;
}

static PyObject *
python_elog_init( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _elog_init( sys.argv )\n";
	Tbl	*arglist;
	int	iarg;
	int	rc;

	if( ! PyArg_ParseTuple( args, "O&", parse_to_strtbl, &arglist ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	if( _stock_argv != (char **) NULL ) {

		for( iarg = 0; iarg < _stock_argc; iarg++ ) {

			free( _stock_argv[iarg] );
		}

		free( _stock_argv );
	}

	_stock_argc = maxtbl( arglist );

	allot( char **, _stock_argv, _stock_argc );

	for( iarg = 0; iarg < _stock_argc; iarg++ ) {

		_stock_argv[iarg] = strdup( gettbl( arglist, iarg ) );
	}
	
	rc = elog_init( _stock_argc, _stock_argv );

	freetbl( arglist, free );

	return Py_BuildValue( "i", rc );
}

static PyObject *
python_elog_notify( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _elog_notify( msg )\n";
	char	*msg;

	if( ! PyArg_ParseTuple( args, "s", &msg ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	elog_notify( 0, "%s", msg );

	return Py_BuildValue( "" );
}

static PyObject *
python_elog_complain( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _elog_complain( msg )\n";
	char	*msg;

	if( ! PyArg_ParseTuple( args, "s", &msg ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	elog_complain( 0, msg );

	return Py_BuildValue( "" );
}

static PyObject *
python_elog_die( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _elog_die( msg )\n";
	char	*msg;

	if( ! PyArg_ParseTuple( args, "s", &msg ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	PyErr_SetString( PyExc_SystemExit, msg );

	return NULL;
}

static PyObject *
python_pfupdate( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _pfupdate( pfname )\n";
	char	*pfname;

	if( ! PyArg_ParseTuple( args, "s", &pfname ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	return Py_BuildValue( "i", updatePf( pfname ) );
}

static PyObject *
python_pffiles( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _pffiles( pfname, all )\n";
	char	*pfname;
	int	all = 0;
	Tbl	*filestbl;
	Pf	*pf;
	PyObject *obj;
	char	errmsg[STRSZ];

	if( ! PyArg_ParseTuple( args, "sO&", &pfname, parse_from_Boolean, &all ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	if( ( pf = getPf( pfname ) ) == (Pf *) NULL ) {
		
		sprintf( errmsg, "Failure opening parameter file '%s'\n", pfname );

		PyErr_SetString( PyExc_RuntimeError, errmsg );

		return NULL;
	}

	filestbl = pffiles( pfname, all );

	obj = strtbl2PyObject( filestbl );

	freetbl( filestbl, 0 );

	return obj;
}

static PyObject *
python_pf2string( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _pf2string( pfname )\n";
	char	*pfname;
	Pf	*pf;
	char	*value;
	PyObject *obj;
	char	errmsg[STRSZ];

	if( ! PyArg_ParseTuple( args, "s", &pfname ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	if( ( pf = getPf( pfname ) ) == (Pf *) NULL ) {
		
		sprintf( errmsg, "Failure opening parameter file '%s'\n", pfname );

		PyErr_SetString( PyExc_RuntimeError, errmsg );

		return NULL;
	}

	value = pf2string( pf );

	obj = Py_BuildValue( "s", value );

	free( value );

	return obj;
}

static PyObject *
python_pf2xml( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _pf2xml( pfname, flags = 0, prolog = None, name = None )\n";
	char	*pfname;
	Pf	*pf;
	char	*value;
	PyObject *obj;
	PyObject *flags_object;
	char	*flags_string;
	char	*prolog = 0;
	char	*name = 0;
	int	flags = 0;
	char	errmsg[STRSZ];

	if( ! PyArg_ParseTuple( args, "sOzz", &pfname, &flags_object, &prolog, &name ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	if( ( pf = getPf( pfname ) ) == (Pf *) NULL ) {
		
		sprintf( errmsg, "Failure opening parameter file '%s'\n", pfname );

		PyErr_SetString( PyExc_RuntimeError, errmsg );

		return NULL;
	}

	if( name == (char *) NULL ) {

		name = pfname;
	}

	if( PyInt_Check( flags_object ) ) {

		flags = (int) PyInt_AsLong( flags_object );

	} else if( PyString_Check( flags_object ) ) {

		flags_string = PyString_AsString( flags_object );

		if( flags_string != NULL &&
	    	strmatches( flags_string, ".*PFXML_STRONG.*", 0 ) ) {

			flags |= PFXML_STRONG;	    	
		}

		if( flags_string != NULL &&
	    	strmatches( flags_string, ".*PFXML_NEWLINES.*", 0 ) ) {

			flags |= PFXML_NEWLINES;	    	
		}

		if( flags_string != NULL &&
	    	strmatches( flags_string, ".*PFXML_PRESERVE_PFFILE.*", 0 ) ) {

			flags |= PFXML_PRESERVE_PFFILE;	    	
		}

	} else {

		PyErr_SetString( PyExc_RuntimeError,  usage );

		return NULL;
	}

	value = pf2xml( pf, name, prolog, flags );

	obj = Py_BuildValue( "s", value );

	free( value );

	return obj;
}

static PyObject *
python_pfget( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _pfget( pfname, pfkey )\n";
	char	*pfname;
	char	*pfkey;
	char	errmsg[STRSZ];
	Pf	*pf;
	Pf	*pfvalue;
	PyObject *obj;
	int	rc;

	if( ! PyArg_ParseTuple( args, "sz", &pfname, &pfkey ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	if( ( pf = getPf( pfname ) ) == (Pf *) NULL ) {
		
		sprintf( errmsg, "Failure opening parameter file '%s'\n", pfname );

		PyErr_SetString( PyExc_RuntimeError, errmsg );

		return NULL;
	}

	if( pfkey == NULL ) {

		obj = pf2PyObject( pf );

	} else {

		rc = pfresolve( pf, pfkey, 0, &pfvalue );

		if( rc 	< 0 ) {

			sprintf( errmsg, "Failed to find parameter '%s' in parameter file '%s'\n", 
						pfkey, pfname );

			PyErr_SetString( PyExc_RuntimeError, errmsg );

			return NULL;
		}

		obj = pf2PyObject( pfvalue );
	}

	return obj;
}

static PyObject *
python_pfget_tbl( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _pfget_tbl( pfname, pfkey )\n";
	char	*pfname;
	char	*pfkey;
	char	errmsg[STRSZ];
	Pf	*pf;
	Pf	*pfvalue;
	int	rc;

	if( ! PyArg_ParseTuple( args, "ss", &pfname, &pfkey ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	if( ( pf = getPf( pfname ) ) == (Pf *) NULL ) {
		
		sprintf( errmsg, "Failure opening parameter file '%s'\n", pfname );

		PyErr_SetString( PyExc_RuntimeError, errmsg );

		return NULL;
	}

	rc = pfresolve( pf, pfkey, 0, &pfvalue );

	if( rc < 0 ) {

		sprintf( errmsg, "Failed to find parameter '%s' in parameter file '%s'\n", pfkey, pfname );

		PyErr_SetString( PyExc_RuntimeError, errmsg );

		return NULL;
	}

	return pf2PyObject( pfvalue );
}

static PyObject *
python_pfget_arr( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _pfget_arr( pfname, pfkey )\n";
	char	*pfname;
	char	*pfkey;
	char	errmsg[STRSZ];
	Pf	*pf;
	Pf	*pfvalue;
	int	rc;

	if( ! PyArg_ParseTuple( args, "ss", &pfname, &pfkey ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	if( ( pf = getPf( pfname ) ) == (Pf *) NULL ) {
		
		sprintf( errmsg, "Failure opening parameter file '%s'\n", pfname );

		PyErr_SetString( PyExc_RuntimeError, errmsg );

		return NULL;
	}

	rc = pfresolve( pf, pfkey, 0, &pfvalue );

	if( rc < 0 ) {

		sprintf( errmsg, "Failed to find parameter '%s' in parameter file '%s'\n", pfkey, pfname );

		PyErr_SetString( PyExc_RuntimeError, errmsg );

		return NULL;
	}

	return pf2PyObject( pfvalue );
}

static PyObject *
python_pfget_string( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _pfget_string( pfname, pfkey )\n";
	char	*pfname;
	char	*pfkey;
	char	*pfvalue;
	char	errmsg[STRSZ];
	Pf	*pf;

	if( ! PyArg_ParseTuple( args, "ss", &pfname, &pfkey ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	if( ( pf = getPf( pfname ) ) == (Pf *) NULL ) {
		
		sprintf( errmsg, "Failure opening parameter file '%s'\n", pfname );

		PyErr_SetString( PyExc_RuntimeError, errmsg );

		return NULL;
	}

	pfvalue = pfget_string( pf, pfkey );

	if( pfvalue == NULL ) {

		sprintf( errmsg, "Failed to find parameter '%s' in parameter file '%s'\n", pfkey, pfname );

		PyErr_SetString( PyExc_RuntimeError, errmsg );

		return NULL;
	}

	return Py_BuildValue( "s", pfvalue );
}

static PyObject *
python_pfget_int( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _pfget_string( pfname, pfkey )\n";
	char	*pfname;
	char	*pfkey;
	long	pfvalue;
	char	errmsg[STRSZ];
	Pf	*pf;

	if( ! PyArg_ParseTuple( args, "ss", &pfname, &pfkey ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	if( ( pf = getPf( pfname ) ) == (Pf *) NULL ) {
		
		sprintf( errmsg, "Failure opening parameter file '%s'\n", pfname );

		PyErr_SetString( PyExc_RuntimeError, errmsg );

		return NULL;
	}

	pfvalue = pfget_int( pf, pfkey );

	return Py_BuildValue( "l", pfvalue );
}

static PyObject *
python_pfget_double( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _pfget_double( pfname, pfkey )\n";
	char	*pfname;
	char	*pfkey;
	double	pfvalue;
	char	errmsg[STRSZ];
	Pf	*pf;

	if( ! PyArg_ParseTuple( args, "ss", &pfname, &pfkey ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	if( ( pf = getPf( pfname ) ) == (Pf *) NULL ) {
		
		sprintf( errmsg, "Failure opening parameter file '%s'\n", pfname );

		PyErr_SetString( PyExc_RuntimeError, errmsg );

		return NULL;
	}

	pfvalue = pfget_double( pf, pfkey );

	return Py_BuildValue( "d", pfvalue );
}

static PyObject *
python_pfget_size( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _pfget_size( pfname, pfkey )\n";
	char	*pfname;
	char	*pfkey;
	double	pfvalue;
	char	errmsg[STRSZ];
	Pf	*pf;

	if( ! PyArg_ParseTuple( args, "ss", &pfname, &pfkey ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	if( ( pf = getPf( pfname ) ) == (Pf *) NULL ) {
		
		sprintf( errmsg, "Failure opening parameter file '%s'\n", pfname );

		PyErr_SetString( PyExc_RuntimeError, errmsg );

		return NULL;
	}

	pfvalue = pfget_size( pf, pfkey );

	return Py_BuildValue( "d", pfvalue );
}

static PyObject *
python_pfget_boolean( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _pfget_boolean( pfname, pfkey )\n";
	char	*pfname;
	char	*pfkey;
	int	pfvalue;
	char	errmsg[STRSZ];
	Pf	*pf;

	if( ! PyArg_ParseTuple( args, "ss", &pfname, &pfkey ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	if( ( pf = getPf( pfname ) ) == (Pf *) NULL ) {
		
		sprintf( errmsg, "Failure opening parameter file '%s'\n", pfname );

		PyErr_SetString( PyExc_RuntimeError, errmsg );

		return NULL;
	}

	pfvalue = pfget_boolean( pf, pfkey );

	if( pfvalue ) {

		Py_INCREF( Py_True );

		return Py_True;

	} else {

		Py_INCREF( Py_False );

		return Py_False;
	}
}

static PyObject *
python_pfget_time( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _pfget_time( pfname, pfkey )\n";
	char	*pfname;
	char	*pfkey;
	double	pfvalue;
	char	errmsg[STRSZ];
	Pf	*pf;

	if( ! PyArg_ParseTuple( args, "ss", &pfname, &pfkey ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	if( ( pf = getPf( pfname ) ) == (Pf *) NULL ) {
		
		sprintf( errmsg, "Failure opening parameter file '%s'\n", pfname );

		PyErr_SetString( PyExc_RuntimeError, errmsg );

		return NULL;
	}

	pfvalue = pfget_time( pf, pfkey );

	return Py_BuildValue( "d", pfvalue );
}

static PyObject *
python_strtdelta( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _strtdelta( epoch )\n";
	PyObject *obj;
	double	epoch;
	char 	*s;

	if( ! PyArg_ParseTuple( args, "d", &epoch ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	s = strtdelta( epoch );

	obj = Py_BuildValue( "s", s );

	if( s ) {

		free( s );
	}

	return obj;
}

static PyObject *
python_strtime( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _strtime( epoch )\n";
	PyObject *obj;
	double	epoch;
	char 	*s;

	if( ! PyArg_ParseTuple( args, "d", &epoch ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	s = strtime( epoch );

	obj = Py_BuildValue( "s", s );

	if( s ) {

		free( s );
	}

	return obj;
}

static PyObject *
python_strydtime( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _strydtime( epoch )\n";
	PyObject *obj;
	double	epoch;
	char 	*s;

	if( ! PyArg_ParseTuple( args, "d", &epoch ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	s = strydtime( epoch );

	obj = Py_BuildValue( "s", s );

	if( s ) {

		free( s );
	}

	return obj;
}

static PyObject *
python_strdate( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _strdate( epoch )\n";
	PyObject *obj;
	double	epoch;
	char 	*s;

	if( ! PyArg_ParseTuple( args, "d", &epoch ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	s = strdate( epoch );

	obj = Py_BuildValue( "s", s );

	if( s ) {

		free( s );
	}

	return obj;
}

static PyObject *
python_strlocaltime( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _strlocaltime( epoch )\n";
	PyObject *obj;
	double	epoch;
	char 	*s;

	if( ! PyArg_ParseTuple( args, "d", &epoch ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	s = strlocaltime( epoch );

	obj = Py_BuildValue( "s", s );

	if( s ) {

		free( s );
	}

	return obj;
}

static PyObject *
python_strlocalydtime( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _strlocalydtime( epoch )\n";
	PyObject *obj;
	double	epoch;
	char 	*s;

	if( ! PyArg_ParseTuple( args, "d", &epoch ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	s = strlocalydtime( epoch );

	obj = Py_BuildValue( "s", s );

	if( s ) {

		free( s );
	}

	return obj;
}

static PyObject *
python_strlocaldate( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _strlocaldate( epoch )\n";
	PyObject *obj;
	double	epoch;
	char 	*s;

	if( ! PyArg_ParseTuple( args, "d", &epoch ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	s = strlocaldate( epoch );

	obj = Py_BuildValue( "s", s );

	if( s ) {

		free( s );
	}

	return obj;
}

static PyObject *
python_str2epoch( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _str2epoch( string )\n";
	PyObject *obj;
	double	epoch;
	char 	*astring;

	if( ! PyArg_ParseTuple( args, "s", &astring ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	epoch = str2epoch( astring );

	obj = Py_BuildValue( "d", epoch );

	return obj;
}

static PyObject *
python_epoch2str( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _epoch2str( epoch, fmt, tz = None )\n";
	PyObject *obj;
	double	epoch;
	char 	*fmt;
	char 	*tz;
	char	*s;

	if( ! PyArg_ParseTuple( args, "dsz", &epoch, &fmt, &tz ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	if( tz == (char *) NULL ) {

		s = epoch2str( epoch, fmt );

	} else {

		s = zepoch2str( epoch, fmt, tz );
	}

	obj = Py_BuildValue( "s", s );

	if( s ) {

		free( s );
	}

	return obj;
}

static PyObject *
python_epoch( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _epoch( yearday )\n";
	PyObject *obj;
	double	e;
	int	yd;

	if( ! PyArg_ParseTuple( args, "i", &yd ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	e = epoch( yd );

	obj = Py_BuildValue( "d", e );

	return obj;
}

static PyObject *
python_yearday( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _yearday( epoch )\n";
	PyObject *obj;
	double	e;
	int	yd;

	if( ! PyArg_ParseTuple( args, "d", &e ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	yd = yearday( e );

	obj = Py_BuildValue( "i", yd );

	return obj;
}

static PyObject *
python_now( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _now()\n";
	PyObject *obj;
	double	epoch;

	if( ! PyArg_ParseTuple( args, "" ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	epoch = now();

	obj = Py_BuildValue( "d", epoch );

	return obj;
}

static void
add_stock_constants( PyObject *mod ) {

	PyModule_AddIntConstant( mod, "PFXML_NEWLINES", PFXML_NEWLINES );
	PyModule_AddIntConstant( mod, "PFXML_STRONG", PFXML_STRONG );
	PyModule_AddIntConstant( mod, "PFXML_PRESERVE_PFFILE", PFXML_PRESERVE_PFFILE );
	
	return;	
}

PyMODINIT_FUNC 
init_stock( void ) {
	PyObject *mod;

	mod = Py_InitModule( "_stock", stock_methods );

	add_stock_constants( mod );
}
