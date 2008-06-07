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

#include "Python.h"
#include "db.h"
#include "tr.h"

#ifdef __APPLE__

/* The scaffold below works around a problem on Darwin */

char **environ;
char *__progname = "Python";

#endif

static PyObject *python_dbinvalid( PyObject *self, PyObject *args );
static PyObject *python_dbopen( PyObject *self, PyObject *args );
static PyObject *python_dblookup( PyObject *self, PyObject *args );
static PyObject *python_dbsort( PyObject *self, PyObject *args );
static PyObject *python_dbsubset( PyObject *self, PyObject *args );
static PyObject *python_dbjoin( PyObject *self, PyObject *args );
static PyObject *python_dbgetv( PyObject *self, PyObject *args );
static PyObject *python_trloadchan( PyObject *self, PyObject *args );
static PyObject *python_trdata( PyObject *self, PyObject *args );
static void add_datascope_constants( PyObject *mod );
PyMODINIT_FUNC init_datascope( void );

static struct PyMethodDef _datascope_methods[] = {
	{ "_dbopen",   	python_dbopen,   	METH_VARARGS, "Open Datascope database" },
	{ "_dblookup", 	python_dblookup, 	METH_VARARGS, "Lookup Datascope indices" },
	{ "_dbsort",   	python_dbsort,   	METH_VARARGS, "Sort Datascope table" },
	{ "_dbsubset", 	python_dbsubset, 	METH_VARARGS, "Subset Datascope table" },
	{ "_dbjoin",   	python_dbjoin,   	METH_VARARGS, "Join Datascope tables" },
	{ "_dbinvalid", python_dbinvalid,   	METH_VARARGS, "Create an invalid database pointer" },
	{ "_dbgetv",    python_dbgetv,   	METH_VARARGS, "Retrieve values from a database row" },
	{ "_trloadchan", python_trloadchan,	METH_VARARGS, "Read channel waveform data" },
	{ "_trdata",	python_trdata,		METH_VARARGS, "Extract data points from trace table record" },
	{ NULL, NULL, 0, NULL }
};

static PyObject *
Dbptr2PyObject( Dbptr db )
{
	return Py_BuildValue( "[iiii]", db.database, db.table, db.field, db.record );
}

static int
convert_Dbptr( PyObject *obj, void *addr )
{
	Dbptr	*db = (Dbptr *) addr;

	if( PyList_Check( obj ) &&
	    PyList_Size( obj ) == 4 &&
	    PyInt_Check( PyList_GetItem( obj, 0 ) ) &&
	    PyInt_Check( PyList_GetItem( obj, 1 ) ) &&
	    PyInt_Check( PyList_GetItem( obj, 2 ) ) &&
	    PyInt_Check( PyList_GetItem( obj, 3 ) ) ) {

		db->database = (int) PyInt_AsLong( PyList_GetItem( obj, 0 ) );
		db->table    = (int) PyInt_AsLong( PyList_GetItem( obj, 1 ) );
		db->field    = (int) PyInt_AsLong( PyList_GetItem( obj, 2 ) );
		db->record   = (int) PyInt_AsLong( PyList_GetItem( obj, 3 ) );

		return 1;

	} else {

		PyErr_Warn( PyExc_RuntimeWarning, 
			"Dbptr is not a Dbptr object or 4-integer list" );
	}

	return 0;
}

static PyObject *
python_dbopen( PyObject *self, PyObject *args ) {
	char	*dbname;
	char	*perm;
	Dbptr	db;
	int	rc;

	if( ! PyArg_ParseTuple( args, "ss", &dbname, &perm ) ) {
		
		PyErr_SetString( PyExc_RuntimeError, 
			"Error parsing input arguments\n" );

		return NULL;
	}

	rc = dbopen( dbname, perm, &db );

	if( rc < 0 ) {

		return Py_BuildValue( "" );

	} else {

		return Dbptr2PyObject( db );
	}
}

static PyObject *
python_dblookup( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _dblookup( db, database, table, field, record )\n";
	Dbptr	db;
	char	*database = 0;
	char	*table = 0;
	char	*field = 0;
	char	*record = 0;

	if( ! PyArg_ParseTuple( args, "O&ssss", convert_Dbptr, &db, 
				      &database, &table, &field, &record ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	db = dblookup( db, database, table, field, record );

	return Dbptr2PyObject( db );
}

static PyObject *
python_dbsubset( PyObject *self, PyObject *args ) {
	char	*usage;
	Dbptr	db;
	char	*expr = 0;

	if( ! PyArg_ParseTuple( args, "O&s", convert_Dbptr, &db, &expr ) ) {

		usage = "Usage: _dbsubset( db, expr )\n";

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	db = dbsubset( db, expr, 0 );

	return Dbptr2PyObject( db );
}


static PyObject *
python_dbinvalid( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _dbinvalid()\n";
	Dbptr	db;

	if( ! PyArg_ParseTuple( args, "" ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	db = dbinvalid();

	return Dbptr2PyObject( db );
}

static PyObject *
python_dbsort( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _dbsort( db, key )\n";
	Dbptr	db;
	char	*akey = 0;
	Tbl 	*keys;

	if( ! PyArg_ParseTuple( args, "O&s", convert_Dbptr, &db, &akey ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	keys = strtbl( akey, 0 );

	db = dbsort( db, keys, 0, 0 );

	freetbl( keys, 0 );

	return Dbptr2PyObject( db );
}

static PyObject *
python_dbjoin( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _dbjoin( db1, db2 )\n";
	Dbptr	db1;
	Dbptr	db2;
	Dbptr	dbout;

	if( ! PyArg_ParseTuple( args, "O&O&", convert_Dbptr, &db1, convert_Dbptr, &db2 ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	dbout = dbjoin( db1, db2, 0, 0, 0, 0, 0 );

	return Dbptr2PyObject( dbout );
}

static PyObject *
python_dbgetv( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _dbgetv( db, field [, field...] )\n";
	Dbptr	db;
	Dbvalue	val;
	PyObject *arg;
	PyObject *vals;
	char	*field;
	char	errmsg[STRSZ];
	int	type;
	int	nargs;
	int	iarg;
	int	rc;

	nargs = PyTuple_Size( args );

	if( nargs < 2 ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;

	} else if( ! convert_Dbptr( PyTuple_GetItem( args, 0 ), &db ) ) {

		sprintf( errmsg, "Argument 0 to _dbgetv must be a Dbptr or four-element list of integers" );

		PyErr_SetString( PyExc_TypeError, errmsg );

		return NULL;
	}

	vals = PyTuple_New( nargs - 1 );

	for( iarg = 1; iarg < nargs; iarg++ ) {
		
		arg = PyTuple_GetItem( args, iarg );
		
		if( ! PyString_Check( arg ) ) {

			if( iarg == 1 ) {

				strcpy( errmsg, "1st " );

			} else if( iarg == 2 ) {

				strcpy( errmsg, "2nd " );

			} else if( iarg == 3 ) {

				strcpy( errmsg, "3rd " );

			} else {

				sprintf( errmsg, "%dth ", iarg );
			}

			strcat( errmsg, "field-name argument to _dbgetv must be a string" );

			PyErr_SetString( PyExc_TypeError, errmsg );

			return NULL;
		}

		field = PyString_AsString( arg );

		db = dblookup( db, 0, 0, field, 0 );

		if( db.field < 0 ) {

			sprintf( errmsg, "_dbgetv: failed to find field named '%s' in database row", field );

			PyErr_SetString( PyExc_RuntimeError, errmsg );

			return NULL;
		}

		rc = dbgetv( db, 0, field, &val, 0 );

		if( rc < 0 ) {

			sprintf( errmsg, "_dbgetv: failed to extract value for field named '%s' from database row", field );

			PyErr_SetString( PyExc_RuntimeError, errmsg );

			return NULL;
		}

		dbquery( db, dbFIELD_TYPE, &type );

		switch( type ) {

		case dbDBPTR:

			PyTuple_SetItem( vals, iarg - 1, Dbptr2PyObject( val.db ) ); 
			break;

		case dbSTRING:

			PyTuple_SetItem( vals, iarg - 1, PyString_FromString( val.s ) ); 
			break;

		case dbBOOLEAN:

			PyTuple_SetItem( vals, iarg - 1, PyBool_FromLong( val.i ) ); 
			break;

		case dbINTEGER:
		case dbYEARDAY:

			PyTuple_SetItem( vals, iarg - 1, PyInt_FromLong( val.i ) ); 
			break;

		case dbREAL:
		case dbTIME:

			PyTuple_SetItem( vals, iarg - 1, PyFloat_FromDouble( val.d ) ); 
			break;

		default:
			sprintf( errmsg, "_dbgetv internal error: type '%d' for field named '%s' not understood", type, field );

			PyErr_SetString( PyExc_RuntimeError, errmsg );

			return NULL;
		}
	}

	return vals;
}

static PyObject *
python_trloadchan( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _trloadchan( db, t0, t1, sta, chan )\n";
	Dbptr	db;
	Dbptr	tr;
	double	t0;
	double	t1;
	char	*sta;
	char	*chan;

	if( ! PyArg_ParseTuple( args, "O&ddss", convert_Dbptr, 
				       &db, &t0, &t1, &sta, &chan ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	tr = trloadchan( db, t0, t1, sta, chan ); 

	return Dbptr2PyObject( tr );
}

static PyObject *
python_trdata( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _trdata( tr )\n";
	Dbptr	tr;
	float	*data;
	int	result;
	int	nsamp;
	int	i;
	PyObject *obj;

	if( ! PyArg_ParseTuple( args, "O&", convert_Dbptr, &tr ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	if( tr.record < 0 ) {

		PyErr_SetString( PyExc_RuntimeError, 
			"trdata requires a single record number in tr.record\n" );

		return NULL;
	} 

	result = dbgetv( tr, 0, "nsamp", &nsamp, "data", &data, 0 );

	if( result != 0 || data == 0 ) {

		PyErr_SetString( PyExc_RuntimeError, 
			"trdata failed to retrieve nsamp and data-pointer from record\n" );

		return NULL;
	}

	obj = PyTuple_New( nsamp );

	for( i = 0; i < nsamp; i++ ) {

		PyTuple_SetItem( obj, i, PyFloat_FromDouble( data[i] ) );
	}

	return obj;
}

static void
add_datascope_constants( PyObject *mod ) {
	int	i;

	for( i = 0; i < NDbxlat; i++ ) {

		PyModule_AddIntConstant( mod, Dbxlat[i].name, Dbxlat[i].num );
	}
	
	return;	
}

PyMODINIT_FUNC
init_datascope( void ) {
	PyObject *mod;

	mod = Py_InitModule( "_datascope", _datascope_methods );

	add_datascope_constants( mod );
}
