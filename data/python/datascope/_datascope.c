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
	{ "_trloadchan", python_trloadchan,	METH_VARARGS, "Read channel waveform data" },
	{ "_trdata",	python_trdata,		METH_VARARGS, "Extract data points from trace table record" },
	{ NULL, NULL, 0, NULL }
};

static PyObject *
Dbptr2pyobject( Dbptr db )
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

		return Dbptr2pyobject( db );
	}
}

static PyObject *
python_dblookup( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: dblookup( db, database, table, field, record )\n";
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

	return Dbptr2pyobject( db );
}

static PyObject *
python_dbsubset( PyObject *self, PyObject *args ) {
	char	*usage;
	Dbptr	db;
	char	*expr = 0;

	if( ! PyArg_ParseTuple( args, "O&s", convert_Dbptr, &db, &expr ) ) {

		usage = "Usage: dbsubset( db, expr )\n";

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	db = dbsubset( db, expr, 0 );

	return Dbptr2pyobject( db );
}


static PyObject *
python_dbinvalid( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: dbinvalid()\n";
	Dbptr	db;

	if( ! PyArg_ParseTuple( args, "" ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	db = dbinvalid();

	return Dbptr2pyobject( db );
}

static PyObject *
python_dbsort( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: dbsort( db, key )\n";
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

	return Dbptr2pyobject( db );
}

static PyObject *
python_dbjoin( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: dbjoin( db1, db2 )\n";
	Dbptr	db1;
	Dbptr	db2;
	Dbptr	dbout;

	if( ! PyArg_ParseTuple( args, "O&O&", convert_Dbptr, &db1, convert_Dbptr, &db2 ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	dbout = dbjoin( db1, db2, 0, 0, 0, 0, 0 );

	return Dbptr2pyobject( dbout );
}

static PyObject *
python_trloadchan( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: trloadchan( db, t0, t1, sta, chan )\n";
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

	return Dbptr2pyobject( tr );
}

static PyObject *
python_trdata( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: trdata( tr ) SCAFFOLD \n";
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
