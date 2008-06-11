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
#include "dbxml.h"
#include "tr.h"

#ifdef __APPLE__

/* The scaffold below works around a problem on Darwin */

char **environ;
char *__progname = "Python";

#endif

static PyObject *python_dbinvalid( PyObject *self, PyObject *args );
static PyObject *python_dbopen( PyObject *self, PyObject *args );
static PyObject *python_dbclose( PyObject *self, PyObject *args );
static PyObject *python_dbfree( PyObject *self, PyObject *args );
static PyObject *python_dblookup( PyObject *self, PyObject *args );
static PyObject *python_dbsort( PyObject *self, PyObject *args );
static PyObject *python_dbsubset( PyObject *self, PyObject *args );
static PyObject *python_dbjoin( PyObject *self, PyObject *args );
static PyObject *python_dbgetv( PyObject *self, PyObject *args );
static PyObject *python_dbquery( PyObject *self, PyObject *args );
static PyObject *python_db2xml( PyObject *self, PyObject *args );
static PyObject *python_trloadchan( PyObject *self, PyObject *args );
static PyObject *python_trdata( PyObject *self, PyObject *args );
static void add_datascope_constants( PyObject *mod );
PyMODINIT_FUNC init_datascope( void );

static struct PyMethodDef _datascope_methods[] = {
	{ "_dbopen",   	python_dbopen,   	METH_VARARGS, "Open Datascope database" },
	{ "_dbclose",  	python_dbclose,   	METH_VARARGS, "Close a Datascope database" },
	{ "_dbfree",  	python_dbfree,   	METH_VARARGS, "Free Datascope memory" },
	{ "_dblookup", 	python_dblookup, 	METH_VARARGS, "Lookup Datascope indices" },
	{ "_dbsort",   	python_dbsort,   	METH_VARARGS, "Sort Datascope table" },
	{ "_dbsubset", 	python_dbsubset, 	METH_VARARGS, "Subset Datascope table" },
	{ "_dbjoin",   	python_dbjoin,   	METH_VARARGS, "Join Datascope tables" },
	{ "_dbinvalid", python_dbinvalid,   	METH_VARARGS, "Create an invalid database pointer" },
	{ "_dbgetv",    python_dbgetv,   	METH_VARARGS, "Retrieve values from a database row" },
	{ "_dbquery",   python_dbquery,   	METH_VARARGS, "Get ancillary information about a database" },
	{ "_db2xml",    python_db2xml,   	METH_VARARGS, "convert a database view to XML" },
	{ "_trloadchan", python_trloadchan,	METH_VARARGS, "Read channel waveform data" },
	{ "_trdata",	python_trdata,		METH_VARARGS, "Extract data points from trace table record" },
	{ NULL, NULL, 0, NULL }
};

static PyObject *
Dbptr2PyObject( Dbptr db )
{
	return Py_BuildValue( "[iiii]", db.database, db.table, db.field, db.record );
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

static int
parse_to_Dbptr( PyObject *obj, void *addr )
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

		PyErr_SetString( PyExc_TypeError, 
			"Dbptr is not a Dbptr object or 4-integer list" );
	}

	return 0;
}

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

		*atbl = 0;

		return 1;
	} 

	if( PyString_Check( obj ) ) {

		*atbl = strtbl( PyString_AsString( obj ), NULL );

		return 1;
	}

	if( ! PySequence_Check( obj ) ) {

		PyErr_SetString( PyExc_TypeError, 
			"Attempt to convert sequence to table of strings failed: input argument is not a sequence" );

		return 0;
	}

	nitems = PySequence_Size( obj );

	*atbl = newtbl( nitems );

	for( iitem = 0; iitem < nitems; iitem++ ) {
		
		seqobj = PySequence_GetItem( obj, iitem );

		if( ! seqobj ) {

			freetbl( *atbl, 0 );

			*atbl = 0;

			sprintf( errmsg, 
				"Attempt to convert sequence to table of strings failed: "
				"failed to extract item %d (counting from 0)", iitem );

			PyErr_SetString( PyExc_TypeError, errmsg );

			return 0;
		}

		if( ! PyString_Check( seqobj ) ) {

			freetbl( *atbl, 0 );

			*atbl = 0;

			sprintf( errmsg, 
				"Attempt to convert sequence to table of strings failed: "
				"item %d (counting from 0) is not a string", iitem );

			PyErr_SetString( PyExc_TypeError, errmsg );

			return 0;
		}

		if( ( astring = PyString_AsString( seqobj ) ) == NULL ) {

			freetbl( *atbl, 0 );

			*atbl = 0;

			sprintf( errmsg, 
				"Attempt to convert sequence to table of strings failed: "
				"conversion of item %d (counting from 0) to string failed", iitem );

			PyErr_SetString( PyExc_TypeError, errmsg );

			return 0;
		}

		pushtbl( *atbl, astring );
	}

	return 1;
}

static PyObject *
python_dbopen( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _dbopen( dbname, perm )\n";
	char	*dbname;
	char	*perm;
	Dbptr	db;
	int	rc;

	if( ! PyArg_ParseTuple( args, "ss", &dbname, &perm ) ) {
		
		PyErr_SetString( PyExc_RuntimeError, usage );

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
python_dbclose( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _dbclose( db )\n";
	Dbptr	db;
	int	rc;

	if( ! PyArg_ParseTuple( args, "O&", parse_to_Dbptr, &db ) ) {

		if( ! PyErr_Occurred() ) {

			PyErr_SetString( PyExc_RuntimeError, usage );
		}

		return NULL;
	}

	rc = dbclose( db );

	if( rc < 0 ) {

		PyErr_SetString( PyExc_RuntimeError, "error closing database" );

		return NULL;
	}

	return Py_BuildValue( "" );
}

static PyObject *
python_dbfree( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _dbfree( db )\n";
	Dbptr	db;
	int	rc;

	if( ! PyArg_ParseTuple( args, "O&", parse_to_Dbptr, &db ) ) {

		if( ! PyErr_Occurred() ) {

			PyErr_SetString( PyExc_RuntimeError, usage );
		}

		return NULL;
	}

	rc = dbfree( db );

	if( rc < 0 ) {

		PyErr_SetString( PyExc_RuntimeError, "error freeing datascope memory" );

		return NULL;
	}

	return Py_BuildValue( "" );
}

static PyObject *
python_dblookup( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _dblookup( db, database, table, field, record )\n";
	Dbptr	db;
	char	*database = 0;
	char	*table = 0;
	char	*field = 0;
	char	*record = 0;

	if( ! PyArg_ParseTuple( args, "O&ssss", parse_to_Dbptr, &db, 
				      &database, &table, &field, &record ) ) {

		if( ! PyErr_Occurred() ) {

			PyErr_SetString( PyExc_RuntimeError, usage );
		}

		return NULL;
	}

	db = dblookup( db, database, table, field, record );

	return Dbptr2PyObject( db );
}

static PyObject *
python_dbsubset( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _dbsubset( db, expr, name )\n";
	Dbptr	db;
	char	*expr = 0;
	char	*name = 0;

	if( ! PyArg_ParseTuple( args, "O&sz", parse_to_Dbptr, &db, &expr, &name ) ) {

		if( ! PyErr_Occurred() ) {

			PyErr_SetString( PyExc_RuntimeError, usage );
		}

		return NULL;
	}

	db = dbsubset( db, expr, name );

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
	char	*usage = "Usage: _dbsort( db, keys, unique, reverse, name )\n";
	Dbptr	db;
	Tbl 	*keys = 0;
	char	*name = 0;
	int	flags = 0;
	int	reverse = 0;
	int	unique = 0;

	if( ! PyArg_ParseTuple( args, "O&O&O&O&z", parse_to_Dbptr, &db, 
						   parse_to_strtbl, &keys, 
						   parse_from_Boolean, &unique, 
						   parse_from_Boolean, &reverse, 
						   &name ) ) {

		if( ! PyErr_Occurred() ) {

			PyErr_SetString( PyExc_RuntimeError, usage );
		}

		return NULL;
	}

	if( reverse ) {

		flags |= dbSORT_REVERSE;
	} 

	if( unique ) {

		flags |= dbSORT_UNIQUE;
	} 

	db = dbsort( db, keys, flags, name );

	if( keys != NULL ) {

		freetbl( keys, 0 );
	}

	return Dbptr2PyObject( db );
}

static PyObject *
python_dbjoin( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _dbjoin( db1, db2, outer, pattern1, pattern2, name )\n";
	Dbptr	db1;
	Dbptr	db2;
	Dbptr	dbout;
	Tbl	*pattern1 = 0;
	Tbl 	*pattern2 = 0;
	int	outer = 0;
	int	duplicate_pattern = 0;
	char	*name = 0;

	if( ! PyArg_ParseTuple( args, "O&O&O&O&O&z", parse_to_Dbptr, &db1, 
					       parse_to_Dbptr, &db2, 
					       parse_to_strtbl, &pattern1, 
					       parse_to_strtbl, &pattern2,
					       parse_from_Boolean, &outer, 
					       &name ) ) {

		if( ! PyErr_Occurred() ) {

			PyErr_SetString( PyExc_RuntimeError, usage );
		}

		return NULL;
	}

	if( pattern1 != 0 && pattern2 == 0 ) {

		pattern2 = pattern1; 

		duplicate_pattern++;
	}

	dbout = dbjoin( db1, db2, &pattern1, &pattern2, outer, 0, name );

	if( pattern1 != 0 ) {

		freetbl( pattern1, 0 );
	}

	if( pattern2 != 0 && ! duplicate_pattern ) {

		freetbl( pattern2, 0 );
	}

	return Dbptr2PyObject( dbout );
}

static PyObject *
python_db2xml( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _db2xml( db, rootnode, rownode, fields, expressions, primary )\n";
	PyObject *obj;
	Dbptr	db;
	char	*rootnode = 0;
	char	*rownode = 0; 
	char	*xml = 0;
	Tbl	*fields = 0;
	Tbl	*expressions = 0;
	int 	flags = 0;
	int	primary = 0;
	int	rc;
	
	if( ! PyArg_ParseTuple( args, "O&zzO&O&O&", parse_to_Dbptr, &db, 
						    &rootnode, &rownode, 
						    parse_to_strtbl, &fields, 
						    parse_to_strtbl, &expressions, 
						    parse_from_Boolean, &primary ) ) {

		if( ! PyErr_Occurred() ) {

			PyErr_SetString( PyExc_RuntimeError, usage );
		}

		return NULL;
	}

	if( primary ) {

		flags |= DBXML_PRIMARY;
	}

	rc = db2xml( db, rootnode, rownode, fields, expressions, (void **) &xml, flags );

	if( fields ) {

		freetbl( fields, 0 );
	}

	if( expressions ) {

		freetbl( expressions, 0 );
	}

	if( rc < 0 || xml == NULL) {

		PyErr_SetString( PyExc_RuntimeError, "db2xml failed" );

		return NULL;
	}

	obj = PyString_FromString( xml );

	free( xml );

	return obj;
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

	} else if( ! parse_to_Dbptr( PyTuple_GetItem( args, 0 ), &db ) ) {

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
python_dbquery( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _dbquery( db, code )\n";
	Dbptr	db;
	PyObject *obj;
	char	*string;
	char	errmsg[STRSZ];
	Tbl	*tbl;
	int	dbcode;
	int	n;
	int	rc;

	if( ! PyArg_ParseTuple( args, "O&i", parse_to_Dbptr, &db, &dbcode ) ) {

		if( ! PyErr_Occurred() ) {

			PyErr_SetString( PyExc_RuntimeError, usage );
		}

		return NULL;
	}

	switch( dbcode )	
	{
	case dbSCHEMA_DEFAULT:
	case dbDATABASE_FILENAME:
	case dbIDSERVER:
	case dbLOCKS:
        case dbSCHEMA_DESCRIPTION:
        case dbTIMEDATE_NAME:
        case dbDATABASE_DESCRIPTION:
        case dbTABLE_DESCRIPTION:
        case dbFIELD_DESCRIPTION:
        case dbSCHEMA_DETAIL:
        case dbDATABASE_DETAIL:
        case dbTABLE_DETAIL:
        case dbFIELD_DETAIL:
        case dbSCHEMA_NAME:
        case dbDATABASE_NAME:
        case dbTABLE_NAME:
        case dbFIELD_NAME:
        case dbTABLE_FILENAME:
        case dbTABLE_DIRNAME:
        case dbFIELD_RANGE:
        case dbFIELD_FORMAT:
        case dbDBPATH:
        case dbFORMAT:
        case dbFIELD_UNITS:
        case dbFIELD_BASE_TABLE:
        case dbUNIQUE_ID_NAME:

		if( ( rc = dbquery(db, dbcode, &string) ) >= 0 ) {

			obj = Py_BuildValue( "s", string );

		} else {

			PyErr_SetString( PyExc_RuntimeError, "dbquery failed to extract value" );

			obj = NULL;
		}

		break;

        case dbDATABASE_COUNT:
        case dbTABLE_COUNT:
        case dbFIELD_COUNT:
        case dbRECORD_COUNT:
        case dbTABLE_SIZE:
        case dbFIELD_SIZE:
        case dbFIELD_INDEX:
        case dbVIEW_TABLE_COUNT:
        case dbRECORD_SIZE:
        case dbTABLE_IS_WRITEABLE:
        case dbTABLE_IS_VIEW:
	case dbDATABASE_IS_WRITABLE:
	case dbTABLE_PRESENT:
	case dbTABLE_IS_TRANSIENT:
        case dbFIELD_TYPE:

		if( ( rc = dbquery(db, dbcode, &n) ) >= 0 ) {
			
			obj = Py_BuildValue( "i", n );

		} else {

			PyErr_SetString( PyExc_RuntimeError, "dbquery failed to extract value" );

			obj = NULL;
		}

                break;  

        case dbLINK_FIELDS:
        case dbSCHEMA_FIELDS:
	case dbSCHEMA_TABLES:
        case dbFIELD_TABLES:
        case dbVIEW_TABLES:
        case dbTABLE_FIELDS:
        case dbPRIMARY_KEY:
        case dbALTERNATE_KEY:
        case dbFOREIGN_KEYS:

		if( ( rc = dbquery(db, dbcode, &tbl) ) >= 0 ) {

			obj = strtbl2PyObject( tbl );

		} else {

			PyErr_SetString( PyExc_RuntimeError, "dbquery failed to extract value" );

			obj = NULL;
		}

                break;  
 
        default:

		sprintf( errmsg, "dbquery: bad code '%d'", dbcode );

		PyErr_SetString( PyExc_RuntimeError, errmsg );

		obj = NULL;

		break ;
	}

	return obj;
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

	if( ! PyArg_ParseTuple( args, "O&ddss", parse_to_Dbptr, 
				       &db, &t0, &t1, &sta, &chan ) ) {

		if( ! PyErr_Occurred() ) {

			PyErr_SetString( PyExc_RuntimeError, usage );
		}

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

	if( ! PyArg_ParseTuple( args, "O&", parse_to_Dbptr, &tr ) ) {

		if( ! PyErr_Occurred() ) {

			PyErr_SetString( PyExc_RuntimeError, usage );
		}

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
