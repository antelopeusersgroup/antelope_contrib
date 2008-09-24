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

#include <stdio.h>
#include "Python.h"
#include "orb.h"

#ifdef __APPLE__

/* The scaffold below works around a problem on Darwin */

char **environ;
char *__progname = "Python";

#endif

static PyObject *python_orbopen( PyObject *self, PyObject *args );
static PyObject *python_orbclose( PyObject *self, PyObject *args );
static PyObject *python_orbping( PyObject *self, PyObject *args );
static PyObject *python_orbtell( PyObject *self, PyObject *args );

static void add_orb_constants( PyObject *mod );

PyMODINIT_FUNC init_orb( void );

static struct PyMethodDef _orb_methods[] = {
	{ "_orbopen",  	python_orbopen,   	METH_VARARGS, "Open an Antelope orb connection" },
	{ "_orbclose", 	python_orbclose,   	METH_VARARGS, "Close an Antelope orb connection" },
	{ "_orbping", 	python_orbping,   	METH_VARARGS, "Query orbserver version" },
	{ "_orbtell", 	python_orbtell,   	METH_VARARGS, "Query current orb read-head position" },
	{ NULL, NULL, 0, NULL }
};

static PyObject *
python_orbopen( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orbopen(dbname, perm)\n";
	char	*orbname;
	char	*perm;
	int	orbfd;

	if( ! PyArg_ParseTuple( args, "ss", &orbname, &perm ) ) {
		
		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	orbfd = orbopen( orbname, perm );

	if( orbfd < 0 ) {

		return Py_BuildValue( "" );

	} else {

		return Py_BuildValue( "i", orbfd );
	}
}

static PyObject *
python_orbclose( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orbclose(orb)\n";
	int	orbfd;
	int	rc;

	if( ! PyArg_ParseTuple( args, "i", &orbfd ) ) {

		if( ! PyErr_Occurred() ) {

			PyErr_SetString( PyExc_RuntimeError, usage );
		}

		return NULL;
	}

	rc = orbclose( orbfd );

	if( rc < 0 ) {

		PyErr_SetString( PyExc_RuntimeError, "error closing orb connection" );

		return NULL;
	}

	return Py_BuildValue( "" );
}

static PyObject *
python_orbtell( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orbtell(orb)\n";
	int	orbfd;
	int	pktid;
	int	rc;

	if( ! PyArg_ParseTuple( args, "i", &orbfd ) ) {

		if( ! PyErr_Occurred() ) {

			PyErr_SetString( PyExc_RuntimeError, usage );
		}

		return NULL;
	}

	pktid = orbtell( orbfd );

	if( rc < 0 ) {

		PyErr_SetString( PyExc_RuntimeError, "error querying orb position" );

		return NULL;
	}

	return Py_BuildValue( "i", pktid );
}

static PyObject *
python_orbping( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orbping(orb)\n";
	int	orbfd;
	int	orbversion;
	int	rc;

	if( ! PyArg_ParseTuple( args, "i", &orbfd ) ) {

		if( ! PyErr_Occurred() ) {

			PyErr_SetString( PyExc_RuntimeError, usage );
		}

		return NULL;
	}

	rc = orbping( orbfd, &orbversion );

	if( rc < 0 ) {

		PyErr_SetString( PyExc_RuntimeError, "error pinging orb" );

		return NULL;
	}

	return Py_BuildValue( "i", orbversion );
}

static void
add_orb_constants( PyObject *mod ) {
	int	i;

	for( i = 0; i < Orbxlatn; i++ ) {

		PyModule_AddIntConstant( mod, Orbxlat[i].name, Orbxlat[i].num );
	}
	
	return;	
}

PyMODINIT_FUNC
init_orb( void ) {
	PyObject *mod;

	mod = Py_InitModule( "_orb", _orb_methods );

	add_orb_constants( mod );
}

