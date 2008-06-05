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
#include "stock.h"

#ifdef __APPLE__

/* The scaffold below works around a problem on Darwin */

char **environ;
char *__progname = "Python";

#endif

static PyObject *python_strtime( PyObject *self, PyObject *args );
static PyObject *python_str2epoch( PyObject *self, PyObject *args );
PyMODINIT_FUNC init_stock( void );

static struct PyMethodDef stock_methods[] = {
	{ "_strtime",   	python_strtime,   	METH_VARARGS, "Compute a string representation of epoch time" },
	{ "_str2epoch",   	python_str2epoch,   	METH_VARARGS, "Compute an epoch time from a string" },
	{ NULL, NULL, 0, NULL }
};

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

	if( s ) {

		free( s );
	}

	obj = Py_BuildValue( "s", s );

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

PyMODINIT_FUNC 
init_stock( void ) {

	Py_InitModule( "_stock", stock_methods );
}
