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
#include "Pkt.h"

#ifdef __APPLE__

/* The scaffold below works around a problem on Darwin */

char **environ;
char *__progname = "Python";

#endif

static PyObject *python_unstuffPkt( PyObject *self, PyObject *args );
static PyObject *python_split_srcname( PyObject *self, PyObject *args );

PyMODINIT_FUNC init_Pkt( void );

static struct PyMethodDef _Pkt_methods[] = {
	{ "_unstuffPkt",  	python_unstuffPkt,   	METH_VARARGS, "Unstuff an Antelope orbserver packet" },
	{ "_split_srcname",  	python_split_srcname,  	METH_VARARGS, "Decompose a packet srcname into component parts" },
	{ NULL, NULL, 0, NULL }
};

static PyObject *
python_unstuffPkt( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _unstuffPkt(srcname, time, packet, nbytes)\n";
	char	srcname[ORBSRCNAME_SIZE];
	double	pkttime;
	char	*packet = 0;
	int	nbytes_pckt = 0;
	int	nbytes = 0;
	PyObject *obj;		

	if( ! PyArg_ParseTuple( args, "sds#i", srcname, &pkttime, &packet, &nbytes_pckt, &nbytes) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	} 

	/* SCAFFOLD */

	obj = Py_BuildValue( "" );

	return obj;
}

static PyObject *
python_split_srcname( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _split_srcname(srcname)\n";
	char	*srcname;
	Srcname parts;
	
	if( ! PyArg_ParseTuple( args, "s", &srcname ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	} 

	split_srcname( srcname, &parts );

	return Py_BuildValue( "ssssss", parts.src_net, 
				        parts.src_sta,
					parts.src_chan,
					parts.src_loc,
					parts.src_suffix,
					parts.src_subcode );
}

PyMODINIT_FUNC
init_Pkt( void ) {
	PyObject *mod;

	mod = Py_InitModule( "_Pkt", _Pkt_methods );
}
