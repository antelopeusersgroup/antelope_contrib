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

/* Include these for 4.11 libdeviants workaround on Darwin */

#include "deviants.h"
#include "sysdata.h"

extern void proc2pidstat( void *kinfo, void *process );

#endif

typedef struct {
	PyObject_HEAD
	int	displaymode;
	int	pktid;
	char	srcname[ORBSRCNAME_SIZE];
	double	time;
	Packet  *pkt;
	char	*raw;
	int	raw_nbytes;
	int	type;
} _pktobject;

staticforward PyTypeObject _pkttype;

#define is__pktobject( v ) ( (v)->ob_type == &_pkttype )

static PyObject *python_unstuffPkt( PyObject *self, PyObject *args );
static PyObject *python_join_srcname( PyObject *self, PyObject *args );
static PyObject *python_split_srcname( PyObject *self, PyObject *args );
static PyObject *python_pkt_type( PyObject *self, PyObject *args );
static PyObject *python_pkt_srcname( PyObject *self, PyObject *args );
static PyObject *python_pkt_time( PyObject *self, PyObject *args );

static void add_pkt_constants( PyObject *mod );
static _pktobject *new_pktobject();
static PyObject *_pkttype_new( PyObject *self, PyObject *args );
static int _pkt_print( _pktobject *self, FILE *fp, int flags );
static void _pkt_dealloc( _pktobject *self );
static PyObject *_pkt_getattr( _pktobject *self, char *name );

PyMODINIT_FUNC init_Pkt( void );

static PyTypeObject _pkttype = {
	PyObject_HEAD_INIT( &PyType_Type )
	0,					/* ob_size */
	"_pkt",					/* tp_name */
	sizeof(_pktobject),			/* tp_basicsize */
	0,					/* tp_itemsize */
	(destructor)	_pkt_dealloc,	
	(printfunc)	_pkt_print,
	(getattrfunc)	_pkt_getattr,
	(setattrfunc)	0,
	(cmpfunc)	0,
	(reprfunc)	0,
	0,					/* tp_as_number */
	0,					/* tp_as_sequence */
	0,					/* tp_as_mapping */
	(hashfunc)	0,			/* tp_hash */
	(ternaryfunc)	0,			/* tp_call */
	(reprfunc)	0,			/* tp_str */
};

static struct PyMethodDef _Pkt_methods[] = {
	{ "_unstuffPkt",  	python_unstuffPkt,   	METH_VARARGS, "Unstuff an Antelope orbserver packet" },
	{ "_join_srcname",  	python_join_srcname,  	METH_VARARGS, "Combine component parts into a packet srcname" },
	{ "_split_srcname",  	python_split_srcname,  	METH_VARARGS, "Decompose a packet srcname into component parts" },
	{ "_pkt",  		_pkttype_new,  		METH_VARARGS, "Create a new _pkt object" },
	{ NULL, NULL, 0, NULL }
};

static struct PyMethodDef _pkt_methods[] = {
	{ "type", 		python_pkt_type,	METH_VARARGS, "Return the integer packet type for a _pkt object" },
	{ "srcname", 		python_pkt_srcname,	METH_VARARGS, "Return the srcname for a _pkt object" },
	{ "time", 		python_pkt_time,	METH_VARARGS, "Return the internal packet time for a _pkt object" },
	{ NULL, NULL, 0, NULL }
};

#ifdef __APPLE__

void
proc2pidstat ( void *kinfo, void *process) {
        /* Sidestep Antelope 4.11 problem with unresolved symbol in libdeviants under Darwin */
	return;
}

#endif

static PyObject *
python_unstuffPkt( PyObject *self, PyObject *args ) 
{
	char	*usage = "Usage: _unstuffPkt(srcname, time, packet, nbytes)\n";
	char	*srcname = 0;
	double	pkttime;
	char	*packet = 0;
	int	nbytes_packet = 0;
	int	nbytes = 0;
	int	type;
	Packet	*pkt = 0;
	PyObject *obj;		

	if( ! PyArg_ParseTuple( args, "sds#i", &srcname, &pkttime, &packet, &nbytes_packet, &nbytes) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	} 

	type = unstuffPkt( srcname, pkttime, packet, nbytes_packet, &pkt );

	/* SCAFFOLD */

	obj = Py_BuildValue( "i", type );

	return obj;
}

static PyObject *
python_join_srcname( PyObject *self, PyObject *args ) 
{
	char	*usage = "Usage: _join_srcname(net, sta, chan, loc, suffix, subcode)\n";
	char	srcname[ORBSRCNAME_SIZE];
	char	*net;
	char	*sta;
	char	*chan;
	char	*loc;
	char	*suffix;
	char	*subcode;
	Srcname parts;
	
	if( ! PyArg_ParseTuple( args, "ssssss", &net, &sta, &chan, &loc, &suffix, &subcode ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	} 

	strcpy( parts.src_net, net );
	strcpy( parts.src_sta, sta );
	strcpy( parts.src_chan, chan );
	strcpy( parts.src_loc, loc );
	strcpy( parts.src_suffix, suffix );
	strcpy( parts.src_subcode, subcode );

	join_srcname( &parts, srcname );

	return Py_BuildValue( "s", srcname );
}

static PyObject *
python_split_srcname( PyObject *self, PyObject *args ) 
{
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

static _pktobject *
new_pktobject()
{
	_pktobject *self;

	self = PyObject_New( _pktobject, &_pkttype );

	if( self == NULL ) {
		
		return NULL;
	}

	self->displaymode = PKT_TERSE;

	self->pktid = -1;

	self->pkt = 0;
	self->time = 0;
	self->raw = 0;
	self->raw_nbytes = 0;

	strcpy( self->srcname, "" );

	return self;
}

static PyObject *
_pkttype_new( PyObject *self, PyObject *args ) 
{
	char	*usage = "Usage: _pkt(srcname, time, packet, nbytes, pktid)\n";
	PyObject *obj;
	char	*srcname = 0;
	double	pkttime;
	char	*packet = 0;
	int	nbytes_packet = 0;
	int	nbytes = 0;
	int	pktid = -1;
	int	rc;

	if( ! PyArg_ParseTuple( args, "sds#ii", &srcname, &pkttime, &packet, &nbytes_packet, &nbytes, &pktid) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;

	} else {
		
		obj = (PyObject *) new_pktobject();
	}

	((_pktobject *) obj)->pktid = pktid;
	((_pktobject *) obj)->time = pkttime;
	((_pktobject *) obj)->raw_nbytes = nbytes;

	strcpy( ((_pktobject *) obj)->srcname, srcname );

	allot( char *, ((_pktobject *) obj)->raw, nbytes_packet );
	memcpy( ((_pktobject *) obj)->raw, packet, sizeof( char ) );

	rc = unstuffPkt( ((_pktobject *) obj)->srcname, 
			 ((_pktobject *) obj)->time, 
			 ((_pktobject *) obj)->raw, 
			 ((_pktobject *) obj)->raw_nbytes, 
			 &((_pktobject *) obj)->pkt );

	if( rc > 0 ) {

		((_pktobject *) obj)->type = rc;

	} else {

		/* SCAFFOLD deal with the error return */
	}

	return obj;
}

static void
_pkt_dealloc( _pktobject *self )
{
	if( self->raw != NULL ) {
		
		free( self->raw );

		self->raw = NULL;
	}

	if( self->pkt != NULL ) {

		freePkt( self->pkt );
	}

	PyObject_Free( self );
}

static int
_pkt_print( _pktobject *self, FILE *fp, int flags ) 
{
	
	fprintf( fp, "[_pkt:\n" );
	fprintf( fp, "\tAntelope pkt object\n" );
	showPkt( self->pktid, self->srcname, self->time, self->raw, self->raw_nbytes, fp, self->displaymode );
	fprintf( fp, "]\n" );

	return 0;
}

static PyObject *
_pkt_getattr( _pktobject *self, char *name )
{
	if( strcmp( name, "__members__" ) == 0 ) {
		
		return Py_BuildValue( "[s]", "type" );

	} else {

		return Py_FindMethod( _pkt_methods, (PyObject *) self, name );
	}
}

static PyObject *
python_pkt_type( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: type()\n";

	if( ! PyArg_ParseTuple( args, "" ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	return Py_BuildValue( "i", ((_pktobject *) self)->type );
}

static PyObject *
python_pkt_srcname( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: srcname()\n";

	if( ! PyArg_ParseTuple( args, "" ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	return Py_BuildValue( "s", ((_pktobject *) self)->srcname );
}

static PyObject *
python_pkt_time( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: time()\n";

	if( ! PyArg_ParseTuple( args, "" ) ) {

		PyErr_SetString( PyExc_RuntimeError, usage );

		return NULL;
	}

	return Py_BuildValue( "f", ((_pktobject *) self)->pkt->time );
}

static void
add_pkt_constants( PyObject *mod ) 
{
	int	i;

	for( i = 0; i < Pktxlatn; i++ ) {

		PyModule_AddIntConstant( mod, Pktxlat[i].name, Pktxlat[i].num );
	}

	return;
}

PyMODINIT_FUNC
init_Pkt( void ) 
{
	PyObject *mod;

	mod = Py_InitModule( "_Pkt", _Pkt_methods );

	add_pkt_constants( mod );
}
