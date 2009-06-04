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
#include "forb.h"

#ifdef __APPLE__

/* The scaffold below works around a problem on Darwin */

char **environ;
char *__progname = "Python";

/* Include these for 4.11 libdeviants workaround on Darwin */

#include "deviants.h"
#include "sysdata.h"

extern void proc2pidstat( void *kinfo, void *process );

#endif

static PyObject *python_orbopen( PyObject *self, PyObject *args );
static PyObject *python_orbclose( PyObject *self, PyObject *args );
static PyObject *python_orbping( PyObject *self, PyObject *args );
static PyObject *python_orbtell( PyObject *self, PyObject *args );
static PyObject *python_orbselect( PyObject *self, PyObject *args );
static PyObject *python_orbreject( PyObject *self, PyObject *args );
static PyObject *python_orbposition( PyObject *self, PyObject *args );
static PyObject *python_orbreap( PyObject *self, PyObject *args );
static PyObject *python_orbreap_nd( PyObject *self, PyObject *args );
static PyObject *python_orbreap_timeout( PyObject *self, PyObject *args );
static PyObject *python_orbget( PyObject *self, PyObject *args );
static PyObject *python_orbseek( PyObject *self, PyObject *args );
static PyObject *python_orbafter( PyObject *self, PyObject *args );
static PyObject *python_orbpkt_string( PyObject *self, PyObject *args );

static void add_orb_constants( PyObject *mod );

PyMODINIT_FUNC init_orb( void );

static struct PyMethodDef _orb_methods[] = {
	{ "_orbopen",  	python_orbopen,   	METH_VARARGS, "Open an Antelope orb connection" },
	{ "_orbclose", 	python_orbclose,   	METH_VARARGS, "Close an Antelope orb connection" },
	{ "_orbping", 	python_orbping,   	METH_VARARGS, "Query orbserver version" },
	{ "_orbtell", 	python_orbtell,   	METH_VARARGS, "Query current orb read-head position" },
	{ "_orbselect",	python_orbselect,   	METH_VARARGS, "Select orb source names" },
	{ "_orbreject",	python_orbreject,   	METH_VARARGS, "Reject orb source names" },
	{ "_orbposition", python_orbposition,  	METH_VARARGS, "Position the orb read head by time" },
	{ "_orbreap",	python_orbreap,   	METH_VARARGS, "Get the next packet from an orb" },
	{ "_orbreap_nd", python_orbreap_nd,   	METH_VARARGS, "Get the next packet from an orb with no delay" },
	{ "_orbreap_timeout", python_orbreap_timeout, METH_VARARGS, "Get the next packet from an orb, waiting a maximum number of seconds" },
	{ "_orbget",	python_orbget,   	METH_VARARGS, "Get a specified packet from an orb" },
	{ "_orbseek", 	python_orbseek,  	METH_VARARGS, "Position the orb read head by pktid" },
	{ "_orbafter", 	python_orbafter,  	METH_VARARGS, "Position the orb read head by epoch time" },
	{ "_orbpkt_string", python_orbpkt_string, METH_VARARGS, "Return forb(5) representation of packet" },
	{ NULL, NULL, 0, NULL }
};

#ifdef __APPLE__

void 
proc2pidstat ( void *kinfo, void *process) {
	/* Resolve Antelope 4.11 problem with unresolved symbol in libdeviants under Darwin */
	return;
}

#endif

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
python_orbselect( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orbselect(orb, regex)\n";
	int	orbfd;
	char	*select;
	int	rc;

	if( ! PyArg_ParseTuple( args, "is", &orbfd, &select ) ) {

		if( ! PyErr_Occurred() ) {

			PyErr_SetString( PyExc_RuntimeError, usage );
		}

		return NULL;
	}

	rc = orbselect( orbfd, select );

	return Py_BuildValue( "i", rc );
}

static PyObject *
python_orbreject( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orbreject(orb, regex)\n";
	int	orbfd;
	char	*reject;
	int	rc;

	if( ! PyArg_ParseTuple( args, "is", &orbfd, &reject ) ) {

		if( ! PyErr_Occurred() ) {

			PyErr_SetString( PyExc_RuntimeError, usage );
		}

		return NULL;
	}

	rc = orbreject( orbfd, reject );

	return Py_BuildValue( "i", rc );
}

static PyObject *
python_orbposition( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orbposition(orb, where)\n";
	int	orbfd;
	char	*where;
	int	pktid;

	if( ! PyArg_ParseTuple( args, "is", &orbfd, &where ) ) {

		if( ! PyErr_Occurred() ) {

			PyErr_SetString( PyExc_RuntimeError, usage );
		}

		return NULL;
	}

	pktid = orbposition( orbfd, where );

	return Py_BuildValue( "i", pktid );
}

static PyObject *
python_orbseek( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orbseek(orb, whichpkt)\n";
	int	orbfd;
	int	whichpkt;
	int	pktid;

	if( ! PyArg_ParseTuple( args, "ii", &orbfd, &whichpkt ) ) {

		if( ! PyErr_Occurred() ) {

			PyErr_SetString( PyExc_RuntimeError, usage );
		}

		return NULL;
	}

	pktid = orbseek( orbfd, whichpkt );

	return Py_BuildValue( "i", pktid );
}

static PyObject *
python_orbafter( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orbafter(orb, time)\n";
	int	orbfd;
	int	pktid;
	double	epoch;

	if( ! PyArg_ParseTuple( args, "id", &orbfd, &epoch ) ) {

		if( ! PyErr_Occurred() ) {

			PyErr_SetString( PyExc_RuntimeError, usage );
		}

		return NULL;
	}

	pktid = orbafter( orbfd, epoch );

	return Py_BuildValue( "i", pktid );
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

static PyObject *
python_orbreap( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orbreap(orb)\n";
	int	orbfd;
	int	pktid;
	char	srcname[ORBSRCNAME_SIZE];
	double	pkttime;
	char	*pkt = 0;
	int	nbytes = 0;
	int	bufsize = 0;
	PyObject *obj;
	int	rc;

	if( ! PyArg_ParseTuple( args, "i", &orbfd ) ) {

		if( ! PyErr_Occurred() ) {

			PyErr_SetString( PyExc_RuntimeError, usage );
		}

		return NULL;
	}

	rc = orbreap( orbfd, &pktid, srcname, &pkttime, &pkt, &nbytes, &bufsize );

	obj = Py_BuildValue( "s#", pkt, nbytes );

	free( pkt );

	return Py_BuildValue( "isdOi", pktid, srcname, pkttime, obj, nbytes );
}

static PyObject *
python_orbreap_nd( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orbreap_nd(orb)\n";
	int	orbfd;
	int	pktid;
	char	srcname[ORBSRCNAME_SIZE];
	double	pkttime;
	char	*pkt = 0;
	int	nbytes = 0;
	int	bufsize = 0;
	PyObject *obj;
	int	rc;

	if( ! PyArg_ParseTuple( args, "i", &orbfd ) ) {

		if( ! PyErr_Occurred() ) {

			PyErr_SetString( PyExc_RuntimeError, usage );
		}

		return NULL;
	}

	rc = orbreap_nd( orbfd, &pktid, srcname, &pkttime, &pkt, &nbytes, &bufsize );

	if( rc < 0 ) {

		return Py_BuildValue( "OOOOO",
				      Py_BuildValue( "" ),
				      Py_BuildValue( "" ),
				      Py_BuildValue( "" ),
				      Py_BuildValue( "" ),
				      Py_BuildValue( "" )
				      );

	} else {

		obj = Py_BuildValue( "s#", pkt, nbytes );

		free( pkt );

		return Py_BuildValue( "isdOi", pktid, srcname, pkttime, obj, nbytes );
	}
}

static PyObject *
python_orbreap_timeout( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orbreap_timeout(orb)\n";
	int	orbfd;
	int	pktid;
	int	maxseconds;
	char	srcname[ORBSRCNAME_SIZE];
	double	pkttime;
	char	*pkt = 0;
	int	nbytes = 0;
	int	bufsize = 0;
	PyObject *obj;
	int	rc;

	if( ! PyArg_ParseTuple( args, "ii", &orbfd, &maxseconds ) ) {

		if( ! PyErr_Occurred() ) {

			PyErr_SetString( PyExc_RuntimeError, usage );
		}

		return NULL;
	}

	rc = orbreap_timeout( orbfd, maxseconds, &pktid, srcname, &pkttime, &pkt, &nbytes, &bufsize );

	if( rc < 0 ) {

		return Py_BuildValue( "OOOOO",
				      Py_BuildValue( "" ),
				      Py_BuildValue( "" ),
				      Py_BuildValue( "" ),
				      Py_BuildValue( "" ),
				      Py_BuildValue( "" )
				      );

	} else {

		obj = Py_BuildValue( "s#", pkt, nbytes );

		free( pkt );

		return Py_BuildValue( "isdOi", pktid, srcname, pkttime, obj, nbytes );
	}
}

static PyObject *
python_orbget( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orbget(orb, whichpkt)\n";
	int	orbfd;
	int	whichpkt;
	int	pktid;
	char	srcname[ORBSRCNAME_SIZE];
	double	pkttime;
	char	*pkt = 0;
	int	nbytes = 0;
	int	bufsize = 0;
	PyObject *obj;
	int	rc;

	if( ! PyArg_ParseTuple( args, "ii", &orbfd, &whichpkt ) ) {

		if( ! PyErr_Occurred() ) {

			PyErr_SetString( PyExc_RuntimeError, usage );
		}

		return NULL;
	}


	rc = orbget( orbfd, whichpkt, &pktid, srcname, &pkttime, &pkt, &nbytes, &bufsize );

	obj = Py_BuildValue( "s#", pkt, nbytes );

	free( pkt );

	return Py_BuildValue( "isdOi", pktid, srcname, pkttime, obj, nbytes );
}

static PyObject *
python_orbpkt_string( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orbpkt_string(srcname, time, packet, nbytes)\n";
	char	srcname[ORBSRCNAME_SIZE];
	double	pkttime;
	char	*pkt = 0;
	char	*packet_string = 0;
	int	nbytes = 0;
	int	nbytes_pkt = 0;
	PyObject *obj;

	if( ! PyArg_ParseTuple( args, "sds#i", srcname, &pkttime, &pkt, &nbytes_pkt, &nbytes) ) {

		if( ! PyErr_Occurred() ) {

			PyErr_SetString( PyExc_RuntimeError, usage );
		}

		return NULL;
	}

	packet_string = orbpkt_string( srcname, pkttime, pkt, nbytes_pkt );

	if( packet_string == NULL ) {

		return Py_BuildValue( "" );

	} else {

		obj = Py_BuildValue( "s", packet_string );

		free( packet_string );

		return Py_BuildValue( "O", obj );
	}
}


static void
add_orb_constants( PyObject *mod ) {
	int	i;

	for( i = 0; i < Orbxlatn; i++ ) {

		PyModule_AddIntConstant( mod, Orbxlat[i].name, Orbxlat[i].num );
	}

	PyModule_AddIntConstant( mod, "ORBCURRENT", ORBCURRENT );
	PyModule_AddIntConstant( mod, "ORBNEXT", ORBNEXT );
	PyModule_AddIntConstant( mod, "ORBPREV", ORBPREV );
	PyModule_AddIntConstant( mod, "ORBOLDEST", ORBOLDEST );
	PyModule_AddIntConstant( mod, "ORBNEWEST", ORBNEWEST );
	PyModule_AddIntConstant( mod, "ORBNEXT_WAIT", ORBNEXT_WAIT );
	PyModule_AddIntConstant( mod, "ORBSTASH", ORBSTASH );
	PyModule_AddIntConstant( mod, "ORBPREVSTASH", ORBPREVSTASH );

	return;	
}

PyMODINIT_FUNC
init_orb( void ) {
	PyObject *mod;

	mod = Py_InitModule( "_orb", _orb_methods );

	add_orb_constants( mod );
}

