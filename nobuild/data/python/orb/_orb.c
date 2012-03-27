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

#define USAGE raise_elog( ELOG_COMPLAIN, usage )

#ifdef __APPLE__

/* The scaffold below works around a problem on Darwin */

char **environ;
char *__progname = "Python";

/* Include these for 4.11 libdeviants workaround on Darwin */

#include "deviants.h"
#include "sysdata.h"

extern void proc2pidstat( void *kinfo, void *process );

#endif

#define PYTHONORB_NULL_PKTTIME -9999999999.999

typedef struct Orb_relic {
	int	orbfd;
	char	orbfd_str[10];
	char	*orbname;
	char	*pktid_relicname;
	char	*pkttime_relicname;
	double	orb_start;
	int	pktid;
	double	pkttime;
	double	maxpkttime;
} Orb_relic;

static Arr *Orb_relics;

static PyObject *_orb_ElogException;

static PyObject *python_orbopen( PyObject *self, PyObject *args );
static PyObject *python_orbclose( PyObject *self, PyObject *args );
static PyObject *python_orbping( PyObject *self, PyObject *args );
static PyObject *python_orbtell( PyObject *self, PyObject *args );
static PyObject *python_orbselect( PyObject *self, PyObject *args );
static PyObject *python_orbreject( PyObject *self, PyObject *args );
static PyObject *python_orbposition( PyObject *self, PyObject *args );
static PyObject *python_orbreap( PyObject *self, PyObject *args );
static PyObject *python_orbreap_timeout( PyObject *self, PyObject *args );
static PyObject *python_orbget( PyObject *self, PyObject *args );
static PyObject *python_orbput( PyObject *self, PyObject *args );
static PyObject *python_orbputx( PyObject *self, PyObject *args );
static PyObject *python_orbseek( PyObject *self, PyObject *args );
static PyObject *python_orbafter( PyObject *self, PyObject *args );
static PyObject *python_orblag( PyObject *self, PyObject *args );
static PyObject *python_orbstat( PyObject *self, PyObject *args );
static PyObject *python_orbsources( PyObject *self, PyObject *args );
static PyObject *python_orbclients( PyObject *self, PyObject *args );
static PyObject *python_orbresurrect( PyObject *self, PyObject *args );
static PyObject *python_orbexhume( PyObject *self, PyObject *args );
static PyObject *python_orbbury( PyObject *self, PyObject *args );
static PyObject *python_orbpkt_string( PyObject *self, PyObject *args );

static void add_orb_constants( PyObject *mod );
static void add_elog_exception( PyObject *mod );
static void raise_elog( int severity, char *string );
static Orb_relic *new_Orb_relic( int orbfd );
static Orb_relic *get_Orb_relic( int orbfd );
static void delete_Orb_relic( int orbfd );

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
	{ "_orbreap_timeout", python_orbreap_timeout, METH_VARARGS, "Get the next packet from an orb, waiting a maximum number of seconds" },
	{ "_orbget",	python_orbget,   	METH_VARARGS, "Get a specified packet from an orb" },
	{ "_orbput",	python_orbput,   	METH_VARARGS, "Put a packet onto an orb" },
	{ "_orbputx",	python_orbputx,   	METH_VARARGS, "Put a packet onto an orb, returning pktid" },
	{ "_orbseek", 	python_orbseek,  	METH_VARARGS, "Position the orb read head by pktid" },
	{ "_orbafter", 	python_orbafter,  	METH_VARARGS, "Position the orb read head by epoch time" },
	{ "_orblag", 	python_orblag,  	METH_VARARGS, "Return parameters on how far clients are behind" },
	{ "_orbstat", 	python_orbstat,  	METH_VARARGS, "Return parameters on status of an orbserver" },
	{ "_orbsources", python_orbsources,  	METH_VARARGS, "Return information on data-streams in an orbserver" },
	{ "_orbclients", python_orbclients,  	METH_VARARGS, "Return information on clients of an orbserver" },
	{ "_orbresurrect", python_orbresurrect, METH_VARARGS, "Recover an orbserver's previous position" },
	{ "_orbexhume", python_orbexhume, 	METH_VARARGS, "Read and initiate a state file for orb tracking" },
	{ "_orbbury", python_orbbury, 		METH_VARARGS, "Save orb tracking variables in state file" },
	{ "_orbpkt_string", python_orbpkt_string, METH_VARARGS, "Return forb(5) representation of packet" },
	{ NULL, NULL, 0, NULL }
};

#ifdef __APPLE__

void 
proc2pidstat ( void *kinfo, void *process) {
	/* Sidestep Antelope 4.11 problem with unresolved symbol in libdeviants under Darwin */
	return;
}

#endif

static void
raise_elog( int severity, char *string )
{
        PyObject_SetAttrString( _orb_ElogException, "severity", PyInt_FromLong( (long) severity ) );
        PyObject_SetAttrString( _orb_ElogException, "string", PyString_FromString( string ) );

        PyErr_SetObject( _orb_ElogException, _orb_ElogException );

        return;
}

static PyObject *
python_orbopen( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orbopen(dbname, perm)\n";
	char	*orbname;
	char	*perm;
	int	orbfd;

	if( ! PyArg_ParseTuple( args, "ss", &orbname, &perm ) ) {
		
		USAGE;

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

		USAGE;

		return NULL;
	}

	delete_Orb_relic( orbfd );

	rc = orbclose( orbfd );

	if( rc < 0 ) {

		raise_elog( ELOG_COMPLAIN, "error closing orb connection" );

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

		USAGE;

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

		USAGE;

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

		USAGE;

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

		USAGE;

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

		USAGE;

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

	if( ! PyArg_ParseTuple( args, "i", &orbfd ) ) {

		USAGE;

		return NULL;
	}

	pktid = orbtell( orbfd );

	if( pktid < 0 ) {

		raise_elog( ELOG_COMPLAIN, "error querying orb position" );

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

		USAGE;

		return NULL;
	}

	rc = orbping( orbfd, &orbversion );

	if( rc < 0 ) {

		raise_elog( ELOG_COMPLAIN, "error pinging orb" );

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

		USAGE;

		return NULL;
	}

	rc = orbreap( orbfd, &pktid, srcname, &pkttime, &pkt, &nbytes, &bufsize );

	if( rc < 0 ) {

		elog_flush( 1, 0 );
		
		raise_elog( ELOG_COMPLAIN, "orbreap failed\n" );

		return NULL;
	}

	obj = Py_BuildValue( "s#", pkt, nbytes );

	free( pkt );

	return Py_BuildValue( "isdOi", pktid, srcname, pkttime, obj, nbytes );
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

		USAGE;

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

		USAGE;

		return NULL;
	}


	rc = orbget( orbfd, whichpkt, &pktid, srcname, &pkttime, &pkt, &nbytes, &bufsize );

	obj = Py_BuildValue( "s#", pkt, nbytes );

	free( pkt );

	return Py_BuildValue( "isdOi", pktid, srcname, pkttime, obj, nbytes );
}

static PyObject *
python_orbput( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orbput(orb, srcname, time, packet, nbytes)\n";
	int	orbfd;
	char	*srcname = 0;
	double	pkttime;
	char	*pkt = 0;
	int	nbytes = 0;
	int	nbytes_pkt = 0;
	int	rc;

	if( ! PyArg_ParseTuple( args, "isds#i", &orbfd, &srcname, &pkttime, &pkt, &nbytes_pkt, &nbytes) ) {

		USAGE;

		return NULL;
	}

	rc = orbput( orbfd, srcname, pkttime, pkt, nbytes_pkt );

	return Py_BuildValue( "i", rc );
}

static PyObject *
python_orbputx( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orbputx(orb, srcname, time, packet, nbytes)\n";
	int	orbfd;
	char	srcname[ORBSRCNAME_SIZE];
	double	pkttime;
	char	*pkt = 0;
	int	nbytes = 0;
	int	nbytes_pkt = 0;
	int	pktid;

	if( ! PyArg_ParseTuple( args, "isds#i", &orbfd, srcname, &pkttime, &pkt, &nbytes_pkt, &nbytes) ) {

		USAGE;

		return NULL;
	}

	pktid = orbputx( orbfd, srcname, pkttime, pkt, nbytes_pkt );

	return Py_BuildValue( "i", pktid );
}

static PyObject *
python_orblag( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orblag(orb, match, reject)\n";
	int	orbfd;
	char	*match = 0;
	char	*reject = 0;
	char	err[STRSZ];
	double	rc;
	long	ilaggard;
	long	nlaggards;
	ClientLag *cl = 0;
	Laggards *laggards = 0;
	PyObject *laggards_obj;
	PyObject *clientlag_obj;
	PyObject *obj;

	if( ! PyArg_ParseTuple( args, "izz", &orbfd, &match, &reject) ) {

		USAGE;

		return NULL;
	}

	rc = orblag( orbfd, match, reject, &laggards );

	if( rc == -1 ) {

		PyErr_WarnEx( NULL, "orblag: no clients matched select/reject criteria", 1 );

		return Py_BuildValue( "" );

	} else if( rc < -1 ) {

		sprintf( err, "Error: orblag returned %f\n", rc );

		raise_elog( ELOG_COMPLAIN, err );

		return NULL;
	}

	nlaggards = maxtbl( laggards->list );

	laggards_obj = PyTuple_New( nlaggards );

	for( ilaggard = 0; ilaggard < nlaggards; ilaggard++ ) {

		cl = gettbl( laggards->list, ilaggard );

		clientlag_obj = PyTuple_New( 5 );

		PyTuple_SetItem( clientlag_obj, 0, PyFloat_FromDouble( cl->lag ) );
		PyTuple_SetItem( clientlag_obj, 1, PyInt_FromLong( (long) cl->thread ) );
		PyTuple_SetItem( clientlag_obj, 2, PyInt_FromLong( (long) cl->pktid ) );
		PyTuple_SetItem( clientlag_obj, 3, PyString_FromString( cl->who ) );
		PyTuple_SetItem( clientlag_obj, 4, PyString_FromString( cl->what ) );

		PyTuple_SetItem( laggards_obj, ilaggard, clientlag_obj );
	}

	obj = Py_BuildValue( "iiiiO", laggards->oldest,
				      laggards->newest,
				      laggards->maxpktid, 
				      laggards->range, 
				      laggards_obj );

	freeLaggards( laggards );

	return obj;
}

static PyObject *
python_orbstat( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orbstat(orb)\n";
	int	orbfd;
	Orbstat *os = 0;
	char	*ip;
	struct	in_addr addr;
	int	rc;
	PyObject *obj;

	if( ! PyArg_ParseTuple( args, "i", &orbfd ) ) {

		USAGE;

		return NULL;
	}

	rc = orbstat( orbfd, &os );

	if( rc < 0 ) {

		raise_elog( ELOG_COMPLAIN, "error querying orb status" );

		return NULL;
	}

	memcpy( &addr.s_addr, &os->address, 4 );
	ip = inet_ntoa( addr );

	obj = PyDict_New();

	PyDict_SetItemString( obj, "when", Py_BuildValue( "f", os->when ) );
	PyDict_SetItemString( obj, "started", Py_BuildValue( "f", os->started ) );
	PyDict_SetItemString( obj, "orb_start", Py_BuildValue( "f", os->orb_start ) );
	PyDict_SetItemString( obj, "connections", Py_BuildValue( "i", os->connections ) );
	PyDict_SetItemString( obj, "messages", Py_BuildValue( "i", os->messages ) );
	PyDict_SetItemString( obj, "maxdata", PyInt_FromLong( (long) os->maxdata ) );
	PyDict_SetItemString( obj, "errors", Py_BuildValue( "i", os->errors ) );
	PyDict_SetItemString( obj, "rejected", Py_BuildValue( "i", os->rejected ) );
	PyDict_SetItemString( obj, "closes", Py_BuildValue( "i", os->closes ) );
	PyDict_SetItemString( obj, "opens", Py_BuildValue( "i", os->opens ) );
	PyDict_SetItemString( obj, "port", Py_BuildValue( "i", os->port ) );
	PyDict_SetItemString( obj, "address", Py_BuildValue( "s", ip ) );
	PyDict_SetItemString( obj, "pid", Py_BuildValue( "i", os->pid ) );
	PyDict_SetItemString( obj, "nsources", Py_BuildValue( "i", os->nsources ) );
	PyDict_SetItemString( obj, "nclients", Py_BuildValue( "i", os->nclients ) );
	PyDict_SetItemString( obj, "maxsrc", Py_BuildValue( "i", os->maxsrc ) );
	PyDict_SetItemString( obj, "maxpkts", Py_BuildValue( "i", os->maxpktid ) );
	PyDict_SetItemString( obj, "version", Py_BuildValue( "s", os->version ) );
	PyDict_SetItemString( obj, "who", Py_BuildValue( "s", os->who ) );
	PyDict_SetItemString( obj, "host", Py_BuildValue( "s", os->host ) );

	return obj;
}

static PyObject *
python_orbsources( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orbsources(orb)\n";
	int	orbfd;
	Orbsrc *os = 0;
	double 	atime;
	int	nsources;
	int	isource;
	int	rc;
	PyObject *obj;
	PyObject *source_obj;

	if( ! PyArg_ParseTuple( args, "i", &orbfd ) ) {

		USAGE;

		return NULL;
	}

	rc = orbsources( orbfd, &atime, &os, &nsources );

	if( rc < 0 ) {

		raise_elog( ELOG_COMPLAIN, "error querying orb sources" );

		return NULL;
	}

	obj = PyTuple_New( nsources );

	for( isource = 0; isource < nsources; isource++ ) {

		source_obj = PyDict_New();

		PyDict_SetItemString( source_obj, "srcname", Py_BuildValue( "s", os[isource].srcname ) );
		PyDict_SetItemString( source_obj, "active", Py_BuildValue( "i", os[isource].active ) );
		PyDict_SetItemString( source_obj, "nbytes", Py_BuildValue( "i", os[isource].nbytes ) );
		PyDict_SetItemString( source_obj, "npkts", Py_BuildValue( "i", os[isource].npkts ) );
		PyDict_SetItemString( source_obj, "slatest", Py_BuildValue( "i", os[isource].slatest ) );
		PyDict_SetItemString( source_obj, "soldest", Py_BuildValue( "i", os[isource].soldest ) );
		PyDict_SetItemString( source_obj, "slatest_time", Py_BuildValue( "f", os[isource].slatest_time ) );
		PyDict_SetItemString( source_obj, "soldest_time", Py_BuildValue( "f", os[isource].soldest_time ) );

		PyTuple_SetItem( obj, isource, source_obj );
	}

	return Py_BuildValue( "fO", atime, obj );
}

static PyObject *
python_orbclients( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orbclients(orb)\n";
	int	orbfd;
	Orbclient *oc = 0;
	double 	atime;
	int	nclients;
	int	iclient;
	int	rc;
	char	*ip;
	struct	in_addr addr;
	PyObject *obj;
	PyObject *client_obj;

	if( ! PyArg_ParseTuple( args, "i", &orbfd ) ) {

		USAGE;

		return NULL;
	}

	rc = orbclients( orbfd, &atime, &oc, &nclients );

	if( rc < 0 ) {

		raise_elog( ELOG_COMPLAIN, "error querying orb clients" );

		return NULL;
	}

	obj = PyTuple_New( nclients );

	for( iclient = 0; iclient < nclients; iclient++ ) {

		memcpy( &addr.s_addr, oc[iclient].address, 4 );
		ip = inet_ntoa( addr );

		client_obj = PyDict_New();

		PyDict_SetItemString( client_obj, "lastpkt", Py_BuildValue( "f", oc[iclient].lastpkt ) );
		PyDict_SetItemString( client_obj, "started", Py_BuildValue( "f", oc[iclient].started ) );
		PyDict_SetItemString( client_obj, "read", PyInt_FromLong( (long) oc[iclient].read ) );
		PyDict_SetItemString( client_obj, "pid", Py_BuildValue( "i", oc[iclient].pid ) );
		PyDict_SetItemString( client_obj, "bytes", PyInt_FromLong( (long) oc[iclient].bytes ) );
		PyDict_SetItemString( client_obj, "packets", PyInt_FromLong( (long) oc[iclient].packets ) );
		PyDict_SetItemString( client_obj, "pktid", Py_BuildValue( "i", oc[iclient].pktid ) );
		PyDict_SetItemString( client_obj, "port", Py_BuildValue( "i", oc[iclient].port ) );
		PyDict_SetItemString( client_obj, "address", Py_BuildValue( "s", ip ) );
		PyDict_SetItemString( client_obj, "thread", Py_BuildValue( "i", oc[iclient].thread ) );
		PyDict_SetItemString( client_obj, "fd", Py_BuildValue( "i", oc[iclient].fd ) );
		PyDict_SetItemString( client_obj, "nreject", Py_BuildValue( "i", oc[iclient].nreject ) );
		PyDict_SetItemString( client_obj, "nselect", Py_BuildValue( "i", oc[iclient].nselect ) );
		PyDict_SetItemString( client_obj, "errors", Py_BuildValue( "i", oc[iclient].errors ) );
		PyDict_SetItemString( client_obj, "priority", Py_BuildValue( "i", oc[iclient].priority ) );
		PyDict_SetItemString( client_obj, "lastrequest", Py_BuildValue( "i", oc[iclient].lastrequest ) );
		PyDict_SetItemString( client_obj, "nrequests", PyInt_FromLong( (long) oc[iclient].nrequests ) );
		PyDict_SetItemString( client_obj, "nwrites", PyInt_FromLong( (long) oc[iclient].nwrites ) );
		PyDict_SetItemString( client_obj, "nreads", PyInt_FromLong( (long) oc[iclient].nreads ) );
		PyDict_SetItemString( client_obj, "written", PyInt_FromLong( (long) oc[iclient].written ) );
		PyDict_SetItemString( client_obj, "perm", PyString_FromFormat( "%c", oc[iclient].perm ) );
		PyDict_SetItemString( client_obj, "what", Py_BuildValue( "s", oc[iclient].what ) );
		PyDict_SetItemString( client_obj, "host", Py_BuildValue( "s", oc[iclient].host ) );
		PyDict_SetItemString( client_obj, "who", Py_BuildValue( "s", oc[iclient].who ) );
		PyDict_SetItemString( client_obj, "select", Py_BuildValue( "s", oc[iclient].select ) );
		PyDict_SetItemString( client_obj, "reject", Py_BuildValue( "s", oc[iclient].reject ) );

		PyTuple_SetItem( obj, iclient, client_obj );
	}

	return Py_BuildValue( "fO", atime, obj );
}

static Orb_relic *
new_Orb_relic( int orbfd ) {
	Orb_relic *or;

	allot( Orb_relic *, or, 1 );

	or->orbfd = orbfd;
	or->orbname = NULL;
	or->orb_start = PYTHONORB_NULL_PKTTIME;
	or->pkttime = PYTHONORB_NULL_PKTTIME;
	or->maxpkttime = PYTHONORB_NULL_PKTTIME;
	or->pktid = -1;

	sprintf( or->orbfd_str, "%d", or->orbfd );

	orbresurrect4perl( orbfd, &or->orbname, &or->orb_start, &or->maxpkttime );

	or->pktid_relicname = strconcat( or->orbname, "{last_pktid}", NULL );
	or->pkttime_relicname = strconcat( or->orbname, "{last_pkttime}", NULL );

	if( Orb_relics == (Arr *) NULL ) {

		Orb_relics = newarr( 0 );
	}

	setarr( Orb_relics, or->orbfd_str, (void *) or );

	return or;
}

static Orb_relic *
get_Orb_relic( int orbfd ) {
	Orb_relic *or;
	char	key[STRSZ];

	sprintf( key, "%d", orbfd );

	if( Orb_relics == (Arr *) NULL ) {

		elog_complain( 0, "Unexpected failure finding orb relic\n" );

		return (Orb_relic *) NULL;
	}

	or = getarr( Orb_relics, key );

	if( or == (Orb_relic *) NULL ) {

		elog_complain( 0, "Unexpected failure finding orb relic\n" );

		return (Orb_relic *) NULL;
	}

	return or;
}

static void
delete_Orb_relic( int orbfd ) {
	char	key[STRSZ];

	if( Orb_relics == (Arr *) NULL ) {

		return;
	}

	sprintf( key, "%d", orbfd );

	delarr( Orb_relics, key );

	return;
}

static PyObject *
python_orbresurrect( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orbresurrect(orb)\n";
	int	orbfd;
	Orb_relic *or;
	Relic	relic;

	if( ! PyArg_ParseTuple( args, "i", &orbfd ) ) {

		USAGE;

		return NULL;
	}

	or = new_Orb_relic( orbfd );
	
	relic.ip = &or->pktid;

	if( resurrect( or->pktid_relicname, relic, INT_RELIC ) == 0 ) {

		elog_log( 0, "resurrected pktid %d\n", or->pktid );
	}

	relic.dp = &or->pkttime;

	if( resurrect( or->pkttime_relicname, relic, DOUBLE_RELIC ) == 0 ) {

		elog_log( 0, "resurrected pkttime %f\n", or->pkttime );
	}

	return Py_BuildValue( "id", or->pktid, or->pkttime );
}

static PyObject *
python_orbbury( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orbbury(orb, pktid, pkttime)\n";
	Orb_relic *or;
	int	orbfd;
	int	pktid;
	double	pkttime;
	int	rc;

	if( ! PyArg_ParseTuple( args, "iid", &orbfd, &pktid, &pkttime ) ) {

		USAGE;

		return NULL;
	}

	or = get_Orb_relic( orbfd );

	or->pktid = pktid;
	or->pkttime = pkttime;

	rc = bury();

	return Py_BuildValue( "i", rc );
}

static PyObject *
python_orbexhume( PyObject *self, PyObject *args ) {
	char	*usage = "Usage: _orbexhume(filename)\n";
	char	*filename;
	int	rc;

	if( ! PyArg_ParseTuple( args, "s", &filename ) ) {

		USAGE;

		return NULL;
	}

	rc = exhume( filename, 0, 0, 0 );

	return Py_BuildValue( "i", rc );
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

		USAGE;

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
	PyObject *named_constants;

	named_constants = PyDict_New();

	for( i = 0; i < Orbxlatn; i++ ) {

		PyModule_AddIntConstant( mod, Orbxlat[i].name, Orbxlat[i].num );

		PyDict_SetItemString( named_constants, Orbxlat[i].name, PyInt_FromLong( Orbxlat[i].num ) );
	}

	PyModule_AddIntConstant( mod, "ORBCURRENT", ORBCURRENT );
	PyModule_AddIntConstant( mod, "ORBNEXT", ORBNEXT );
	PyModule_AddIntConstant( mod, "ORBPREV", ORBPREV );
	PyModule_AddIntConstant( mod, "ORBOLDEST", ORBOLDEST );
	PyModule_AddIntConstant( mod, "ORBNEWEST", ORBNEWEST );
	PyModule_AddIntConstant( mod, "ORBNEXT_WAIT", ORBNEXT_WAIT );
	PyModule_AddIntConstant( mod, "ORBSTASH", ORBSTASH );
	PyModule_AddIntConstant( mod, "ORBPREVSTASH", ORBPREVSTASH );

	PyDict_SetItemString( named_constants, "ORBCURRENT", PyInt_FromLong( ORBCURRENT ) );
	PyDict_SetItemString( named_constants, "ORBNEXT", PyInt_FromLong( ORBNEXT ) );
	PyDict_SetItemString( named_constants, "ORBPREV", PyInt_FromLong( ORBPREV ) );
	PyDict_SetItemString( named_constants, "ORBOLDEST", PyInt_FromLong( ORBOLDEST ) );
	PyDict_SetItemString( named_constants, "ORBNEWEST", PyInt_FromLong( ORBNEWEST ) );
	PyDict_SetItemString( named_constants, "ORBNEXT_WAIT", PyInt_FromLong( ORBNEXT_WAIT ) );
	PyDict_SetItemString( named_constants, "ORBSTASH", PyInt_FromLong( ORBSTASH ) );
	PyDict_SetItemString( named_constants, "ORBPREVSTASH", PyInt_FromLong( ORBPREVSTASH ) );

	PyModule_AddObject( mod, "_constants", named_constants );

	return;	
}

static void
add_elog_exception( PyObject *mod ) {
        PyObject *dict;

        dict = PyDict_New();

        PyDict_SetItemString( dict, "severity", Py_None );

        Py_INCREF( Py_None );

        PyDict_SetItemString( dict, "string", Py_None );

        Py_INCREF( Py_None );

        _orb_ElogException = PyErr_NewException( "_orb._ElogException", PyExc_Exception, dict );

        Py_INCREF( _orb_ElogException );

        PyModule_AddObject( mod, "_ElogException", _orb_ElogException );

        return;
}

PyMODINIT_FUNC
init_orb( void ) {
	PyObject *mod;

	mod = Py_InitModule( "_orb", _orb_methods );

	add_orb_constants( mod );

	add_elog_exception( mod );
}

