/*
 *   Copyright (c) 2007-2010 Lindquist Consulting, Inc.
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
#include "pf.h"
#include "db.h"

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

typedef struct {
	PyObject_HEAD
	int	displaymode;
	int	pktid;
	char	srcname[ORBSRCNAME_SIZE];
	double	time;
	Packet  *pkt;
	char	*raw;
	int	raw_nbytes;
	PacketType *type;
} _pktobject;

typedef struct {
	PyObject_HEAD
	PktChannel *pktchan;
	PyObject *pktobj;
} _pktchannelobject;

staticforward PyTypeObject _pkttype;
staticforward PyTypeObject _pktchanneltype;

static PyObject *_Pkt_ElogException;

/* Reserve for later use
#define is__pktobject( v ) ( (v)->ob_type == &_pkttype )
#define is__pktchannelobject( v ) ( (v)->ob_type == &_pktchanneltype )
*/

static PyObject *python_join_srcname( PyObject *self, PyObject *args );
static PyObject *python_split_srcname( PyObject *self, PyObject *args );
static PyObject *python_pkt_type( PyObject *self, PyObject *args );
static PyObject *python_pkt_PacketType( PyObject *self, PyObject *args );
static PyObject *python_pkt_srcname( PyObject *self, PyObject *args );
static PyObject *python_pkt_time( PyObject *self, PyObject *args );
static PyObject *python_pkt_parts( PyObject *self, PyObject *args );
static PyObject *python_pkt_nchannels( PyObject *self, PyObject *args );
static PyObject *python_pkt_string( PyObject *self, PyObject *args );
static PyObject *python_pkt_version( PyObject *self, PyObject *args );
static PyObject *python_pkt_dfile( PyObject *self, PyObject *args );
static PyObject *python_pkt_pf( PyObject *self, PyObject *args );
static PyObject *python_pkt_db( PyObject *self, PyObject *args );
static PyObject *python_pkt_channels( PyObject *self, PyObject *args );
static PyObject *python_pkt_show( PyObject *self, PyObject *args );

static PyObject *python_pktchannel_time( PyObject *self, PyObject *args );
static PyObject *python_pktchannel_net( PyObject *self, PyObject *args );
static PyObject *python_pktchannel_sta( PyObject *self, PyObject *args );
static PyObject *python_pktchannel_chan( PyObject *self, PyObject *args );
static PyObject *python_pktchannel_loc( PyObject *self, PyObject *args );
static PyObject *python_pktchannel_nsamp( PyObject *self, PyObject *args );
static PyObject *python_pktchannel_samprate( PyObject *self, PyObject *args );
static PyObject *python_pktchannel_calib( PyObject *self, PyObject *args );
static PyObject *python_pktchannel_calper( PyObject *self, PyObject *args );
static PyObject *python_pktchannel_segtype( PyObject *self, PyObject *args );
static PyObject *python_pktchannel_data( PyObject *self, PyObject *args );
static PyObject *python_pktchannel_duser1( PyObject *self, PyObject *args );
static PyObject *python_pktchannel_duser2( PyObject *self, PyObject *args );
static PyObject *python_pktchannel_iuser1( PyObject *self, PyObject *args );
static PyObject *python_pktchannel_iuser2( PyObject *self, PyObject *args );
static PyObject *python_pktchannel_iuser3( PyObject *self, PyObject *args );
static PyObject *python_pktchannel_cuser1( PyObject *self, PyObject *args );
static PyObject *python_pktchannel_cuser2( PyObject *self, PyObject *args );

static void add_pkt_constants( PyObject *mod );
static void add_elog_exception( PyObject *mod );
static void raise_elog( int severity, char *string );
static _pktobject *new_pktobject();
static PyObject *_pkttype_new( PyObject *self, PyObject *args );
static int _pkt_print( _pktobject *self, FILE *fp, int flags );
static void _pkt_dealloc( _pktobject *self );
static PyObject *_pkt_getattr( _pktobject *self, char *name );
static PyObject *new_pktchannelobject( PyObject *pktobj, int ichannel );
static PyObject *_pktchanneltype_new( PyObject *self, PyObject *args );
static int _pktchannel_print( _pktchannelobject *self, FILE *fp, int flags );
static void _pktchannel_dealloc( _pktchannelobject *self );
static PyObject *_pktchannel_getattr( _pktchannelobject *self, char *name );

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

static PyTypeObject _pktchanneltype = {
	PyObject_HEAD_INIT( &PyType_Type )
	0,					/* ob_size */
	"_pktchannel",				/* tp_name */
	sizeof(_pktchannelobject),		/* tp_basicsize */
	0,					/* tp_itemsize */
	(destructor)	_pktchannel_dealloc,	
	(printfunc)	_pktchannel_print,
	(getattrfunc)	_pktchannel_getattr,
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
	{ "_join_srcname",  	python_join_srcname,  	METH_VARARGS, "Combine component parts into a packet srcname" },
	{ "_split_srcname",  	python_split_srcname,  	METH_VARARGS, "Decompose a packet srcname into component parts" },
	{ "_pkt",  		_pkttype_new,  		METH_VARARGS, "Create a new _pkt object" },
	{ "_pktchannel",	_pktchanneltype_new,	METH_VARARGS, "Create a new _pktchannel object" },
	{ NULL, NULL, 0, NULL }
};

static struct PyMethodDef _pkt_methods[] = {
	{ "type", 		python_pkt_type,	METH_VARARGS, "Return the integer packet type for a _pkt object" },
	{ "PacketType",		python_pkt_PacketType,	METH_VARARGS, "Return packet type name, type, and type description for a _pkt object" },
	{ "srcname", 		python_pkt_srcname,	METH_VARARGS, "Return the srcname for a _pkt object" },
	{ "time", 		python_pkt_time,	METH_VARARGS, "Return the internal packet time for a _pkt object" },
	{ "parts", 		python_pkt_parts,	METH_VARARGS, "Return the srcname parts for a _pkt object" },
	{ "nchannels", 		python_pkt_nchannels,	METH_VARARGS, "Return the number of channels in a _pkt object" },
	{ "string", 		python_pkt_string,	METH_VARARGS, "Return the string encapsulated in a _pkt object, if any" },
	{ "version", 		python_pkt_version,	METH_VARARGS, "Return the version of a _pkt object" },
	{ "dfile", 		python_pkt_dfile,	METH_VARARGS, "Return the dfile included in a _pkt object, if any" },
	{ "pf", 		python_pkt_pf,		METH_VARARGS, "Return the parameter file object encapsulated in a _pkt object, if any" },
	{ "db", 		python_pkt_db,		METH_VARARGS, "Return the database pointer encapsulated in a _pkt object, if any" },
	{ "channels", 		python_pkt_channels,	METH_VARARGS, "Return one _pktchannel data channel object from a _pkt object" },
	{ "show", 		python_pkt_show,	METH_VARARGS, "Display the contents of a _pkt object" },
	{ NULL, NULL, 0, NULL }
};

static struct PyMethodDef _pktchannel_methods[] = {
	{ "time", 		python_pktchannel_time,	METH_VARARGS, "Return the internal packet channel time for a _pktchannel object" },
	{ "net", 		python_pktchannel_net,	METH_VARARGS, "Return the internal packet channel net for a _pktchannel object" },
	{ "sta", 		python_pktchannel_sta,	METH_VARARGS, "Return the internal packet channel sta for a _pktchannel object" },
	{ "chan", 		python_pktchannel_chan,	METH_VARARGS, "Return the internal packet channel chan for a _pktchannel object" },
	{ "loc", 		python_pktchannel_loc,	METH_VARARGS, "Return the internal packet channel loc for a _pktchannel object" },
	{ "nsamp", 		python_pktchannel_nsamp, METH_VARARGS, "Return the internal packet channel nsamp for a _pktchannel object" },
	{ "samprate", 		python_pktchannel_samprate, METH_VARARGS, "Return the internal packet channel samprate for a _pktchannel object" },
	{ "calib", 		python_pktchannel_calib, METH_VARARGS, "Return the internal packet channel calib for a _pktchannel object" },
	{ "calper", 		python_pktchannel_calper, METH_VARARGS, "Return the internal packet channel calper for a _pktchannel object" },
	{ "segtype", 		python_pktchannel_segtype, METH_VARARGS, "Return the internal packet channel segtype for a _pktchannel object" },
	{ "data", 		python_pktchannel_data, METH_VARARGS, "Return the data samples for a _pktchannel object" },
	{ "duser1", 		python_pktchannel_duser1, METH_VARARGS, "Return floating-point user data item 1 from a _pktchannel object" },
	{ "duser2", 		python_pktchannel_duser2, METH_VARARGS, "Return floating-point user data item 2 from a _pktchannel object" },
	{ "iuser1", 		python_pktchannel_iuser1, METH_VARARGS, "Return integer user data item 1 from a _pktchannel object" },
	{ "iuser2", 		python_pktchannel_iuser2, METH_VARARGS, "Return integer user data item 2 from a _pktchannel object" },
	{ "iuser3", 		python_pktchannel_iuser3, METH_VARARGS, "Return integer user data item 3 from a _pktchannel object" },
	{ "cuser1", 		python_pktchannel_cuser1, METH_VARARGS, "Return character user data item 1 from a _pktchannel object" },
	{ "cuser2", 		python_pktchannel_cuser2, METH_VARARGS, "Return character user data item 2 from a _pktchannel object" },
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
        PyObject_SetAttrString( _Pkt_ElogException, "severity", PyInt_FromLong( (long) severity ) );
        PyObject_SetAttrString( _Pkt_ElogException, "string", PyString_FromString( string ) );

        PyErr_SetObject( _Pkt_ElogException, _Pkt_ElogException );

        return;
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

		USAGE;

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
	char	srcname_copy[ORBSRCNAME_SIZE];
	Srcname parts;
	
	if( ! PyArg_ParseTuple( args, "s", &srcname ) ) {

		USAGE;

		return NULL;
	} 

	strcpy( srcname_copy, srcname );

	split_srcname( srcname_copy, &parts );

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

		USAGE;

		return NULL;

	} else {
		
		obj = (PyObject *) new_pktobject();
	}

	((_pktobject *) obj)->pktid = pktid;
	((_pktobject *) obj)->time = pkttime;
	((_pktobject *) obj)->raw_nbytes = nbytes;

	strcpy( ((_pktobject *) obj)->srcname, srcname );

	allot( char *, ((_pktobject *) obj)->raw, nbytes_packet );
	memcpy( ((_pktobject *) obj)->raw, packet, nbytes_packet * sizeof( char ) );

	rc = unstuffPkt( ((_pktobject *) obj)->srcname, 
			 ((_pktobject *) obj)->time, 
			 ((_pktobject *) obj)->raw, 
			 ((_pktobject *) obj)->raw_nbytes, 
			 &((_pktobject *) obj)->pkt );

	if( rc < 0 ) {

		elog_complain( 0, "Failure to unstuff packet from %s (unstuffPkt return code %d)\n", 
				((_pktobject *) obj)->srcname, rc );

		((_pktobject *) obj)->pkt = NULL;
		((_pktobject *) obj)->type = NULL;

	} else {

		((_pktobject *) obj)->type = suffix2pkttype( ((_pktobject *) obj)->pkt->parts.src_suffix );
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
		
		return Py_BuildValue( "[sssssssssss]", 
				      "type", 
				      "time", 
				      "srcname",
				      "parts",
				      "PacketType",
				      "nchannels",
				      "string",
				      "version",
				      "dfile",
				      "pf",
				      "db" );

	} else {

		return Py_FindMethod( _pkt_methods, (PyObject *) self, name );
	}
}

static PyObject *
python_pkt_type( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: type()\n";
	PyObject *obj;

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	if( ((_pktobject *) self)->type == NULL ) {

		obj = Py_BuildValue( "" );

	} else {

		obj = Py_BuildValue( "i", ((_pktobject *) self)->type->content );
	}

	return obj;
}

static PyObject *
python_pkt_PacketType( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: PacketType()\n";

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	if( ((_pktobject *) self)->type == NULL ) {

		return Py_BuildValue( "" );
	}

	return Py_BuildValue( "sss", 
			      ((_pktobject *) self)->type->name,
			      xlatnum( ((_pktobject *) self)->type->content, Pktxlat, Pktxlatn ), 
			      ((_pktobject *) self)->type->desc );
}

static PyObject *
python_pkt_show( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: show(mode = PKT_TERSE)\n";
	int	mode = PKT_TERSE;
	FILE	*file = stdout;

	if( ! PyArg_ParseTuple( args, "i", &mode ) ) {

		USAGE;

		return NULL;
	}

	showPkt( ((_pktobject *) self)->pktid, 
		 ((_pktobject *) self)->srcname,
		 ((_pktobject *) self)->time,
		 ((_pktobject *) self)->raw,
		 ((_pktobject *) self)->raw_nbytes,
		 file, 
		 mode );

	return Py_BuildValue( "" );
}

static PyObject *
python_pkt_srcname( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: srcname()\n";

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	return Py_BuildValue( "s", ((_pktobject *) self)->srcname );
}

static PyObject *
python_pkt_time( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: time()\n";
	PyObject *obj;

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	if( ((_pktobject *) self)->pkt == NULL ) {

		obj = Py_BuildValue( "f", ((_pktobject *) self)->time );

	} else {

		obj = Py_BuildValue( "f", ((_pktobject *) self)->pkt->time );
	}

	return obj;
}

static PyObject *
python_pkt_parts( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: parts()\n";
	PyObject *obj;

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	if( ((_pktobject *) self)->pkt == NULL ) {

		obj = Py_BuildValue( "" );

	} else {

		obj = Py_BuildValue( "ssssss", 
			     	     ((_pktobject *) self)->pkt->parts.src_net, 
			     	     ((_pktobject *) self)->pkt->parts.src_sta, 
			     	     ((_pktobject *) self)->pkt->parts.src_chan, 
			     	     ((_pktobject *) self)->pkt->parts.src_loc, 
			     	     ((_pktobject *) self)->pkt->parts.src_suffix, 
			     	     ((_pktobject *) self)->pkt->parts.src_subcode );
	}

	return obj;
}

static PyObject *
python_pkt_nchannels( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: nchannels()\n";
	PyObject *obj;

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	if( ((_pktobject *) self)->pkt == NULL ) {

		obj = Py_BuildValue( "" );

	} else {

		obj = Py_BuildValue( "i", ((_pktobject *) self)->pkt->nchannels );
	}

	return obj;
}

static PyObject *
python_pkt_channels( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: channels(ichannel)\n";
	int	ichannel;
	PyObject *obj;

	if( ! PyArg_ParseTuple( args, "i", &ichannel ) ) {

		USAGE;

		return NULL;

	} else if( ichannel < 0 ) {

		raise_elog( ELOG_COMPLAIN, "channel index must be >= 0\n" );

		return NULL;

	} else if( ((_pktobject *) self)->pkt->channels == NULL ) {

		raise_elog( ELOG_COMPLAIN, "no channels table in pkt structure\n" );

		return NULL;

	} else if( ichannel >= maxtbl( ((_pktobject *) self)->pkt->channels )) {

		raise_elog( ELOG_COMPLAIN, "channel index exceeds number of available channels\n" );

		return NULL;
	}

	obj = new_pktchannelobject( self, ichannel );

	return obj;
}

static PyObject *
python_pkt_string( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: string()\n";
	PyObject *obj;

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	if( ((_pktobject *) self)->pkt == NULL ||
	    ((_pktobject *) self)->pkt->string == NULL ) {

		obj = Py_BuildValue( "" );

	} else {

		obj = Py_BuildValue( "s", ((_pktobject *) self)->pkt->string );
	}

	return obj;
}

static PyObject *
python_pkt_version( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: version()\n";
	PyObject *obj;

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	if( ((_pktobject *) self)->pkt == NULL ) {

		obj = Py_BuildValue( "" );

	} else {

		obj = Py_BuildValue( "i", ((_pktobject *) self)->pkt->version );
	}

	return obj;
}

static PyObject *
python_pkt_dfile( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: dfile()\n";
	PyObject *obj;

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	if( ((_pktobject *) self)->pkt == NULL ||
	    ((_pktobject *) self)->pkt->dfile == NULL || 
	    ((_pktobject *) self)->pkt->dfile_size <= 0 ) {

		obj = Py_BuildValue( "" );

	} else {

		obj = Py_BuildValue( "s#", 
			((_pktobject *) self)->pkt->dfile, 
			((_pktobject *) self)->pkt->dfile_size );
	}

	return obj;
}

static PyObject *
python_pkt_pf( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: pf()\n";
	char	*name = "Packet::pf";
	PyObject *obj;

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	if( ((_pktobject *) self)->pkt == NULL ||
	    ((_pktobject *) self)->pkt->pf == NULL ) {

		obj = Py_BuildValue( "" );

	} else {

		putPf_nofree( name, ((_pktobject *) self)->pkt->pf );

		obj = Py_BuildValue( "s", name );
	}

	return obj;
}

static PyObject *
python_pkt_db( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: db()\n";
	PyObject *obj;

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	if( ((_pktobject *) self)->pkt == NULL ) {

		obj = Py_BuildValue( "[iiii]", dbINVALID, dbINVALID, dbINVALID, dbINVALID );

	} else {

		obj = Py_BuildValue( "[iiii]", 
				     ((_pktobject *) self)->pkt->db.database,
				     ((_pktobject *) self)->pkt->db.table,
				     ((_pktobject *) self)->pkt->db.field,
				     ((_pktobject *) self)->pkt->db.record
				     );
	}

	return obj;
}

static PyObject *
new_pktchannelobject( PyObject *pktobj, int ichannel )
{
	_pktchannelobject *self;

	self = PyObject_New( _pktchannelobject, &_pktchanneltype );

	if( self == NULL ) {
		
		return NULL;
	}

	self->pktobj = pktobj;

	Py_INCREF( pktobj );

	if( ichannel >= 0 &&
	    self->pktobj != NULL &&
	    ((_pktobject *) self->pktobj)->pkt != NULL &&
	    ((_pktobject *) self->pktobj)->pkt->channels != NULL ) {

		self->pktchan = (PktChannel *) gettbl( ((_pktobject *) self->pktobj)->pkt->channels, ichannel );

	} else {

		self->pktchan = NULL;
	}

	return (PyObject *) self;
}

static PyObject *
_pktchanneltype_new( PyObject *self, PyObject *args ) 
{
	char	*usage = "Usage: _pktchannel(_pkt, ichannel)\n";
	PyObject *obj;
	PyObject *pktobj;
	int	ichannel;

	if( ! PyArg_ParseTuple( args, "Oi", &pktobj, &ichannel) ) {

		USAGE;

		return NULL;

	} else if( ichannel < 0 ) {

		raise_elog( ELOG_COMPLAIN, "channel index must be >= 0\n" );

		return NULL;

	} else if( ((_pktobject *) pktobj)->pkt->channels == NULL ||
	           ichannel >= maxtbl( ((_pktobject *) obj)->pkt->channels )) {

		raise_elog( ELOG_COMPLAIN, "channel index exceeds number of available channels\n" );

		return NULL;
	}
		
	obj = new_pktchannelobject( pktobj, ichannel );

	return obj;
}

static void
_pktchannel_dealloc( _pktchannelobject *self )
{
	Py_DECREF( ((_pktchannelobject *) self)->pktobj );

	/* deallocation of PktChannel structure handled by freePkt within _pkt_dealloc */

	PyObject_Free( self );
}

static int
_pktchannel_print( _pktchannelobject *self, FILE *fp, int flags ) 
{
	char	*s;
	
	fprintf( fp, "[_pktchannel:\n" );
	fprintf( fp, "\tAntelope pktchannel object\n" );
	fprintf( fp, "\tnet:\t%s\n", self->pktchan->net );
	fprintf( fp, "\tsta:\t%s\n", self->pktchan->sta );
	fprintf( fp, "\tchan:\t%s\n", self->pktchan->chan );
	fprintf( fp, "\tloc:\t%s\n", self->pktchan->loc );
	fprintf( fp, "\ttime:\t%s\n", s = strtime( self->pktchan->time ) );
	fprintf( fp, "\tnsamp:\t%d\n", self->pktchan->nsamp );
	fprintf( fp, "\tsamprate:\t%lf\n", self->pktchan->samprate );
	fprintf( fp, "]\n" );

	free( s );

	return 0;
}

static PyObject *
_pktchannel_getattr( _pktchannelobject *self, char *name )
{
	if( strcmp( name, "__members__" ) == 0 ) {
		
		return Py_BuildValue( "[ssssssssssssssssss]", 
				      "time",
				      "net",
				      "sta",
				      "chan",
				      "loc",
				      "nsamp",
				      "samprate",
				      "data",
				      "calib",
				      "calper",
				      "segtype",
				      "duser1",
				      "duser2",
				      "iuser1",
				      "iuser2",
				      "iuser3",
				      "cuser1",
				      "cuser2"
				      );

	} else {

		return Py_FindMethod( _pktchannel_methods, (PyObject *) self, name );
	}
}

static PyObject *
python_pktchannel_time( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: time()\n";

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	return Py_BuildValue( "f", ((_pktchannelobject *) self)->pktchan->time );
}

static PyObject *
python_pktchannel_net( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: net()\n";

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	return Py_BuildValue( "s", ((_pktchannelobject *) self)->pktchan->net );
}

static PyObject *
python_pktchannel_sta( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: sta()\n";

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	return Py_BuildValue( "s", ((_pktchannelobject *) self)->pktchan->sta );
}

static PyObject *
python_pktchannel_chan( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: chan()\n";

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	return Py_BuildValue( "s", ((_pktchannelobject *) self)->pktchan->chan );
}

static PyObject *
python_pktchannel_loc( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: loc()\n";

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	return Py_BuildValue( "s", ((_pktchannelobject *) self)->pktchan->loc );
}

static PyObject *
python_pktchannel_nsamp( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: nsamp()\n";

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	return Py_BuildValue( "i", ((_pktchannelobject *) self)->pktchan->nsamp );
}

static PyObject *
python_pktchannel_samprate( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: samprate()\n";

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	return Py_BuildValue( "f", ((_pktchannelobject *) self)->pktchan->samprate );
}

static PyObject *
python_pktchannel_calib( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: calib()\n";

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	return Py_BuildValue( "f", ((_pktchannelobject *) self)->pktchan->calib );
}

static PyObject *
python_pktchannel_calper( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: calper()\n";

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	return Py_BuildValue( "f", ((_pktchannelobject *) self)->pktchan->calper );
}

static PyObject *
python_pktchannel_segtype( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: segtype()\n";

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	return Py_BuildValue( "s", ((_pktchannelobject *) self)->pktchan->segtype );
}

static PyObject *
python_pktchannel_data( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: data()\n";
	PyObject *obj;
	int	i;

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	obj = PyTuple_New( ((_pktchannelobject *) self)->pktchan->nsamp );

	for( i = 0; i < ((_pktchannelobject *) self)->pktchan->nsamp; i++ ) {

		PyTuple_SetItem( obj, i, PyInt_FromLong( (long) ((_pktchannelobject *) self)->pktchan->data[i] ) );
	}

	return obj;
}

static PyObject *
python_pktchannel_duser1( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: duser1()\n";

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	return Py_BuildValue( "f", ((_pktchannelobject *) self)->pktchan->duser1 );
}

static PyObject *
python_pktchannel_duser2( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: duser2()\n";

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	return Py_BuildValue( "f", ((_pktchannelobject *) self)->pktchan->duser2 );
}

static PyObject *
python_pktchannel_iuser1( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: iuser1()\n";

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	return Py_BuildValue( "i", ((_pktchannelobject *) self)->pktchan->iuser1 );
}

static PyObject *
python_pktchannel_iuser2( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: iuser2()\n";

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	return Py_BuildValue( "i", ((_pktchannelobject *) self)->pktchan->iuser2 );
}

static PyObject *
python_pktchannel_iuser3( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: iuser3()\n";

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	return Py_BuildValue( "i", ((_pktchannelobject *) self)->pktchan->iuser3 );
}

static PyObject *
python_pktchannel_cuser1( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: cuser1()\n";

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	return Py_BuildValue( "s", ((_pktchannelobject *) self)->pktchan->cuser1 );
}

static PyObject *
python_pktchannel_cuser2( PyObject *self, PyObject *args )
{
	char	*usage = "Usage: cuser2()\n";

	if( ! PyArg_ParseTuple( args, "" ) ) {

		USAGE;

		return NULL;
	}

	return Py_BuildValue( "s", ((_pktchannelobject *) self)->pktchan->cuser2 );
}

static void
add_pkt_constants( PyObject *mod ) 
{
	int	i;
	PyObject *named_constants;

	named_constants = PyDict_New();

	for( i = 0; i < Pktxlatn; i++ ) {

		PyModule_AddIntConstant( mod, Pktxlat[i].name, Pktxlat[i].num );

		PyDict_SetItemString( named_constants, Pktxlat[i].name, PyInt_FromLong( Pktxlat[i].num ) );
	}

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

        _Pkt_ElogException = PyErr_NewException( "_Pkt._ElogException", PyExc_Exception, dict );

        Py_INCREF( _Pkt_ElogException );

        PyModule_AddObject( mod, "_ElogException", _Pkt_ElogException );

        return;
}

PyMODINIT_FUNC
init_Pkt( void ) 
{
	PyObject *mod;

	mod = Py_InitModule( "_Pkt", _Pkt_methods );

	add_pkt_constants( mod );

	add_elog_exception( mod );
}
