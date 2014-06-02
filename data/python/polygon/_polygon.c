#include <stdio.h>
#include <math.h>
#include <string.h>
#include "Python.h"
#include "coords.h"
#include "elog.h"
#include "pyutil.h"

#include "polygon.h"

#define USAGE squawk( usage ) 


static PyObject *python_inwhichpolygons( PyObject *self, PyObject *args );
static PyObject *python_windrose( PyObject *self, PyObject *args );

PyMODINIT_FUNC init_polygon( void );

static struct PyMethodDef _polygon_methods[] = {
	{ "_inwhichpolygons",  	python_inwhichpolygons, METH_VARARGS, "find polygon enclosing point" },
	{ "_windrose",  	    python_windrose,   	    METH_VARARGS, "windrose, 3 chars indicating direction" },
	{ NULL, NULL, 0, NULL }
};


static PyObject *
python_inwhichpolygons( PyObject *self, PyObject *args ) {
	char    *usage = "Usage: dbout= _polygon._inwhichpolygons( dbin, lat, lon )\n";
	Dbptr	db,dbr;
	Point 	P;
	double  lat,lon;
	
	if( ! PyArg_ParseTuple( args, "O&dd",parse_to_Dbptr,&db, &lat, &lon ) ) {

		USAGE;

		return NULL;
	}
	P.lat=lat;
	P.lon=lon;
	dbr=inWhichPolygons(db,P);
	return Dbptr2PyObject(dbr);
	return NULL;
}
static PyObject *
python_windrose( PyObject *self, PyObject *args ) {
	char    *usage = "Usage: wr= _polygon._windrose( azimuth )\n";
	PyObject *obj;
	double  azimuth;
	char	*wr;
	
	if( ! PyArg_ParseTuple( args, "d", &azimuth ) ) {

		USAGE;

		return NULL;
	}
	wr=windrose( azimuth );

	obj = Py_BuildValue("s", wr);
	return obj;
	
}
	

PyMODINIT_FUNC
init_polygon( void ) {
    PyObject *mod;

    mod = Py_InitModule( "_polygon", _polygon_methods );

}
