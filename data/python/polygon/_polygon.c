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
static PyObject *python_distancetopolygon( PyObject *self, PyObject *args );
static PyObject *python_readpolygon( PyObject *self, PyObject *args );
static PyObject *python_writepolygon( PyObject *self, PyObject *args );
static PyObject *python_windrose( PyObject *self, PyObject *args );

static struct PyMethodDef _polygon_methods[] = {
	{ "_inwhichpolygons",  	python_inwhichpolygons,   METH_VARARGS, "find polygon enclosing point" },
	{ "_distancetopolygon", python_distancetopolygon, METH_VARARGS, "find minimum distance to polygon" },
	{ "_readpolygon",       python_readpolygon,       METH_VARARGS, "read polygon from database" },
//	{ "_writepolygon",      python_writpolygon,       METH_VARARGS, "read polygon from database" },
	{ "_windrose",  	    python_windrose,   	      METH_VARARGS, "windrose, 3 chars indicating direction" },
	{ NULL, NULL, 0, NULL }
};


static struct PyModuleDef _polygon_module = {
    PyModuleDef_HEAD_INIT,
    "polygon",   /* name of module */
    NULL,       /* module documentation, may be NULL */
    -1,         /* size of per-interpreter state of the module,
                or -1 if the module keeps state in global variables. */
    _polygon_methods
};

PyMODINIT_FUNC
PyInit__polygon(void)
{
    return PyModule_Create(&_polygon_module);
}

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
}
static PyObject *
python_distancetopolygon( PyObject *self, PyObject *args ) {
	char    *usage = "Usage: dbout= _polygon._distancetopolygon( dbin, lat, lon )\n";
	Dbptr	db;
	Point 	P;
	double  lat,lon;
    double  mindist;
	PyObject *obj;
	
	if( ! PyArg_ParseTuple( args, "O&dd",parse_to_Dbptr,&db, &lat, &lon ) ) {

		USAGE;

		return NULL;
	}
	P.lat=lat;
	P.lon=lon;
	mindist=distanceToPolygon(db,P);
	obj =  Py_BuildValue( "d", mindist );
	return obj;
}

static PyObject *
python_readpolygon( PyObject *self, PyObject *args ) {
	char    *usage = "Usage: dbout= _polygon._readpolygon( dbin )\n";
	Dbptr	db;
	Point 	*poly;
    char    pname[1024];
    long    npolygons, nvertices;
	PyObject *obj;
	
	if( ! PyArg_ParseTuple( args, "O&",parse_to_Dbptr,&db ) ) {

		USAGE;

		return NULL;
	}
    if (db.table < 0) {
        db=dblookup(db, 0, "polygon", 0, 0);
    }
    dbquery(db,dbRECORD_COUNT, &npolygons);
    for (db.record=0; db.record < npolygons; db.record++) {
        nvertices=readPolygon(db, &poly);
        dbgetv(db, 0, "pname", &pname); 
        for (int i=0; i< nvertices; i++) {
            printf("%.4f %.4f\n", poly[i].lon, poly[i].lat);
        }
        free(poly);
    }

	obj =  Py_BuildValue( "l", nvertices );
	return obj;
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
