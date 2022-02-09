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
	{ "_writepolygon",      python_writepolygon,      METH_VARARGS, "write polygon to database" },
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
	char    *usage = "Usage: poly = _polygon._readpolygon( dbin )\n";
	Dbptr	db;
	Point 	*poly;
    char    pname[1024];
    long    npolygons, nvertices;
	PyObject *obj;
	PyObject *poby;
	
	if( ! PyArg_ParseTuple( args, "O&",parse_to_Dbptr,&db ) ) {

		USAGE;

		return NULL;
	}
    if (db.table < 0) {
        db=dblookup(db, 0, "polygon", 0, 0);
    }
    dbquery(db,dbRECORD_COUNT, &npolygons);
    obj = PyList_New(npolygons);
    //could check here if we have more than one polygon
    for (db.record=0; db.record < npolygons; db.record++) {
        nvertices=readPolygon(db, &poly);
        poby = PyList_New(nvertices);
        //dbgetv(db, 0, "pname", &pname); 
        for (int i=0; i< nvertices; i++) {
            PyObject *item = PyTuple_New(2);
            PyTuple_SET_ITEM(item, 0, PyFloat_FromDouble(poly[i].lon));
            PyTuple_SET_ITEM(item, 1, PyFloat_FromDouble(poly[i].lat));
            PyList_SetItem(poby,i,item);
        }
        free(poly);
        PyList_SetItem(obj,db.record, poby);
    }

	return obj;
}
static PyObject *
python_writepolygon( PyObject *self, PyObject *args ) {
	char    *usage = "Usage: _polygon._writepolygon( dbout, polygon, pname, closed, level, ptype, auth, dir, dfile )\n";
    static PyObject *obj;
    PyObject *p_obj;
    Dbptr db;
    char *pname = NULL;
    Point *poly;
    long plen;
    int closed;
    int level;
    char *ptype = NULL;
    char *auth = NULL;
    char *dir = NULL;
    char *dfile = NULL;
    int recno;
    int ftype=polyFLOAT;

	if( ! PyArg_ParseTuple( args, "O&Ospnssss", parse_to_Dbptr, &db, &p_obj, &pname, &closed, &level,&ptype,&auth,&dir,&dfile  ) ) {

		USAGE;

		return NULL;
    }
	if (!PyList_Check( p_obj)) {
		USAGE;
		return NULL;
    }
    plen = PyList_Size( p_obj );
    poly = malloc(2 * plen * sizeof(double));
    poly[0].lon = 12.3;
    poly[0].lat = 12.3;
    for (int i=0; i<plen;i++) {
        PyObject *tuple = PyList_GetItem(p_obj, i);
        poly[i].lon = PyFloat_AsDouble( PyTuple_GetItem( tuple, 0 ) );
        poly[i].lat = PyFloat_AsDouble( PyTuple_GetItem( tuple, 1 ) );
    }

    recno = writePolygonData(db, poly, plen, pname, closed, level, ptype, auth, dir, dfile, ftype);
    free(poly);
	obj = Py_BuildValue("l", recno);
    /**
      if (closed) {
        printf("%s\n",pname);
    } else {
        printf(" nicht: %s %s %s\n",pname, dir, dfile);
    }
    **/
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
