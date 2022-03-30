#include <stdio.h>
#include <math.h>
#include <string.h>
#include "Python.h"
#include "xtra.h"
#include "elog.h"
#include "stock.h"
#include "pyutil.h"

#define USAGE squawk( "%s\n", usage )


struct module_state
{
    PyObject *error;
};

static PyObject *python_dbget_calib (PyObject * self, PyObject * args);
static PyObject *python_TIME2SAMP (PyObject * self, PyObject * args);
static PyObject *python_SAMP2TIME (PyObject * self, PyObject * args);
static PyObject *python_SAMPRATE (PyObject * self, PyObject * args);
static PyObject *python_NSAMP (PyObject * self, PyObject * args);
static PyObject *python_ENDTIME (PyObject * self, PyObject * args);
static PyObject *python_dbget_remark (PyObject * self, PyObject * args);
static PyObject *python_dbadd_remark (PyObject * self, PyObject * args);

static struct PyMethodDef _missing_methods[] = {
    {"_dbget_calib", python_dbget_calib, METH_VARARGS,
     "get calibration values"},
    {"_TIME2SAMP", python_TIME2SAMP, METH_VARARGS,
     "get number of samples"},
    {"_SAMP2TIME", python_SAMP2TIME, METH_VARARGS,
     "get endtime"},
    {"_SAMPRATE", python_SAMPRATE, METH_VARARGS,
     "get sampling rate"},
    {"_NSAMP", python_NSAMP, METH_VARARGS,
     "get number of samples between start- and endtime"},
    {"_ENDTIME", python_ENDTIME, METH_VARARGS,
     "get endtime from samprate and number of samples"},
    {"_dbget_remark", python_dbget_remark, METH_VARARGS,
     "get remark for specified record and table"},
    {"_dbadd_remark", python_dbadd_remark, METH_VARARGS,
     "add remark for specified record and table. Returns commid"},
    {NULL, NULL, 0, NULL}
};

static struct PyModuleDef _missing_module = {
    PyModuleDef_HEAD_INIT,
    "missing",                  /* name of module */
    NULL,                       /* module documentation, may be NULL */
    -1,                         /* size of per-interpreter state of the module,
                                   or -1 if the module keeps state in global variables. */
    _missing_methods
};

PyMODINIT_FUNC PyInit__missing (void)
{
    return PyModule_Create (&_missing_module);
}

static PyObject *python_dbget_calib (PyObject * self, PyObject * args)
{
    char *usage = "Usage: _missing._dbget_calib( sta, chan, time, dbname )\n";
    PyObject *obj;
    char *sta;
    char *chan;
    double time;
    char *dbname;
    int result = 0;
    double calib = 0.0;
    double calper = -1.0;
    char segtype[4];


    if (!PyArg_ParseTuple (args, "ssds", &sta, &chan, &time, &dbname)) {
        USAGE;
        return NULL;
    }
    result = dbget_calib (sta, chan, time, dbname, &calib, &calper, segtype);

    obj = PyTuple_New (3);
    PyTuple_SetItem (obj, 0, PyFloat_FromDouble (calib));
    PyTuple_SetItem (obj, 1, PyFloat_FromDouble (calper));
    PyTuple_SetItem (obj, 2, PyUnicode_FromString (segtype));
    return obj;

}

static PyObject *python_TIME2SAMP (PyObject * self, PyObject * args)
{
    char *usage =
        "Usage: _missing._TIME2SAMP( starttime, samprate, endtime )\n";
    PyObject *obj;
    double starttime, endtime;
    double samprate;
    long nsamp;
    if (!PyArg_ParseTuple (args, "ddd", &starttime, &samprate, &endtime)) {
        USAGE;
        return NULL;
    }
    nsamp = TIME2SAMP (starttime, samprate, endtime);
    obj = Py_BuildValue ("l", nsamp);
    return obj;
}

static PyObject *python_SAMP2TIME (PyObject * self, PyObject * args)
{
    char *usage = "Usage: _missing._SAMP2TIME(starttime, samprate, nsamp)\n";
    PyObject *obj;
    double starttime, endtime;
    double samprate;
    long nsamp;
    if (!PyArg_ParseTuple (args, "ddl", &starttime, &samprate, &nsamp)) {
        USAGE;
        return NULL;
    }
    endtime = SAMP2TIME (starttime, samprate, nsamp);
    obj = Py_BuildValue ("f", endtime);
    return obj;
}

static PyObject *python_SAMPRATE (PyObject * self, PyObject * args)
{
    char *usage = "Usage: _missing._SAMPRATE( starttime, nsamp, endtime )\n";
    PyObject *obj;
    double starttime, endtime;
    double samprate;
    long nsamp;
    if (!PyArg_ParseTuple (args, "dld", &starttime, &nsamp, &endtime)) {
        USAGE;
        return NULL;
    }
    samprate = SAMPRATE (starttime, nsamp, endtime);
    obj = PyFloat_FromDouble (samprate);
    return obj;
}

static PyObject *python_NSAMP (PyObject * self, PyObject * args)
{
    char *usage = "Usage: _missing._NSAMP( starttime, samprate, endtime )\n";
    PyObject *obj;
    double starttime, endtime;
    double samprate;
    long nsamp;
    if (!PyArg_ParseTuple (args, "ddd", &starttime, &samprate, &endtime)) {
        USAGE;
        return NULL;
    }
    nsamp = NSAMP (starttime, samprate, endtime);
    obj = Py_BuildValue ("l", nsamp);
    return obj;
}

static PyObject *python_ENDTIME (PyObject * self, PyObject * args)
{
    char *usage = "Usage: _missing._ENDTIME( starttime, samprate, nsamp )\n";
    PyObject *obj;
    double starttime, endtime;
    double samprate;
    long nsamp;
    if (!PyArg_ParseTuple (args, "ddl", &starttime, &samprate, &nsamp)) {
        USAGE;
        return NULL;
    }
    endtime = ENDTIME (starttime, samprate, nsamp);
    obj = Py_BuildValue ("d", endtime);
    return obj;
}
static PyObject *python_dbget_remark (PyObject * self, PyObject * args)
{
    char *usage = "Usage: remark = _missing._dbget_remark( db )\n";
    PyObject *obj;

    Dbptr db;
    int result;
    char *remark=NULL;
    if (!PyArg_ParseTuple (args, "O&", parse_to_Dbptr,&db)) {
        USAGE;
        return NULL;
    }
    printf("%ld %ld %ld %ld\n", db.database, db.table, db.record, db.field);
    result = dbget_remark (db, &remark);
    obj = Py_BuildValue ("s", remark);
    return obj;
}
static PyObject *python_dbadd_remark (PyObject * self, PyObject * args)
{
    char *usage = "Usage: _missing._dbadd_remark( db, remark )\n";
    PyObject *obj;

    Dbptr db;
    long commid;
    char *remark=NULL;
    if (!PyArg_ParseTuple (args, "O&s", parse_to_Dbptr,&db, &remark)) {
        USAGE;
        return NULL;
    }
    commid = dbadd_remark (db, remark);
    obj = Py_BuildValue ("l", commid);
    return obj;
}
