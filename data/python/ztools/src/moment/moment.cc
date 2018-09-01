#include <iostream>
#include <vector>
#include <cstdlib>
#include <cmath>
#include "Python.h"
#include "numpy/arrayobject.h"

#define TPI 6.283185307
#define PI 3.14159265359

double source_model2 (double f, double omega, double fc, double n, double Q, double t) {
    // Basic displacement spectrum source model
    return TPI * f * exp(-PI*f*t/Q)* omega / (1 + pow(f/fc, n));
}
static PyObject* fit_spectrum2 (PyObject *dummy, PyObject *args) {
    // Declarations
    double best_o, best_fc, best_n, best_Q;
    double ssq, best_ssq, t;
    int i, j, k, l, f;
    PyObject *arg1=NULL, *arg2=NULL, *arg3=NULL, *arg4=NULL, *arg5=NULL, *arg6=NULL;
    PyArrayObject *spec=NULL, *freqs=NULL, *list_fc=NULL;
    PyArrayObject *list_o=NULL, *list_n=NULL, *list_Q;

    // Parse python arguments
    if (!PyArg_ParseTuple(args, "OOOOOOd", &arg1, &arg2, &arg3, &arg4, &arg5, \
                          &arg6, &t))
        return NULL;

    // Convert python objects into numpy array type
    spec = (PyArrayObject*)PyArray_FROM_OTF(arg1, NPY_DOUBLE, NPY_IN_ARRAY);
    if (spec == NULL) return NULL;
    freqs = (PyArrayObject*)PyArray_FROM_OTF(arg2, NPY_DOUBLE, NPY_IN_ARRAY);
    if (freqs == NULL) return NULL;
    list_fc = (PyArrayObject*)PyArray_FROM_OTF(arg3, NPY_DOUBLE, NPY_IN_ARRAY);
    if (list_fc == NULL)return NULL;
    list_o = (PyArrayObject*)PyArray_FROM_OTF(arg4, NPY_DOUBLE, NPY_IN_ARRAY);
    if (list_o == NULL) return NULL;
    list_n = (PyArrayObject*)PyArray_FROM_OTF(arg5, NPY_DOUBLE, NPY_IN_ARRAY);
    if (list_n == NULL) return NULL;
    list_Q = (PyArrayObject*)PyArray_FROM_OTF(arg6, NPY_DOUBLE, NPY_IN_ARRAY);
    if (list_Q == NULL) return NULL;

    // Find the number of elements in the spectrum
    npy_intp *n_freq = PyArray_DIMS(spec);
    npy_intp *num_fc = PyArray_DIMS(list_fc);
    npy_intp *num_o = PyArray_DIMS(list_o);
    npy_intp *num_n = PyArray_DIMS(list_n);
    npy_intp *num_Q = PyArray_DIMS(list_Q);

    // Simple least-sq grid search over the o, fc, and n ranges provided
    double *sp, *freq, *fc, *o, *n, *Q, func;
    best_o = *((double*)PyArray_GETPTR1(list_o, 0));
    best_fc = *((double*)PyArray_GETPTR1(list_fc, 0));
    best_n = *((double*)PyArray_GETPTR1(list_n, 0));
    best_Q = *((double*)PyArray_GETPTR1(list_Q, 0));
    best_ssq = 1.0/0.0;
    for (i = 0; i < *num_o; i++) {
        for (j = 0; j < *num_fc; j++) {
            for (k = 0; k < *num_n; k++) {
                for (l = 0; l < *num_Q; l++) {
                    ssq = 0;
                    for (f = 0; f < *n_freq; f++) {
                        o = (double*)PyArray_GETPTR1(list_o, i);
                        fc = (double*)PyArray_GETPTR1(list_fc, j);
                        n = (double*)PyArray_GETPTR1(list_n, k);
                        Q = (double*)PyArray_GETPTR1(list_Q, l);
                        freq = (double*)PyArray_GETPTR1(freqs, f);
                        func = source_model2(*freq, *o, *fc, *n, *Q, t);
                        sp = (double*)PyArray_GETPTR1(spec, f);
                        //ssq = ssq + pow(*sp-func, 2);
                        ssq = ssq + abs(*sp-func);
                    }
                    if (ssq < best_ssq) {
                        best_ssq = ssq;
                        best_o = *o;
                        best_fc = *fc;
                        best_n = *n;
                        best_Q = *Q;
                    }
                }
            }
        }
    }

    Py_DECREF(spec);
    Py_DECREF(freqs);
    Py_DECREF(list_fc);
    Py_DECREF(list_o);
    Py_DECREF(list_n);
    Py_DECREF(list_Q);
    return Py_BuildValue("dddd", best_fc, best_o, best_n, best_Q);
}

double source_model (double f, double omega, double fc, double n) {
    // Basic displacement spectrum source model
    //return TPI * f * omega / (1 + pow(f/fc, n));
    return omega / (1 + pow(f/fc, n));
}

static PyObject* fit_spectrum (PyObject *dummy, PyObject *args) {
    // Declarations
    double best_o, best_fc, best_n;
    double ssq, best_ssq;
    int i, j, k, f;
    PyObject *arg1=NULL, *arg2=NULL, *arg3=NULL, *arg4=NULL, *arg5=NULL;
    PyArrayObject *spec=NULL, *freqs=NULL, *list_fc=NULL;
    PyArrayObject *list_o=NULL, *list_n=NULL;

    // Parse python arguments
    if (!PyArg_ParseTuple(args, "OOOOO", &arg1, &arg2, &arg3, &arg4, &arg5))
        return NULL;

    // Convert python objects into numpy array type
    spec = (PyArrayObject*)PyArray_FROM_OTF(arg1, NPY_DOUBLE, NPY_IN_ARRAY);
    if (spec == NULL) return NULL;
    freqs = (PyArrayObject*)PyArray_FROM_OTF(arg2, NPY_DOUBLE, NPY_IN_ARRAY);
    if (freqs == NULL) return NULL;
    list_fc = (PyArrayObject*)PyArray_FROM_OTF(arg3, NPY_DOUBLE, NPY_IN_ARRAY);
    if (list_fc == NULL)return NULL;
    list_o = (PyArrayObject*)PyArray_FROM_OTF(arg4, NPY_DOUBLE, NPY_IN_ARRAY);
    if (list_o == NULL) return NULL;
    list_n = (PyArrayObject*)PyArray_FROM_OTF(arg5, NPY_DOUBLE, NPY_IN_ARRAY);
    if (list_n == NULL) return NULL;

    // Find the number of elements in the spectrum
    npy_intp *n_freq = PyArray_DIMS(spec);
    npy_intp *num_fc = PyArray_DIMS(list_fc);
    npy_intp *num_o = PyArray_DIMS(list_o);
    npy_intp *num_n = PyArray_DIMS(list_n);

    // Simple least-sq grid search over the o, fc, and n ranges provided
    double *sp, *freq, *fc, *o, *n, func;
    best_o = *((double*)PyArray_GETPTR1(list_o, 0));
    best_fc = *((double*)PyArray_GETPTR1(list_fc, 0));
    best_n = *((double*)PyArray_GETPTR1(list_n, 0));
    best_ssq = 1.0/0.0;
    for (i = 0; i < *num_o; i++) {
        for (j = 0; j < *num_fc; j++) {
            for (k = 0; k < *num_n; k++) {
                ssq = 0;
                for (f = 0; f < *n_freq; f++) {
                    o = (double*)PyArray_GETPTR1(list_o, i);
                    fc = (double*)PyArray_GETPTR1(list_fc, j);
                    n = (double*)PyArray_GETPTR1(list_n, k);
                    freq = (double*)PyArray_GETPTR1(freqs, f);
                    func = source_model(*freq, *o, *fc, *n);
                    sp = (double*)PyArray_GETPTR1(spec, f);
                    ssq = ssq + pow(*sp-func, 2);
                    //ssq = ssq + abs(*sp-func);
                }
                if (ssq < best_ssq) {
                    best_ssq = ssq;
                    best_o = *o;
                    best_fc = *fc;
                    best_n = *n;
                }
            }
        }
    }

    Py_DECREF(spec);
    Py_DECREF(freqs);
    Py_DECREF(list_fc);
    Py_DECREF(list_o);
    Py_DECREF(list_n);
    return Py_BuildValue("ddd", best_fc, best_o, best_n);
}

static struct PyMethodDef methods[] =
{
    {"fit_spectrum", fit_spectrum, METH_VARARGS, "fit a source model to spectrum"},
    {"fit_spectrum2", fit_spectrum2, METH_VARARGS, "fit a source model to spectrum"},
    {NULL, NULL, 0, NULL}
};


PyMODINIT_FUNC initmoment (void)
{
    (void)Py_InitModule("moment", methods);
    import_array();
}
