#include <iostream>
#include <vector>
#include <cstdlib>
#include <cmath>
#include <algorithm>
#include <numeric>
#include <iterator>
#include <Eigen/Dense>
#include <Eigen/Eigenvalues>
#include "picker.h"
#include "Python.h"
#include "numpy/arrayobject.h"

using std::vector;

//static inline double square (double x) { return x*x; }

void PyArrayToVector (PyArrayObject* arr, vector<double>& out, int n) {
    int i;
    double* p;
    out.resize(n);
    for (i = 0; i < n; i++) {
        p = (double*) PyArray_GETPTR1(arr, i);
        out[i] = *p;
    }
    return;
}

static PyObject* dbshear (PyObject *dummy, PyObject *args) {
    int i, p_pick;
    double s1_pick, s2_pick, snr_s1, snr_s2, dt, cov_len, k_len;
    double sta(1.0), lta(5.0);
    vector<double> N, E, Z;

    // Python wrapper declarations
    PyObject *arg1=NULL, *arg2=NULL, *arg3=NULL;
    PyArrayObject *PZ=NULL, *PN=NULL, *PE=NULL;
    PyArrayObject *filter, *K1, *K2, *S1, *S2;

    if (!PyArg_ParseTuple(args, "OOOddd", &arg1, &arg2,
                          &arg3, &cov_len, &dt, &k_len)) {
        return NULL;
    }
    PZ = (PyArrayObject*)PyArray_FROM_OTF(arg1, NPY_DOUBLE, NPY_IN_ARRAY);
    if (PZ == NULL) return NULL;
    PN = (PyArrayObject*)PyArray_FROM_OTF(arg2, NPY_DOUBLE, NPY_IN_ARRAY);
    if (PN == NULL) return NULL;
    PE = (PyArrayObject*)PyArray_FROM_OTF(arg3, NPY_DOUBLE, NPY_IN_ARRAY);
    if (PE == NULL)return NULL;

    // Various output related definitions for Python wrapper
    // Find the number of elements in the spectrum
    npy_intp *nz = PyArray_DIMS(PZ);
    npy_intp *nn = PyArray_DIMS(PN);
    npy_intp *ne = PyArray_DIMS(PE);
    PyArrayToVector(PZ, Z, *nz);
    PyArrayToVector(PN, N, *nn);
    PyArrayToVector(PE, E, *ne);

    // Check that all three traces are the same length
    if ((*nz != *ne) || (*nz != *nn) || (*ne != *nn)) return NULL;
    vector<double> pol_fltr,  cftK1(*nz), cftK2(*nz);
    vector<double> cftS1(*nz), cftS2(*nz), snr;

    // Begin picking algorithm & calculate polarization filter
    Polarizer polar_fltr(cov_len, dt);
    pol_fltr = polar_fltr.filter(Z, N, E);

    // Calculate STA/LTA and try to find correct trigger window
    int start, stop;
    double peak_snr;
    snr = lstalta(Z, int(sta/dt), int(lta/dt));
    trigger(snr, 5.0, 2.5, 2.0, dt, start, stop, peak_snr);
    if (start == -1 || stop == -1) {
        p_pick = -1;
    }
    else {
        p_pick = start;
    }
    vector<double> kurt;
    kurt = kurtosis(Z, int(k_len/dt));
    // Window around trial P-pick to refine through k-rate
    start = p_pick - int(1.0/dt);
    stop = p_pick + int(1.0/dt);
    if (stop > kurt.size()) stop = kurt.size()-1;
    vector<double>::iterator it = kurt.begin();
    p_pick = std::distance(it, std::max_element(it+start, it+stop));

    // Attempt to pick S-waves on both horizontals
    ShearPicker SPicker(dt, k_len, sta, lta, 5.0, 2.5, 2, p_pick, pol_fltr);
    SPicker.pick(N, s1_pick, snr_s1);
    cftS1 = SPicker.get_cftS();
    cftK1 = SPicker.get_cftK();
    SPicker.pick(E, s2_pick, snr_s2);
    cftS2 = SPicker.get_cftS();
    cftK2 = SPicker.get_cftK();

    filter = (PyArrayObject*) PyArray_SimpleNew(1, nz, NPY_DOUBLE);
    K1 = (PyArrayObject*) PyArray_SimpleNew(1, nz, NPY_DOUBLE);
    K2 = (PyArrayObject*) PyArray_SimpleNew(1, nz, NPY_DOUBLE);
    S1 = (PyArrayObject*) PyArray_SimpleNew(1, nz, NPY_DOUBLE);
    S2 = (PyArrayObject*) PyArray_SimpleNew(1, nz, NPY_DOUBLE);
    PyArray_FILLWBYTE(filter, 0);
    PyArray_FILLWBYTE(K1, 0);
    PyArray_FILLWBYTE(K2, 0);
    PyArray_FILLWBYTE(S1, 0);
    PyArray_FILLWBYTE(S2, 0);
    double *ptr;
    for (i = 0; i < *nz; i++) {
        ptr = (double*)PyArray_GETPTR1(filter, i);
        *ptr = pol_fltr[i];
        ptr = (double*)PyArray_GETPTR1(K1, i);
        *ptr = cftK1[i];
        ptr = (double*)PyArray_GETPTR1(K2, i);
        *ptr = cftK2[i];
        ptr = (double*)PyArray_GETPTR1(S1, i);
        *ptr = cftS1[i];
        ptr = (double*)PyArray_GETPTR1(S2, i);
        *ptr = cftS2[i];
    }
    Py_INCREF(filter);
    Py_INCREF(K1);
    Py_INCREF(K2);
    Py_INCREF(S1);
    Py_INCREF(S2);
    Py_DECREF(PZ);
    Py_DECREF(PN);
    Py_DECREF(PE);
    return Py_BuildValue("ffffOOOOO", s1_pick, s2_pick, snr_s1, snr_s2,
                         filter, S1, S2, K1, K2);
}

static struct PyMethodDef methods[] =
{
    {"dbshear", dbshear, METH_VARARGS, "Polarization filters recursively"},
    {NULL, NULL, 0, NULL}
};

static struct PyModuleDef dbshear_moddef = {
    PyModuleDef_HEAD_INIT,
    "dbshear", /* name of module */
    "",        /* module documentation */
    -1,        /* size of per-interpreter state of the module, or -1 if the moduel keeps state in global variables */
    methods
};


PyMODINIT_FUNC PyInit_dbshear (void)
{
    return PyModule_Create(&dbshear_moddef);
}
