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


    if (!PyArg_ParseTuple(args, "OOOddd", &PZ, &PN,
                          &PE, &cov_len, &dt, &k_len)) {
        return NULL;
    }

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
    SPicker.pick(E, s2_pick, snr_s2);

    //Py_DECREF(PZ);
    //Py_DECREF(PN);
    //Py_DECREF(PE);

    return Py_BuildValue("ffff", s1_pick, s2_pick, snr_s1, snr_s2);
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
