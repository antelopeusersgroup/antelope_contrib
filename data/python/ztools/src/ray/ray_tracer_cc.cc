#include <iostream>
#include <vector>
#include <cstdlib>
#include <cmath>
#include <algorithm>
#include <numeric>
#include <iterator>
#include "Python.h"
#include "numpy/arrayobject.h"

using std::vector;

int layer_xt(double p, double h, double u_top, double u_bot,
            double &dx, double &dt) {
    // From Shearer's Introduction to Seismology
    // Inputs:
    //       p     = horizontal slowness
    //       h     = layer thickness
    //       u_top = slowness at top of layer
    //       u_bot = slowness at bottom of layer
    // Returns:
    //       dx    = range offset (horiz distance)
    //       dt    = travel time
    //       irtr  = return code
    //             = -1, zero thickness layer
    //             = 0, ray turned above layer
    //             = 1, ray passed through layer
    //             = 2, ray turned in layer, only 1 leg of path counted.

    double u1, u2, v1, v2, b, eta1, eta2, x1, x2, tau1, tau2, dtau;

    if (p >= u_top) {
        dx = 0;
        dt = 0;
        return 0;
    }
    else if (h == 0) {
        dx = 0;
        dt = 0;
        return -1;
    }

    u1 = u_top;
    u2 = u_bot;
    v1 = 1./u1;
    v2 = 1./u2;
    b = (v2 - v1)/h;

    eta1 = sqrt(u1*u1 - p*p);

    if (b == 0) {
        dx = h*p/eta1;
        dt = h*u1*u1/eta1;
        return 1;
    }

    x1 = eta1 / (u1*b*p);
    tau1 = (log((u1+eta1)/p)-eta1/u1)/b;
    if (p >= u_bot) {
        dx = x1;
        dtau = tau1;
        dt = dtau + p*dx;
        return 2;
    }

    eta2 = sqrt(u2*u2 - p*p);
    x2 = eta2/(u2*b*p);
    tau2 = (log((u2+eta2)/p)-eta2/u2)/b;

    dx = x1-x2;
    dtau = tau1-tau2;

    dt = dtau + p*dx;
    return 1;
}

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

static PyObject* ray_trace (PyObject *dummy, PyObject *args) {
    vector<double> layers, c, Q;
    double inc, evdp, b, x(0), t(0), dx, dt, status, p, t_star(0), r;
    int i_src(0), i;

    // Python wrapper declarations
    PyObject *arg1=NULL, *arg2=NULL, *arg3=NULL;
    PyArrayObject *py_layers=NULL, *py_c=NULL, *py_Q=NULL;

    if (!PyArg_ParseTuple(args, "OOOdd", &arg1, &arg2, &arg3, &inc, &evdp)) {
        return NULL;
    }
    py_layers = (PyArrayObject*)PyArray_FROM_OTF(arg1, NPY_DOUBLE, NPY_IN_ARRAY);
    if (py_layers == NULL) return NULL;
    py_c = (PyArrayObject*)PyArray_FROM_OTF(arg2, NPY_DOUBLE, NPY_IN_ARRAY);
    if (py_c == NULL) return NULL;
    py_Q = (PyArrayObject*)PyArray_FROM_OTF(arg3, NPY_DOUBLE, NPY_IN_ARRAY);
    if (py_Q == NULL)return NULL;

    // Various output related definitions for Python wrapper
    // Find the number of elements in the spectrum
    npy_intp *nlay = PyArray_DIMS(py_layers);
    PyArrayToVector(py_layers, layers, *nlay);
    PyArrayToVector(py_c, c, *nlay);
    PyArrayToVector(py_Q, Q, *nlay);

    // Check first to see if source is on a boundary; else split layer in two
    vector<double>::iterator it;
    if (!std::binary_search(layers.begin(), layers.end(), evdp)) {
        for (i = 1; i < *nlay; i++) {
            if (evdp < layers[i]) {
                b = (c[i] - c[i-1])/(layers[i] - layers[i-1]);
                it = c.begin()+i;
                c.insert(it, b * (evdp - layers[i-1]) + c[i-1]);

                b = (Q[i] - Q[i-1])/(layers[i] - layers[i-1]);
                it = Q.begin()+i;
                Q.insert(it, b * (evdp - layers[i-1]) + Q[i-1]);

                it = layers.begin()+i;
                layers.insert(it, evdp);
                i_src = i;

                *nlay += 1;
                break;
            }
        }
    }
    else {
        it = std::find(layers.begin(), layers.end(), evdp);
        i_src = std::distance(layers.begin(), it);
    }
    // Initialize p
    p = sin(inc*M_PI/180.0) / c[i_src];
    for (i = 1; i < *nlay; i++) {
        status = layer_xt(p, layers[i]-layers[i-1], 1/c[i-1], 1/c[i], dx, dt);
        //std::cout << "** " << dx << " " << dt << " " << i << " " << i_src << " "<< status << std::endl;
        //if (status == 0) {
        //    std::cout << "ERROR: p less than uppermost slowness" << std::endl;
        //    return NULL;
        //}
        if (i > i_src) {
            // Branch for down-going rays
            x += 2*dx;
            t += 2*dt;
            t_star += 2*dt / ((Q[i]+Q[i-1])/2.0);
        }
        else {
            // Branch for up-going rays
            x += dx;
            t += dt;
            t_star += dt / ((Q[i]+Q[i-1])/2.0);
        }
        if (inc > 90) {
            if (i == i_src) {
                break;
            }
        }
        if (status == 2) break;
    }
    /*
    if (i == *nlay) {
        x = 0.0 / 0.0;
        t = 0.0 / 0.0;
        t_star = 0.0 / 0.0;
    }
    */
    Py_DECREF(py_layers);
    Py_DECREF(py_c);
    Py_DECREF(py_Q);
    return Py_BuildValue("fff", x, t, t_star);
}

static struct PyMethodDef methods[] =
{
    {"ray_trace", ray_trace, METH_VARARGS, "Ray tracer / t* calculator"},
    {NULL, NULL, 0, NULL}
};


PyMODINIT_FUNC initray_tracer (void)
{
    (void)Py_InitModule("ray_tracer", methods);
    import_array();
}
