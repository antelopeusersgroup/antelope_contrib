#include <Python.h>
#include <float.h>

static PyObject *
core_tools_c_grid_search(PyObject *self, PyObject *args)
{
    PyObject* qx;
    PyObject* qy;
    PyObject* qz;
    PyObject* ix;
    PyObject* iy;
    PyObject* iz;
    PyObject* arrs;
    PyObject* pred_tts;
    PyObject* lind;
    PyObject* sta = NULL;
    PyObject* pha = NULL;
    PyObject* ptt;
    PyObject* at;
    PyObject* estots;
    PyObject* resids;
    PyObject* temp;
    Py_ssize_t index;
    int qxl;
    int qyl;
    int qzl;
    int arrsl;
    int i,j,k,l;
    int flag;
    long ist, jst, kst;
    double estot;
    double msft;
    double bmsft;
    double bot;

    if (!PyArg_ParseTuple(args, "O!O!O!O!O!O",
                          &PyList_Type, &qx,
                          &PyList_Type, &qy,
                          &PyList_Type, &qz,
                          &PyList_Type, &arrs,
                          &PyDict_Type, &pred_tts,
                          &lind))
        return NULL;
    ist = 0;
    jst = 0;
    kst = 0;
    qxl = PyList_Size(qx);
    qyl = PyList_Size(qy);
    qzl = PyList_Size(qz);
    arrsl = PyList_Size(arrs);

    bmsft = DBL_MAX;
    for (i = ist; i < qxl; i++ ){
        for (j = jst; j < qyl; j++ ){
            for (k = kst; k < qzl; k++ ){
                bot = 4.0;
                estots = PyList_New(0);
                resids = PyList_New(0);
                temp = PyObject_CallMethod(lind, "convert_to_1D", "iii", i, j, k);
                index = PyInt_AsSsize_t(temp);
                Py_DECREF(temp);
                flag = 0;
                for (l = 0; l < arrsl; l++){
                    sta = PyObject_GetAttrString(PyList_GetItem(arrs, l), "sta");
                    pha = PyObject_GetAttrString(PyList_GetItem(arrs, l), "phase");
                    ptt = PyList_GetItem(PyDict_GetItem(PyDict_GetItem(pred_tts, sta), pha), index);
                    if ( PyFloat_AsDouble(ptt) < 0 ){
                        flag = 1;
                        Py_DECREF(sta);
                        Py_DECREF(pha);
                        break;
                    }
                    at = PyObject_GetAttrString(PyList_GetItem(arrs, l), "time");
                    temp = PyFloat_FromDouble(PyFloat_AsDouble(at) - PyFloat_AsDouble(ptt));
                    PyList_Append(estots, temp);
                    Py_DECREF(at);
                    Py_DECREF(temp);
                    Py_DECREF(sta);
                    Py_DECREF(pha);
                }
                if ( flag == 1 || PyList_Size(estots) == 0){
                    Py_DECREF(estots);
                    Py_DECREF(resids);
                    continue;
                }
                estot = 0.0;
                for (l = 0; l < PyList_Size(estots); l++){
                    estot += PyFloat_AsDouble(PyList_GetItem(estots, l));
                }
                estot /= (double) l;
                for (l = 0; l < PyList_Size(estots); l++){
                    temp = PyFloat_FromDouble(PyFloat_AsDouble(PyList_GetItem(estots, l)) - estot);
                    PyList_Append(resids, temp);
                    Py_DECREF(temp);
                }
                msft = 0.0;
                for (l = 0; l < PyList_Size(resids); l++){
                    msft += fabs(PyFloat_AsDouble(PyList_GetItem(resids, l)));
                }
                if ( msft < bmsft ){
                    bmsft = msft;
                    ix = PyList_GetItem(qx, i);
                    iy = PyList_GetItem(qy, j);
                    iz = PyList_GetItem(qz, k);
                    bot = estot;
                }
                Py_DECREF(estots);
                Py_DECREF(resids);
            }
        }
    }
    return Py_BuildValue("OOOdd", ix, iy, iz, bot, bmsft);
}

static PyMethodDef CoreToolsCMethods[] = {
    {"grid_search", core_tools_c_grid_search, METH_VARARGS,
     "Perform grid search for best-fitting hypocenter location."},
    {NULL, NULL, 0, NULL}
};

PyMODINIT_FUNC
initcore_tools_c(void)
{
    (void) Py_InitModule("core_tools_c", CoreToolsCMethods);
}
