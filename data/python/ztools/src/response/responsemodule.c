#include <Python.h>
#include "structmember.h"
#include "response.h"

typedef struct {
    PyObject_HEAD
    /* Type-specific fields go here. */
    Response *response; /* Response object */
} PyResponse;

static void
PyResponse_dealloc(PyResponse* self)
{
    free_response(self->response);
    self->ob_type->tp_free((PyObject*)self);
}

static PyObject *
PyResponse_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
    PyResponse *self;
    self = (PyResponse *)type->tp_alloc(type, 0);
    return (PyObject *)self;
}

static int
PyResponse_init(PyResponse *self, PyObject *args)
{
    char *resp_file;
    int ok;
    Response *response;
    if (! PyArg_ParseTuple(args, "s", &resp_file))
        return -1;
    if (get_response(resp_file, &response) != 0)
        return -1;
    self->response = response;
    return 0;
}

static PyMemberDef PyResponse_members[] = {
    {"response", T_OBJECT_EX, offsetof(PyResponse, response), 0, "response"},
    {NULL} /* Sentinel */
};

static PyObject *
PyResponse_eval_response(PyResponse *self, PyObject *args)
{
    double omega, real, imag;
    PyObject *result;
    if (! PyArg_ParseTuple(args, "d", &omega))
        return NULL;
    if (eval_response(omega, self->response, &real, &imag) != 0)
        return NULL;
    result = PyComplex_FromDoubles(real, imag);
    if (result == NULL)
        return NULL;
    return result;
}

static PyMethodDef PyResponse_methods[] = {
    {"eval_response", (PyCFunction)PyResponse_eval_response, METH_VARARGS,
     "Return the complex value of instrument response at angular frequency omega."
    },
    {NULL} /* Sentinel */
};

static PyTypeObject PyResponseType = {
    PyObject_HEAD_INIT(NULL)
    0,                                  /*ob_size*/
    "response.PyResponse",              /*tp_name*/
    sizeof(PyResponse),                 /*tp_basicsize*/
    0,                                  /*tp_itemsize*/
    (destructor)PyResponse_dealloc,     /*tp_dealloc*/
    0,                                  /*tp_print*/
    0,                                  /*tp_getattr*/
    0,                                  /*tp_setattr*/
    0,                                  /*tp_compare*/
    0,                                  /*tp_repr*/
    0,                                  /*tp_as_number*/
    0,                                  /*tp_as_sequence*/
    0,                                  /*tp_as_mapping*/
    0,                                  /*tp_hash*/
    0,                                  /*tp_call*/
    0,                                  /*tp_str*/
    0,                                  /*tp_getattro*/
    0,                                  /*tp_setattro*/
    0,                                  /*tp_asb_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "PyResponse objects",               /*tp_doc*/
    0,                                  /*tp_traverse*/
    0,                                  /*tp_clear*/
    0,                                  /*tp_richcompare*/
    0,                                  /*tp_weaklistoffest*/
    0,                                  /*tp_iter*/
    0,                                  /*tp_iter_next*/
    PyResponse_methods,                 /*tp_methods*/
    PyResponse_members,                 /*tp_members*/
    0,                                  /*tp_getset*/
    0,                                  /*tp_base*/
    0,                                  /*tp_dict*/
    0,                                  /*tp_descr_get*/
    0,                                  /*tp_descr_set*/
    0,                                  /*tp_dictoffset*/
    (initproc)PyResponse_init,          /*tp_init*/
    0,                                  /*tp_alloc*/
    PyResponse_new,                     /*tp_new*/
};

static PyMethodDef module_methods[] = {
    {NULL}  /* Sentinel */
};

#ifndef PyMODINIT_FUNC      /* declarations for DLL import/export */
#define PyMODINIT_FUNC void
#endif
PyMODINIT_FUNC
initresponse(void)
{
    PyObject* m;
    if (PyType_Ready(&PyResponseType) < 0)
        return;
    m = Py_InitModule3("response", module_methods,
                       "Module to manipulate instrument responses.");
    if (m == NULL)
        return;
    Py_INCREF(&PyResponseType);
    PyModule_AddObject(m, "PyResponse", (PyObject *)&PyResponseType);
};
