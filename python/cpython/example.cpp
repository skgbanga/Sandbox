#include "Python.h"

namespace {

static uint64_t cfib(uint64_t n) {
  uint64_t a = 1;
  uint64_t b = 1;
  uint64_t c;
  if (n <= 1) {
    return 1;
  }

  while (--n > 1) {
    c = a + b;
    a = b;
    b = c;
  }

  return b;
}

}  // namespace

// define c function that understands python vocab
static PyObject* pyfib(PyObject*, PyObject* n) {
  uint64_t x = PyLong_AsUnsignedLong(n);  // TODO error handling
  PyObject* result = PyLong_FromUnsignedLong(cfib(x));
  return result;
}

// INTERSTING NOTE:
//
// We can write another fibonacci function which operates on PyObject* directly
// and then uses PyNumber_Add and PyNumber_Multiply to get a fibonacci number to
// arbitraty precision. Pretty Neat!!
//
// TODO: Do this with pybind py types in c++ code

// define python method
PyDoc_STRVAR(fibdoc, "computes the nth fib number");
PyMethodDef methods[] = {
    {"fib", (PyCFunction)pyfib, METH_O, fibdoc},
    // {NULL}
    {NULL, NULL, 0, NULL},
};

//  define python module
PyDoc_STRVAR(native_module_doc, "provides a fib function");
PyModuleDef native_module = {PyModuleDef_HEAD_INIT,
                             "fib",
                             native_module_doc,
                             -1,
                             methods,
                             NULL,
                             NULL,
                             NULL,
                             NULL};

// define function to create module
// only thing which requires C bindings
PyMODINIT_FUNC PyInit_native() {
  return PyModule_Create(&native_module);
}
