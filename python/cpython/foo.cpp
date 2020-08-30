#include <pybind11/embed.h>
#include <pybind11/pybind11.h>
#include <Eigen/Dense>
#include <cstdio>
#include <iostream>

namespace py = pybind11;

py::int_ fib(int n) {
  py::int_ a = 1;
  py::int_ b = 1;
  py::int_ c;
  while (--n > 1) {
    c = a + b;
    a = b;
    b = c;
  }

  return b;
}

int main() {
  Eigen::MatrixXd m = Eigen::MatrixXd::Random(3, 3);
  std::cout << "m =" << std::endl << m << std::endl;
}
