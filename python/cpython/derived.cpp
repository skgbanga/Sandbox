#include <pybind11/eigen.h>
#include <pybind11/numpy.h>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include <Eigen/Dense>

#include <chrono>
#include <cstdio>
#include <numeric>
#include <vector>

namespace py = pybind11;

// default case
std::vector<float> frodo() {
  std::vector<float> result(10);
  std::iota(result.begin(), result.end(), 1.0);
  return result;
}

// one step better
py::array_t<float> sam() {
  auto vec = frodo();

  py::array_t<float> result(vec.size());
  auto buffer = result.request();
  float* ptr = (float*)buffer.ptr;
  std::copy(vec.begin(), vec.end(), ptr);

  return result;
}

// two steps better
struct Data {
  std::vector<float> data;
};

Data gandalf() {
  Data d;
  d.data.resize(10);
  std::iota(d.data.begin(), d.data.end(), 1.0);
  return d;
}

// best
template <typename Sequence,
          typename = std::enable_if_t<std::is_rvalue_reference_v<Sequence&&>>>
inline py::array_t<typename Sequence::value_type> as_pyarray(Sequence&& seq) {
  auto size = seq.size();
  auto data = seq.data();
  auto seq_ptr = std::make_unique<Sequence>(std::move(seq));
  auto capsule = py::capsule(seq_ptr.get(), [](void* p) {
    std::unique_ptr<Sequence>(reinterpret_cast<Sequence*>(p));
  });
  seq_ptr.release();
  return py::array(size, data, capsule);
}

// world building
struct Characters {
  std::vector<float> hobbits;
  std::vector<float> elves;
};

Characters build() {
  return Characters{{1.0, 2.0}, {3.0, 4.0}};
}

// Eigen
class Mordor {
 public:
  using matf =
      Eigen::Matrix<float, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;
  Mordor() { orcs_ = matf::Random(10, 3); }

  const matf& orcs() const { return orcs_; }

  matf orcs_;
};

Mordor evil() {
  return Mordor();
}

// np.array as input
float sum(const std::vector<float>& nums) {
  return std::accumulate(nums.begin(), nums.end(), 0);
}

namespace sc = std::chrono;

std::pair<uint64_t, std::vector<float>> cumsum(py::array_t<float> nums) {
  auto unchecked = nums.unchecked();
  auto start = sc::high_resolution_clock::now();
  const auto size = nums.size();
  std::vector<float> result(size);
  float sum = 0;
  for (size_t i = 0; i < size; ++i) {
    sum += unchecked[i];
    result[i] = sum;
  }
  auto end = sc::high_resolution_clock::now();
  auto taken = sc::duration_cast<sc::microseconds>(end - start);

  return std::make_pair(taken.count(), std::move(result));
}

class Default {
 public:
  Default(int i = 10) : i_(i) {}

 private:
  int i_;
};

int add(std::optional<int> nums) {
  return 10;
}

PYBIND11_MODULE(derived, m) {
  m.def("add", &add);

  py::class_<Characters>(m, "characters")
      .def_property_readonly("hobbits", [](const Characters& c) {
        const auto& vec = c.hobbits;
        return py::array(vec.size(), vec.data(), py::cast(&c));
      });
  m.def("build", &build);

  m.def("frodo", &frodo);
  m.def("sam", &sam);

  py::class_<Data>(m, "data", py::buffer_protocol())
      .def_buffer([](Data& d) -> py::buffer_info {
        return py::buffer_info(d.data.data(), d.data.size(), true);
      });
  m.def("gandalf", &gandalf);

  m.def("aragorn", []() {
    auto result = frodo();
    return as_pyarray(std::move(result));
  });

  py::class_<Mordor>(m, "mordor").def("orcs", &Mordor::orcs);
  m.def("evil", &evil);

  m.def("sum", &sum);
  py::class_<Default>(m, "Default").def(py::init<int>(), py::arg("i") = 10);

  m.def("cumsum", [](py::array_t<float> nums) {
    auto [taken, result] = cumsum(nums);
    return std::make_pair(taken, as_pyarray(std::move(result)));
  });
}
