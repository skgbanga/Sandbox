cmake_minimum_required(VERSION 3.10)
project(CMake VERSION 1.0.0)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup(TARGETS)

macro(add_binary CUSTOM)
  set(sources ${ARGN})
  add_executable(${CUSTOM}
    ${sources}
  )
  target_link_libraries(${CUSTOM}
    CONAN_PKG::tbb
  )
  target_compile_definitions(${CUSTOM} PUBLIC -DTBB_SUPPRESS_DEPRECATED_MESSAGES)
endmacro()


# 01
add_binary(before1
  01_oneTBB_Hello_TBB/before/hello-tbb.cpp
)

add_binary(after1
  01_oneTBB_Hello_TBB/before/hello-tbb-parallel.cpp
)

# 02
add_binary(serial_pi
  02_oneTBB_algorithms/before/pi-serial.cpp
)

add_binary(parallel_pi
  02_oneTBB_algorithms/before/pi-parallel.cpp
)

add_binary(pipeline_serial
  02_oneTBB_algorithms/before/pipeline-serial.cpp
  02_oneTBB_algorithms/common/case.cpp
)

add_binary(pipeline_parallel
  02_oneTBB_algorithms/before/pipeline-parallel.cpp
  02_oneTBB_algorithms/common/case.cpp
)

# 05
add_binary(split_default
  05_oneTBB_controlling_max_concurrency/before/split-default.cpp
)
add_binary(scalability_gc
  05_oneTBB_controlling_max_concurrency/before/scalability-gc.cpp
)
add_binary(scalability_gc_ta
  05_oneTBB_controlling_max_concurrency/before/scalability-gc-ta.cpp
)
add_binary(split_unevenly
  05_oneTBB_controlling_max_concurrency/before/split-unevenly.cpp
)


# 07
add_binary(observer
  07_oneTBB_observers/before/observer.cpp
)


# 10
add_binary(mat_mul
  10_oneTBB_composability/lab/mat_mul.cpp
)
target_link_libraries(mat_mul
  CONAN_PKG::intel_mkl
)

