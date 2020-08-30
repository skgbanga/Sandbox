cmake_minimum_required(VERSION 3.10)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 17)

include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup(TARGETS)

find_package(Python3 COMPONENTS Development REQUIRED)

Python3_add_library(native
  native.cpp
)
target_compile_options(native PRIVATE -Werror -pedantic-errors -Wall -Wextra -Wconversion -Wsign-conversion)

Python3_add_library(derived
  derived.cpp
)
target_link_libraries(derived PRIVATE
  CONAN_PKG::pybind11
  CONAN_PKG::eigen
)

add_executable(foo
  foo.cpp
)
target_link_libraries(foo PRIVATE
  CONAN_PKG::pybind11
  CONAN_PKG::eigen
  Python3::Python
 )
