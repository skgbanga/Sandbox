cmake_minimum_required(VERSION 3.10)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 17)

if(${CMAKE_BUILD_TYPE} STREQUAL "Release")
  set(CMAKE_CXX_FLAGS_RELEASE "-O3 -DNDEBUG")
endif()

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
set_target_properties(derived
  PROPERTIES
  LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/mylibs/
)

add_executable(foo
  foo.cpp
)
target_link_libraries(foo PRIVATE
  CONAN_PKG::pybind11
  CONAN_PKG::eigen
  Python3::Python
)
