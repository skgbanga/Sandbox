cmake_minimum_required(VERSION 3.10)
project (pybind-example VERSION 1.0.0)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-exceptions")

# static library
add_library(static
  static.cpp
)
# set_target_properties(static
#   PROPERTIES
#   POSITION_INDEPENDENT_CODE ON
# )


# shared library
add_library(shared SHARED
  shared.cpp
)
target_link_libraries(shared PRIVATE
  static
)
