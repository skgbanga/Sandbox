cmake_minimum_required(VERSION 3.10)
project(CMake VERSION 1.0.0)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup(TARGETS)

# set(CMAKE_CXX_VISIBILITY_PRESET     hidden)
# set(CMAKE_VISIBILITY_INLINES_HIDDEN YES)

find_package(fmt CONFIG REQUIRED)

add_library(mylib SHARED
  mylib.cpp
)
target_link_options(mylib PUBLIC -s)
target_link_libraries(mylib
  # PRIVATE
  #   "-Wl,--exclude-libs,ALL"
  fmt::fmt
)

add_executable(main
  main.cpp
)
target_link_libraries(main
  mylib
)
