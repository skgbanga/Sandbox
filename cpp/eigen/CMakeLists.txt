cmake_minimum_required(VERSION 3.10)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 17)

if(${CMAKE_BUILD_TYPE} STREQUAL "Release")
  set(CMAKE_CXX_FLAGS_RELEASE "-O3 -DNDEBUG")
endif()

include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup(TARGETS)

add_library(etl
  etl.cpp
)
target_link_libraries(etl PUBLIC
  CONAN_PKG::eigen
)

add_executable(main
  main.cpp
)
target_link_libraries(main
  PRIVATE
    etl
)

