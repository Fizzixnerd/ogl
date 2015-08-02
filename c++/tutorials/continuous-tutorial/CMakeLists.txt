cmake_minimum_required(VERSION 2.8)

add_executable(main main.cpp scratch.cpp shader.cpp shaderprogram.cpp context.cpp uniform.cpp matrix.cpp)

find_package(PkgConfig REQUIRED)
pkg_search_module(GLFW REQUIRED glfw3)
include_directories(${GLFW_INCLUDE_DIRS})
target_link_libraries(main GL GLEW ${GLFW_LIBRARIES})
set(CMAKE_CXX_FLAGS "-Wall -std=c++11")
set(CMAKE_CXX_COMPILER "clang++-3.6")
set(CMAKE_C_COMPILER "clang-3.6")
