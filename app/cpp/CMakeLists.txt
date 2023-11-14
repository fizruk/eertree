set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Add your source files
add_executable(msubstr ./MSUBSTR.cpp)
add_executable(queries ./queries.cpp)
add_executable(rich ./rich.cpp)

cmake_minimum_required(VERSION 3.5)
project(eertree_app)

set(SOURCES
./eertree.h
./eertree2.h
./MSUBSTR.cpp
./queries.cpp
./rich.cpp
)
