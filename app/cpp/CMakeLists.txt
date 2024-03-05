set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)
include_directories(${CMAKE_SOURCE_DIR}/lib)

# Add your source files
add_executable(msubstr-classic ./src/msubstr/msubstr_classic.cpp)
add_executable(msubstr-direct ./src/msubstr/msubstr_direct.cpp)
add_executable(msubstr-semi-pers ./src/msubstr/msubstr_semi_pers.cpp)
add_executable(queries-direct ./src/queries/queries_direct.cpp)
add_executable(queries-semi-pers ./src/queries/queries_semi_pers.cpp)
add_executable(rich-direct ./src/rich/rich_direct.cpp)
add_executable(rich-semi-pers ./src/rich/rich_semi_pers.cpp)

cmake_minimum_required(VERSION 3.5)
project(eertree_app)

set(SOURCES
./lib/eertree_classic.h
./lib/eertree_direct.h
./lib/eertree_semi_pers.h
)
