set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)
include_directories(${CMAKE_SOURCE_DIR}/lib)

# Add your source files

add_executable(msubstr-classic ./src/msubstr/msubstr.cpp)
add_executable(msubstr-direct ./src/msubstr/msubstr.cpp)
add_executable(msubstr-semi-pers ./src/msubstr/msubstr.cpp)
add_executable(queries-direct ./src/queries/queries.cpp)
add_executable(queries-semi-pers ./src/queries/queries.cpp)
add_executable(rich-simple ./src/rich/rich.cpp)
add_executable(rich-direct ./src/rich/rich.cpp)
add_executable(rich-semi-pers ./src/rich/rich.cpp)

target_compile_definitions(msubstr-direct PRIVATE EERTREE_DIRECT)
target_compile_definitions(msubstr-semi-pers PRIVATE EERTREE_SEMI_PERS)
target_compile_definitions(queries-direct PRIVATE EERTREE_DIRECT)
target_compile_definitions(queries-semi-pers PRIVATE EERTREE_SEMI_PERS)
target_compile_definitions(rich-simple PRIVATE SIMPLE_RICH)
target_compile_definitions(rich-direct PRIVATE EERTREE_DIRECT)
target_compile_definitions(rich-semi-pers PRIVATE EERTREE_SEMI_PERS)

cmake_minimum_required(VERSION 3.5)
project(eertree_app)

set(SOURCES
./lib/eertree_classic.h
./lib/eertree_direct.h
./lib/eertree_semi_pers.h
./src/msubstr/msubstr.h
./src/queries/queries.h
./src/rich/rich.h
./src/rich/rich_simple.h
)
