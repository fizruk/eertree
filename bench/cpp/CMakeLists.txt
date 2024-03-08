set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)
cmake_minimum_required(VERSION 3.5)
include_directories(${CMAKE_SOURCE_DIR}/../../app/cpp/src)
include_directories(${CMAKE_SOURCE_DIR}/../../app/cpp/lib)

project(eertree_bench)

# Add your source files
add_executable(bench-simple ./simple/bench_simple.cpp)
add_executable(bench-simple-direct ./simple/bench_simple.cpp)
add_executable(bench-simple-semi-pers ./simple/bench_simple.cpp)
add_executable(bench-msubstr ./msubstr/bench_msubstr.cpp)
add_executable(bench-msubstr-direct ./msubstr/bench_msubstr.cpp)
add_executable(bench-msubstr-semi-pers ./msubstr/bench_msubstr.cpp)
add_executable(bench-queries-direct ./queries/bench_queries.cpp)
add_executable(bench-queries-semi-pers ./queries/bench_queries.cpp)
add_executable(bench-rich-direct ./rich/bench_rich.cpp)
add_executable(bench-rich-semi-pers ./rich/bench_rich.cpp)

# Add the macros necessary for customization
target_compile_definitions(bench-simple-direct PRIVATE EERTREE_DIRECT)
target_compile_definitions(bench-simple-semi-pers PRIVATE EERTREE_SEMI_PERS)
target_compile_definitions(bench-msubstr-direct PRIVATE EERTREE_DIRECT)
target_compile_definitions(bench-msubstr-semi-pers PRIVATE EERTREE_SEMI_PERS)
target_compile_definitions(bench-queries-direct PRIVATE EERTREE_DIRECT)
target_compile_definitions(bench-queries-semi-pers PRIVATE EERTREE_SEMI_PERS)
target_compile_definitions(bench-rich-direct PRIVATE EERTREE_DIRECT)
target_compile_definitions(bench-rich-semi-pers PRIVATE EERTREE_SEMI_PERS)

find_package(benchmark REQUIRED)

# link benchmark to all executables
# todo: There's prb an easier way, will check later.
target_link_libraries(bench-simple benchmark::benchmark)
target_link_libraries(bench-simple-direct benchmark::benchmark)
target_link_libraries(bench-simple-semi-pers benchmark::benchmark)
target_link_libraries(bench-msubstr benchmark::benchmark)
target_link_libraries(bench-msubstr-direct benchmark::benchmark)
target_link_libraries(bench-msubstr-semi-pers benchmark::benchmark)
target_link_libraries(bench-queries-direct benchmark::benchmark)
target_link_libraries(bench-queries-semi-pers benchmark::benchmark)
target_link_libraries(bench-rich-direct benchmark::benchmark)
target_link_libraries(bench-rich-semi-pers benchmark::benchmark)

set(SOURCES
./src/msubstr/msubstr.h 
./src/queries/queries.h
./src/rich/rich.h
./lib/eertree_classic.h
./lib/eertree_direct.h
./lib/eertree_semi_pers.h
)
