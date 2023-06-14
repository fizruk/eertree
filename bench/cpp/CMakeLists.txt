

# Add your source files
add_executable(bench1 ./simple/bench1.cpp)
add_executable(bench2 ./simple/bench2.cpp)
add_executable(bench3 ./A216264/bench1.cpp)
add_executable(bench4 ./A216264/bench2.cpp)

# Link against the required package
target_link_libraries(your_executable ${YourPackage_LIBRARIES})

cmake_minimum_required(VERSION 3.5)
project(eertree)

find_package(benchmark REQUIRED)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)


set(SOURCES
./simple/eertree1.h
./simple/eertree2.h
./simple/bench1.cpp
./simple/bench2.cpp
./A216264/eertree1.h
./A216264/eertree2.h
./A216264/bench1.cpp
./A216264/bench2.cpp
)
target_link_libraries(bench1 benchmark::benchmark)
target_link_libraries(bench2 benchmark::benchmark)
target_link_libraries(bench3 benchmark::benchmark)
target_link_libraries(bench4 benchmark::benchmark)