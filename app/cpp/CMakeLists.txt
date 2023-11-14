set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Add your source files
add_executable(msubstr ./MSUBSTR.cpp)
add_executable(queries ./queries.cpp)

# Link against the required package
# target_link_libraries(your_executable ${YourPackage_LIBRARIES})

cmake_minimum_required(VERSION 3.5)
project(eertree_app)

set(SOURCES
./eertree.h
./MSUBSTR.cpp
./queries.cpp
)
