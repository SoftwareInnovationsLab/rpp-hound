cmake_minimum_required(VERSION 3.28)
project(HelloWorldTool)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find LLVM package
find_package(LLVM REQUIRED CONFIG)

# Add LLVM include directories
include_directories(${LLVM_INCLUDE_DIRS})
link_directories(${LLVM_LIBRARY_DIRS})

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti")

# Add LLVM definitions
add_definitions(${LLVM_DEFINITIONS})

# Link against LLVM libraries
llvm_map_components_to_libnames(llvm_libs support core irreader)

add_library(RarePathCoverageCommon RarePathCoverageCommon.cpp)
add_library(ScopeUpdater ScopeUpdater.cpp)
add_library(FunctionInserter FunctionInserter.cpp)
add_library(IfStmtInstrumenter IfStmtInstrumenter.cpp)
add_library(ReplacePlaceholders ReplacePlaceholders.cpp)

add_executable(rpcov-instrument rpcov-instrument.cpp)

target_link_libraries(rpcov-instrument
    ${llvm_libs}
    clang-cpp
    FunctionInserter
    ScopeUpdater
    IfStmtInstrumenter
    ReplacePlaceholders
    RarePathCoverageCommon
)
