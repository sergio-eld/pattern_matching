cmake_minimum_required(VERSION 3.14.2 FATAL_ERROR)

project(pattern_matching LANGUAGES CXX)

add_library(pattern_matching INTERFACE)
target_include_directories(pattern_matching INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)
add_library(eld::pattern_matching ALIAS pattern_matching)

get_directory_property(HAS_PARENT PARENT_DIRECTORY)
if(HAS_PARENT)
    set(DEFAULT_BUILD_EXAMPLE OFF)
else()
    set(DEFAULT_BUILD_EXAMPLE ON)
endif()

option(ELD_BUILD_EXAMPLE "Build the example executable" ${DEFAULT_BUILD_EXAMPLE})

# todo: example should be built only if option ELD_BUILD_EXAMPLE is true,
# and it should be false by default.
# todo: if the option is true, mpark/variant should be fetched (to a build dir)
if(ELD_BUILD_EXAMPLE)
    include(FetchContent)
    FetchContent_Declare(
        mpark_variant
        GIT_REPOSITORY https://github.com/mpark/variant.git
        GIT_TAG v1.4.0
    )
    FetchContent_MakeAvailable(mpark_variant)

    message(STATUS "${PROJECT_NAME}: compiling example")

    add_executable(example example.cpp)
    target_link_libraries(example 
        PRIVATE 
            eld::pattern_matching
            mpark_variant
    )
    set_target_properties(example
        PROPERTIES
        # todo: CXX_STANDARD 11
            CXX_STANDARD 14
    )
# todo: benchmark
else()
    message(STATUS "... example disabled when included as subdirectory")
endif()
