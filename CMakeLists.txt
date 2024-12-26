cmake_minimum_required(VERSION 3.14.2 FATAL_ERROR)

project(pattern_matching LANGUAGES CXX)

# todo: example should be built only if option ELD_BUILD_EXAMPLE is true,
# and it should be false by default.
# todo: if the option is true, mpark/variant should be fetched (to a build dir)
get_directory_property(HAS_PARENT PARENT_DIRECTORY)
if(HAS_PARENT)
    message(STATUS "... example disabled when included as subdirectory")
else()
    message(STATUS "${PROJECT_NAME}: compiling example")

    add_executable(example example.cpp)
    target_include_directories(example PRIVATE include)
    set_target_properties(example
        PROPERTIES
            CXX_STANDARD 17
    )
endif()
