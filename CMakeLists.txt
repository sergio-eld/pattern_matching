cmake_minimum_required(VERSION 3.14.2 FATAL_ERROR)

project(pattern_matching LANGUAGES CXX)

add_library(pattern_matching INTERFACE)
target_include_directories(pattern_matching INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)
add_library(eld::pattern_matching ALIAS pattern_matching)

# todo: example should be built only if option ELD_BUILD_EXAMPLE is true,
# and it should be false by default.
# todo: if the option is true, mpark/variant should be fetched (to a build dir)
get_directory_property(HAS_PARENT PARENT_DIRECTORY)
if(HAS_PARENT)
    message(STATUS "... example disabled when included as subdirectory")
else()
    message(STATUS "${PROJECT_NAME}: compiling example")

    add_executable(example example.cpp)
    target_link_libraries(example PRIVATE eld::pattern_matching)
    set_target_properties(example
        PROPERTIES
        # todo: CXX_STANDARD 11
            CXX_STANDARD 17
    )
endif()
