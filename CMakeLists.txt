cmake_minimum_required(VERSION 3.0)
project(DNAD Fortran)

set(IS_NOT_SUBDIRECTORY ${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_CURRENT_SOURCE_DIR})

# If built as a subdirectory, we don't want to do any testing, 
# because testing forces a specific number of derivatives
if (${IS_NOT_SUBDIRECTORY})
    set (DNAD_NDERIV 4 CACHE STRING "Number of Independent Variables" FORCE)
else()
    set (DNAD_NDERIV 0 CACHE STRING "Number of Independent Variables")
endif()
    
if (${DNAD_NDERIV} EQUAL 0)
    message(FATAL_ERROR "Please set DNAD_NDERIV to integer value")
endif()

configure_file(src/dnad_nderiv.f90.in dnad_nderiv.f90)

add_library(dnad STATIC src/dnad.f90 dnad_nderiv.f90)
add_library(DNAD::dnad ALIAS dnad)

if (${IS_NOT_SUBDIRECTORY})
        enable_testing()
    
        add_executable(unittests test/unittests.f90)
        target_link_libraries(unittests dnad)
        add_test(NAME unittests_test
		         COMMAND $<TARGET_FILE:unittests> ${CMAKE_CURRENT_BINARY_DIR}
		         WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endif()

