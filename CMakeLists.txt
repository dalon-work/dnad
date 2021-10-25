cmake_minimum_required(VERSION 3.0)
project(DNAD Fortran)

set (DNAD_NDERIV 0 CACHE type STRING)

if (${DNAD_NDERIV} EQUAL 0)
	message(FATAL_ERROR "Please set DNAD_NDERIV to integer value")
endif()

configure_file(src/dnad_nderiv.f90.in dnad_nderiv.f90)

add_library(dnad STATIC src/dnad.f90 dnad_nderiv.f90)

