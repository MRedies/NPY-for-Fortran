cmake_minimum_required(VERSION 3.5)
project(STB)

file(GLOB_RECURSE sources  src/*.f90 
                           src/*.h )

add_executable(npy.x ${sources})

enable_language(Fortran)
#set(CMAKE_Fortran_COMPILER ifort)
#set(CMAKE_Fortran_COMPILER_ID "Intel")


if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(dialect "-ffree-form -std=f2008 -fimplicit-none")
	#set(bounds "-fbounds-check")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(dialect "-stand=f08 -implicitnone")
	#set(bounds "-check bounds")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "PGI")
    set(dialect "-Mfreeform -Mdclchk -Mstandard -Mallocatable=03")
	#set(bounds "-C")
endif()

set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${bounds}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${dialect}")

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
MESSAGE( STATUS "cmake_module_path:    " ${CMAKE_MODULE_PATH})

#
# Compile.
#
