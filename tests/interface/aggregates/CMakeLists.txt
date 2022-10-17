# Souffle - A Datalog Compiler
# Copyright (c) 2022 The Souffle Developers. All rights reserved
# Licensed under the Universal Permissive License v 1.0 as shown at:
# - https://opensource.org/licenses/UPL
# - <souffle root>/licenses/SOUFFLE-UPL.txt

add_library(afunctors SHARED functors.cpp)
target_include_directories(afunctors PRIVATE "${CMAKE_SOURCE_DIR}/src/include")

target_compile_features(afunctors
                        PUBLIC cxx_std_17)

set_target_properties(afunctors PROPERTIES
  CXX_EXTENSIONS OFF
  WINDOWS_EXPORT_ALL_SYMBOLS ON)

set_target_properties(afunctors PROPERTIES OUTPUT_NAME "functors")

if (OPENMP_FOUND)
    target_link_libraries(afunctors PUBLIC OpenMP::OpenMP_CXX)
endif()

if (Threads_FOUND)
  target_link_libraries(afunctors PUBLIC Threads::Threads)
endif ()

if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  if (CMAKE_CXX_COMPILER_VERSION VERSION_LESS 9)
    target_link_libraries(afunctors PUBLIC stdc++fs)
  endif ()
endif ()

if (NOT MSVC)
  target_compile_options(afunctors
    PUBLIC "-Wall;-Wextra;-Werror;-fwrapv")
else ()
  target_compile_options(afunctors PUBLIC /W3 /WX /EHsc)
endif()

if (WIN32)
  # Prefix all shared libraries with 'lib'.
  set(CMAKE_SHARED_LIBRARY_PREFIX "lib")

  # Prefix all static libraries with 'lib'.
  set(CMAKE_STATIC_LIBRARY_PREFIX "lib")
endif ()

if (SOUFFLE_DOMAIN_64BIT)
    target_compile_definitions(afunctors
                               PUBLIC RAM_DOMAIN_SIZE=64)
endif()
