# Souffle - A Datalog Compiler
# Copyright (c) 2021 The Souffle Developers. All rights reserved
# Licensed under the Universal Permissive License v 1.0 as shown at:
# - https://opensource.org/licenses/UPL
# - <souffle root>/licenses/SOUFFLE-UPL.txt

include(SouffleTests)

function(SOUFFLE_LINK_TEST)
    cmake_parse_arguments(
        PARAM
        "NEGATIVE"
        "TEST_NAME"
        "DATALOG_FILES"
        ${ARGV}
    )

    set(INPUT_DIR "${CMAKE_CURRENT_SOURCE_DIR}/${PARAM_TEST_NAME}")
    set(FACTS_DIR "${INPUT_DIR}/facts")

    if (PARAM_NEGATIVE)
        set(TEST_SUFFIX "_negative")
        set (NAMESPACE_FLAG "")
    else ()
        set(NAMESPACE_FLAG "--namespace")
    endif()

    set(OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/${PARAM_TEST_NAME}${TEST_SUFFIX}")


    ADD_TEST(NAME "${PARAM_TEST_NAME}${TEST_SUFFIX}"
        COMMAND
        ${Python3_EXECUTABLE} ${PROJECT_SOURCE_DIR}/cmake/redirect.py
        --out ${PARAM_TEST_NAME}.out
        --err ${PARAM_TEST_NAME}.err
        ${Python3_EXECUTABLE}
        ${INPUT_DIR}/test.py
        --input_dir ${INPUT_DIR}
        --output_dir ${OUTPUT_DIR}
        --cxx ${CMAKE_CXX_COMPILER}
        --souffle $<TARGET_FILE:souffle>
        --driver driver.cpp
        --include "${CMAKE_SOURCE_DIR}/src/include"
        ${NAMESPACE_FLAG}
        ${PARAM_DATALOG_FILES}
        COMMAND_EXPAND_LISTS
        WORKING_DIRECTORY ${INPUT_DIR}
    )

    if (PARAM_NEGATIVE)
        set_tests_properties("${PARAM_TEST_NAME}${TEST_SUFFIX}" PROPERTIES WILL_FAIL TRUE)
    endif()

endfunction()

if (UNIX)
    souffle_link_test(TEST_NAME link2 DATALOG_FILES insert_for1.dl insert_for2.dl)
    souffle_link_test(NEGATIVE TEST_NAME link2 DATALOG_FILES insert_for1.dl insert_for2.dl)
endif(UNIX)
