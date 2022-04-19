# Souffle - A Datalog Compiler
# Copyright (c) 2021 The Souffle Developers. All rights reserved
# Licensed under the Universal Permissive License v 1.0 as shown at:
# - https://opensource.org/licenses/UPL
# - <souffle root>/licenses/SOUFFLE-UPL.txt

include(SouffleTests)

function(MOVERY_TEST CATEGORY NAME)
    add_test(NAME movery_${CATEGORY}_${NAME}
            COMMAND
            $<TARGET_FILE:souffle>
            "--smt=MUZ"
            "--macro=UNIT_TEST"
            ${CMAKE_CURRENT_SOURCE_DIR}/${CATEGORY}/${NAME}.dl)
endfunction()

movery_test(semantics Language)