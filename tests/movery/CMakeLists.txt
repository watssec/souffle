# Souffle - A Datalog Compiler
# Copyright (c) 2021 The Souffle Developers. All rights reserved
# Licensed under the Universal Permissive License v 1.0 as shown at:
# - https://opensource.org/licenses/UPL
# - <souffle root>/licenses/SOUFFLE-UPL.txt

include(SouffleTests)

function(MOVERY_TEST_WITH_SMT CATEGORY NAME SMT)
  add_test(NAME movery_${CATEGORY}_${NAME}_${SMT}
      COMMAND
      $<TARGET_FILE:souffle>
      "--smt=${SMT}"
      "--macro=UNIT_TEST"
      ${CMAKE_CURRENT_SOURCE_DIR}/${CATEGORY}/${NAME}.dl)
endfunction()

function(MOVERY_TEST_WITH_SOUFFLE CATEGORY NAME)
  add_test(NAME movery_${CATEGORY}_${NAME}_Souffle
          COMMAND
          $<TARGET_FILE:souffle>
          "--macro=UNIT_TEST"
          ${CMAKE_CURRENT_SOURCE_DIR}/${CATEGORY}/${NAME}.dl)
endfunction()

function(MOVERY_TEST CATEGORY NAME)
  movery_test_with_smt(${CATEGORY} ${NAME} "MUZ")
  movery_test_with_smt(${CATEGORY} ${NAME} "Z3")
  movery_test_with_smt(${CATEGORY} ${NAME} "CVC")
  movery_test_with_souffle(${CATEGORY} ${NAME})
endfunction()

movery_test(semantics Language)