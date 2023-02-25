# Souffle - A Datalog Compiler
# Copyright (c) 2023 The Souffle Developers. All rights reserved
# Licensed under the Universal Permissive License v 1.0 as shown at:
# - https://opensource.org/licenses/UPL
# - <souffle root>/licenses/SOUFFLE-UPL.txt

include(SouffleTests)

function(NEGATIVE_TEST NAME)
  souffle_run_test_helper(NEGATIVE TEST_NAME ${NAME} CATEGORY errors EXPLAIN_ERROR ${ARGV})
endfunction()

negative_test(error_deduce_type)
