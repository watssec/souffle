/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2023, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ErrorAnalyzer.h
 *
 * Extract reasons why something went wrong.
 *
 ***********************************************************************/

#pragma once

#include <map>
#include <memory>
#include <ostream>
#include <set>
#include <vector>

#include "ConstraintSystem.h"
#include "ast/Argument.h"

namespace souffle::ast::analysis {

template <typename Var>
class ErrorAnalyzer {
public:
    using constraint_type = Constraint<Var>;
    using constraint_ptr_type = std::shared_ptr<constraint_type>;
    using unsat_core_type = typename std::set<constraint_ptr_type>;

    ErrorAnalyzer() {
        std::cout << "Constructing ErrorAnalyzer" << std::endl;
    }

    ~ErrorAnalyzer() {
        std::cout << "Desstructing ErrorAnalyzer" << std::endl;
    }

    void addUnsatCore(const Argument* arg, const unsat_core_type& unsat_core) {
        unsatCores[arg] = unsat_core;
    }

    void print(std::ostream& os) const {
        for (const auto& [var, unsat_core] : unsatCores) {
            os << var << " -- " << *var << "(";
            for (const auto& constraint : unsat_core) {
                os << *constraint << ", ";
            }
            os << ")\n";
        }
    }

    void explain(const Argument* var) {
        std::cout << "Explaining " << var << " -- " << *var << std::endl;
        if (auto it = unsatCores.find(var); it != unsatCores.end()) {
            for (const auto& constraint : it->second) {
                std::cout << *constraint << ", ";
            }
            std::cout << std::endl;
        }
    }

private:
    std::map<const Argument*, unsat_core_type> unsatCores;
};

}  // namespace souffle::ast::analysis
