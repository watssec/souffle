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
#include <sstream>
#include <vector>

#include "ConstraintSystem.h"
#include "ast/Argument.h"
#include "reports/ErrorReport.h"

namespace souffle::ast::analysis {

template <typename Var>
class ErrorAnalyzer {
public:
    using constraint_type = Constraint<Var>;
    using constraint_ptr_type = std::shared_ptr<constraint_type>;
    using unsat_core_type = typename std::set<constraint_ptr_type>;

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

    void explain(ErrorReport& report, const Argument* var, std::string message) {
        std::stringstream ms;
        if (auto it = unsatCores.find(var); it != unsatCores.end()) {
            ms << "Following constraints cannot hold:";
            for (const auto& constraint : it->second) {
                if (auto customMessage = constraint->customMessage(); customMessage) {
                    ms << "\n   " << *customMessage;
                } else {
                    ms << "\n   " << *constraint;
                }
            }
        }
        Diagnostic diag{Diagnostic::Type::ERROR, DiagnosticMessage{message, var->getSrcLoc()},
                {DiagnosticMessage{ms.str()}}};
        report.addDiagnostic(diag);
    }

private:
    std::map<const Argument*, unsat_core_type> unsatCores;
};

}  // namespace souffle::ast::analysis
