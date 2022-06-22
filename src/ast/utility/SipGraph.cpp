/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2022, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file SipGraph.cpp
 *
 * Defines the SipGraph class
 *
 *
 ***********************************************************************/

#include "ast/utility/SipGraph.h"
#include "Global.h"
#include "ast/Clause.h"
#include "ast/Variable.h"
#include "ast/utility/Utils.h"
#include "ast/utility/Visitor.h"
#include <vector>

namespace souffle::ast {

void SipGraph::computeBindings() {
    // get atoms and constraints from the clause
    auto atoms = ast::getBodyLiterals<ast::Atom>(*clause);
    auto constraints = ast::getBodyLiterals<ast::BinaryConstraint>(*clause);

    // map variable name to constants if possible
    std::unordered_map<VarName, std::string> varToConstant;

    // map variables to necessary variables on other side of the equality
    // i.e. x = y + z we should map x -> { y, z }
    std::unordered_map<VarName, VarSet> varToOtherVars;

    // map variable name to the lower and upper bounds of the inequality
    // i.e. EA < Addr < EA + Size we should map Addr -> { { EA }, { EA, Size } }
    std::unordered_map<VarName, std::pair<VarSet, VarSet>> ineqToUpperLower;

    // construct bindings from the constraints
    for (auto* constraint : constraints) {
        auto* lhs = constraint->getLHS();
        auto* rhs = constraint->getRHS();
        auto op = constraint->getBaseOperator();

        // ignore any non-equality or inequality operator
        if (!isEqConstraint(op) && !isIneqConstraint(op)) {
            continue;
        }

        // handle inequalities
        if (isIneqConstraint(op)) {
            if (auto* var = as<ast::Variable>(lhs)) {
                VarSet otherVars;
                visit(rhs, [&](const ast::Variable& v) { otherVars.insert(v.getName()); });
                if (isLessThan(op) || isLessEqual(op)) {
                    ineqToUpperLower[var->getName()].second = otherVars;
                }
                if (isGreaterThan(op) || isGreaterEqual(op)) {
                    ineqToUpperLower[var->getName()].first = otherVars;
                }
            }

            if (auto* var = as<ast::Variable>(rhs)) {
                VarSet otherVars;
                visit(lhs, [&](const ast::Variable& v) { otherVars.insert(v.getName()); });
                if (isLessThan(op) || isLessEqual(op)) {
                    ineqToUpperLower[var->getName()].first = otherVars;
                }
                if (isGreaterThan(op) || isGreaterEqual(op)) {
                    ineqToUpperLower[var->getName()].second = otherVars;
                }
            }
        }

        // x = <constant>
        else if (isA<ast::Variable>(lhs) && isA<ast::Constant>(rhs)) {
            varToConstant[as<ast::Variable>(lhs)->getName()] = translateConstant(*as<ast::Constant>(rhs));
        }

        // <constant> = x
        else if (isA<ast::Constant>(lhs) && isA<ast::Variable>(rhs)) {
            varToConstant[as<ast::Variable>(rhs)->getName()] = translateConstant(*as<ast::Constant>(lhs));
        }

        // x = <expr>
        else if (auto* var = as<ast::Variable>(lhs)) {
            VarSet otherVars;
            visit(rhs, [&](const ast::Variable& v) { otherVars.insert(v.getName()); });
            varToOtherVars[var->getName()] = otherVars;
        }

        // <expr> = x
        else if (auto* var = as<ast::Variable>(rhs)) {
            VarSet otherVars;
            visit(lhs, [&](const ast::Variable& v) { otherVars.insert(v.getName()); });
            varToOtherVars[var->getName()] = otherVars;
        }
    }

    // check for bounded inequality i.e. EA < EA2 < EA + Size
    for (auto& p : ineqToUpperLower) {
        // consider this like an equality
        auto& [lower, upper] = p.second;
        if (!lower.empty() && !upper.empty() &&
                std::includes(upper.begin(), upper.end(), lower.begin(), lower.end())) {
            varToOtherVars[p.first] = upper;
        }
    }

    // map each atom idx to the variables it grounds
    std::unordered_map<const Atom*, VarSet> atomToGroundedVars;
    for (const auto* atom : atoms) {
        VarSet groundedVars;
        visit(*atom, [&](const ast::Variable& v) { groundedVars.insert(v.getName()); });
        atomToGroundedVars[atom] = groundedVars;
    }

    // construct constant bindings for each atom
    for (const auto* atom : atoms) {
        auto arguments = atom->getArguments();
        std::set<std::pair<std::size_t, std::string>> indexConstant;
        for (std::size_t argIdx = 0; argIdx < arguments.size(); ++argIdx) {
            auto* argument = arguments[argIdx];

            // Case 1: the argument is a constant
            if (auto* constant = as<ast::Constant>(argument)) {
                std::string constantValue = translateConstant(*constant);
                indexConstant.insert(std::make_pair(argIdx, constantValue));
            }

            // Case 2: the argument is a variable i.e. x and we have a constraint x = <constant>
            else if (auto* var = as<ast::Variable>(argument)) {
                if (varToConstant.count(var->getName())) {
                    std::string constantValue = varToConstant[var->getName()];
                    indexConstant.insert(std::make_pair(argIdx, constantValue));
                }
            }
        }

        constantsMap.insert(std::make_pair(atom, indexConstant));
    }

    // construct bindings from one atom to another
    for (const auto* from : atoms) {
        for (const auto* to : atoms) {
            if (from == to) {
                continue;
            }
            auto atomPair = std::make_pair(from, to);
            std::set<std::size_t> boundColumns;
            const auto groundedVars = atomToGroundedVars[from];

            // for each argument in "to"
            auto arguments = to->getArguments();
            for (std::size_t argIdx = 0; argIdx < arguments.size(); ++argIdx) {
                auto* argument = arguments[argIdx];
                if (auto* var = as<ast::Variable>(argument)) {
                    // Case 1: this variable is grounded by "from"
                    if (groundedVars.count(var->getName()) > 0) {
                        boundColumns.insert(argIdx);
                    }

                    // Case 2: this variable is bound by multiple variables
                    else if (varToOtherVars.count(var->getName()) > 0) {
                        auto& dependentVars = varToOtherVars.at(var->getName());

                        // and all of these variables are grounded by "from"
                        if (std::includes(groundedVars.begin(), groundedVars.end(), dependentVars.begin(),
                                    dependentVars.end())) {
                            boundColumns.insert(argIdx);
                        }
                    }
                }
            }

            bindingsMap.insert(std::make_pair(atomPair, boundColumns));
        }
    }
}

}  // namespace souffle::ast
