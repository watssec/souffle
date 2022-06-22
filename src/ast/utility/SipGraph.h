/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2022, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file SipGraph.h
 *
 * Defines the sideways information passing (SIP) graph class
 *
 ***********************************************************************/

#pragma once

#include "ast/Clause.h"
#include "ast/Constant.h"
#include <functional>
#include <map>
#include <set>
#include <unordered_map>
#include <utility>

namespace souffle::ast {

class SipGraph {
public:
    using AstConstantTranslator = std::function<std::string(const ast::Constant& constant)>;

    SipGraph(const Clause* clause, AstConstantTranslator translateConstant)
            : clause(clause), translateConstant(translateConstant) {
        computeBindings();
    }

    using VarName = std::string;
    using VarSet = std::set<VarName>;
    using ArgIdx = std::size_t;
    using AtomIdx = std::size_t;
    using AtomSet = std::set<std::size_t>;

    void computeBindings();

    std::set<std::size_t> getBoundIndices(const Atom* from, const Atom* to) const {
        return bindingsMap.at(std::make_pair(from, to));
    }

    std::set<std::pair<std::size_t, std::string>> getConstantsMap(const Atom* atom) const {
        return constantsMap.at(atom);
    }

private:
    // the clause to compute the SIP graph for
    const Clause* clause;

    // user provided lambda function to convert AST constant to RAM expression
    AstConstantTranslator translateConstant;

    // map pairs of atoms to bound indices
    std::map<std::pair<const Atom*, const Atom*>, std::set<std::size_t>> bindingsMap;

    // map atom to the constants bound at each index
    std::map<const Atom*, std::set<std::pair<std::size_t, std::string>>> constantsMap;
};

}  // namespace souffle::ast
