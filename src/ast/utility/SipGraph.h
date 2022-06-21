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
#include <map>
#include <set>
#include <unordered_map>
#include <utility>

namespace souffle::ast {

class SipGraph {
public:
    SipGraph(const Clause* clause) : clause(clause) {
        computeBindingsMap();
    }

    using VarName = std::string;
    using VarSet = std::set<VarName>;
    using ArgIdx = std::size_t;
    using AtomIdx = std::size_t;
    using AtomSet = std::set<std::size_t>;

    void computeBindingsMap();

    std::set<std::size_t> getBoundIndices(const Atom* from, const Atom* to) const {
        return bindingsMap.at(std::make_pair(from, to));
    }

private:
    // the clause to compute the SIP graph for
    const Clause* clause;

    // map pairs of atoms to bound indices
    std::map<std::pair<const Atom*, const Atom*>, std::set<std::size_t>> bindingsMap;
};

}  // namespace souffle::ast
