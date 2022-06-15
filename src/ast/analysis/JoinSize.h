/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2022, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file JoinSize.h

 * Computes for every stratum, which EstimateJoinSize nodes to emit in the RAM
 * This is useful for the auto-scheduler to accumulate selectivity statistics
 *
 ***********************************************************************/

#pragma once

#include "ast/Program.h"
#include "ast/Relation.h"
#include "ast/TranslationUnit.h"
#include "ast/analysis/RecursiveClauses.h"
#include "ast/analysis/SCCGraph.h"
#include "ast/analysis/TopologicallySortedSCCGraph.h"
#include "ast/analysis/typesystem/PolymorphicObjects.h"
#include "ast/utility/Visitor.h"
#include "ast2ram/ClauseTranslator.h"
#include "ram/EstimateJoinSize.h"
#include "ram/Expression.h"
#include <ostream>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace souffle::ast::analysis {

/**
 * Analysis pass computing a schedule for computing relations.
 */
using PowerSet = std::vector<std::vector<std::size_t>>;
using StratumJoinSize = std::vector<Own<ram::EstimateJoinSize>>;

class JoinSizeAnalysis : public Analysis {
public:
    static constexpr const char* name = "join-size";

    JoinSizeAnalysis() : Analysis(name) {}

    void run(const TranslationUnit& translationUnit) override;

    /** Dump this relation schedule to standard error. */
    void print(std::ostream& os) const override;

    const StratumJoinSize& getJoinSizeStatementsInSCC(std::size_t scc) const {
        return joinSizeStatements[scc];
    }

private:
    std::vector<StratumJoinSize> joinSizeStatements;

    std::set<std::string> seenNodes;
    ast::Program* program = nullptr;
    SCCGraphAnalysis* sccGraph = nullptr;
    TopologicallySortedSCCGraphAnalysis* topsortSCCGraphAnalysis = nullptr;
    RecursiveClausesAnalysis* recursiveClauses = nullptr;
    PolymorphicObjectsAnalysis* polyAnalysis = nullptr;

    // for each stratum compute the EstimateJoinSize nodes to emit
    std::vector<StratumJoinSize> computeJoinSizeStatements();
    StratumJoinSize computeRuleVersionStatements(const std::set<const ast::Relation*>& sccRelations,
            const ast::Clause& clause, std::optional<std::size_t> version,
            ast2ram::TranslationMode mode = ast2ram::TranslationMode::DEFAULT);
    const PowerSet& getSubsets(std::size_t N, std::size_t K) const;
    mutable std::map<std::pair<std::size_t, std::size_t>, PowerSet> cache;
};

}  // namespace souffle::ast::analysis
