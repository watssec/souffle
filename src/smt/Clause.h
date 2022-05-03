/************************************************************************
 *
 * @file Clause.h
 *
 * Hosts the clause registry
 *
 ***********************************************************************/

#pragma once

#include <list>
#include <map>
#include <vector>

#include "smt/ClauseExpr.h"
#include "smt/ClauseTerm.h"
#include "smt/Utils.h"

namespace souffle::smt {

// forward declarations
class Frontend;

/**
 * A registry of clauses appeared in the whole program
 */
class ClauseRegistry {
    friend Frontend;

private:
    // environment
    const TypeRegistry& typeRegistry;
    const RelationRegistry& relationRegistry;

protected:
    // registry
    std::map<RelationIndex, std::vector<ClauseExprAnalyzer>> mapping{};

    // relation registration sequence
    std::list<SCC<RelationIndex>> sequence{};

public:
    ClauseRegistry(const ast::TranslationUnit& unit, const TypeRegistry& typeRegistry_,
            const RelationRegistry& relationRegistry_)
            : typeRegistry(typeRegistry_), relationRegistry(relationRegistry_) {
        // add rules and their dependencies
        Graph<RelationIndex> dep_graph;

        // populate the nodes in the dep graph
        for (const auto& [_, val] : relationRegistry.mapping) {
            dep_graph.addNode(val.index);
            mapping[val.index].clear();
        }

        // ensure that all terms and exprs have unique indices
        size_t counter_term = 0;
        size_t counter_expr = 0;

        // do the actual analysis
        const auto& program = unit.getProgram();
        const auto& type_analysis = unit.getAnalysis<ast::analysis::TypeAnalysis>();
        for (const auto clause : program.getClauses()) {
            // analyze the clause
            ClauseTermAnalyzer analyzer_term(
                    clause, type_analysis, typeRegistry, relationRegistry, counter_term);
            counter_term = analyzer_term.counter;

            // populate the edges in the dep graph
            const auto main = analyzer_term.get_main();
            for (const auto& dep : analyzer_term.get_deps()) {
                dep_graph.addEdge(main, dep);
            }

            // create the exprs
            const auto relation =
                    relationRegistry.retrieve_relation(clause->getHead()->getQualifiedName().toString());
            const auto& analyzer_expr = mapping[relation].emplace_back(
                    typeRegistry, relationRegistry, analyzer_term, counter_expr);
            counter_expr = analyzer_expr.counter;
        }

        // derive the relation registration sequence
        sequence = dep_graph.deriveSCC();
    }
};

}  // namespace souffle::smt