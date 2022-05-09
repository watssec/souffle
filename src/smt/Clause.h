/************************************************************************
 *
 * @file Clause.h
 *
 * Hosts the clause registry
 *
 ***********************************************************************/

#pragma once

#include "smt/ClauseTerm.h"
#include "smt/Common.h"
#include "smt/Utils.h"

namespace souffle::smt {

/**
 * A registry of clauses appeared in the whole program
 */
class ClauseRegistry {
private:
    // environment
    const TypeRegistry& typeRegistry;
    const RelationRegistry& relationRegistry;

protected:
    // registry
    std::map<RelationIndex, std::vector<ClauseTermAnalyzer>> mapping{};

    // relation registration sequence
    std::list<SCC<RelationIndex>> sequence{};

public:
    ClauseRegistry(const ast::TranslationUnit& unit, const TypeRegistry& typeRegistry_,
            const RelationRegistry& relationRegistry_)
            : typeRegistry(typeRegistry_), relationRegistry(relationRegistry_) {
#ifdef SMT_DEBUG
        std::cout << "[clause] analysis started" << std::endl;
#endif

        // add rules and their dependencies
        Graph<RelationIndex> dep_graph;

        // populate the nodes in the dep graph
        const auto& rels = relationRegistry.all_relations();
        for (const auto& [_, val] : rels) {
            dep_graph.addNode(val.index);
            mapping[val.index].clear();
        }

        // ensure that all terms have unique indices
        size_t counter = 0;

        // do the actual analysis
        const auto& program = unit.getProgram();
        const auto& type_analysis = unit.getAnalysis<ast::analysis::TypeAnalysis>();
        for (const auto clause : program.getClauses()) {
#ifdef SMT_DEBUG
            std::cout << "[clause] analyzing: " << *clause << std::endl;
#endif
            // analyze the clause
            const auto rel_name = clause->getHead()->getQualifiedName().toString();
            const auto rel_index = relationRegistry.retrieve_relation(rel_name);
            const auto& analyzer = mapping[rel_index].emplace_back(
                    clause, type_analysis, typeRegistry, relationRegistry, counter);
            counter = analyzer.counter;

            // populate the edges in the dep graph
            const auto main = analyzer.get_main();
            for (const auto& dep : analyzer.get_deps()) {
                dep_graph.addEdge(main, dep);
            }
        }

        // derive the relation registration sequence
        sequence = dep_graph.deriveSCC();
#ifdef SMT_DEBUG
        for (const auto& scc : sequence) {
            std::cout << "[clause] relation cluster [" << std::endl;
            for (const auto& rel : scc.nodes) {
                std::cout << "  " << relationRegistry.retrieve_details(rel).name << std::endl;
            }
            std::cout << "]" << std::endl;
        }
#endif

#ifdef SMT_DEBUG
        std::cout << "[clause] analysis completed" << std::endl;
#endif
    }

public:
    const std::vector<ClauseTermAnalyzer>& get_terms(const RelationIndex& index) const {
        return mapping.at(index);
    }

public:
    const std::list<SCC<RelationIndex>>& get_relations_sequence() const {
        return sequence;
    }
};

}  // namespace souffle::smt