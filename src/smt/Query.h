/************************************************************************
 *
 * @file Query.h
 *
 * Hosts the query registry. A query must satisfy the following conditions
 *
 * 1) The relation has zero arith
 * 2) The relation cannot have any fact, i.e., the relation has rules only
 *
 ***********************************************************************/

#pragma once

#include "smt/Clause.h"
#include "smt/Common.h"
#include "smt/Relation.h"

namespace souffle::smt {

/**
 * A registry of queries appeared in the whole program
 */
class QueryRegistry {
private:
    std::set<RelationIndex> queries;

    // environment
    const RelationRegistry& relationRegistry;
    const ClauseRegistry& clauseRegistry;

public:
    explicit QueryRegistry(const ast::TranslationUnit& unit, const RelationRegistry& relationRegistry_,
            const ClauseRegistry& clauseRegistry_)
            : relationRegistry(relationRegistry_), clauseRegistry(clauseRegistry_) {
#ifdef SMT_DEBUG
        std::cout << "[query] analysis started" << std::endl;
#endif

        const auto& program = unit.getProgram();
        for (const auto* directive : program.getDirectives()) {
            if (directive->getType() != ast::DirectiveType::output) {
                throw std::runtime_error("Only the `output` directive is supported now");
            }

            const auto name = directive->getQualifiedName().toString();
#ifdef SMT_DEBUG
            std::cout << "[query] found a query predicate: " << name << std::endl;
#endif

            const auto& rel_index = relationRegistry.retrieve_relation(name);
            const auto [_, inserted] = queries.emplace(rel_index);
            assert(inserted);

            // check that a query relation has zero arity
            const auto& rel_info = relationRegistry.retrieve_details(rel_index);
            if (!rel_info.params.empty()) {
                throw std::runtime_error("Query must have zero arith: " + name);
            }

            // check that a query relation has zero facts
            const auto& rel_analyzers = clauseRegistry.get_terms(rel_index);
            for (const auto& analyzer : rel_analyzers) {
                if (analyzer.get_body().empty()) {
                    throw std::runtime_error("Query must have zero facts: " + name);
                }
            }
        }

#ifdef SMT_DEBUG
        std::cout << "[query] analysis completed" << std::endl;
#endif
    }
};

}  // namespace souffle::smt