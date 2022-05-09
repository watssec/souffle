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

#include "smt/Common.h"

namespace souffle::smt {

/**
 * A registry of queries appeared in the whole program
 */
class QueryRegistry {
private:
    std::set<std::string> queries;

public:
    explicit QueryRegistry(const ast::TranslationUnit& unit) {
        const auto& program = unit.getProgram();
        for (const auto* directive : program.getDirectives()) {
            if (directive->getType() != ast::DirectiveType::output) {
                throw std::runtime_error("Only the `output` directive is supported now");
            }

            const auto& name = directive->getQualifiedName();
            const auto [_, inserted] = queries.emplace(name.toString());
            assert(inserted);

            // check that a query relation has zero arity
            for (const auto* rel : program.getRelations()) {
                if (rel->getQualifiedName() == name) {
                    if (rel->getArity() != 0) {
                        throw std::runtime_error("Query must have zero arith: " + name.toString());
                    }
                }
            }

            // check that a query relation has zero facts
            for (const auto* clause : program.getClauses()) {
                if (clause->getHead()->getQualifiedName() == name) {
                    if (clause->getBodyLiterals().empty()) {
                        throw std::runtime_error("Query must have zero facts: " + name.toString());
                    }
                }
            }
        }
    }

public:
    bool is_query(const std::string& name) const {
        return queries.find(name) != queries.end();
    }
};

}  // namespace souffle::smt