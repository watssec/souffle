/************************************************************************
 *
 * @file Frontend.h
 *
 * Frontend that converts Souffle AST into IR.
 *
 ***********************************************************************/

#pragma once

#include <tuple>
#include <vector>

#include "smt/Clause.h"
#include "smt/Relation.h"
#include "smt/Typing.h"

namespace souffle::smt {

/**
 * The frontend of AST conversion.
 *
 * - Iterate over the Souffle program and existing analysis results
 * - Type-check and restructure the AST into a form consumable by an SMT backend
 */
class Frontend {
private:
    TypeRegistry types;
    RelationRegistry relations;
    ClauseRegistry clauses;
    std::vector<RelationIndex> queries;

public:
    explicit Frontend(const ast::TranslationUnit& unit)
            : types(unit), relations(unit, types), clauses(unit, types, relations) {
        // prepare queries
        const auto& program = unit.getProgram();
        for (const auto* directive : program.getDirectives()) {
            if (directive->getType() != ast::DirectiveType::output) {
                throw std::runtime_error("Only the `output` directive is supported now");
            }
            const auto rel = relations.retrieve_relation(directive->getQualifiedName().toString());

            // TODO: a current limitation, we only handle PASS/FAIL relations
            assert(relations.retrieve_details(rel).params.empty());
            queries.emplace_back(rel);
        }
    };
};

}  // namespace souffle::smt