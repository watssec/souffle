/************************************************************************
 *
 * @file Frontend.h
 *
 * Frontend that converts Souffle AST into IR.
 *
 ***********************************************************************/

#pragma once

#include "smt/Clause.h"
#include "smt/Query.h"
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
    QueryRegistry queries;
    RelationRegistry relations;
    ClauseRegistry clauses;

public:
    explicit Frontend(const ast::TranslationUnit& unit)
            : types(unit), queries(unit), relations(unit, types), clauses(unit, types, queries, relations) {}
};

}  // namespace souffle::smt