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

#include "smt/Backend.h"
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

public:
    void populate_backend(Backend& backend) const {
        // types
        backend.mkTypeNumber(types.type_number);
        backend.mkTypeUnsigned(types.type_unsigned);
        for (const auto& [name, index] : types.type_idents) {
            backend.mkTypeIdent(index, name);
        }
        for (const auto& group : types.adt_sequence) {
            backend.mkTypeRecords(group);
        }

        // relations
        for (const auto& scc : clauses.sequence) {
            if (scc.is_cyclic) {
                // create declarations
                for (const auto& index : scc.nodes) {
                    const auto& rel = relations.retrieve_details(index);
                    backend.mkRelDeclRecursive(rel.index, rel.name, rel.params);
                }

                // register definitions
                for (const auto& index : scc.nodes) {
                    const auto& rel = relations.retrieve_details(index);

                    backend.initDef();
                    const auto defs = prepare_relation_definitions(backend, rel);
                    if (!defs.empty()) {
                        // only register definitions when there exists associated rules
                        backend.mkRelDefRecursive(rel.index, rel.params, defs);
                    }
                    backend.finiDef();
                }
            } else {
                assert(scc.nodes.size() == 1);
                const auto& index = *scc.nodes.begin();
                const auto& rel = relations.retrieve_details(index);

                backend.initDef();
                const auto defs = prepare_relation_definitions(backend, rel);
                if (defs.empty()) {
                    // make a declaration when there does not exist any associated rules
                    backend.mkRelDeclSimple(rel.index, rel.name, rel.params);
                } else {
                    // make a definition when there exists rules for this relation
                    backend.mkRelDefSimple(rel.index, rel.params, defs);
                }
                backend.finiDef();
            }
        }

        // facts
        for (const auto& [_, analyzers] : clauses.mapping) {
            for (const auto& analyzer : analyzers) {
                if (analyzer.is_rule) {
                    continue;
                }

                // prepare the exprs
                const auto sequence = analyzer.create_sequence();
                build_exprs_by_sequence(backend, sequence);
                backend.fact(analyzer.root);
            }
        }

        // queries
        for (const auto& query : queries) {
            auto result = backend.query(query);
            assert(result == QueryResult::PASS);
        }
    }

private:
    std::vector<ExprIndex> prepare_relation_definitions(Backend& backend, const RelationInfo& rel) const {
        // quickly scan for definitions
        bool has_def = false;
        for (const auto& analyzer : clauses.mapping.at(rel.index)) {
            if (analyzer.is_rule) {
                has_def = true;
                break;
            }
        }

        // shortcut if there is no rules associated with this relation
        if (!has_def) {
            return {};
        }

        // register params
        for (const auto& [name, type] : rel.params) {
            backend.mkVarParam(name, type);
        }

        // register definitions
        std::vector<ExprIndex> defs;
        for (const auto& analyzer : clauses.mapping.at(rel.index)) {
            if (!analyzer.is_rule) {
                continue;
            }

            // prepare the exprs
            const auto sequence = analyzer.create_sequence();
            build_exprs_by_sequence(backend, sequence);
            defs.push_back(analyzer.root);
        }

        return defs;
    }

    void build_exprs_by_sequence(Backend& backend, const std::vector<const Expr*>& seq) const {
        for (auto expr : seq) {
            auto index = expr->index;

            // constants
            if (auto const_bool = dynamic_cast<const ExprConstBool*>(expr)) {
                backend.mkExprConstBool(index, const_bool->value);
                continue;
            }
            if (auto const_number = dynamic_cast<const ExprConstNumber*>(expr)) {
                backend.mkExprConstNumber(index, const_number->value);
                continue;
            }
            if (auto const_unsigned = dynamic_cast<const ExprConstUnsigned*>(expr)) {
                backend.mkExprConstUnsigned(index, const_unsigned->value);
                continue;
            }

            // variables
            if (auto var_param = dynamic_cast<const ExprVarParam*>(expr)) {
                backend.mkExprVarParamRef(index, var_param->name);
                continue;
            }
            if (auto var_quant = dynamic_cast<const ExprVarQuant*>(expr)) {
                backend.mkExprVarQuantRef(index, var_quant->name);
                continue;
            }

            // identifier
            if (auto ident = dynamic_cast<const ExprIdent*>(expr)) {
                backend.mkExprIdent(index, ident->type, ident->value);
                continue;
            }

            // recursive
            if (auto adt_ctor = dynamic_cast<const ExprADTCtor*>(expr)) {
                backend.mkExprADTCtor(index, adt_ctor->adt, adt_ctor->branch, adt_ctor->args);
                continue;
            }
            if (auto adt_test = dynamic_cast<const ExprADTTest*>(expr)) {
                backend.mkExprADTTest(index, adt_test->adt, adt_test->branch, adt_test->child);
                continue;
            }
            if (auto adt_getter = dynamic_cast<const ExprADTGetter*>(expr)) {
                backend.mkExprADTGetter(
                        index, adt_getter->adt, adt_getter->branch, adt_getter->field, adt_getter->child);
                continue;
            }

            if (auto atom = dynamic_cast<const ExprAtom*>(expr)) {
                backend.mkExprAtom(index, atom->relation, atom->args);
                continue;
            }
            if (auto negation = dynamic_cast<const ExprNegation*>(expr)) {
                backend.mkExprNegation(index, negation->child);
                continue;
            }

            if (auto functor = dynamic_cast<const ExprFunctor*>(expr)) {
                backend.mkExprFunctor(index, functor->op, functor->lhs, functor->rhs);
                continue;
            }
            if (auto constraint = dynamic_cast<const ExprConstraint*>(expr)) {
                backend.mkExprConstraint(index, constraint->op, constraint->lhs, constraint->rhs);
                continue;
            }

            // collectives
            if (auto predicates = dynamic_cast<const ExprPredicates*>(expr)) {
                if (predicates->is_conjunction) {
                    backend.mkExprPredConjunction(index, predicates->args);
                } else {
                    backend.mkExprPredDisjunction(index, predicates->args);
                }
                continue;
            }

            // quantifiers
            if (auto quant_vars = dynamic_cast<const ExprQuantifierVars*>(expr)) {
                backend.initQuantifier();
                for (const auto& [name, type] : quant_vars->vars) {
                    backend.mkVarQuant(name, type);
                }
                continue;
            }
            if (auto quant_full = dynamic_cast<const ExprQuantifierFull*>(expr)) {
                std::vector<std::pair<std::string, TypeIndex>> quant_vars;
                for (const auto& [name, type] : quant_full->vars) {
                    quant_vars.emplace_back(name, type);
                }

                if (quant_full->is_forall) {
                    backend.mkExprQuantifierForall(index, quant_vars, quant_full->rhs);
                } else {
                    backend.mkExprQuantifierExists(index, quant_vars, quant_full->rhs);
                }
                backend.finiQuantifier();
                continue;
            }

            // catch all
            throw new std::runtime_error("Unsupported terms");
        }
    }
};

}  // namespace souffle::smt