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
#include "smt/Expr.h"
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

            // TODO: a current limitation, we only handle SAT/UNSAT relations
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
                for (const auto& index : scc.nodes) {
                    const auto& rel = relations.retrieve_details(index);
                    backend.mkRelDeclRecursive(rel.index, rel.name, rel.params);
                    build_relation(rel.index);
                }
            } else {
                assert(scc.nodes.size() == 1);
                const auto& rel = relations.retrieve_details(*scc.nodes.begin());
                backend.mkRelDeclSimple(rel.index, rel.name, rel.params);
                build_relation(rel.index);
            }
        }
    }

    // TODO: to be removed
    void populate_backend2(Backend& backend) const {
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
        for (const auto& [name, vals] : relations.mapping) {
            backend.mkRelation(vals.index, name, vals.params);
        }

        // clauses
        for (const auto& [_, analyzers] : clauses.mapping) {
            for (const auto& analyzer : analyzers) {
                auto inst = analyzer.get_vars();
                auto order = analyzer.create_sequence();

                // mark the start of clause declaration
                backend.initClause();

                // declare vars
                for (const auto& [key, val] : inst.vars_named) {
                    backend.mkVar(key, val);
                }
                for (const auto& [key, val] : inst.vars_unnamed) {
                    backend.mkVar(inst.anon_names.at(key), val);
                }

                // build terms
                build_terms_by_sequence(backend, inst, order.head);
                for (const auto& seq : order.body) {
                    build_terms_by_sequence(backend, inst, seq);
                }

                // make facts/rules
                auto head_index = order.head.back()->index;
                if (order.body.empty()) {
                    backend.mkFact(head_index);
                } else {
                    std::vector<TermIndex> body_indices;
                    for (const auto& seq : order.body) {
                        body_indices.push_back(seq.back()->index);
                    }
                    backend.mkRule(head_index, body_indices);
                }

                // mark the end of clause declaration
                backend.finiClause();
            }

            // queries
            for (const auto& rel : queries) {
                std::cout << backend.query(rel) << std::endl;
            }
        }
    }

private:
    void build_relation(const RelationIndex& index) const {
        const auto& defs = clauses.mapping.at(index);
        for (const auto& analyzer : defs) {
            const auto& rule = RuleAnalyzer(types, relations, analyzer);
            // TODO impl
            assert(rule.counter != 0);
        }
    }

    void build_terms_by_sequence(
            Backend& backend, const ClauseInstantiation& inst, const std::vector<const Term*>& seq) const {
        for (auto term : seq) {
            auto index = term->index;

            // constants
            if (auto term_const_bool = dynamic_cast<const TermConstBool*>(term)) {
                backend.mkTermConstBool(index, term_const_bool->value);
                continue;
            }
            if (auto term_const_number = dynamic_cast<const TermConstNumber*>(term)) {
                backend.mkTermConstNumber(index, term_const_number->value);
                continue;
            }
            if (auto term_const_unsigned = dynamic_cast<const TermConstUnsigned*>(term)) {
                backend.mkTermConstUnsigned(index, term_const_unsigned->value);
                continue;
            }

            // variables
            if (auto term_var_named = dynamic_cast<const TermVarNamed*>(term)) {
                auto it = inst.vars_named.find(term_var_named->name);
                assert(it != inst.vars_named.end());
                backend.mkTermVarRef(index, term_var_named->name);
                continue;
            }
            if (auto term_var_unnamed = dynamic_cast<const TermVarUnnamed*>(term)) {
                auto it = inst.vars_unnamed.find(term_var_unnamed->ptr);
                assert(it != inst.vars_unnamed.end());
                backend.mkTermVarRef(index, inst.anon_names.at(it->first));
                continue;
            }

            // identifier
            if (auto term_ident = dynamic_cast<const TermIdent*>(term)) {
                backend.mkTermIdent(index, term_ident->type.value(), term_ident->value);
                continue;
            }

            // recursive nodes
            if (auto term_functor = dynamic_cast<const TermFunctorOp*>(term)) {
                backend.mkTermFunctor(index, term_functor->op, term_functor->lhs, term_functor->rhs);
                continue;
            }
            if (auto term_ctor = dynamic_cast<const TermCtor*>(term)) {
                backend.mkTermCtor(index, term_ctor->adt, term_ctor->branch, term_ctor->args);
                continue;
            }
            if (auto term_atom = dynamic_cast<const TermAtom*>(term)) {
                backend.mkTermAtom(index, term_atom->relation, term_atom->args);
                continue;
            }
            if (auto term_negation = dynamic_cast<const TermNegation*>(term)) {
                backend.mkTermNegation(index, term_negation->child);
                continue;
            }
            if (auto term_constraint = dynamic_cast<const TermConstraint*>(term)) {
                backend.mkTermConstraint(
                        index, term_constraint->op, term_constraint->lhs, term_constraint->rhs);
                continue;
            }

            // catch all
            throw new std::runtime_error("Unsupported terms");
        }
    }
};

}  // namespace souffle::smt