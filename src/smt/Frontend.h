/************************************************************************
 *
 * @file Frontend.h
 *
 * Frontend that converts Souffle AST into IR.
 *
 ***********************************************************************/

#pragma once

#include "smt/Backend.h"
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
    RelationRegistry relations;
    ClauseRegistry clauses;
    QueryRegistry queries;

public:
    explicit Frontend(const ast::TranslationUnit& unit)
            : types(unit), relations(unit, types), clauses(unit, types, relations),
              queries(unit, relations, clauses) {}

public:
    void populate_backend(Backend& backend) const {
        // types
        backend.mkTypeNumber(types.get_index_for_number());
        backend.mkTypeUnsigned(types.get_index_for_unsigned());
        for (const auto& [name, index] : types.get_sequence_for_idents()) {
            backend.mkTypeIdent(index, name);
        }
        for (const auto& group : types.get_sequence_for_adts()) {
            backend.mkTypeRecords(group);
        }

        // relations
        for (const auto& scc : clauses.get_relations_sequence()) {
#ifdef SMT_DEBUG
            std::cout << "[frontend] relation SCC(" << (scc.is_cyclic ? "cyclic" : "normal") << "): ["
                      << std::endl;
            for (const auto& rel : scc.nodes) {
                std::cout << "  " << relations.retrieve_details(rel).name << std::endl;
            }
            std::cout << "]" << std::endl;
#endif
            for (const auto& rel : scc.nodes) {
                const auto& details = relations.retrieve_details(rel);
#ifdef SMT_DEBUG
                std::cout << "[frontend] populating relation: " << details.name << " {" << std::endl;
#endif
                // add the clauses
                const auto& analyzers = clauses.get_analyzers(rel);
                for (const auto& analyzer : analyzers) {
                    // mark the start of clause definition
                    const auto vars = analyzer.get_vars();
                    const auto& body = analyzer.get_body();

                    // when this is a fact
                    if (body.empty()) {
                        const auto head_index = analyzer.get_head();
                        const auto head_terms = analyzer.get_term_sequence(head_index);
#ifdef SMT_DEBUG
                        std::cout << "  FACT: ";
                        analyzer.print_term(std::cout, vars, head_index);
                        std::cout << std::endl;
#endif
                    }

                    // when this is a rule
                    else {
                        const auto head_index = analyzer.get_head();
                        const auto head_terms = analyzer.get_term_sequence(head_index);
#ifdef SMT_DEBUG
                        std::cout << "  RULE: ";
                        analyzer.print_term(std::cout, vars, head_index);
                        std::cout << " [" << std::endl;
                        for (const auto& body_index : body) {
                            std::cout << "    ";
                            analyzer.print_term(std::cout, vars, body_index);
                            std::cout << std::endl;
                        }
                        std::cout << "  ]" << std::endl;
#endif
                    }

                    // mark the end of clause definition
                }
#ifdef SMT_DEBUG
                std::cout << "}" << std::endl;
#endif
            }
        }
    }

private:
    void build_terms_by_sequence(
            Backend& backend, const ClauseVars& vars, const std::vector<const Term*>& seq) const {
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
                auto it = vars.vars_named.find(term_var_named->name);
                assert(it != vars.vars_named.end());
                backend.mkTermVarRef(index, term_var_named->name);
                continue;
            }
            if (auto term_var_unnamed = dynamic_cast<const TermVarUnnamed*>(term)) {
                auto it = vars.vars_unnamed.find(term_var_unnamed->ptr);
                assert(it != vars.vars_unnamed.end());
                backend.mkTermVarRef(index, vars.anon_names.at(it->first));
                continue;
            }

            // identifier
            if (auto term_ident = dynamic_cast<const TermIdent*>(term)) {
                backend.mkTermIdent(index, term_ident->type.value(), term_ident->value);
                continue;
            }

            // recursive nodes
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

            if (auto term_functor = dynamic_cast<const TermFunctorOp*>(term)) {
                backend.mkTermFunctor(index, term_functor->op, term_functor->lhs, term_functor->rhs);
                continue;
            }
            if (auto term_constraint = dynamic_cast<const TermConstraint*>(term)) {
                backend.mkTermConstraint(
                        index, term_constraint->op, term_constraint->lhs, term_constraint->rhs);
                continue;
            }

            if (auto term_counter = dynamic_cast<const TermCounter*>(term)) {
                backend.mkTermCount(index, term_counter->args);
                continue;
            }

            // catch all
            throw new std::runtime_error("Unsupported terms");
        }
    }
};

}  // namespace souffle::smt