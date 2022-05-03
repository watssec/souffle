/************************************************************************
 *
 * @file ClauseExpr.h
 *
 * Hosts the clauses appeared in the program (in expr view)
 *
 ***********************************************************************/

#pragma once

#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include "smt/ClauseTerm.h"
#include "smt/Relation.h"
#include "smt/Typing.h"

namespace souffle::smt {

// forward declarations
class ClauseExprAnalyzer;
class ClauseRegistry;
class Frontend;

/**
 * An index that uniquely identifies an expression
 */
class ExprIndex : public Index {
    friend ClauseExprAnalyzer;

protected:
    explicit ExprIndex(size_t index_) : Index(index_) {}
};

/**
 * An information package about an expr
 */
class Expr {
public:
    const ExprIndex index;

public:
    virtual std::vector<ExprIndex> children() const = 0;

protected:
    explicit Expr(ExprIndex index_) : index(index_) {}
};

struct ExprLeaf : public Expr {
protected:
    explicit ExprLeaf(ExprIndex index_) : Expr(index_) {}

public:
    std::vector<ExprIndex> children() const override {
        return {};
    }
};

struct ExprUnary : public Expr {
public:
    const ExprIndex child;

protected:
    ExprUnary(ExprIndex index_, ExprIndex child_) : Expr(index_), child(child_) {}

public:
    std::vector<ExprIndex> children() const override {
        return {child};
    }
};

struct ExprBinary : public Expr {
public:
    const ExprIndex lhs;
    const ExprIndex rhs;

protected:
    ExprBinary(ExprIndex index_, ExprIndex lhs_, ExprIndex rhs_) : Expr(index_), lhs(lhs_), rhs(rhs_) {}

public:
    std::vector<ExprIndex> children() const override {
        return {lhs, rhs};
    }
};

struct ExprVariadic : public Expr {
public:
    const std::vector<ExprIndex> args;

protected:
    ExprVariadic(ExprIndex index_, std::vector<ExprIndex> args_) : Expr(index_), args(std::move(args_)) {}

public:
    std::vector<ExprIndex> children() const override {
        return args;
    }
};

// const exprs

struct ExprConstBool : public ExprLeaf {
    friend ClauseExprAnalyzer;

public:
    const bool value;

protected:
    ExprConstBool(ExprIndex index_, bool value_) : ExprLeaf(index_), value(value_) {}
};

struct ExprConstNumber : public ExprLeaf {
    friend ClauseExprAnalyzer;

public:
    const int64_t value;

protected:
    ExprConstNumber(ExprIndex index_, int64_t value_) : ExprLeaf(index_), value(value_) {}
};

struct ExprConstUnsigned : public ExprLeaf {
    friend ClauseExprAnalyzer;

public:
    const uint64_t value;

protected:
    ExprConstUnsigned(ExprIndex index_, uint64_t value_) : ExprLeaf(index_), value(value_) {}
};

// var exprs

struct ExprVar : public Expr {
public:
    const std::string name;

protected:
    ExprVar(ExprIndex index_, std::string name_) : Expr(index_), name(std::move(name_)) {}

public:
    std::vector<ExprIndex> children() const override {
        return {};
    }
};

struct ExprVarParam : public ExprVar {
    friend ClauseExprAnalyzer;

protected:
    ExprVarParam(ExprIndex index_, std::string name_) : ExprVar(index_, name_) {}
};

struct ExprVarQuant : public ExprVar {
    friend ClauseExprAnalyzer;

protected:
    ExprVarQuant(ExprIndex index_, std::string name_) : ExprVar(index_, name_) {}
};

// ident expr

struct ExprIdent : public ExprLeaf {
    friend ClauseExprAnalyzer;

public:
    const TypeIndex type;
    const std::string value;

protected:
    ExprIdent(ExprIndex index_, TypeIndex type_, std::string value_)
            : ExprLeaf(index_), type(type_), value(std::move(value_)) {}
};

// recursive exprs

struct ExprADTCtor : public ExprVariadic {
    friend ClauseExprAnalyzer;

public:
    const TypeIndex adt;
    const std::string branch;

protected:
    ExprADTCtor(ExprIndex index_, TypeIndex adt_, std::string branch_, std::vector<ExprIndex> children_)
            : ExprVariadic(index_, children_), adt(adt_), branch(std::move(branch_)) {}
};

struct ExprADTTest : public ExprUnary {
    friend ClauseExprAnalyzer;

public:
    const TypeIndex adt;
    const std::string branch;

protected:
    ExprADTTest(ExprIndex index_, TypeIndex adt_, std::string branch_, ExprIndex child_)
            : ExprUnary(index_, child_), adt(adt_), branch(std::move(branch_)) {}
};

struct ExprADTGetter : public ExprUnary {
    friend ClauseExprAnalyzer;

public:
    const TypeIndex adt;
    const std::string branch;
    const std::string field;

protected:
    ExprADTGetter(ExprIndex index_, TypeIndex adt_, std::string branch_, std::string field_, ExprIndex child_)
            : ExprUnary(index_, child_), adt(adt_), branch(std::move(branch_)), field(std::move(field_)) {}
};

struct ExprAtom : public ExprVariadic {
    friend ClauseExprAnalyzer;

public:
    const RelationIndex relation;

protected:
    ExprAtom(ExprIndex index_, RelationIndex relation_, std::vector<ExprIndex> children_)
            : ExprVariadic(index_, children_), relation(relation_) {}
};

struct ExprNegation : public ExprUnary {
    friend ClauseExprAnalyzer;

protected:
    ExprNegation(ExprIndex index_, ExprIndex child_) : ExprUnary(index_, child_) {}
};

struct ExprFunctor : public ExprBinary {
    friend ClauseExprAnalyzer;

public:
    const FunctorOp op;

protected:
    ExprFunctor(ExprIndex index_, FunctorOp op_, ExprIndex lhs_, ExprIndex rhs_)
            : ExprBinary(index_, lhs_, rhs_), op(op_) {}
};

struct ExprConstraint : public ExprBinary {
    friend ClauseExprAnalyzer;

public:
    const BinaryConstraintOp op;

protected:
    ExprConstraint(ExprIndex index_, BinaryConstraintOp op_, ExprIndex lhs_, ExprIndex rhs_)
            : ExprBinary(index_, lhs_, rhs_), op(op_) {}
};

struct ExprPredicates : public ExprVariadic {
    friend ClauseExprAnalyzer;

public:
    const bool is_conjunction;

protected:
    ExprPredicates(ExprIndex index_, bool is_conjunction_, std::vector<ExprIndex> children_)
            : ExprVariadic(index_, children_), is_conjunction(is_conjunction_) {}
};

struct ExprQuantifierVars : public ExprLeaf {
    friend ClauseExprAnalyzer;

public:
    const std::map<std::string, TypeIndex> vars;

protected:
    ExprQuantifierVars(ExprIndex index_, std::map<std::string, TypeIndex> vars_)
            : ExprLeaf(index_), vars(std::move(vars_)) {}
};

struct ExprQuantifierFull : public ExprBinary {
    friend ClauseExprAnalyzer;

public:
    const std::map<std::string, TypeIndex> vars;
    const bool is_forall;

protected:
    ExprQuantifierFull(ExprIndex index_, std::map<std::string, TypeIndex> vars_, bool is_forall_,
            ExprIndex lhs_, ExprIndex rhs_)
            : ExprBinary(index_, lhs_, rhs_), vars(std::move(vars_)), is_forall(is_forall_) {}
};

/**
 * A registry of expressions appeared in one rule
 */
class ClauseExprAnalyzer {
    friend Frontend;
    friend ClauseRegistry;

private:
    // environment
    const TypeRegistry& typeRegistry;
    const RelationRegistry& relationRegistry;
    const ClauseTermAnalyzer& clauseAnalysis;

    // counter
    size_t counter;

protected:
    // expr registry
    std::map<ExprIndex, std::unique_ptr<Expr>> exprs{};

    // variable binding
    std::map<std::string, ExprIndex> param_vars_named{};
    std::map<const ast::UnnamedVariable*, ExprIndex> param_vars_unnamed{};
    std::vector<ExprIndex> binding_conds{};

    // quantified variables
    std::map<std::string, TypeIndex> quant_vars_named{};
    std::map<const ast::UnnamedVariable*, TypeIndex> quant_vars_unnamed{};
    std::map<std::string, TypeIndex> quant_var_types{};

    // root
    bool is_rule;
    ExprIndex root{0};

public:
    ClauseExprAnalyzer(const TypeRegistry& typeRegistry_, const RelationRegistry& relationRegistry_,
            const ClauseTermAnalyzer& clauseAnalysis_, size_t counter_)
            : typeRegistry(typeRegistry_), relationRegistry(relationRegistry_),
              clauseAnalysis(clauseAnalysis_), counter(counter_) {
        // heavy lifting
        if (clauseAnalysis.body.empty()) {
            transform_fact();
        } else {
            transform_rule();
        }
    }

public:
    std::vector<const Expr*> create_sequence() const {
        return visit_exprs(root);
    }

private:
    /// Create a new index
    ExprIndex new_index() {
        return ExprIndex(counter++);
    }

private:
    template <typename T, typename... ARGS>
    ExprIndex register_expr(ARGS... args) {
        auto index = new_index();
        exprs.emplace(index, new T(index, args...));
        return index;
    }

private:
    void transform_fact() {
        root = convert(clauseAnalysis.head);
        is_rule = false;
    }

    void transform_rule() {
        // pre-define the parameters
        const auto& rel = relationRegistry.retrieve_details(clauseAnalysis.get_main());
        std::vector<ExprIndex> params;
        for (const auto& [name, type] : rel.params) {
            auto index = register_expr<ExprVarParam>(name);
            params.push_back(index);
        }

        // follow header argument terms to resolve variables
        const auto& terms = clauseAnalysis.terms;
        auto atom = dynamic_cast<const TermAtom*>(terms.at(clauseAnalysis.head).get());
        assert(params.size() == atom->args.size());

        for (std::size_t i = 0; i < params.size(); i++) {
            const auto* arg = terms.at(atom->args[i]).get();
            follow_header_argument(arg, exprs[params[i]].get());
        }

        // any unresolved variable will become existentially quantified
        for (const auto& [name, type] : clauseAnalysis.vars_named) {
            const auto it = param_vars_named.find(name);
            if (it == param_vars_named.end()) {
                quant_vars_named.emplace(name, type);
            }
        }
        for (const auto& [ptr, type] : clauseAnalysis.vars_unnamed) {
            const auto it = param_vars_unnamed.find(ptr);
            if (it == param_vars_unnamed.end()) {
                quant_vars_unnamed.emplace(ptr, type);
            }
        }

        // convert the body literals
        std::vector<ExprIndex> literals;
        for (const auto& item : clauseAnalysis.body) {
            literals.push_back(convert(item));
        }

        std::vector<ExprIndex> predicates;
        predicates.insert(predicates.end(), binding_conds.cbegin(), binding_conds.cend());
        if (quant_var_types.empty()) {
            // combine the body literals and binding conditions
            predicates.insert(predicates.end(), literals.cbegin(), literals.cend());
        } else {
            // encapsulate body literals with an existential quantifier
            const auto quant_vars_index = register_expr<ExprQuantifierVars>(quant_var_types);
            const auto quant_body_index = register_expr<ExprPredicates>(true, literals);
            const auto quant_index = register_expr<ExprQuantifierFull>(
                    quant_var_types, false, quant_vars_index, quant_body_index);
            predicates.push_back(quant_index);
        }

        // final registration
        root = register_expr<ExprPredicates>(true, predicates);
        is_rule = true;
    }

    void follow_header_argument(const Term* term, const Expr* cursor) {
        // constants mark the end of tracing
        if (auto const_number = dynamic_cast<const TermConstNumber*>(term)) {
            auto rhs = register_expr<ExprConstNumber>(const_number->value);
            auto cond = register_expr<ExprConstraint>(BinaryConstraintOp::EQ, cursor->index, rhs);
            binding_conds.push_back(cond);
            return;
        }
        if (auto const_unsigned = dynamic_cast<const TermConstUnsigned*>(term)) {
            auto rhs = register_expr<ExprConstUnsigned>(const_unsigned->value);
            auto cond = register_expr<ExprConstraint>(BinaryConstraintOp::EQ, cursor->index, rhs);
            binding_conds.push_back(cond);
            return;
        }

        // variables also mark the end of tracing
        if (auto var_named = dynamic_cast<const TermVarNamed*>(term)) {
            const auto [_, inserted] = param_vars_named.emplace(var_named->name, cursor->index);
            // NOTE: this assertion is in fact not necessary.
            // It is perfectly OK to have the same souffle variable bounded to two params.
            // But for simplicity, we keep the condition here.
            assert(inserted);
            return;
        }
        if (auto var_unnamed = dynamic_cast<const TermVarUnnamed*>(term)) {
            const auto [_, inserted] = param_vars_unnamed.emplace(var_unnamed->ptr, cursor->index);
            // NOTE: unlike the named var case, this assertion is necessary
            assert(inserted);
            return;
        }

        // ADTs need to be further unpacked
        if (auto adt_ctor = dynamic_cast<const TermCtor*>(term)) {
            auto cond = register_expr<ExprADTTest>(adt_ctor->adt, adt_ctor->branch, cursor->index);
            binding_conds.push_back(cond);

            // cascade into each argument
            const auto& details = typeRegistry.retrieve_adt(adt_ctor->adt);

            // find target branch
            const ADTBranch* branch = nullptr;
            for (const auto& item : details.branches) {
                if (item.name == adt_ctor->branch) {
                    assert(branch == nullptr);
                    branch = &item;
                }
            }
            assert(branch != nullptr);

            // further analyze the fields
            auto fields_len = branch->fields.size();
            assert(fields_len == adt_ctor->args.size());
            for (size_t i = 0; i < fields_len; i++) {
                const auto* sub_arg = clauseAnalysis.terms.at(adt_ctor->args[i]).get();
                auto new_cursor = register_expr<ExprADTGetter>(
                        adt_ctor->adt, adt_ctor->branch, branch->fields[i].name, cursor->index);
                follow_header_argument(sub_arg, exprs.at(new_cursor).get());
            }
            return;
        }

        // all other terms are not allowed
        throw std::runtime_error("Unexpected term during header tracing");
    }

private:
    ExprIndex convert(const TermIndex& index) {
        const auto* term = clauseAnalysis.terms.at(index).get();

        // constants
        if (auto term_const_bool = dynamic_cast<const TermConstBool*>(term)) {
            return register_expr<ExprConstBool>(term_const_bool->value);
        }
        if (auto term_const_number = dynamic_cast<const TermConstNumber*>(term)) {
            return register_expr<ExprConstNumber>(term_const_number->value);
        }
        if (auto term_const_unsigned = dynamic_cast<const TermConstUnsigned*>(term)) {
            return register_expr<ExprConstUnsigned>(term_const_unsigned->value);
        }

        // variables
        if (auto term_var_named = dynamic_cast<const TermVarNamed*>(term)) {
            auto it_param = param_vars_named.find(term_var_named->name);
            if (it_param != param_vars_named.end()) {
                return it_param->second;
            }

            // this is a quantified var
            auto it_quant = quant_vars_named.find(term_var_named->name);
            assert(it_quant != quant_vars_named.end());

            auto new_name = "$_" + term_var_named->name;
            const auto [it, inserted] = quant_var_types.emplace(new_name, it_quant->second);
            if (!inserted) {
                assert(it->second == it_quant->second);
            }
            return register_expr<ExprVarQuant>(new_name);
        }
        if (auto term_var_unnamed = dynamic_cast<const TermVarUnnamed*>(term)) {
            // unnamed vars in body literals must not be in params
            auto it_param = param_vars_unnamed.find(term_var_unnamed->ptr);
            assert(it_param == param_vars_unnamed.end());

            auto it_quant = quant_vars_unnamed.find(term_var_unnamed->ptr);
            assert(it_quant != quant_vars_unnamed.end());

            auto new_name = "$_unnamed_" + std::to_string(quant_vars_named.size());
            const auto [_, inserted] = quant_var_types.emplace(new_name, it_quant->second);
            assert(inserted);
            return register_expr<ExprVarQuant>(new_name);
        }

        // identifier
        if (auto term_ident = dynamic_cast<const TermIdent*>(term)) {
            return register_expr<ExprIdent>(term_ident->type.value(), term_ident->value);
        }

        // recursive nodes
        if (auto term_functor = dynamic_cast<const TermFunctorOp*>(term)) {
            auto lhs = convert(term_functor->rhs);
            auto rhs = convert(term_functor->rhs);
            return register_expr<ExprFunctor>(term_functor->op, lhs, rhs);
        }
        if (auto term_ctor = dynamic_cast<const TermCtor*>(term)) {
            std::vector<ExprIndex> args;
            for (const auto& item : term_ctor->args) {
                args.push_back(convert(item));
            }
            return register_expr<ExprADTCtor>(term_ctor->adt, term_ctor->branch, args);
        }
        if (auto term_atom = dynamic_cast<const TermAtom*>(term)) {
            std::vector<ExprIndex> args;
            for (const auto& item : term_atom->args) {
                args.push_back(convert(item));
            }
            return register_expr<ExprAtom>(term_atom->relation, args);
        }
        if (auto term_negation = dynamic_cast<const TermNegation*>(term)) {
            return register_expr<ExprNegation>(convert(term_negation->child));
        }
        if (auto term_constraint = dynamic_cast<const TermConstraint*>(term)) {
            auto lhs = convert(term_constraint->rhs);
            auto rhs = convert(term_constraint->rhs);
            return register_expr<ExprConstraint>(term_constraint->op, lhs, rhs);
        }

        // catch all
        throw new std::runtime_error("Unsupported terms");
    }

private:
    void visit_exprs_recursive(const ExprIndex& index, std::vector<const Expr*>& sequence) const {
        const auto& expr = exprs.at(index);
        for (const auto& child : expr->children()) {
            visit_exprs_recursive(child, sequence);
        }
        sequence.push_back(expr.get());
    }

    std::vector<const Expr*> visit_exprs(const ExprIndex& index) const {
        std::vector<const Expr*> sequence;
        visit_exprs_recursive(index, sequence);
        return sequence;
    }
};

}  // namespace souffle::smt