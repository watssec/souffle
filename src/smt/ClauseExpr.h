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

// recursive nodes

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

struct ExprConstraint : public ExprBinary {
    friend ClauseExprAnalyzer;

public:
    const BinaryConstraintOp op;

protected:
    ExprConstraint(ExprIndex index_, BinaryConstraintOp op_, ExprIndex lhs_, ExprIndex rhs_)
            : ExprBinary(index_, lhs_, rhs_), op(op_) {}
};

/**
 * A registry of expressions appeared in one rule
 */
class ClauseExprAnalyzer {
    friend Frontend;

private:
    // environment
    const TypeRegistry& typeRegistry;
    const RelationRegistry& relationRegistry;
    const ClauseTermAnalyzer& clauseAnalysis;

    // counter
    size_t counter = 1;

protected:
    // expr registry
    std::map<ExprIndex, std::unique_ptr<Expr>> exprs{};

    // variable binding
    std::map<std::string, ExprIndex> vars_named{};
    std::map<const ast::UnnamedVariable*, ExprIndex> vars_unnamed{};
    std::vector<ExprIndex> binding_conds{};

public:
    ClauseExprAnalyzer(const TypeRegistry& typeRegistry_, const RelationRegistry& relationRegistry_,
            const ClauseTermAnalyzer& clauseAnalysis_)
            : typeRegistry(typeRegistry_), relationRegistry(relationRegistry_),
              clauseAnalysis(clauseAnalysis_) {
        // heavy lifting
        transform();
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
    void transform() {
        // pre-define the parameters
        const auto& rel = relationRegistry.retrieve_details(clauseAnalysis.get_head());
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
            const auto [_, inserted] = vars_named.emplace(var_named->name, cursor->index);
            // NOTE: this assertion is in fact not necessary.
            // It is perfectly OK to have the same souffle variable bounded to two params.
            // But for simplicity, we keep the condition here.
            assert(inserted);
            return;
        }
        if (auto var_unnamed = dynamic_cast<const TermVarUnnamed*>(term)) {
            const auto [_, inserted] = vars_unnamed.emplace(var_unnamed->ptr, cursor->index);
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
};

}  // namespace souffle::smt