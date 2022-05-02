/************************************************************************
 *
 * @file Expr.h
 *
 * Hosts the expressions appeared in the program
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

#include "smt/Clause.h"
#include "smt/Relation.h"
#include "smt/Typing.h"

namespace souffle::smt {

// forward declarations
class RuleAnalyzer;
class Frontend;

/**
 * An index that uniquely identifies an expression
 */
class ExprIndex : public Index {
    friend RuleAnalyzer;

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

// const exprs

struct ExprConstBool : public ExprLeaf {
    friend RuleAnalyzer;

public:
    const bool value;

protected:
    ExprConstBool(ExprIndex index_, bool value_) : ExprLeaf(index_), value(value_) {}
};

struct ExprConstNumber : public ExprLeaf {
    friend RuleAnalyzer;

public:
    const int64_t value;

protected:
    ExprConstNumber(ExprIndex index_, int64_t value_) : ExprLeaf(index_), value(value_) {}
};

struct ExprConstUnsigned : public ExprLeaf {
    friend RuleAnalyzer;

public:
    const uint64_t value;

protected:
    ExprConstUnsigned(ExprIndex index_, uint64_t value_) : ExprLeaf(index_), value(value_) {}
};

// param exprs

struct ExprParam : public Expr {
public:
    const std::string name;

protected:
    ExprParam(ExprIndex index_, std::string name_) : Expr(index_), name(std::move(name_)) {}

public:
    std::vector<ExprIndex> children() const override {
        return {};
    }
};

struct ExprParamArg : public ExprParam {
    friend RuleAnalyzer;

protected:
    ExprParamArg(ExprIndex index_, std::string name_) : ExprParam(index_, name_) {}
};

struct ExprParamVar : public ExprParam {
    friend RuleAnalyzer;

protected:
    ExprParamVar(ExprIndex index_, std::string name_) : ExprParam(index_, name_) {}
};

/**
 * A registry of expressions appeared in one rule
 */
class RuleAnalyzer {
    friend Frontend;

private:
    // environment
    const TypeRegistry& typeRegistry;
    const RelationRegistry& relationRegistry;
    const ClauseAnalyzer& clauseAnalysis;

    // clause specific settings
    const std::vector<ExprIndex>& params;

    // counter
    size_t counter = 1;

protected:
    // expr registry
    std::map<ExprIndex, std::unique_ptr<Expr>> exprs{};

public:
    RuleAnalyzer(const TypeRegistry& typeRegistry_, const RelationRegistry& relationRegistry_,
            const ClauseAnalyzer& clauseAnalysis_, const std::vector<ExprIndex>& params_)
            : typeRegistry(typeRegistry_), relationRegistry(relationRegistry_),
              clauseAnalysis(clauseAnalysis_), params(params_) {
        // pre-populate instantiations
        const auto inst = clauseAnalysis.get_vars();

        // follow header argument terms to resolve variables
        const auto& terms = clauseAnalysis.terms;
        auto atom = dynamic_cast<const TermAtom*>(terms.at(clauseAnalysis.head).get());
        for (const auto& child : atom->children()) {
            const auto* arg = terms.at(child).get();
            follow_header_argument(arg, arg);
        }
    }

private:
    void follow_header_argument(const Term* term, const Term* cursor) {
        // constants are allowed
        // TODO
        assert(term == cursor);
    }
};

}  // namespace souffle::smt