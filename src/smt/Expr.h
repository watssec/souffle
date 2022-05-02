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
    virtual std::vector<ExprIndex> children() = 0;

protected:
    explicit Expr(ExprIndex index_) : index(index_) {}
};

// leaf exprs

struct ExprConst : public Expr {
protected:
    explicit ExprConst(ExprIndex index_) : Expr(index_) {}

public:
    std::vector<ExprIndex> children() override {
        return {};
    }
};

struct ExprConstBool : public ExprConst {
    friend RuleAnalyzer;

public:
    const bool value;

protected:
    ExprConstBool(ExprIndex index_, bool value_) : ExprConst(index_), value(value_) {}
};

struct ExprConstNumber : public ExprConst {
    friend RuleAnalyzer;

public:
    int64_t value;

protected:
    ExprConstNumber(ExprIndex index_, int64_t value_) : ExprConst(index_), value(value_) {}
};

struct ExprConstUnsigned : public ExprConst {
    friend RuleAnalyzer;

public:
    uint64_t value;

protected:
    ExprConstUnsigned(ExprIndex index_, uint64_t value_) : ExprConst(index_), value(value_) {}
};

}  // namespace souffle::smt