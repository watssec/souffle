/************************************************************************
 *
 * @file Backend.h
 *
 * Backend that models the IR with SMT representations
 *
 ***********************************************************************/

#pragma once

#include "smt/ClauseTerm.h"
#include "smt/ClauseExpr.h"
#include "smt/Relation.h"
#include "smt/Typing.h"

namespace souffle::smt {

enum class SMTResult {
    UNKNOWN = -1,
    UNSAT = 0,
    SAT = 1,
};

inline std::ostream& operator<<(std::ostream& os, const SMTResult& result) {
    switch (result) {
        case SMTResult::UNKNOWN: {
            os << "UNKNOWN";
            break;
        }
        case SMTResult::UNSAT: {
            os << "UNSAT";
            break;
        }
        case SMTResult::SAT: {
            os << "SAT";
            break;
        }
    }
    return os;
}

/**
 * The backend of SMT modeling.
 */
class Backend {
public:
    // types
    virtual void mkTypeNumber(const TypeIndex& index) = 0;
    virtual void mkTypeUnsigned(const TypeIndex& index) = 0;
    virtual void mkTypeIdent(const TypeIndex& index, const std::string& name) = 0;
    virtual void mkTypeRecords(const ADTGroup& group) = 0;

    // relations
    virtual void mkRelDeclSimple(const RelationIndex& index, const std::string& name,
            const std::vector<std::pair<std::string, TypeIndex>>& params) = 0;
    virtual void mkRelDeclRecursive(const RelationIndex& index, const std::string& name,
            const std::vector<std::pair<std::string, TypeIndex>>& params) = 0;

    // terms
    virtual void mkTermVarRef(const TermIndex& index, const std::string& name) = 0;
    virtual void mkTermConstBool(const TermIndex& index, bool value) = 0;
    virtual void mkTermConstNumber(const TermIndex& index, int64_t value) = 0;
    virtual void mkTermConstUnsigned(const TermIndex& index, uint64_t value) = 0;
    virtual void mkTermIdent(const TermIndex& index, const TypeIndex& type, const std::string& ident) = 0;
    virtual void mkTermFunctor(
            const TermIndex& index, const FunctorOp& op, const TermIndex& lhs, const TermIndex& rhs) = 0;
    virtual void mkTermCtor(const TermIndex& index, const TypeIndex& adt, const std::string& branch,
            const std::vector<TermIndex>& args) = 0;
    virtual void mkTermAtom(
            const TermIndex& index, const RelationIndex& relation, const std::vector<TermIndex>& args) = 0;
    virtual void mkTermNegation(const TermIndex& index, const TermIndex& term) = 0;
    virtual void mkTermConstraint(const TermIndex& index, const BinaryConstraintOp& op, const TermIndex& lhs,
            const TermIndex& rhs) = 0;

    // clause
    virtual void initClause() = 0;
    virtual void mkVar(const std::string& name, const TypeIndex& type) = 0;
    virtual void mkFact(const TermIndex& head) = 0;
    virtual void mkRule(const TermIndex& head, const std::vector<TermIndex>& body) = 0;
    virtual void finiClause() = 0;

    // query
    virtual SMTResult query(const RelationIndex& index) = 0;
};

}  // namespace souffle::smt