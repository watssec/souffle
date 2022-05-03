/************************************************************************
 *
 * @file Backend.h
 *
 * Backend that models the IR with SMT representations
 *
 ***********************************************************************/

#pragma once

#include "smt/ClauseExpr.h"
#include "smt/ClauseTerm.h"
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
    virtual void mkRelDefSimple(const RelationIndex& index, const std::string& name,
            const std::vector<std::pair<std::string, TypeIndex>>& params,
            const std::vector<ExprIndex>& defs) = 0;

    virtual void mkRelDeclRecursive(const RelationIndex& index, const std::string& name,
            const std::vector<std::pair<std::string, TypeIndex>>& params) = 0;
    virtual void mkRelDefRecursive(const RelationIndex& index, const std::vector<ExprIndex>& defs) = 0;

    virtual void mkFact(const ExprIndex& expr) = 0;

    // contexts
    virtual void initDef() = 0;
    virtual void mkVarParam(const std::string& name, const TypeIndex& type) = 0;
    virtual void finiDef() = 0;

    virtual void initQuantifier() = 0;
    virtual void mkVarQuant(const std::string& name, const TypeIndex& type) = 0;
    virtual void finiQuantifier() = 0;

    // exprs
    virtual void mkExprVarParamRef(const ExprIndex& index, const std::string& name) = 0;
    virtual void mkExprVarQuantRef(const ExprIndex& index, const std::string& name) = 0;

    virtual void mkExprConstBool(const ExprIndex& index, bool value) = 0;
    virtual void mkExprConstNumber(const ExprIndex& index, int64_t value) = 0;
    virtual void mkExprConstUnsigned(const ExprIndex& index, uint64_t value) = 0;

    virtual void mkExprIdent(const ExprIndex& index, const TypeIndex& type, const std::string& ident) = 0;

    virtual void mkExprADTCtor(const ExprIndex& index, const TypeIndex& adt, const std::string& branch,
            const std::vector<ExprIndex>& args) = 0;
    virtual void mkExprADTTest(const ExprIndex& index, const TypeIndex& adt, const std::string& branch,
            const ExprIndex& sub) = 0;
    virtual void mkExprADTGetter(const ExprIndex& index, const TypeIndex& adt, const std::string& branch,
            const std::string& field, const ExprIndex& sub) = 0;

    virtual void mkExprAtom(
            const ExprIndex& index, const RelationIndex& relation, const std::vector<ExprIndex>& args) = 0;
    virtual void mkExprNegation(const ExprIndex& index, const ExprIndex& sub) = 0;

    virtual void mkExprFunctor(
            const ExprIndex& index, const FunctorOp& op, const ExprIndex& lhs, const ExprIndex& rhs) = 0;
    virtual void mkExprConstraint(const ExprIndex& index, const BinaryConstraintOp& op, const ExprIndex& lhs,
            const ExprIndex& rhs) = 0;

    virtual void mkExprPredConjunction(const ExprIndex& index, const std::vector<ExprIndex>& args) = 0;
    virtual void mkExprPredDisjunction(const ExprIndex& index, const std::vector<ExprIndex>& args) = 0;

    virtual void mkExprQuantifierExists(const ExprIndex& index, const ExprIndex& body) = 0;
    virtual void mkExprQuantifierForall(const ExprIndex& index, const ExprIndex& body) = 0;

    // query
    virtual SMTResult query(const RelationIndex& index) = 0;
};

}  // namespace souffle::smt