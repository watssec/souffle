
/************************************************************************
 *
 * @file Backend.h
 *
 * Backend that models the IR with SMT representations
 *
 ***********************************************************************/

#pragma once

#include "smt/ClauseTerm.h"
#include "smt/Relation.h"
#include "smt/Typing.h"

namespace souffle::smt {

/**
 * The backend of SMT modeling.
 */
class Backend {
public:
    // types
    virtual void mkTypeNumber(const TypeIndex& index) = 0;
    virtual void mkTypeUnsigned(const TypeIndex& index) = 0;
    virtual void mkTypeIdent(const TypeIndex& index, const std::string& name) = 0;
    virtual void mkTypeRecords(const ADTBuilderGroup& group) = 0;

    // relations
    virtual void mkRelation(
            const RelationIndex& index, const std::string& name, const std::vector<TypeIndex>& domains) = 0;

    // terms
    virtual void mkTermVarRef(const TermIndex& index, const std::string& name) = 0;

    virtual void mkTermConstBool(const TermIndex& index, bool value) = 0;
    virtual void mkTermConstNumber(const TermIndex& index, int64_t value) = 0;
    virtual void mkTermConstUnsigned(const TermIndex& index, uint64_t value) = 0;

    virtual void mkTermIdent(const TermIndex& index, const TypeIndex& type, const std::string& ident) = 0;

    virtual void mkTermCtor(const TermIndex& index, const TypeIndex& adt, const std::string& branch,
            const std::vector<TermIndex>& args) = 0;

    virtual void mkTermAtom(
            const TermIndex& index, const RelationIndex& relation, const std::vector<TermIndex>& args) = 0;
    virtual void mkTermNegation(const TermIndex& index, const TermIndex& term) = 0;

    virtual void mkTermFunctor(
            const TermIndex& index, const FunctorOp& op, const TermIndex& lhs, const TermIndex& rhs) = 0;
    virtual void mkTermConstraint(const TermIndex& index, const BinaryConstraintOp& op, const TermIndex& lhs,
            const TermIndex& rhs) = 0;

    virtual void mkTermCount(const TermIndex& index, const std::vector<TermIndex>& args) = 0;

    // clause
    virtual void initClause() = 0;
    virtual void finiClause() = 0;

    virtual void mkVar(const std::string& name, const TypeIndex& type) = 0;
    virtual void mkFact(const TermIndex& head) = 0;
    virtual void mkRule(const TermIndex& head, const std::vector<TermIndex>& body) = 0;
};

}  // namespace souffle::smt