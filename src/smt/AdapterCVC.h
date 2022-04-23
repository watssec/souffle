/************************************************************************
 *
 * @file AdapterCVC.h
 *
 * Adapter for the CVC5 SMT solver
 *
 ***********************************************************************/

#pragma once

#include <tuple>
#include <vector>

#include <cvc5/cvc5.h>

#include "smt/Adapter.h"

namespace souffle::smt {

class SortCVC5;
class SortNumberCVC5;
class SortUnsignedCVC5;
class SortIdentCVC5;
class SortRecordCVC5;
class RelationCVC5;
class RelationCVC5Rec;

/**
 * A base class to host the context for all CVC5-based solvers
 */
class ContextCVC5 {
    friend SortCVC5;
    friend SortNumberCVC5;
    friend SortUnsignedCVC5;
    friend SortIdentCVC5;
    friend SortRecordCVC5;
    friend RelationCVC5;
    friend RelationCVC5Rec;

protected:
    cvc5::Solver solver;

protected:
    explicit ContextCVC5() = default;

public:
    ~ContextCVC5() = default;
};

/**
 * A context for SMT based on CVC5 recursive functions
 */
class ContextCVC5Rec : public ContextCVC5 {
    friend RelationCVC5Rec;

public:
    using SORT_BASE = SortCVC5;
    using SORT_NUMBER = SortNumberCVC5;
    using SORT_UNSIGNED = SortUnsignedCVC5;
    using SORT_IDENT = SortIdentCVC5;
    using SORT_RECORD = SortRecordCVC5;
    using RELATION = RelationCVC5Rec;

public:
    ContextCVC5Rec() = default;
};

/**
 * A base class for all sorts in CVC5
 */
class SortCVC5 {
    friend SortNumberCVC5;
    friend SortUnsignedCVC5;
    friend SortIdentCVC5;
    friend SortRecordCVC5;
    friend RelationCVC5;

protected:
    cvc5::Sort sort;

protected:
    SortCVC5() = default;
};

class SortNumberCVC5 : public SortCVC5 {
public:
    explicit SortNumberCVC5(ContextCVC5& ctx) {
        sort = ctx.solver.getIntegerSort();
    }
};

class SortUnsignedCVC5 : public SortCVC5 {
public:
    explicit SortUnsignedCVC5(ContextCVC5& ctx) {
        sort = ctx.solver.mkBitVectorSort(RAM_DOMAIN_SIZE);
    }
};

class SortIdentCVC5 : public SortCVC5 {
public:
    SortIdentCVC5(ContextCVC5& ctx, const ast::QualifiedName& name) {
        sort = ctx.solver.mkUninterpretedSort(name.toString());
    }
};

class SortRecordCVC5 : public SortCVC5 {
protected:
    cvc5::Datatype record;

protected:
    SortRecordCVC5() = default;

public:
    static std::vector<SortRecordCVC5> mkCoInductiveSorts(
            ContextCVC5& ctx, const std::vector<ADT<SortCVC5>>& decls) {
        // prepare the declarations
        std::vector<cvc5::DatatypeDecl> holders;
        for (const auto& decl : decls) {
            auto decl_holder = ctx.solver.mkDatatypeDecl(decl.name, false);
            for (const auto& branch : decl.branches) {
                auto branch_holder = ctx.solver.mkDatatypeConstructorDecl(branch.name);
                for (const auto& field : branch.fields) {
                    if (std::holds_alternative<size_t>(field.type)) {
                        const auto& target = decls[std::get<size_t>(field.type)];
                        if (target.name == decl.name) {
                            branch_holder.addSelectorSelf(field.name);
                        } else {
                            branch_holder.addSelectorUnresolved(field.name, target.name);
                        }
                    } else {
                        branch_holder.addSelector(field.name, std::get<const SortCVC5*>(field.type)->sort);
                    }
                }
                decl_holder.addConstructor(branch_holder);
            }
            holders.push_back(decl_holder);
        }

        // done with the preparation, now do the construction
        auto sorts = ctx.solver.mkDatatypeSorts(holders);
        std::vector<SortRecordCVC5> result;
        for (const auto& sort : sorts) {
            assert(sort.isDatatype());

            SortRecordCVC5 item;
            item.sort = sort;
            item.record = sort.getDatatype();
            result.push_back(item);
        }
        return result;
    }
};

/**
 * A base class for all relations in CVC5
 */
class RelationCVC5 {
protected:
    cvc5::Term fun;

protected:
    RelationCVC5(ContextCVC5& ctx, const std::string& name, const std::vector<const SortCVC5*>& domain) {
        std::vector<cvc5::Sort> domain_sorts;
        for (const auto i : domain) {
            domain_sorts.emplace_back(i->sort);
        }
        fun = ctx.solver.declareFun(name, domain_sorts, ctx.solver.getBooleanSort());
    }
};

class RelationCVC5Rec : public RelationCVC5 {
public:
    RelationCVC5Rec(ContextCVC5Rec& ctx, const std::string& name, const std::vector<const SortCVC5*>& domain)
            : RelationCVC5(ctx, name, domain) {}
};

}  // namespace souffle::smt