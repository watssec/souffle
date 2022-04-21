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

namespace souffle::smt {

class SortCVC5;
class SortNumberCVC5;
class SortUnsignedCVC5;
class SortUninterpretedCVC5;
class SortRecordCVC5;

/**
 * A base class to host the context for all CVC5-based solvers
 */
class ContextCVC5 {
    friend SortCVC5;
    friend SortNumberCVC5;
    friend SortUnsignedCVC5;
    friend SortUninterpretedCVC5;
    friend SortRecordCVC5;

protected:
    cvc5::Solver solver;

protected:  // disables instantiation
    explicit ContextCVC5() = default;

public:
    ~ContextCVC5() = default;
};

/**
 * A context for SMT based on CVC5 recursive functions
 */
class ContextCVC5Rec : public ContextCVC5 {
public:
    ContextCVC5Rec() = default;
};

/**
 * A base class for all sorts in CVC5
 */
class SortCVC5 {
    friend SortNumberCVC5;
    friend SortUnsignedCVC5;
    friend SortUninterpretedCVC5;
    friend SortRecordCVC5;

protected:
    cvc5::Sort sort;

protected:  // disables instantiation
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

class SortUninterpretedCVC5 : public SortCVC5 {
public:
    SortUninterpretedCVC5(ContextCVC5& ctx, const ast::QualifiedName& name) {
        sort = ctx.solver.mkUninterpretedSort(name.toString());
    }
};

class SortRecordCVC5 : public SortCVC5 {
protected:
    cvc5::Datatype record;

public:
    SortRecordCVC5(ContextCVC5& ctx, const ast::QualifiedName& name,
            const std::vector<std::tuple<const ast::QualifiedName&, const SortCVC5&>>& fields) {
        auto ctor_decl = ctx.solver.mkDatatypeConstructorDecl("");
        for (const auto& [field_name, field_sort] : fields) {
            ctor_decl.addSelector(field_name.toString(), field_sort.sort);
        }

        auto sort_decl = ctx.solver.mkDatatypeDecl(name.toString());
        sort_decl.addConstructor(ctor_decl);
        sort = ctx.solver.mkDatatypeSort(sort_decl);

        assert(sort.isDatatype());
        record = sort.getDatatype();
    }
};

}  // namespace souffle::smt