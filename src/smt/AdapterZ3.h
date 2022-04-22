/************************************************************************
 *
 * @file AdapterZ3.h
 *
 * Adapter for the Z3 SMT solver
 *
 ***********************************************************************/

#pragma once

#include <tuple>
#include <vector>

#include <z3.h>

#include "smt/Adapter.h"

namespace souffle::smt {

class SortZ3;
class SortNumberZ3;
class SortUnsignedZ3;
class SortIdentZ3;
class SortRecordZ3;

/**
 * A base class to host the context for all Z3-based solvers
 */
class ContextZ3 {
    friend SortZ3;
    friend SortNumberZ3;
    friend SortUnsignedZ3;
    friend SortIdentZ3;
    friend SortRecordZ3;

protected:
    Z3_context ctx;

protected:  // disables instantiation
    explicit ContextZ3(Z3_config cfg) {
        ctx = Z3_mk_context(cfg);
        Z3_del_config(cfg);
    }

public:
    ~ContextZ3() {
        Z3_del_context(ctx);
        ctx = nullptr;
    }
};

/**
 * A context for SMT based on Z3 MuZ facilities
 */
class ContextZ3MuZ : public ContextZ3 {
public:
    ContextZ3MuZ() : ContextZ3(mkConfig()) {}

private:
    static inline Z3_config mkConfig() {
        auto cfg = Z3_mk_config();
        return cfg;
    }

public:
    using SORT_BASE = SortZ3;
    using SORT_NUMBER = SortNumberZ3;
    using SORT_UNSIGNED = SortUnsignedZ3;
    using SORT_IDENT = SortIdentZ3;
    using SORT_RECORD = SortRecordZ3;
};

/**
 * A context for SMT based on Z3 recursive functions
 */
class ContextZ3Rec : public ContextZ3 {
public:
    ContextZ3Rec() : ContextZ3(mkConfig()) {}

private:
    static inline Z3_config mkConfig() {
        auto cfg = Z3_mk_config();
        return cfg;
    }

public:
    using SORT_BASE = SortZ3;
    using SORT_NUMBER = SortNumberZ3;
    using SORT_UNSIGNED = SortUnsignedZ3;
    using SORT_IDENT = SortIdentZ3;
    using SORT_RECORD = SortRecordZ3;
};

/**
 * A base class for all sorts in Z3
 */
class SortZ3 {
    friend SortNumberZ3;
    friend SortUnsignedZ3;
    friend SortIdentZ3;
    friend SortRecordZ3;

protected:
    Z3_sort sort;

protected:  // disables instantiation
    SortZ3() = default;
};

class SortNumberZ3 : public SortZ3 {
public:
    explicit SortNumberZ3(ContextZ3& ctx) {
        sort = Z3_mk_int_sort(ctx.ctx);
    }
};

class SortUnsignedZ3 : public SortZ3 {
public:
    explicit SortUnsignedZ3(ContextZ3& ctx) {
        sort = Z3_mk_bv_sort(ctx.ctx, RAM_DOMAIN_SIZE);
    }
};

class SortIdentZ3 : public SortZ3 {
public:
    SortIdentZ3(ContextZ3& ctx, const ast::QualifiedName& name) {
        sort = Z3_mk_uninterpreted_sort(ctx.ctx, Z3_mk_string_symbol(ctx.ctx, name.toString().c_str()));
    }
};

class SortRecordZ3 : public SortZ3 {
protected:
    Z3_func_decl ctor;
    std::vector<Z3_func_decl> getters;

public:
    static std::vector<SortRecordZ3> batchCreate(const std::vector<ADT<SortZ3>>& decls) {
        // TODO: implement
        return std::vector<SortRecordZ3>(decls.size());
    };
};

}  // namespace souffle::smt