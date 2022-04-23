/************************************************************************
 *
 * @file AdapterZ3.h
 *
 * Adapter for the Z3 SMT solver
 *
 ***********************************************************************/

#pragma once

#include <tuple>
#include <variant>
#include <vector>

#include <z3.h>

#include "smt/Adapter.h"

namespace souffle::smt {

class SortZ3;
class SortNumberZ3;
class SortUnsignedZ3;
class SortIdentZ3;
class SortRecordZ3;
class RelationZ3;
class RelationZ3MuZ;
class RelationZ3Rec;

/**
 * A base class to host the context for all Z3-based solvers
 */
class ContextZ3 {
    friend SortZ3;
    friend SortNumberZ3;
    friend SortUnsignedZ3;
    friend SortIdentZ3;
    friend SortRecordZ3;
    friend RelationZ3;
    friend RelationZ3MuZ;
    friend RelationZ3Rec;

protected:
    Z3_context ctx;

protected:
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
    friend RelationZ3MuZ;

public:
    using SORT_BASE = SortZ3;
    using SORT_NUMBER = SortNumberZ3;
    using SORT_UNSIGNED = SortUnsignedZ3;
    using SORT_IDENT = SortIdentZ3;
    using SORT_RECORD = SortRecordZ3;
    using RELATION = RelationZ3MuZ;

protected:
    Z3_fixedpoint fp;

public:
    ContextZ3MuZ() : ContextZ3(mkConfig()) {
        fp = Z3_mk_fixedpoint(ctx);
        Z3_fixedpoint_inc_ref(ctx, fp);
    }
    ~ContextZ3MuZ() {
        Z3_fixedpoint_dec_ref(ctx, fp);
    }

private:
    static inline Z3_config mkConfig() {
        auto cfg = Z3_mk_config();
        return cfg;
    }
};

/**
 * A context for SMT based on Z3 recursive functions
 */
class ContextZ3Rec : public ContextZ3 {
    friend RelationZ3Rec;

public:
    using SORT_BASE = SortZ3;
    using SORT_NUMBER = SortNumberZ3;
    using SORT_UNSIGNED = SortUnsignedZ3;
    using SORT_IDENT = SortIdentZ3;
    using SORT_RECORD = SortRecordZ3;
    using RELATION = RelationZ3Rec;

protected:
    Z3_solver solver;

public:
    ContextZ3Rec() : ContextZ3(mkConfig()) {
        solver = Z3_mk_solver(ctx);
        Z3_solver_inc_ref(ctx, solver);
    }
    ~ContextZ3Rec() {
        Z3_solver_dec_ref(ctx, solver);
    }

private:
    static inline Z3_config mkConfig() {
        auto cfg = Z3_mk_config();
        return cfg;
    }
};

/**
 * A base class for all sorts in Z3
 */
class SortZ3 {
    friend SortNumberZ3;
    friend SortUnsignedZ3;
    friend SortIdentZ3;
    friend SortRecordZ3;
    friend RelationZ3;

protected:
    Z3_sort sort;

protected:
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
public:
    struct SortRecordZ3Variant {
        Z3_func_decl ctor;
        Z3_func_decl test;
        std::vector<Z3_func_decl> getters;
    };

protected:
    std::map<std::string, SortRecordZ3Variant> variants;

protected:
    SortRecordZ3() = default;

public:
    static std::vector<SortRecordZ3> mkCoInductiveSorts(
            ContextZ3& ctx, const std::vector<ADT<SortZ3>>& decls) {
        // holds all constructors to be re-claimed
        std::vector<Z3_constructor> freelist;

        auto decl_size = decls.size();
        Z3_symbol decl_names[decl_size];
        Z3_constructor_list decl_lists[decl_size];
        for (unsigned k = 0; k < decl_size; k++) {
            const auto& decl = decls[k];

            decl_names[k] = Z3_mk_string_symbol(ctx.ctx, decl.name.c_str());
            auto branch_size = decl.branches.size();
            Z3_constructor branch_ctors[branch_size];
            for (unsigned j = 0; j < branch_size; j++) {
                const auto& branch = decl.branches[j];

                auto field_size = branch.fields.size();
                Z3_symbol field_names[field_size];
                Z3_sort_opt field_sorts[field_size];
                unsigned field_sort_refs[field_size];
                for (unsigned i = 0; i < field_size; i++) {
                    const auto& field = branch.fields[i];

                    field_names[i] = Z3_mk_string_symbol(ctx.ctx, field.name.c_str());
                    if (std::holds_alternative<size_t>(field.type)) {
                        field_sorts[i] = nullptr;
                        field_sort_refs[i] = std::get<size_t>(field.type);
                    } else {
                        field_sorts[i] = std::get<const SortZ3*>(field.type)->sort;
                        field_sort_refs[i] = 0;
                    }
                }

                // build the constructor
                auto branch_ctor =
                        Z3_mk_constructor(ctx.ctx, Z3_mk_string_symbol(ctx.ctx, branch.name.c_str()),
                                Z3_mk_string_symbol(ctx.ctx, ("is_" + branch.name).c_str()), field_size,
                                field_names, field_sorts, field_sort_refs);
                branch_ctors[j] = branch_ctor;
                freelist.push_back(branch_ctor);
            }

            // add to list
            decl_lists[k] = Z3_mk_constructor_list(ctx.ctx, branch_size, branch_ctors);
        }

        // done with the preparation, now do the construction
        Z3_sort decl_sorts[decl_size];
        Z3_mk_datatypes(ctx.ctx, decl_size, decl_names, decl_sorts, decl_lists);

        // create the types
        std::vector<SortRecordZ3> result;
        unsigned c = 0;
        for (unsigned k = 0; k < decl_size; k++) {
            const auto& decl = decls[k];

            SortRecordZ3 sort;
            sort.sort = decl_sorts[k];

            for (const auto& branch : decl.branches) {
                const auto& fields = branch.fields;
                auto holder = freelist[c++];

                // query the constructor
                Z3_func_decl fun_ctor;
                Z3_func_decl fun_test;
                Z3_func_decl fun_getters[fields.size()];
                Z3_query_constructor(ctx.ctx, holder, fields.size(), &fun_ctor, &fun_test, fun_getters);

                // construct and embed the variant
                SortRecordZ3Variant variant;
                variant.ctor = fun_ctor;
                variant.test = fun_test;
                variant.getters.assign(fun_getters, fun_getters + fields.size());
                sort.variants.emplace(branch.name, variant);
            }

            result.push_back(sort);
        }
        assert(c == freelist.size());

        // clean-up the resources
        for (auto item : freelist) {
            Z3_del_constructor(ctx.ctx, item);
        }
        for (unsigned k = 0; k < decl_size; k++) {
            Z3_del_constructor_list(ctx.ctx, decl_lists[k]);
        }

        return result;
    };
};

/**
 * A base class for all relations in Z3
 */
class RelationZ3 {
protected:
    Z3_func_decl fun;

protected:
    RelationZ3(ContextZ3& ctx, const std::string& name, const std::vector<const SortZ3*>& domain) {
        Z3_sort domain_sorts[domain.size()];
        for (unsigned i = 0; i < domain.size(); i++) {
            domain_sorts[i] = domain[i]->sort;
        }
        fun = Z3_mk_func_decl(ctx.ctx, Z3_mk_string_symbol(ctx.ctx, name.c_str()), domain.size(),
                domain_sorts, Z3_mk_bool_sort(ctx.ctx));
    }
};

class RelationZ3MuZ : public RelationZ3 {
public:
    RelationZ3MuZ(ContextZ3MuZ& ctx, const std::string& name, const std::vector<const SortZ3*>& domain)
            : RelationZ3(ctx, name, domain) {
        Z3_fixedpoint_register_relation(ctx.ctx, ctx.fp, fun);
    }
};

class RelationZ3Rec : public RelationZ3 {
public:
    RelationZ3Rec(ContextZ3Rec& ctx, const std::string& name, const std::vector<const SortZ3*>& domain)
            : RelationZ3(ctx, name, domain) {}
};

}  // namespace souffle::smt