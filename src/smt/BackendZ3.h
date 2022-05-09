/************************************************************************
 *
 * @file BackendZ3.h
 *
 * Backend relies on the Z3 solver
 *
 ***********************************************************************/

#pragma once

#include <z3.h>

#include "smt/Backend.h"

namespace souffle::smt {

class SortRecordZ3 {
public:
    struct Variant {
        Z3_func_decl ctor;
        Z3_func_decl test;
        std::vector<Z3_func_decl> getters;
    };

public:
    const std::map<std::string, Variant> variants;

public:
    SortRecordZ3(std::map<std::string, Variant> variants_) : variants(std::move(variants_)) {}
};

/**
 * The backend of SMT modeling based on Z3
 */
class BackendZ3 : public Backend {
protected:
    Z3_context ctx;
    Z3_sort sort_number;
    Z3_sort sort_unsigned;

    // per-program
    std::map<TypeIndex, Z3_sort> types;
    std::map<TypeIndex, SortRecordZ3> adts;

protected:
    explicit BackendZ3(Z3_config cfg) {
        ctx = Z3_mk_context(cfg);
        Z3_del_config(cfg);

        // preset sorts
        sort_number = Z3_mk_int_sort(ctx);
        sort_unsigned = Z3_mk_bv_sort(ctx, RAM_DOMAIN_SIZE);
    }

public:
    ~BackendZ3() {
        Z3_del_context(ctx);
    }

public:
    // types
    void mkTypeNumber(const TypeIndex& index) override {
        const auto& [_, inserted] = types.emplace(index, sort_number);
        assert(inserted);
    }
    void mkTypeUnsigned(const TypeIndex& index) override {
        const auto& [_, inserted] = types.emplace(index, sort_unsigned);
        assert(inserted);
    }
    void mkTypeIdent(const TypeIndex& index, const std::string& name) override {
        const auto& [_, inserted] =
                types.emplace(index, Z3_mk_uninterpreted_sort(ctx, Z3_mk_string_symbol(ctx, name.c_str())));
        assert(inserted);
    }
    void mkTypeRecords(const ADTBuilderGroup& group) override {
        // holds all constructors to be re-claimed
        std::vector<Z3_constructor> freelist;

        auto decl_size = group.adts.size();
        Z3_symbol decl_names[decl_size];
        Z3_constructor_list decl_lists[decl_size];
        for (unsigned k = 0; k < decl_size; k++) {
            const auto& decl = group.adts.at(k);

            decl_names[k] = Z3_mk_string_symbol(ctx, decl.name.c_str());
            auto branch_size = decl.branches.size();
            Z3_constructor branch_ctors[branch_size];
            for (unsigned j = 0; j < branch_size; j++) {
                const auto& branch = decl.branches.at(j);

                auto field_size = branch.fields.size();
                Z3_symbol field_names[field_size];
                Z3_sort_opt field_sorts[field_size];
                unsigned field_sort_refs[field_size];
                for (unsigned i = 0; i < field_size; i++) {
                    const auto& field = branch.fields.at(i);

                    field_names[i] = Z3_mk_string_symbol(ctx, field.name.c_str());
                    if (std::holds_alternative<size_t>(field.type)) {
                        field_sorts[i] = nullptr;
                        field_sort_refs[i] = std::get<size_t>(field.type);
                    } else {
                        auto field_type_index = std::get<TypeIndex>(field.type);
                        field_sorts[i] = types.at(field_type_index);
                        field_sort_refs[i] = 0;
                    }
                }

                // build the constructor
                auto branch_ctor = Z3_mk_constructor(ctx, Z3_mk_string_symbol(ctx, branch.name.c_str()),
                        Z3_mk_string_symbol(ctx, ("is_" + branch.name).c_str()), field_size, field_names,
                        field_sorts, field_sort_refs);
                branch_ctors[j] = branch_ctor;
                freelist.push_back(branch_ctor);
            }

            // add to list
            decl_lists[k] = Z3_mk_constructor_list(ctx, branch_size, branch_ctors);
        }

        // done with the preparation, now do the construction
        Z3_sort decl_sorts[decl_size];
        Z3_mk_datatypes(ctx, decl_size, decl_names, decl_sorts, decl_lists);

        // create the types
        unsigned c = 0;
        for (unsigned k = 0; k < decl_size; k++) {
            const auto& decl = group.adts.at(k);

            std::map<std::string, SortRecordZ3::Variant> variants;
            for (const auto& branch : decl.branches) {
                const auto& fields = branch.fields;
                auto holder = freelist.at(c);
                c++;

                // query the constructor
                Z3_func_decl fun_ctor;
                Z3_func_decl fun_test;
                Z3_func_decl fun_getters[fields.size()];
                Z3_query_constructor(ctx, holder, fields.size(), &fun_ctor, &fun_test, fun_getters);

                // construct and embed the variant
                SortRecordZ3::Variant variant;
                variant.ctor = fun_ctor;
                variant.test = fun_test;
                variant.getters.assign(fun_getters, fun_getters + fields.size());

                const auto& [_, inserted] = variants.emplace(branch.name, variant);
                assert(inserted);
            }

            // register type
            const auto& [_1, inserted1] = types.emplace(decl.index, decl_sorts[k]);
            assert(inserted1);

            const auto& [_2, inserted2] = adts.emplace(decl.index, variants);
            assert(inserted2);
        }
        assert(c == freelist.size());

        // clean-up the resources
        for (auto item : freelist) {
            Z3_del_constructor(ctx, item);
        }
        for (unsigned k = 0; k < decl_size; k++) {
            Z3_del_constructor_list(ctx, decl_lists[k]);
        }
    }
};

class BackendZ3MuZ : public BackendZ3 {
protected:
    Z3_fixedpoint fp;

public:
    BackendZ3MuZ() : BackendZ3(mkConfig()) {
        fp = Z3_mk_fixedpoint(ctx);
        Z3_fixedpoint_inc_ref(ctx, fp);

        // TODO: tune the parameters
        Z3_params params = Z3_mk_params(ctx);
        Z3_params_set_symbol(
                ctx, params, Z3_mk_string_symbol(ctx, "engine"), Z3_mk_string_symbol(ctx, "spacer"));
        Z3_fixedpoint_set_params(ctx, fp, params);
    }
    ~BackendZ3MuZ() {
        Z3_fixedpoint_dec_ref(ctx, fp);
    }

private:
    static inline Z3_config mkConfig() {
        auto cfg = Z3_mk_config();
        return cfg;
    }
};

class BackendZ3Horn : public BackendZ3 {
protected:
    Z3_solver solver;

public:
    BackendZ3Horn() : BackendZ3(mkConfig()) {
        solver = Z3_mk_solver(ctx);
        Z3_solver_inc_ref(ctx, solver);

        // TODO: fine-tune the parameters
        Z3_params params = Z3_mk_params(ctx);
        Z3_params_set_symbol(ctx, params, Z3_mk_string_symbol(ctx, "logic"), Z3_mk_string_symbol(ctx, "ALL"));
        Z3_params_set_bool(ctx, params, Z3_mk_string_symbol(ctx, "induction"), true);
        Z3_params_set_bool(ctx, params, Z3_mk_string_symbol(ctx, "ematching"), true);
        Z3_params_set_bool(ctx, params, Z3_mk_string_symbol(ctx, "mbqi"), true);
        Z3_solver_set_params(ctx, solver, params);
    }
    ~BackendZ3Horn() {
        Z3_solver_dec_ref(ctx, solver);
    }

private:
    static inline Z3_config mkConfig() {
        auto cfg = Z3_mk_config();
        return cfg;
    }
};

}  // namespace souffle::smt