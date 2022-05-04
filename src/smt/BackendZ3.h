/************************************************************************
 *
 * @file BackendZ3.h
 *
 * Backend relies on the Z3 solver
 *
 ***********************************************************************/

#pragma once

#include <fstream>
#include <iostream>

#include <z3.h>

#include "smt/Backend.h"

namespace souffle::smt {

// forward declarations
class BackendZ3;
class BackendZ3Rec;

class SortZ3 {
    friend BackendZ3;

protected:
    Z3_sort sort;

protected:
    explicit SortZ3(Z3_sort sort_) : sort(sort_) {}

public:
    virtual ~SortZ3() = default;
};

class SortNumberZ3 : public SortZ3 {
    friend BackendZ3;

protected:
    explicit SortNumberZ3(Z3_sort sort_) : SortZ3(sort_) {}
};

class SortUnsignedZ3 : public SortZ3 {
    friend BackendZ3;

protected:
    explicit SortUnsignedZ3(Z3_sort sort_) : SortZ3(sort_) {}
};

class SortIdentZ3 : public SortZ3 {
    friend BackendZ3;

protected:
    SortIdentZ3(Z3_sort sort_) : SortZ3(sort_) {}
};

class SortRecordZ3 : public SortZ3 {
    friend BackendZ3;

public:
    struct Variant {
        Z3_func_decl ctor;
        Z3_func_decl test;
        std::map<std::string, Z3_func_decl> getters;
    };

protected:
    std::map<std::string, Variant> variants;

protected:
    SortRecordZ3(Z3_sort sort_, std::map<std::string, Variant> variants_)
            : SortZ3(sort_), variants(std::move(variants_)) {}
};

class FunctionZ3 {
    friend BackendZ3;

protected:
    std::vector<Z3_ast> params;
    Z3_ast body;

protected:
    FunctionZ3(std::vector<Z3_ast> params_, Z3_ast body_) : params(std::move(params_)), body(body_) {}

public:
    Z3_ast substitute(Z3_context ctx, size_t size, Z3_ast args[]) const {
        assert(size == params.size());
        return Z3_substitute(ctx, body, size, params.data(), args);
    }

    Z3_ast get_body() const {
        return body;
    }
};

/**
 * The backend of SMT modeling based on Z3
 */
class BackendZ3 : public Backend {
protected:
    Z3_context ctx;
    Z3_sort sort_number;
    Z3_sort sort_unsigned;

    // types
    std::map<TypeIndex, std::unique_ptr<SortZ3>> types;

    // relations
    std::map<RelationIndex, Z3_func_decl> rel_decls;
    std::map<RelationIndex, std::unique_ptr<FunctionZ3>> rel_defs;

    // terms
    std::map<ExprIndex, Z3_ast> exprs;

    // per-definition
    std::map<std::string, Z3_ast> vars_param;

    // per-quantifier
    std::map<std::string, Z3_ast> vars_quant;

#ifdef SMT_DEBUG
    std::map<RelationIndex, Z3_ast> rec_fun_defs;
    std::ofstream file_smt;
#endif

protected:
    explicit BackendZ3(Z3_config cfg) {
        // enable parallel solving
        Z3_global_param_set("parallel.enable", "true");

        ctx = Z3_mk_context(cfg);
        Z3_del_config(cfg);

        // preset sorts
        sort_number = Z3_mk_int_sort(ctx);
        sort_unsigned = Z3_mk_bv_sort(ctx, RAM_DOMAIN_SIZE);

#ifdef SMT_DEBUG
        // prepare the file
        Z3_set_ast_print_mode(ctx, Z3_ast_print_mode::Z3_PRINT_SMTLIB2_COMPLIANT);
        file_smt.open("proof.smt2", std::ios::out | std::ios::trunc);
#endif
    }

public:
    ~BackendZ3() {
#ifdef SMT_DEBUG
        // save the dump
        file_smt.close();
#endif

        // clear the context
        Z3_del_context(ctx);
    }

private:
    // exprs
    void registerExpr(const ExprIndex& index, Z3_ast term) {
        const auto& [it, inserted] = exprs.emplace(index, simplify(term));
        if (!inserted) {
            assert(it->second == term);
        }
    }

protected:
    // simplification
    virtual Z3_ast simplify(Z3_ast term) {
#ifdef SMT_DEBUG
        return term;
#else
        return Z3_simplify(ctx, term);
#endif
    }

public:
    // types
    void mkTypeNumber(const TypeIndex& index) override {
        const auto& [_, inserted] = types.emplace(index, new SortNumberZ3(sort_number));
        assert(inserted);
    }
    void mkTypeUnsigned(const TypeIndex& index) override {
        const auto& [_, inserted] = types.emplace(index, new SortUnsignedZ3(sort_unsigned));
        assert(inserted);
    }
    void mkTypeIdent(const TypeIndex& index, const std::string& name) override {
        const auto& [_, inserted] = types.emplace(index,
                new SortIdentZ3(Z3_mk_uninterpreted_sort(ctx, Z3_mk_string_symbol(ctx, name.c_str()))));
        assert(inserted);

#ifdef SMT_DEBUG
        // smt representation
        file_smt << "(declare-sort " << name << " 0)" << std::endl;
#endif
    }
    void mkTypeRecords(const ADTGroup& group) override {
        // holds all constructors to be re-claimed
        std::vector<Z3_constructor> freelist;

        auto decl_size = group.adts.size();
        Z3_symbol decl_names[decl_size];
        Z3_constructor_list decl_lists[decl_size];
        for (unsigned k = 0; k < decl_size; k++) {
            const auto& [_, decl] = group.adts[k];

            decl_names[k] = Z3_mk_string_symbol(ctx, decl.name.c_str());
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

                    field_names[i] = Z3_mk_string_symbol(ctx, field.name.c_str());
                    if (std::holds_alternative<size_t>(field.type)) {
                        field_sorts[i] = nullptr;
                        field_sort_refs[i] = std::get<size_t>(field.type);
                    } else {
                        auto field_type_index = std::get<TypeIndex>(field.type);
                        field_sorts[i] = types[field_type_index]->sort;
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
            const auto& [index, decl] = group.adts[k];

            std::map<std::string, SortRecordZ3::Variant> variants;
            for (const auto& branch : decl.branches) {
                const auto& fields = branch.fields;
                auto holder = freelist[c++];

                // query the constructor
                Z3_func_decl fun_ctor;
                Z3_func_decl fun_test;
                Z3_func_decl fun_getters[fields.size()];
                Z3_query_constructor(ctx, holder, fields.size(), &fun_ctor, &fun_test, fun_getters);

                // construct and embed the variant
                SortRecordZ3::Variant variant;
                variant.ctor = fun_ctor;
                variant.test = fun_test;
                variant.getters.clear();
                for (unsigned i = 0; i < fields.size(); i++) {
                    variant.getters.emplace(fields[i].name, fun_getters[i]);
                }
                variants.emplace(branch.name, variant);
            }

            // register type
            const auto& [_, inserted] = types.emplace(index, new SortRecordZ3(decl_sorts[k], variants));
            assert(inserted);
        }
        assert(c == freelist.size());

        // clean-up the resources
        for (auto item : freelist) {
            Z3_del_constructor(ctx, item);
        }
        for (unsigned k = 0; k < decl_size; k++) {
            Z3_del_constructor_list(ctx, decl_lists[k]);
        }

#ifdef SMT_DEBUG
        file_smt << "(declare-datatypes" << std::endl;

        // declarations
        file_smt << "\t(" << std::endl;
        for (const auto& [_, adt] : group.adts) {
            file_smt << "\t\t(" << adt.name << " 0)" << std::endl;
        }
        file_smt << "\t)" << std::endl;

        // definitions
        file_smt << "\t(" << std::endl;
        for (const auto& [_, adt] : group.adts) {
            file_smt << "\t\t; " << adt.name << std::endl;
            file_smt << "\t\t(" << std::endl;
            for (const auto& branch : adt.branches) {
                file_smt << "\t\t\t(" << branch.name;
                for (const auto& field : branch.fields) {
                    const char* field_type_name;
                    if (std::holds_alternative<size_t>(field.type)) {
                        field_type_name = group.adts.at(std::get<size_t>(field.type)).second.name.c_str();
                    } else {
                        auto field_type_index = std::get<TypeIndex>(field.type);
                        field_type_name = Z3_sort_to_string(ctx, types[field_type_index]->sort);
                    }
                    file_smt << " (" << field.name << " " << field_type_name << ")";
                }
                file_smt << ")" << std::endl;
            }
            file_smt << "\t\t)" << std::endl;
        }
        file_smt << "\t)" << std::endl;

        // end of group
        file_smt << ")" << std::endl;
#endif
    }

    // relations
    void mkRelDeclSimple(const RelationIndex& index, const std::string& name,
            const std::vector<std::pair<std::string, TypeIndex>>& params) override {
        Z3_sort domain_sorts[params.size()];
        for (unsigned i = 0; i < params.size(); i++) {
            domain_sorts[i] = types[params[i].second]->sort;
        }
        auto fun = Z3_mk_func_decl(ctx, Z3_mk_string_symbol(ctx, name.c_str()), params.size(), domain_sorts,
                Z3_mk_bool_sort(ctx));
        const auto& [_, inserted] = rel_decls.emplace(index, fun);
        assert(inserted);

#ifdef SMT_DEBUG
        // smt representation
        file_smt << "(declare-fun " << name << " (";
        for (const auto& [_param_name, type] : params) {
            file_smt << " " << Z3_sort_to_string(ctx, types[type]->sort);
        }
        file_smt << " ) Bool)" << std::endl;
#endif
    }

    void mkRelDefSimple(const RelationIndex& index,
            const std::vector<std::pair<std::string, TypeIndex>>& params,
            const std::vector<ExprIndex>& defs) override {
        // collect params
        assert(params.size() == vars_param.size());
        std::vector<Z3_ast> param_asts;
        for (const auto& [var_name, _] : params) {
            const auto it = vars_param.find(var_name);
            assert(it != vars_param.end());
            param_asts.push_back(it->second);
        }

        // collect body
        assert(!defs.empty());
        Z3_ast def_asts[defs.size()];
        for (size_t i = 0; i < defs.size(); i++) {
            def_asts[i] = exprs[defs[i]];
        }
        auto body_ast = Z3_mk_or(ctx, defs.size(), def_asts);

        // insert into registry
        const auto [_, inserted] = rel_defs.emplace(index, new FunctionZ3(param_asts, body_ast));
        assert(inserted);
    }

    void mkRelDeclRecursive(const RelationIndex& index, const std::string& name,
            const std::vector<std::pair<std::string, TypeIndex>>& params) override {
        Z3_sort domain_sorts[params.size()];
        for (unsigned i = 0; i < params.size(); i++) {
            domain_sorts[i] = types[params[i].second]->sort;
        }
        auto fun = Z3_mk_rec_func_decl(ctx, Z3_mk_string_symbol(ctx, name.c_str()), params.size(),
                domain_sorts, Z3_mk_bool_sort(ctx));
        const auto& [_, inserted] = rel_decls.emplace(index, fun);
        assert(inserted);
    }

    void mkRelDefRecursive(const RelationIndex& index,
            const std::vector<std::pair<std::string, TypeIndex>>& params,
            const std::vector<ExprIndex>& defs) override {
        // collect params
        assert(params.size() == vars_param.size());
        std::vector<Z3_ast> param_asts;
        for (const auto& [var_name, _] : params) {
            const auto it = vars_param.find(var_name);
            assert(it != vars_param.end());
            param_asts.push_back(it->second);
        }

        // collect body
        assert(!defs.empty());
        Z3_ast def_asts[defs.size()];
        for (size_t i = 0; i < defs.size(); i++) {
            def_asts[i] = exprs[defs[i]];
        }
        auto body_ast = Z3_mk_or(ctx, defs.size(), def_asts);

        // insert into registry
        Z3_add_rec_def(ctx, rel_decls[index], params.size(), param_asts.data(), body_ast);

#ifdef SMT_DEBUG
        const auto [_, inserted] = rec_fun_defs.emplace(index, body_ast);
        assert(inserted);
#endif
    }

#ifdef SMT_DEBUG
    void mkRelSCC(const std::vector<RelationInfo>& relations) override {
        file_smt << "(define-funs-rec" << std::endl;

        // declarations
        size_t counter = 0;
        file_smt << "\t(" << std::endl;
        for (const auto& rel : relations) {
            file_smt << "\t\t(" << rel.name << " (";
            for (const auto& [param_name, param_type] : rel.params) {
                file_smt << " (" << param_name << "!" << counter++ << " "
                         << Z3_sort_to_string(ctx, types[param_type]->sort) << ")";
            }
            file_smt << " ) Bool)" << std::endl;
        }
        file_smt << "\t)" << std::endl;

        // definitions
        file_smt << "\t(" << std::endl;
        for (const auto& rel : relations) {
            file_smt << "\t\t; " << rel.name << std::endl;
            file_smt << "\t\t" << Z3_ast_to_string(ctx, rec_fun_defs[rel.index]) << std::endl;
        }
        file_smt << "\t)" << std::endl;

        // end of group
        file_smt << ")" << std::endl;
    }
#endif

    // context
    void initDef() override {
        assert(vars_param.empty());
    }
    void mkVarParam(const std::string& name, const TypeIndex& type) override {
        Z3_sort var_sort = types[type]->sort;
        Z3_ast var_ref = Z3_mk_fresh_const(ctx, name.c_str(), var_sort);
        const auto& [_, inserted] = vars_param.emplace(name, var_ref);
        assert(inserted);
    }
    void finiDef() override {
        vars_param.clear();
    }

    void initQuantifier() override {
        assert(vars_quant.empty());
    }
    void mkVarQuant(const std::string& name, const TypeIndex& type) override {
        Z3_sort var_sort = types[type]->sort;
        Z3_ast var_ref = Z3_mk_bound(ctx, vars_quant.size(), var_sort);
        const auto& [_, inserted] = vars_quant.emplace(name, var_ref);
        assert(inserted);
    }
    void finiQuantifier() override {
        vars_quant.clear();
    }

    // terms
    void mkExprVarParamRef(const ExprIndex& index, const std::string& name) override {
        registerExpr(index, vars_param[name]);
    }
    void mkExprVarQuantRef(const ExprIndex& index, const std::string& name) override {
        registerExpr(index, vars_quant[name]);
    }

    void mkExprConstBool(const ExprIndex& index, bool value) override {
        registerExpr(index, value ? Z3_mk_true(ctx) : Z3_mk_false(ctx));
    }
    void mkExprConstNumber(const ExprIndex& index, int64_t value) override {
        registerExpr(index, Z3_mk_int(ctx, value, sort_number));
    }
    void mkExprConstUnsigned(const ExprIndex& index, uint64_t value) override {
        registerExpr(index, Z3_mk_int(ctx, value, sort_unsigned));
    }

    void mkExprIdent(const ExprIndex& index, const TypeIndex& type, const std::string& ident) override {
        registerExpr(index, Z3_mk_const(ctx, Z3_mk_string_symbol(ctx, ident.c_str()), types[type]->sort));
    };

    void mkExprADTCtor(const ExprIndex& index, const TypeIndex& adt, const std::string& branch,
            const std::vector<ExprIndex>& args) override {
        auto type = dynamic_cast<const SortRecordZ3*>(types[adt].get());
        auto variant = type->variants.at(branch);

        Z3_ast subs[args.size()];
        for (unsigned i = 0; i < args.size(); i++) {
            subs[i] = exprs[args[i]];
        }
        registerExpr(index, Z3_mk_app(ctx, type->variants.at(branch).ctor, args.size(), subs));
    }
    void mkExprADTTest(const ExprIndex& index, const TypeIndex& adt, const std::string& branch,
            const ExprIndex& sub) override {
        auto type = dynamic_cast<const SortRecordZ3*>(types[adt].get());
        auto variant = type->variants.at(branch);

        Z3_ast subs[1] = {exprs[sub]};
        registerExpr(index, Z3_mk_app(ctx, type->variants.at(branch).test, 1, subs));
    }
    void mkExprADTGetter(const ExprIndex& index, const TypeIndex& adt, const std::string& branch,
            const std::string& field, const ExprIndex& sub) override {
        auto type = dynamic_cast<const SortRecordZ3*>(types[adt].get());
        auto variant = type->variants.at(branch);

        Z3_ast subs[1] = {exprs[sub]};
        registerExpr(index, Z3_mk_app(ctx, type->variants.at(branch).getters.at(field), 1, subs));
    }

    void mkExprAtom(const ExprIndex& index, const RelationIndex& relation,
            const std::vector<ExprIndex>& args) override {
        // collect arguments
        Z3_ast subs[args.size()];
        for (unsigned i = 0; i < args.size(); i++) {
            subs[i] = exprs[args[i]];
        }

        // try applying declarations first
        auto it_decl = rel_decls.find(relation);
        if (it_decl != rel_decls.end()) {
            registerExpr(index, Z3_mk_app(ctx, it_decl->second, args.size(), subs));
            return;
        }

        // try applying definitions
        auto it_def = rel_defs.find(relation);
        assert(it_def != rel_defs.end());
        auto applied = it_def->second->substitute(ctx, args.size(), subs);
        registerExpr(index, applied);
    };
    void mkExprNegation(const ExprIndex& index, const ExprIndex& term) override {
        registerExpr(index, Z3_mk_not(ctx, exprs[term]));
    };

    void mkExprFunctor(const ExprIndex& index, const FunctorOp& op, const ExprIndex& lhs,
            const ExprIndex& rhs) override {
        const auto& lhs_term = exprs.at(lhs);
        const auto& rhs_term = exprs.at(rhs);
        Z3_ast result;
        switch (op) {
                // number
            case FunctorOp::ADD: {
                result = Z3_mk_add(ctx, 2, new Z3_ast[2]{lhs_term, rhs_term});
                break;
            }
            case FunctorOp::SUB: {
                result = Z3_mk_sub(ctx, 2, new Z3_ast[2]{lhs_term, rhs_term});
                break;
            }
            case FunctorOp::MUL: {
                result = Z3_mk_mul(ctx, 2, new Z3_ast[2]{lhs_term, rhs_term});
                break;
            }
            case FunctorOp::DIV: {
                result = Z3_mk_div(ctx, lhs_term, rhs_term);
                break;
            }
            case FunctorOp::MOD: {
                result = Z3_mk_mod(ctx, lhs_term, rhs_term);
                break;
            }
                // unsigned
            case FunctorOp::UADD: {
                result = Z3_mk_bvadd(ctx, lhs_term, rhs_term);
                break;
            }
            case FunctorOp::USUB: {
                result = Z3_mk_bvsub(ctx, lhs_term, rhs_term);
                break;
            }
            case FunctorOp::UMUL: {
                result = Z3_mk_bvmul(ctx, lhs_term, rhs_term);
                break;
            }
            case FunctorOp::UDIV: {
                result = Z3_mk_bvudiv(ctx, lhs_term, rhs_term);
                break;
            }
            case FunctorOp::UMOD: {
                result = Z3_mk_bvurem(ctx, lhs_term, rhs_term);
                break;
            }
                // others
            default: {
                throw std::runtime_error("Operation not supported");
            }
        }
        registerExpr(index, result);
    }
    void mkExprConstraint(const ExprIndex& index, const BinaryConstraintOp& op, const ExprIndex& lhs,
            const ExprIndex& rhs) override {
        const auto& lhs_term = exprs.at(lhs);
        const auto& rhs_term = exprs.at(rhs);
        Z3_ast result;
        switch (op) {
                // equality
            case BinaryConstraintOp::EQ: {
                result = Z3_mk_eq(ctx, lhs_term, rhs_term);
                break;
            }
            case BinaryConstraintOp::NE: {
                result = Z3_mk_not(ctx, Z3_mk_eq(ctx, lhs_term, rhs_term));
                break;
            }

                // number
            case BinaryConstraintOp::LT: {
                result = Z3_mk_lt(ctx, lhs_term, rhs_term);
                break;
            }
            case BinaryConstraintOp::LE: {
                result = Z3_mk_le(ctx, lhs_term, rhs_term);
                break;
            }
            case BinaryConstraintOp::GE: {
                result = Z3_mk_ge(ctx, lhs_term, rhs_term);
                break;
            }
            case BinaryConstraintOp::GT: {
                result = Z3_mk_gt(ctx, lhs_term, rhs_term);
                break;
            }

                // unsigned
            case BinaryConstraintOp::ULT: {
                result = Z3_mk_bvult(ctx, lhs_term, rhs_term);
                break;
            }
            case BinaryConstraintOp::ULE: {
                result = Z3_mk_bvule(ctx, lhs_term, rhs_term);
                break;
            }
            case BinaryConstraintOp::UGE: {
                result = Z3_mk_bvuge(ctx, lhs_term, rhs_term);
                break;
            }
            case BinaryConstraintOp::UGT: {
                result = Z3_mk_bvugt(ctx, lhs_term, rhs_term);
                break;
            }
                // others
            default: {
                throw std::runtime_error("Operation not supported");
            }
        }
        registerExpr(index, result);
    }

    void mkExprPredConjunction(const ExprIndex& index, const std::vector<ExprIndex>& args) override {
        Z3_ast preds[args.size()];
        for (size_t i = 0; i < args.size(); i++) {
            preds[i] = exprs[args[i]];
        }
        registerExpr(index, Z3_mk_and(ctx, args.size(), preds));
    }

    void mkExprPredDisjunction(const ExprIndex& index, const std::vector<ExprIndex>& args) override {
        Z3_ast preds[args.size()];
        for (size_t i = 0; i < args.size(); i++) {
            preds[i] = exprs[args[i]];
        }
        registerExpr(index, Z3_mk_or(ctx, args.size(), preds));
    }

    void mkExprQuantifierExists(const ExprIndex& index,
            const std::vector<std::pair<std::string, TypeIndex>>& vars, const ExprIndex& body) override {
        assert(vars.size() == vars_quant.size());
        size_t vars_num = vars.size();

        // collect vars
        Z3_sort quant_var_sorts[vars_num];
        Z3_symbol quant_var_names[vars_num];
        for (size_t i = 0; i < vars_num; i++) {
            const auto& [var_name, var_type] = vars[i];
            const auto it = vars_quant.find(var_name);
            assert(it != vars_param.end());
            // reverse it
            quant_var_sorts[vars_num - 1 - i] = types[var_type]->sort;
            quant_var_names[vars_num - 1 - i] = Z3_mk_string_symbol(ctx, var_name.c_str());
        }

        // create the expression
        Z3_ast quant_expr =
                Z3_mk_exists(ctx, 0, 0, nullptr, vars_num, quant_var_sorts, quant_var_names, exprs[body]);
        registerExpr(index, quant_expr);
    }

    void mkExprQuantifierForall(const ExprIndex& index,
            const std::vector<std::pair<std::string, TypeIndex>>& vars, const ExprIndex& body) override {
        assert(vars.size() == vars_quant.size());
        size_t vars_num = vars.size();

        // collect vars
        Z3_sort quant_var_sorts[vars_num];
        Z3_symbol quant_var_names[vars_num];
        for (size_t i = 0; i < vars_num; i++) {
            const auto& [var_name, var_type] = vars[i];
            const auto it = vars_quant.find(var_name);
            assert(it != vars_param.end());
            // reverse it
            quant_var_sorts[vars_num - 1 - i] = types[var_type]->sort;
            quant_var_names[vars_num - 1 - i] = Z3_mk_string_symbol(ctx, var_name.c_str());
        }

        // create the expression
        Z3_ast quant_expr =
                Z3_mk_forall(ctx, 0, 0, nullptr, vars_num, quant_var_sorts, quant_var_names, exprs[body]);
        registerExpr(index, quant_expr);
    }
};

class BackendZ3Rec : public BackendZ3 {
protected:
    Z3_solver solver;
    std::vector<Z3_ast> facts;

public:
    BackendZ3Rec() : BackendZ3(mkConfig()) {
        solver = Z3_mk_solver(ctx);
        Z3_solver_inc_ref(ctx, solver);

        // TODO: fine-tune the parameters
        Z3_params params = Z3_mk_params(ctx);
        Z3_params_set_symbol(ctx, params, Z3_mk_string_symbol(ctx, "logic"), Z3_mk_string_symbol(ctx, "ALL"));
        Z3_params_set_bool(ctx, params, Z3_mk_string_symbol(ctx, "induction"), true);
        Z3_params_set_bool(ctx, params, Z3_mk_string_symbol(ctx, "ematching"), false);
        Z3_params_set_bool(ctx, params, Z3_mk_string_symbol(ctx, "mbqi"), true);
        Z3_params_set_uint(ctx, params, Z3_mk_string_symbol(ctx, "threads"), 8);
        Z3_solver_set_params(ctx, solver, params);
    }
    ~BackendZ3Rec() {
        Z3_solver_dec_ref(ctx, solver);
    }

private:
    static inline Z3_config mkConfig() {
        auto cfg = Z3_mk_config();
        return cfg;
    }

public:
    void fact(const ExprIndex& expr) override {
        facts.push_back(exprs[expr]);
    }

    QueryResult query(const RelationIndex& index) override {
        // check that all facts are consistent
        Z3_solver_push(ctx, solver);
        for (const auto& fact : facts) {
            Z3_solver_assert(ctx, solver, fact);
        }
        auto result = Z3_solver_check(ctx, solver);
        assert(result == Z3_L_TRUE);
        Z3_solver_pop(ctx, solver, 1);

        // prove that the facts implies the relation
        auto lhs = Z3_mk_and(ctx, facts.size(), facts.data());
        auto rhs = rel_defs[index]->get_body();
        auto needle = simplify(Z3_mk_not(ctx, Z3_mk_implies(ctx, lhs, rhs)));

        Z3_solver_push(ctx, solver);
        Z3_solver_assert(ctx, solver, needle);
#ifdef SMT_DEBUG
        file_smt << Z3_solver_to_string(ctx, solver) << std::endl;
#endif
        result = Z3_solver_check(ctx, solver);
        Z3_solver_pop(ctx, solver, 1);

        switch (result) {
            case Z3_L_UNDEF: {
                return QueryResult::UNKNOWN;
            }
            case Z3_L_FALSE: {
                return QueryResult::PASS;
            }
            case Z3_L_TRUE: {
                return QueryResult::FAIL;
            }
        }
        assert(false);
    }
};

}  // namespace souffle::smt