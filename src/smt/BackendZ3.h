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

// forward declarations
class BackendZ3;
class BackendZ3MuZ;
class BackendZ3Rec;

// utilities
namespace {
SMTResult z3_result_to_smt_result(Z3_lbool result) {
    if (result == Z3_L_TRUE) {
        return SMTResult::SAT;
    }
    if (result == Z3_L_FALSE) {
        return SMTResult::UNSAT;
    }
    assert(result == Z3_L_UNDEF);
    return SMTResult::UNKNOWN;
}
}  // namespace

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
        std::vector<Z3_func_decl> getters;
    };

protected:
    std::map<std::string, Variant> variants;

protected:
    SortRecordZ3(Z3_sort sort_, std::map<std::string, Variant> variants_)
            : SortZ3(sort_), variants(std::move(variants_)) {}
};

class RelationZ3 {
    friend BackendZ3;
    friend BackendZ3MuZ;
    friend BackendZ3Rec;

protected:
    Z3_func_decl fun;

protected:
    RelationZ3(Z3_func_decl fun_) : fun(fun_) {}
};

class TermZ3 {
    friend BackendZ3;

protected:
    Z3_ast term;

protected:
    TermZ3(Z3_ast term_) : term(term_) {}
};

class ClauseZ3 {
    friend BackendZ3;
    friend BackendZ3MuZ;
    friend BackendZ3Rec;

protected:
    Z3_ast term;
    bool is_quantified;

protected:
    ClauseZ3(Z3_ast term_, bool is_quantified_) : term(term_), is_quantified(is_quantified_) {}
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
    std::map<TypeIndex, std::unique_ptr<SortZ3>> types;
    std::map<RelationIndex, std::unique_ptr<RelationZ3>> relations;

    // per-clause
    std::map<std::string, Z3_ast> vars;
    std::vector<std::pair<Z3_symbol, Z3_sort>> vars_decl;
    std::map<TermIndex, std::unique_ptr<TermZ3>> terms;
    std::unique_ptr<ClauseZ3> clause_term;

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

private:
    // terms
    void registerTerm(const TermIndex& index, Z3_ast term) {
        const auto& [_, inserted] = terms.emplace(index, new TermZ3(simplify(term)));
        assert(inserted);
    }

protected:
    // simplification
    virtual Z3_ast simplify(Z3_ast term) {
        return Z3_simplify(ctx, term);
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
                variant.getters.assign(fun_getters, fun_getters + fields.size());
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
        const auto& [_, inserted] = relations.emplace(index, new RelationZ3(fun));
        assert(inserted);
    };

    void mkRelDeclRecursive(const RelationIndex& index, const std::string& name,
            const std::vector<std::pair<std::string, TypeIndex>>& params) override {
        Z3_sort domain_sorts[params.size()];
        for (unsigned i = 0; i < params.size(); i++) {
            domain_sorts[i] = types[params[i].second]->sort;
        }
        auto fun = Z3_mk_rec_func_decl(ctx, Z3_mk_string_symbol(ctx, name.c_str()), params.size(),
                domain_sorts, Z3_mk_bool_sort(ctx));
        const auto& [_, inserted] = relations.emplace(index, new RelationZ3(fun));
        assert(inserted);
    };

    void mkRelation(const RelationIndex& index, const std::string& name,
            const std::vector<std::pair<std::string, TypeIndex>>& params) override {
        Z3_sort domain_sorts[params.size()];
        for (unsigned i = 0; i < params.size(); i++) {
            domain_sorts[i] = types[params[i].second]->sort;
        }
        auto fun = Z3_mk_func_decl(ctx, Z3_mk_string_symbol(ctx, name.c_str()), params.size(), domain_sorts,
                Z3_mk_bool_sort(ctx));
        const auto& [_, inserted] = relations.emplace(index, new RelationZ3(fun));
        assert(inserted);
    };

    // vars
    void mkVar(const std::string& name, const TypeIndex& type) override {
        Z3_sort var_sort = types[type]->sort;
        Z3_symbol var_name = Z3_mk_string_symbol(ctx, name.c_str());

        const auto& [_, inserted] = vars.emplace(name, Z3_mk_bound(ctx, vars_decl.size(), var_sort));
        assert(inserted);

        // update the var decls in reverse order
        vars_decl.emplace(vars_decl.begin(), var_name, var_sort);
    }

    // terms
    void mkTermVarRef(const TermIndex& index, const std::string& name) override {
        registerTerm(index, vars[name]);
    }
    void mkTermConstBool(const TermIndex& index, bool value) override {
        registerTerm(index, value ? Z3_mk_true(ctx) : Z3_mk_false(ctx));
    }
    void mkTermConstNumber(const TermIndex& index, int64_t value) override {
        registerTerm(index, Z3_mk_int(ctx, value, sort_number));
    }
    void mkTermConstUnsigned(const TermIndex& index, uint64_t value) override {
        registerTerm(index, Z3_mk_int(ctx, value, sort_unsigned));
    }
    void mkTermIdent(const TermIndex& index, const TypeIndex& type, const std::string& ident) override {
        registerTerm(index, Z3_mk_const(ctx, Z3_mk_string_symbol(ctx, ident.c_str()), types[type]->sort));
    };
    void mkTermFunctor(const TermIndex& index, const FunctorOp& op, const TermIndex& lhs,
            const TermIndex& rhs) override {
        const auto& lhs_term = terms.at(lhs)->term;
        const auto& rhs_term = terms.at(rhs)->term;
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
        registerTerm(index, result);
    }
    void mkTermCtor(const TermIndex& index, const TypeIndex& adt, const std::string& branch,
            const std::vector<TermIndex>& args) override {
        auto type = dynamic_cast<const SortRecordZ3*>(types[adt].get());
        auto variant = type->variants.at(branch);

        Z3_ast subs[args.size()];
        for (unsigned i = 0; i < args.size(); i++) {
            subs[i] = terms[args[i]]->term;
        }
        registerTerm(index, Z3_mk_app(ctx, type->variants.at(branch).ctor, args.size(), subs));
    }
    void mkTermAtom(const TermIndex& index, const RelationIndex& relation,
            const std::vector<TermIndex>& args) override {
        const auto rel = relations[relation].get();
        Z3_ast subs[args.size()];

        for (unsigned i = 0; i < args.size(); i++) {
            subs[i] = terms[args[i]]->term;
        }
        registerTerm(index, Z3_mk_app(ctx, rel->fun, args.size(), subs));
    };
    void mkTermNegation(const TermIndex& index, const TermIndex& term) override {
        registerTerm(index, Z3_mk_not(ctx, terms[term]->term));
    };
    void mkTermConstraint(const TermIndex& index, const BinaryConstraintOp& op, const TermIndex& lhs,
            const TermIndex& rhs) override {
        Z3_ast result;
        switch (op) {
                // equality
            case BinaryConstraintOp::EQ: {
                result = Z3_mk_eq(ctx, terms[lhs]->term, terms[rhs]->term);
                break;
            }
            case BinaryConstraintOp::NE: {
                result = Z3_mk_not(ctx, Z3_mk_eq(ctx, terms[lhs]->term, terms[rhs]->term));
                break;
            }

                // number
            case BinaryConstraintOp::LT: {
                result = Z3_mk_lt(ctx, terms[lhs]->term, terms[rhs]->term);
                break;
            }
            case BinaryConstraintOp::LE: {
                result = Z3_mk_le(ctx, terms[lhs]->term, terms[rhs]->term);
                break;
            }
            case BinaryConstraintOp::GE: {
                result = Z3_mk_ge(ctx, terms[lhs]->term, terms[rhs]->term);
                break;
            }
            case BinaryConstraintOp::GT: {
                result = Z3_mk_gt(ctx, terms[lhs]->term, terms[rhs]->term);
                break;
            }

                // unsigned
            case BinaryConstraintOp::ULT: {
                result = Z3_mk_bvult(ctx, terms[lhs]->term, terms[rhs]->term);
                break;
            }
            case BinaryConstraintOp::ULE: {
                result = Z3_mk_bvule(ctx, terms[lhs]->term, terms[rhs]->term);
                break;
            }
            case BinaryConstraintOp::UGE: {
                result = Z3_mk_bvuge(ctx, terms[lhs]->term, terms[rhs]->term);
                break;
            }
            case BinaryConstraintOp::UGT: {
                result = Z3_mk_bvugt(ctx, terms[lhs]->term, terms[rhs]->term);
                break;
            }
                // others
            default: {
                throw std::runtime_error("Operation not supported");
            }
        }
        registerTerm(index, result);
    }

    // clause
    void initClause() override {
        assert(vars.empty());
        assert(vars_decl.empty());
        assert(terms.empty());
        assert(clause_term == nullptr);
    }
    void mkFact(const TermIndex& head) override {
        Z3_ast head_atom = terms[head]->term;

        // first check if this is quantified
        if (vars_decl.empty()) {
            clause_term.reset(new ClauseZ3(head_atom, false));
            return;
        }

        // handle quantified case
        size_t vars_num = vars_decl.size();
        Z3_sort forall_var_sorts[vars_num];
        Z3_symbol forall_var_names[vars_num];
        for (unsigned i = 0; i < vars_decl.size(); i++) {
            forall_var_names[i] = vars_decl[i].first;
            forall_var_sorts[i] = vars_decl[i].second;
        }
        Z3_ast forall =
                Z3_mk_forall(ctx, 0, 0, nullptr, vars_num, forall_var_sorts, forall_var_names, head_atom);
        clause_term.reset(new ClauseZ3(forall, true));
    }
    void mkRule(const TermIndex& head, const std::vector<TermIndex>& body) override {
        assert(!body.empty());

        Z3_ast head_atom = terms[head]->term;
        Z3_ast items[body.size()];
        for (unsigned i = 0; i < body.size(); i++) {
            items[i] = terms[body[i]]->term;
        }
        Z3_ast implication = Z3_mk_implies(ctx, Z3_mk_and(ctx, body.size(), items), head_atom);

        // first check if this is quantified
        if (vars_decl.empty()) {
            clause_term.reset(new ClauseZ3(implication, false));
            return;
        }

        // handle quantified case
        size_t vars_num = vars_decl.size();
        Z3_sort forall_var_sorts[vars_num];
        Z3_symbol forall_var_names[vars_num];
        for (unsigned i = 0; i < vars_decl.size(); i++) {
            forall_var_names[i] = vars_decl[i].first;
            forall_var_sorts[i] = vars_decl[i].second;
        }
        Z3_ast forall =
                Z3_mk_forall(ctx, 0, 0, nullptr, vars_num, forall_var_sorts, forall_var_names, implication);
        clause_term.reset(new ClauseZ3(forall, true));
    }
    void finiClause() override {
        vars.clear();
        vars_decl.clear();
        terms.clear();
        clause_term.reset();
    }
};

class BackendZ3Rec : public BackendZ3 {
protected:
    Z3_solver solver;

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
    void mkFact(const TermIndex& head) override {
        BackendZ3::mkFact(head);
        Z3_solver_assert(ctx, solver, clause_term->term);
    }
    void mkRule(const TermIndex& head, const std::vector<TermIndex>& body) override {
        BackendZ3::mkRule(head, body);
        Z3_solver_assert(ctx, solver, clause_term->term);
    }

    SMTResult query(const RelationIndex& index) override {
        auto needle = Z3_mk_not(ctx, Z3_mk_app(ctx, relations[index]->fun, 0, nullptr));
        Z3_solver_push(ctx, solver);
        Z3_solver_assert(ctx, solver, needle);
        auto result = Z3_solver_check(ctx, solver);
        Z3_solver_pop(ctx, solver, 1);
        return z3_result_to_smt_result(result);
    }
};

}  // namespace souffle::smt