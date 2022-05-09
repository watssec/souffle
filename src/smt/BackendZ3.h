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

class ClauseZ3 {
public:
    const Z3_ast term;
    const bool is_quantified;

public:
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
    std::map<TypeIndex, Z3_sort> types;
    std::map<TypeIndex, SortRecordZ3> adts;
    std::map<RelationIndex, Z3_func_decl> relations;

    // per-clause
    std::map<std::string, Z3_ast> vars;
    std::vector<std::pair<Z3_symbol, Z3_sort>> vars_decl;
    std::map<TermIndex, Z3_ast> terms;
    std::unique_ptr<ClauseZ3> clause;

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
        const auto& [_, inserted] = terms.emplace(index, term);
        assert(inserted);
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

    // relations
    void mkRelation(const RelationIndex& index, const std::string& name,
            const std::vector<TypeIndex>& domains) override {
        Z3_sort domain_sorts[domains.size()];
        for (unsigned i = 0; i < domains.size(); i++) {
            domain_sorts[i] = types.at(domains[i]);
        }
        auto fun = Z3_mk_func_decl(ctx, Z3_mk_string_symbol(ctx, name.c_str()), domains.size(), domain_sorts,
                Z3_mk_bool_sort(ctx));
        const auto& [_, inserted] = relations.emplace(index, fun);
        assert(inserted);
    };

    // terms
    void mkTermVarRef(const TermIndex& index, const std::string& name) override {
        registerTerm(index, vars.at(name));
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
        registerTerm(index, Z3_mk_const(ctx, Z3_mk_string_symbol(ctx, ident.c_str()), types.at(type)));
    };

    void mkTermCtor(const TermIndex& index, const TypeIndex& adt, const std::string& branch,
            const std::vector<TermIndex>& args) override {
        const auto& details = adts.at(adt);
        const auto& variant = details.variants.at(branch);

        Z3_ast subs[args.size()];
        for (unsigned i = 0; i < args.size(); i++) {
            subs[i] = terms.at(args[i]);
        }
        registerTerm(index, Z3_mk_app(ctx, variant.ctor, args.size(), subs));
    }

    void mkTermAtom(const TermIndex& index, const RelationIndex& relation,
            const std::vector<TermIndex>& args) override {
        const auto rel = relations.at(relation);
        Z3_ast subs[args.size()];

        for (unsigned i = 0; i < args.size(); i++) {
            subs[i] = terms.at(args[i]);
        }
        registerTerm(index, Z3_mk_app(ctx, rel, args.size(), subs));
    };
    void mkTermNegation(const TermIndex& index, const TermIndex& term) override {
        registerTerm(index, Z3_mk_not(ctx, terms.at(term)));
    };

    void mkTermFunctor(const TermIndex& index, const FunctorOp& op, const TermIndex& lhs,
            const TermIndex& rhs) override {
        const auto& lhs_term = terms.at(lhs);
        const auto& rhs_term = terms.at(rhs);
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
    void mkTermConstraint(const TermIndex& index, const BinaryConstraintOp& op, const TermIndex& lhs,
            const TermIndex& rhs) override {
        const auto& lhs_term = terms.at(lhs);
        const auto& rhs_term = terms.at(rhs);
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
        registerTerm(index, result);
    }

    // clause
    void initClause() override {
        assert(vars.empty());
        assert(vars_decl.empty());
        assert(terms.empty());
        assert(clause == nullptr);
    }

    void finiClause() override {
        vars.clear();
        vars_decl.clear();
        terms.clear();
        clause = nullptr;
    }

    void mkVar(const std::string& name, const TypeIndex& type) override {
        Z3_sort var_sort = types.at(type);
        Z3_symbol var_name = Z3_mk_string_symbol(ctx, name.c_str());

        const auto& [_, inserted] = vars.emplace(name, Z3_mk_bound(ctx, vars_decl.size(), var_sort));
        assert(inserted);

        // update the var decls in reverse order
        vars_decl.emplace(vars_decl.begin(), var_name, var_sort);
    }

    void mkFact(const TermIndex& head) override {
        // a fact is never quantified
        assert(vars_decl.empty());
        clause = std::make_unique<ClauseZ3>(terms.at(head), false);
    }

    void mkRule(const TermIndex& head, const std::vector<TermIndex>& body) override {
        assert(!body.empty());

        // build the implication
        Z3_ast head_atom = terms.at(head);
        Z3_ast items[body.size()];
        for (unsigned i = 0; i < body.size(); i++) {
            items[i] = terms.at(body[i]);
        }
        Z3_ast implication = Z3_mk_implies(ctx, Z3_mk_and(ctx, body.size(), items), head_atom);

        // first check if this is quantified
        if (vars_decl.empty()) {
            clause = std::make_unique<ClauseZ3>(implication, false);
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
        clause = std::make_unique<ClauseZ3>(forall, true);
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

public:
    void mkRelation(const RelationIndex& index, const std::string& name,
            const std::vector<TypeIndex>& domains) override {
        BackendZ3::mkRelation(index, name, domains);
        Z3_fixedpoint_register_relation(ctx, fp, relations.at(index));
    }

    void mkFact(const TermIndex& head) override {
        BackendZ3::mkFact(head);
        assert(!clause->is_quantified);
        Z3_fixedpoint_assert(ctx, fp, clause->term);
    }

    void mkRule(const TermIndex& head, const std::vector<TermIndex>& body) override {
        BackendZ3::mkRule(head, body);
        if (clause->is_quantified) {
            Z3_fixedpoint_add_rule(ctx, fp, clause->term, nullptr);
        } else {
            Z3_fixedpoint_assert(ctx, fp, clause->term);
        }
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

public:
    void mkFact(const TermIndex& head) override {
        BackendZ3::mkFact(head);
        Z3_solver_assert(ctx, solver, clause->term);
    }
    void mkRule(const TermIndex& head, const std::vector<TermIndex>& body) override {
        BackendZ3::mkRule(head, body);
        Z3_solver_assert(ctx, solver, clause->term);
    }
};

}  // namespace souffle::smt