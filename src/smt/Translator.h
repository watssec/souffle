/************************************************************************
 *
 * @file Translator.h
 *
 * Everything about the SMT solver.
 *
 * [More restrictive typing]
 *
 * 1) Only two primitive types are modeled: `number` and `unsigned`.
 *
 * 2) The SMT representation adheres a more restrictive typing rule than Souffle
 * - any type that is a *direct* subset type of `symbol` is an uninterpreted sort
 * - subtyping in any other form is prohibited, i.e., no subtyping of any type other than `symbol`
 * - union type is strictly prohibited
 * In general, these additional typing rules kills any possibility of creating a hierarchy of types.
 *
 * 3) `Record` types cannot be recursive or mutually recursive.
 *    This also invalidates the use of the `nil` constructor to create a record.
 *
 ***********************************************************************/

#pragma once

#include <algorithm>
#include <map>
#include <stdexcept>
#include <tuple>
#include <vector>

#include <cvc5/cvc5.h>
#include <z3.h>

#include "ast/Program.h"
#include "ast/QualifiedName.h"
#include "ast/TranslationUnit.h"
#include "ast/analysis/typesystem/Type.h"
#include "ast/analysis/typesystem/TypeEnvironment.h"

namespace souffle::smt {

/**
 * A base class for AST to SMT conversion
 */
template <typename SORT>
class Translator {
protected:
    std::map<ast::QualifiedName, SORT> types;

protected:
    Translator() = default;

protected:
    /// Create primitive type: number
    virtual SORT create_type_number() = 0;

    /// Create primitive type: unsigned
    virtual SORT create_type_unsigned() = 0;

    /// Create an uninterpreted type
    virtual SORT create_uninterpreted_type(const ast::QualifiedName& name) = 0;

    /// Create a struct type
    virtual SORT create_struct_type(const ast::QualifiedName& name,
            const std::vector<std::tuple<const ast::QualifiedName&, const SORT&>> fields) = 0;

protected:
    /// Checked registration of a new type
    void register_new_type(ast::QualifiedName name, SORT type) {
        if (!types.emplace(name, type).second) {
            throw std::runtime_error("Duplication registration of type (new): " + name.toString());
        }
    }

    /// Create a type alias
    void create_alias_type(ast::QualifiedName name, const ast::QualifiedName& alias) {
        const auto it = types.find(alias);
        if (it == types.end()) {
            throw std::runtime_error("Alias type not registered: " + alias.toString());
        }
        if (!types.emplace(name, it->second).second) {
            throw std::runtime_error("Duplication registration of type (alias): " + name.toString());
        }
    }

public:
    /// Convert the translation unit into an SMT context
    void convert(const ast::TranslationUnit& unit) {
        // prepare primitive types
        register_new_type(ast::QualifiedName("number"), create_type_number());
        register_new_type(ast::QualifiedName("unsigned"), create_type_unsigned());

        // collect information
        const auto& program = unit.getProgram();
        const auto& type_env =
                unit.getAnalysis<ast::analysis::TypeEnvironmentAnalysis>().getTypeEnvironment();
        // TODO: use it
        // const auto& type_analysis = unit.getAnalysis<ast::analysis::TypeAnalysis>();

        // register user-defined types
        for (const auto ast_type : program.getTypes()) {
            auto type = &type_env.getType(*ast_type);
            if (auto type_const = dynamic_cast<const ast::analysis::ConstantType*>(type)) {
                throw std::runtime_error(
                        "Constant type should never be user-defined: " + type_const->getName().toString());
            } else if (auto type_primitive = dynamic_cast<const ast::analysis::PrimitiveType*>(type)) {
                throw std::runtime_error("Primitive type should never be user-defined: " +
                                         type_primitive->getName().toString());
            } else if (auto type_union = dynamic_cast<const ast::analysis::UnionType*>(type)) {
                throw std::runtime_error("Union type is not permitted: " + type_union->getName().toString());
            } else if (auto type_subset = dynamic_cast<const ast::analysis::SubsetType*>(type)) {
                if (type_subset->getBaseType().getName() != "symbol") {
                    throw std::runtime_error("Only the `symbol` type can be subset typed: " +
                                             type_subset->getName().toString());
                }
                create_uninterpreted_type(type_subset->getName());
            } else if (auto type_alias = dynamic_cast<const ast::analysis::AliasType*>(type)) {
                create_alias_type(type_alias->getName(), type_alias->getAliasType().getName());
            } else if (auto type_record = dynamic_cast<const ast::analysis::RecordType*>(type)) {
                auto ast_record = dynamic_cast<const ast::RecordType*>(ast_type);
                assert(ast_record != nullptr);

                const auto& type_fields = type_record->getFields();
                const auto& ast_fields = ast_record->getFields();
                assert(type_fields.size() == ast_fields.size());
                for (unsigned i = 0; i < type_fields.size(); i++) {
                    assert(type_fields[i]->getName() == ast_fields[i]->getTypeName());
                }
                // TODO: implement
            } else if (auto type_adt = dynamic_cast<const ast::analysis::AlgebraicDataType*>(type)) {
                auto ast_adt = dynamic_cast<const ast::AlgebraicDataType*>(ast_type);
                assert(ast_adt != nullptr);

                const auto& type_branches = type_adt->getBranches();
                const auto& ast_branches = ast_adt->getBranches();
                assert(type_branches.size() == ast_branches.size());

                for (const auto& type_branch : type_branches) {
                    auto iter = std::find_if(
                            ast_branches.cbegin(), ast_branches.cend(), [type_branch](const auto ast_branch) {
                                return ast_branch->getBranchName() == type_branch.name;
                            });
                    assert(iter != ast_branches.cend());
                    const auto& ast_branch = *iter;

                    const auto& type_fields = type_branch.types;
                    const auto& ast_fields = ast_branch->getFields();
                    assert(type_fields.size() == ast_fields.size());
                    for (unsigned i = 0; i < type_fields.size(); i++) {
                        assert(type_fields[i]->getName() == ast_fields[i]->getTypeName());
                    }
                }
                // TODO: implement
            }
        }
    }
};

/**
 * An abstract AST to SMT translator based on Z3
 */
class TranslatorZ3 : public Translator<Z3_sort> {
protected:
    Z3_context ctx;

protected:
    explicit TranslatorZ3(Z3_config cfg) {
        ctx = Z3_mk_context(cfg);
        Z3_del_config(cfg);
    }

    ~TranslatorZ3() {
        Z3_del_context(ctx);
        ctx = nullptr;
    }

protected:
    Z3_sort create_type_number() override {
        return Z3_mk_int_sort(ctx);
    }

    Z3_sort create_type_unsigned() override {
        return Z3_mk_bv_sort(ctx, RAM_DOMAIN_SIZE);
    }

    Z3_sort create_uninterpreted_type(const ast::QualifiedName& name) override {
        return Z3_mk_uninterpreted_sort(ctx, Z3_mk_string_symbol(ctx, name.toString().c_str()));
    }
};

// implementation details, hidden in an unnamed namespace
namespace {
inline Z3_config config_for_z3_muz() {
    auto cfg = Z3_mk_config();
    return cfg;
}

inline Z3_config config_for_z3_rec() {
    auto cfg = Z3_mk_config();
    return cfg;
}
}  // namespace

/**
 * A concrete AST to SMT translator based on Z3 MuZ facility
 */
class TranslatorZ3MuZ : public TranslatorZ3 {
public:
    TranslatorZ3MuZ() : TranslatorZ3(config_for_z3_muz()) {}
};

/**
 * A concrete AST to SMT translator based on Z3 recursive function
 */
class TranslatorZ3Rec : public TranslatorZ3 {
public:
    TranslatorZ3Rec() : TranslatorZ3(config_for_z3_rec()) {}
};

/**
 * An abstract AST to SMT translator based on CVC
 */
class TranslatorCVC : public Translator<cvc5::Sort> {
protected:
    cvc5::Solver solver;

protected:
    TranslatorCVC() = default;

protected:
    cvc5::Sort create_type_number() override {
        return solver.getIntegerSort();
    }

    cvc5::Sort create_type_unsigned() override {
        return solver.mkBitVectorSort(RAM_DOMAIN_SIZE);
    }

    cvc5::Sort create_uninterpreted_type(const ast::QualifiedName& name) override {
        return solver.mkUninterpretedSort(name.toString());
    }
};

/**
 * A concrete AST to SMT translator based on CVC recursive function
 */
class TranslatorCVCRec : public TranslatorCVC {
public:
    TranslatorCVCRec() = default;
};

}  // namespace souffle::smt