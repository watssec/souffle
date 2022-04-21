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
 * - type alias is strictly prohibited
 * In general, these additional typing rules kills any possibility of creating a hierarchy of types.
 *
 * 3) `Record` types cannot use the `nil` constructor.
 *    i.e., a `Record` type is essentially an `ADT` with a default constructor.
 *
 * 4) `ADT` types (including `Record` types) can be mutually recursive.
 *
 ***********************************************************************/

#pragma once

#include <algorithm>
#include <map>
#include <stdexcept>
#include <tuple>
#include <variant>
#include <vector>

#include "smt/AdapterCVC.h"
#include "smt/AdapterZ3.h"

#include "ast/Program.h"
#include "ast/QualifiedName.h"
#include "ast/TranslationUnit.h"
#include "ast/analysis/typesystem/Type.h"
#include "ast/analysis/typesystem/TypeEnvironment.h"

namespace souffle::smt {

/**
 * A base class for AST to SMT conversion
 */
template <typename CTX>
class Translator {
private:
    using SORT_DEFINED = std::variant<typename CTX::SORT_UNINTERPRETED, typename CTX::SORT_RECORD>;

protected:
    // context
    CTX ctx;

    // types
    typename CTX::SORT_NUMBER type_number;
    typename CTX::SORT_UNSIGNED type_unsigned;
    std::map<ast::QualifiedName, SORT_DEFINED> type_defined;

protected:
    Translator() : ctx(), type_number(ctx), type_unsigned(ctx) {}

protected:
    /// Checked registration of a new type
    template <typename T>
    void register_type(ast::QualifiedName name, T type) {
        if (!type_defined.emplace(name, type).second) {
            throw std::runtime_error("Duplicated registration of type: " + name.toString());
        }
    }

    /// Retrieve a type, primitive or user-defined
    const typename CTX::SORT_BASE& retrieve_type(const ast::QualifiedName& name) const {
        if (name == "number") {
            return type_number;
        }
        if (name == "unsigned") {
            return type_unsigned;
        }
        const auto it = type_defined.find(name);
        if (it == type_defined.end()) {
            throw std::runtime_error("Unknown type: " + name.toString());
        }
        return std::visit(
                [](auto&& arg) -> const typename CTX::SORT_BASE& {
                    return static_cast<const typename CTX::SORT_BASE&>(arg);
                },
                it->second);
    }

public:
    /// Convert the translation unit into an SMT context
    void convert(const ast::TranslationUnit& unit) {
        // collect information
        const auto& program = unit.getProgram();
        const auto& type_env =
                unit.getAnalysis<ast::analysis::TypeEnvironmentAnalysis>().getTypeEnvironment();
        // TODO: use it
        // const auto& type_analysis = unit.getAnalysis<ast::analysis::TypeAnalysis>();

        // register user-defined types
        for (const auto ast_type : program.getTypes()) {
            auto type = &type_env.getType(*ast_type);

            // filter out invalid cases
            if (auto type_const = dynamic_cast<const ast::analysis::ConstantType*>(type)) {
                throw std::runtime_error(
                        "Constant type should never be user-defined: " + type_const->getName().toString());
            }
            if (auto type_primitive = dynamic_cast<const ast::analysis::PrimitiveType*>(type)) {
                throw std::runtime_error("Primitive type should never be user-defined: " +
                                         type_primitive->getName().toString());
            }
            if (auto type_union = dynamic_cast<const ast::analysis::UnionType*>(type)) {
                throw std::runtime_error("Union type is not permitted: " + type_union->getName().toString());
            }
            if (auto type_alias = dynamic_cast<const ast::analysis::AliasType*>(type)) {
                throw std::runtime_error("Type alias is not permitted: " + type_alias->getName().toString());
            }

            // uninterpreted type
            if (auto type_subset = dynamic_cast<const ast::analysis::SubsetType*>(type)) {
                if (type_subset->getBaseType().getName() != "symbol") {
                    throw std::runtime_error("Only the `symbol` type can be subset typed: " +
                                             type_subset->getName().toString());
                }
                // mark a direct subset type of `symbol` uninterpreted type
                register_type(type_subset->getName(),
                        typename CTX::SORT_UNINTERPRETED(ctx, type_subset->getName()));
                continue;
            }

            // algebraic data types
            if (auto type_record = dynamic_cast<const ast::analysis::RecordType*>(type)) {
                auto ast_record = dynamic_cast<const ast::RecordType*>(ast_type);
                assert(ast_record != nullptr);

                const auto& type_fields = type_record->getFields();
                const auto& ast_fields = ast_record->getFields();
                assert(type_fields.size() == ast_fields.size());

                // collect information about the field
                std::vector<std::tuple<const ast::QualifiedName&, const typename CTX::SORT_BASE&>> fields;
                for (unsigned i = 0; i < type_fields.size(); i++) {
                    assert(type_fields[i]->getName() == ast_fields[i]->getTypeName());
                    // const auto& field_name = ast_fields[i]->getName();
                    // const auto& field_type = retrieve_type(type_fields[i]->getName());
                    // fields.push_back(std::make_tuple(field_name, field_type));
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
            } else {
                throw std::runtime_error("Unknown user-defined type: " + type->getName().toString());
            }
        }
    }
};

/**
 * A concrete AST to SMT translator based on Z3 MuZ facility
 */
class TranslatorZ3MuZ : public Translator<ContextZ3MuZ> {
public:
    TranslatorZ3MuZ() = default;
};

/**
 * A concrete AST to SMT translator based on Z3 recursive function
 */
class TranslatorZ3Rec : public Translator<ContextZ3Rec> {
public:
    TranslatorZ3Rec() = default;
};

/**
 * A concrete AST to SMT translator based on CVC recursive function
 */
class TranslatorCVCRec : public Translator<ContextCVC5Rec> {
public:
    TranslatorCVCRec() = default;
};

}  // namespace souffle::smt