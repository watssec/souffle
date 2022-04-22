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
#include <type_traits>
#include <variant>
#include <vector>

#include "smt/AdapterCVC.h"
#include "smt/AdapterZ3.h"
#include "smt/Utils.h"

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
protected:
    // context
    CTX ctx;

    // types
    typename CTX::SORT_NUMBER type_number;
    typename CTX::SORT_UNSIGNED type_unsigned;
    std::map<ast::QualifiedName, typename CTX::SORT_IDENT> type_idents;
    std::map<ast::QualifiedName, typename CTX::SORT_RECORD> type_records;

protected:
    Translator() : ctx(), type_number(ctx), type_unsigned(ctx) {}

private:
    /// Retrieve a type, primitive or user-defined, or nullptr if non-exist
    const typename CTX::SORT_BASE* retrieve_type_or_null(const ast::QualifiedName& name) const {
        // primitives
        if (name == "number") {
            return &type_number;
        }
        if (name == "unsigned") {
            return &type_unsigned;
        }

        // user-defined
        const auto it_ident = type_idents.find(name);
        if (it_ident != type_idents.end()) {
            return &it_ident->second;
        }
        const auto it_record = type_records.find(name);
        if (it_record != type_records.end()) {
            return &it_record->second;
        }

        // unable to find
        return nullptr;
    }

protected:
    /// Retrieve a type, primitive or user-defined. Panic if nonexistent.
    const typename CTX::SORT_BASE& retrieve_type(const ast::QualifiedName& name) const {
        return *retrieve_type_or_null(name);
    }

    /// Checked registration of a new ident type
    void register_type_ident(ast::QualifiedName name) {
        assert(retrieve_type_or_null(name) == nullptr);
        auto sort = typename CTX::SORT_IDENT(ctx, name);
        const auto [_, inserted] = type_idents.emplace(name, sort);
        assert(inserted);
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

        // register user-defined ident types while also put ADT types into type_graph
        Graph<ast::analysis::Type> type_graph;
        for (const auto ast_type : program.getTypes()) {
            auto type = &type_env.getType(*ast_type);
            assert(type != nullptr);
            assert(type->getName() == ast_type->getQualifiedName());

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

            // ident type (uninterpreted type)
            if (auto type_subset = dynamic_cast<const ast::analysis::SubsetType*>(type)) {
                if (type_subset->getBaseType().getName() != "symbol") {
                    throw std::runtime_error("Only the `symbol` type can be subset typed: " +
                                             type_subset->getName().toString());
                }
                // mark a direct subset type of `symbol` an ident type (i.e., uninterpreted type)
                register_type_ident(type_subset->getName());
                continue;
            }

            // algebraic data types
            if (auto type_record = dynamic_cast<const ast::analysis::RecordType*>(type)) {
                auto ast_record = dynamic_cast<const ast::RecordType*>(ast_type);
                assert(ast_record != nullptr);
                type_graph.addNode(type_record);

                // check information about the fields
                const auto& type_fields = type_record->getFields();
                const auto& ast_fields = ast_record->getFields();
                assert(type_fields.size() == ast_fields.size());

                for (unsigned i = 0; i < type_fields.size(); i++) {
                    const auto* field_type = type_fields[i];
                    assert(field_type->getName() == ast_fields[i]->getTypeName());
                }
            } else if (auto type_adt = dynamic_cast<const ast::analysis::AlgebraicDataType*>(type)) {
                auto ast_adt = dynamic_cast<const ast::AlgebraicDataType*>(ast_type);
                assert(ast_adt != nullptr);
                type_graph.addNode(type_adt);

                // check information about the branches
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

                    // check information about the fields
                    const auto& type_fields = type_branch.types;
                    const auto& ast_fields = ast_branch->getFields();
                    assert(type_fields.size() == ast_fields.size());

                    for (unsigned i = 0; i < type_fields.size(); i++) {
                        const auto* field_type = type_fields[i];
                        assert(field_type->getName() == ast_fields[i]->getTypeName());
                    }
                }
            } else {
                throw std::runtime_error("Unknown user-defined type: " + type->getName().toString());
            }
        }

        // add edges for the ADTs
        for (const auto node : type_graph.allNodes()) {
            if (auto type_record = dynamic_cast<const ast::analysis::RecordType*>(node)) {
                for (const auto field_type : type_record->getFields()) {
                    // add an edge for the ADT if we haven't registered the type somewhere
                    if (retrieve_type_or_null(field_type->getName()) == nullptr) {
                        type_graph.addEdge(type_record, field_type);
                    }
                }
            } else if (auto type_adt = dynamic_cast<const ast::analysis::AlgebraicDataType*>(node)) {
                for (const auto& type_branch : type_adt->getBranches()) {
                    for (const auto field_type : type_branch.types) {
                        // add an edge for the ADT if we haven't registered the type somewhere
                        if (retrieve_type_or_null(field_type->getName()) == nullptr) {
                            type_graph.addEdge(type_adt, field_type);
                        }
                    }
                }
            } else {
                assert(false);
            }
        }

        // derive the SCCs and iterate over them in topological order
        for (const auto& scc : type_graph.deriveSCC()) {
            // case 1 - SCC has a single element
            if (scc.size() == 1) {
                auto scc_type = *scc.begin();

                // case 1.1: a plain record
                if (type_graph.getEdgesByNode(scc_type).empty()) {
                }

                // case 1.2: a recursive ADT
                else {
                }
            }

            // case 2 - SCC represents a set of mutually recursive ADTs
            else {
                assert(!scc.empty());
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