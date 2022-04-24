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
    std::map<ast::QualifiedName, std::tuple<const ast::analysis::Type*, const ast::Type*>> adt_registry;
    typename CTX::SORT_NUMBER type_number;
    typename CTX::SORT_UNSIGNED type_unsigned;
    std::map<ast::QualifiedName, typename CTX::SORT_IDENT> type_idents;
    std::map<ast::QualifiedName, typename CTX::SORT_RECORD> type_records;

    // relations
    std::map<ast::QualifiedName, typename CTX::RELATION> relations;

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
    const typename CTX::SORT_BASE* retrieve_type(const ast::QualifiedName& name) const {
        auto result = retrieve_type_or_null(name);
        assert(result != nullptr);
        return result;
    }

    /// Checked registration of a new ident type
    void register_type_ident(ast::QualifiedName name) {
        assert(retrieve_type_or_null(name) == nullptr);
        auto sort = typename CTX::SORT_IDENT(ctx, name);
        const auto [_, inserted] = type_idents.emplace(name, sort);
        assert(inserted);
    }

    /// Checked registration of a new record type
    void register_type_records(const std::set<const ast::analysis::Type*>& scc) {
        // only three cases possible given an SCC
        // - SCC has a single type that represents a plain record (non-recursive)
        // - SCC has a single type and is a self-inductive ADT
        // - SCC contains multiple types that are mutually recursive ADTs

        // finalize the ordering among the ADTs
        std::map<const ast::analysis::Type*, size_t> indices;
        std::vector<std::tuple<const ast::analysis::Type*, const ast::Type*>> ordered;
        for (auto type : scc) {
            auto it = adt_registry.find(type->getName());
            assert(it != adt_registry.end());

            auto ty_idx = ordered.size();
            ordered.emplace_back(it->second);
            indices[type] = ty_idx;
        }

        // construct the ADT group
        std::vector<ADT<typename CTX::SORT_BASE>> decls;
        for (auto [type_item, ast_item] : ordered) {
            if (auto type_record = dynamic_cast<const ast::analysis::RecordType*>(type_item)) {
                auto ast_record = dynamic_cast<const ast::RecordType*>(ast_item);

                // collect fields
                const auto& type_fields = type_record->getFields();
                const auto& ast_fields = ast_record->getFields();

                std::vector<ADTField<typename CTX::SORT_BASE>> field_decls;
                for (unsigned i = 0; i < type_fields.size(); i++) {
                    const auto* field_type = type_fields[i];
                    auto existing_type = retrieve_type_or_null(field_type->getName());
                    if (existing_type == nullptr) {
                        auto it = indices.find(field_type);
                        assert(it != indices.end());
                        field_decls.emplace_back(ast_fields[i]->getName(), it->second);
                    } else {
                        field_decls.emplace_back(ast_fields[i]->getName(), existing_type);
                    }
                }

                // construct the default branch and the ADT
                ADTBranch<typename CTX::SORT_BASE> default_branch("", move(field_decls));
                ADT<typename CTX::SORT_BASE> adt_decl(type_record->getName().toString(), {default_branch});
                decls.push_back(adt_decl);
            } else if (auto type_adt = dynamic_cast<const ast::analysis::AlgebraicDataType*>(type_item)) {
                auto ast_adt = dynamic_cast<const ast::AlgebraicDataType*>(ast_item);

                // collect branches
                const auto& type_branches = type_adt->getBranches();
                const auto& ast_branches = ast_adt->getBranches();

                std::vector<ADTBranch<typename CTX::SORT_BASE>> branch_decls;
                for (const auto& type_branch : type_branches) {
                    auto iter = std::find_if(
                            ast_branches.cbegin(), ast_branches.cend(), [type_branch](const auto ast_branch) {
                                return ast_branch->getBranchName() == type_branch.name;
                            });
                    const auto& ast_branch = *iter;

                    // collect fields
                    const auto& type_fields = type_branch.types;
                    const auto& ast_fields = ast_branch->getFields();

                    std::vector<ADTField<typename CTX::SORT_BASE>> field_decls;
                    for (unsigned i = 0; i < type_fields.size(); i++) {
                        const auto* field_type = type_fields[i];
                        auto existing_type = retrieve_type_or_null(field_type->getName());
                        if (existing_type == nullptr) {
                            auto it = indices.find(field_type);
                            assert(it != indices.end());
                            field_decls.emplace_back(ast_fields[i]->getName(), it->second);
                        } else {
                            field_decls.emplace_back(ast_fields[i]->getName(), existing_type);
                        }
                    }

                    // construct the branch decl
                    branch_decls.emplace_back(type_branch.name.toString(), move(field_decls));
                }

                // construct the ADT
                ADT<typename CTX::SORT_BASE> adt_decl(type_adt->getName().toString(), move(branch_decls));
                decls.push_back(adt_decl);
            } else {
                assert(false);
            }
        }

        // build the ADT sorts
        auto constructed = CTX::SORT_RECORD::mkCoInductiveSorts(ctx, decls);
        assert(constructed.size() == ordered.size());
        for (unsigned i = 0; i < ordered.size(); i++) {
            const auto& name = std::get<0>(ordered[i])->getName();
            const auto [_, inserted] = type_records.emplace(name, constructed[i]);
            assert(inserted);
        }
    }

private:
    /// Retrieve a relation or nullptr if non-exist
    const typename CTX::RELATION* retrieve_relation_or_null(const ast::QualifiedName& name) const {
        const auto it_ident = relations.find(name);
        if (it_ident != relations.end()) {
            return &it_ident->second;
        }
        return nullptr;
    }

protected:
    /// Retrieve a relation. Panic if nonexistent.
    const typename CTX::RELATION* retrieve_relation(const ast::QualifiedName& name) const {
        auto result = retrieve_relation_or_null(name);
        assert(result != nullptr);
        return result;
    }

    /// Register a relation to the registry
    void register_relation(
            ast::QualifiedName name, const std::vector<const typename CTX::SORT_BASE*>& domain) {
        assert(retrieve_relation_or_null(name) == nullptr);
        auto relation = typename CTX::RELATION(ctx, name.toString(), domain);
        const auto [_, inserted] = relations.emplace(name, relation);
        assert(inserted);
    }

protected:
    /// Check for more restrictive typing rules. Also perform two tasks
    /// - register type idents (uninterpreted types), and
    /// - maintain a registry for ADT types.
    void type_check(const ast::Program& program, const ast::analysis::TypeEnvironment& type_env) {
        for (const auto ast_type : program.getTypes()) {
            const auto& type = type_env.getType(*ast_type);
            assert(type.getName() == ast_type->getQualifiedName());

            // filter out invalid cases
            if (auto type_const = dynamic_cast<const ast::analysis::ConstantType*>(&type)) {
                throw std::runtime_error(
                        "Constant type should never be user-defined: " + type_const->getName().toString());
            }
            if (auto type_primitive = dynamic_cast<const ast::analysis::PrimitiveType*>(&type)) {
                throw std::runtime_error("Primitive type should never be user-defined: " +
                                         type_primitive->getName().toString());
            }
            if (auto type_union = dynamic_cast<const ast::analysis::UnionType*>(&type)) {
                throw std::runtime_error("Union type is not permitted: " + type_union->getName().toString());
            }
            if (auto type_alias = dynamic_cast<const ast::analysis::AliasType*>(&type)) {
                throw std::runtime_error("Type alias is not permitted: " + type_alias->getName().toString());
            }

            // ident type (uninterpreted type)
            if (auto type_subset = dynamic_cast<const ast::analysis::SubsetType*>(&type)) {
                if (type_subset->getBaseType().getName() != "symbol") {
                    throw std::runtime_error("Only the `symbol` type can be subset typed: " +
                                             type_subset->getName().toString());
                }

                // mark a direct subset type of `symbol` an ident type (i.e., uninterpreted type)
                register_type_ident(type_subset->getName());
                continue;
            }

            // algebraic data types
            if (auto type_record = dynamic_cast<const ast::analysis::RecordType*>(&type)) {
                auto ast_record = dynamic_cast<const ast::RecordType*>(ast_type);
                assert(ast_record != nullptr);

                // check information about the fields
                const auto& type_fields = type_record->getFields();
                const auto& ast_fields = ast_record->getFields();
                assert(type_fields.size() == ast_fields.size());

                for (unsigned i = 0; i < type_fields.size(); i++) {
                    const auto* field_type = type_fields[i];
                    assert(field_type->getName() == ast_fields[i]->getTypeName());
                }

                // add its information to registry
                auto r = adt_registry.emplace(
                        type_record->getName(), std::make_tuple(type_record, ast_record));
                assert(r.second);
            } else if (auto type_adt = dynamic_cast<const ast::analysis::AlgebraicDataType*>(&type)) {
                auto ast_adt = dynamic_cast<const ast::AlgebraicDataType*>(ast_type);
                assert(ast_adt != nullptr);

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

                // add its information to registry
                auto r = adt_registry.emplace(type_adt->getName(), std::make_tuple(type_adt, ast_adt));
                assert(r.second);
            } else {
                throw std::runtime_error("Unknown user-defined type: " + type.getName().toString());
            }
        }
    }

    /// The ADTs can be mutually recursive, build a dependency graph to capture their dependencies
    Graph<ast::analysis::Type> build_adt_dep_graph() {
        Graph<ast::analysis::Type> dep_graph;

        // first pass: create nodes in the graph
        for (const auto& [_key, val] : adt_registry) {
            const auto& [type, _ast] = val;
            if (auto type_record = dynamic_cast<const ast::analysis::RecordType*>(type)) {
                dep_graph.addNode(type_record);
            } else if (auto type_adt = dynamic_cast<const ast::analysis::AlgebraicDataType*>(type)) {
                dep_graph.addNode(type_adt);
            } else {
                assert(false);
            }
        }

        // second pass: create edges in the graph
        for (const auto& [_key, val] : adt_registry) {
            const auto& [type, _ast] = val;
            if (auto type_record = dynamic_cast<const ast::analysis::RecordType*>(type)) {
                for (const auto field_type : type_record->getFields()) {
                    // add an edge for the ADT if we haven't registered the type somewhere
                    if (retrieve_type_or_null(field_type->getName()) == nullptr) {
                        auto it = adt_registry.find(field_type->getName());
                        assert(it != adt_registry.end());
                        dep_graph.addEdge(type_record, std::get<0>(it->second));
                    }
                }
            } else if (auto type_adt = dynamic_cast<const ast::analysis::AlgebraicDataType*>(type)) {
                for (const auto& type_branch : type_adt->getBranches()) {
                    for (const auto field_type : type_branch.types) {
                        // add an edge for the ADT if we haven't registered the type somewhere
                        if (retrieve_type_or_null(field_type->getName()) == nullptr) {
                            auto it = adt_registry.find(field_type->getName());
                            assert(it != adt_registry.end());
                            dep_graph.addEdge(type_adt, std::get<0>(it->second));
                        }
                    }
                }
            } else {
                assert(false);
            }
        }

        return dep_graph;
    }

private:
    void clause_check_atom(const ast::Atom* atom, const ast::analysis::TypeAnalysis& typing) {
        assert(retrieve_relation_or_null(atom->getQualifiedName()) != nullptr);
        for (auto arg : atom->getArguments()) {
            auto typeset = typing.getTypes(arg);
            assert(!typeset.empty() && !typeset.isAll());
        }
    }

protected:
    /// Sanity checks for each clause
    void clause_check(const ast::Clause* clause, const ast::analysis::TypeAnalysis& typing) {
        clause_check_atom(clause->getHead(), typing);
    }

public:
    /// Convert the translation unit into an SMT context
    void convert(const ast::TranslationUnit& unit) {
        // collect information
        const auto& program = unit.getProgram();
        const auto& type_env =
                unit.getAnalysis<ast::analysis::TypeEnvironmentAnalysis>().getTypeEnvironment();

        // register types, including the mutually recursive ADTs
        type_check(program, type_env);
        const auto adt_dep_graph = build_adt_dep_graph();
        for (const auto& scc : adt_dep_graph.deriveSCC()) {
            // NOTE: the SCCs are iterated over in a topological order
            assert(!scc.empty());
            register_type_records(scc);
        }

        // register relations
        for (const auto rel : program.getRelations()) {
            if (!rel->getFunctionalDependencies().empty()) {
                // the "choice" features is not supported yet
                throw std::runtime_error(
                        "Functional constraints not supported: " + rel->getQualifiedName().toString());
            }

            // collect domains
            std::vector<const typename CTX::SORT_BASE*> domain;
            for (const auto attr : rel->getAttributes()) {
                auto type = retrieve_type(attr->getTypeName());
                domain.push_back(type);
            }
            register_relation(rel->getQualifiedName(), domain);
        }

        // add rules
        const auto& type_analysis = unit.getAnalysis<ast::analysis::TypeAnalysis>();
        for (const auto rule : program.getClauses()) {
            clause_check(rule, type_analysis);
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