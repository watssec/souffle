/************************************************************************
 *
 * @file Typing.h
 *
 * Transform the Souffle typing system into SMT-friendly typing
 *
 * 1) Only two primitive types are modeled: `number` and `unsigned`.
 *
 * 2) The SMT representation adheres to more restrictive typing rules than Souffle
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

#include <map>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "smt/Utils.h"

namespace souffle::smt {

// constants
constexpr std::string_view BUILTIN_TYPE_NUMBER = "number";
constexpr std::string_view BUILTIN_TYPE_UNSIGNED = "unsigned";
constexpr std::string_view BUILTIN_TYPE_SYMBOL = "symbol";

// forward declarations
class TypeRegistry;
class Frontend;

/**
 * An index that uniquely identifies a type
 */
class TypeIndex : public Index {
    friend TypeRegistry;

protected:
    explicit TypeIndex(size_t index_) : Index(index_) {}
};

/**
 * Declaration of one field in one variant of an ADT
 */
struct ADTField {
public:
    const std::string name;
    const std::variant<TypeIndex, size_t> type;

public:
    ADTField(std::string _name, TypeIndex _type) : name(std::move(_name)), type(_type) {}
    ADTField(std::string _name, size_t _type) : name(std::move(_name)), type(_type) {}

public:
    ADTField fix_sort_index(const std::vector<TypeIndex>& fixup) const {
        if (std::holds_alternative<size_t>(type)) {
            return ADTField(name, fixup[std::get<size_t>(type)]);
        } else {
            return ADTField(name, std::get<TypeIndex>(type));
        }
    }
};

/**
 * Declaration of one variant of an ADT
 */
struct ADTBranch {
public:
    const std::string name;
    const std::vector<ADTField> fields;

public:
    ADTBranch(std::string _name, std::vector<ADTField> _fields)
            : name(std::move(_name)), fields(std::move(_fields)) {}

public:
    ADTBranch fix_sort_index(const std::vector<TypeIndex>& fixup) const {
        std::vector<ADTField> fields_fixed;
        for (const auto& field : fields) {
            fields_fixed.push_back(field.fix_sort_index(fixup));
        }
        return ADTBranch(name, fields_fixed);
    }
};

/**
 * Declaration of one ADT
 */
struct ADT {
public:
    const std::string name;
    const std::vector<ADTBranch> branches;

public:
    ADT(std::string _name, std::vector<ADTBranch> _branches)
            : name(std::move(_name)), branches(std::move(_branches)) {}

public:
    ADT fix_sort_index(const std::vector<TypeIndex>& fixup) const {
        std::vector<ADTBranch> branches_fixed;
        for (const auto& branch : branches) {
            branches_fixed.push_back(branch.fix_sort_index(fixup));
        }
        return ADT(name, branches_fixed);
    }
};

/**
 * A group of mutually recursive ADTs
 */
struct ADTGroup {
public:
    const std::vector<std::pair<TypeIndex, ADT>> adts;

public:
    explicit ADTGroup(std::vector<std::pair<TypeIndex, ADT>> _adts) : adts(std::move(_adts)) {}
};

/**
 * A registry of types appeared
 */
class TypeRegistry {
    friend Frontend;

private:
    // counter
    size_t counter = 3;

    // records
    std::map<std::string, std::tuple<const ast::analysis::Type*, const ast::Type*>> adt_registry;

protected:
    // built-int types
    TypeIndex type_number{1};
    TypeIndex type_unsigned{2};

    // user-defined types
    std::map<std::string, TypeIndex> type_idents{};
    std::map<std::string, std::pair<TypeIndex, ADT>> type_records{};

    // adt registration sequence
    std::vector<ADTGroup> adt_sequence{};

public:
    explicit TypeRegistry(const ast::TranslationUnit& unit) {
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
    }

public:
    /// Retrieve a type by its name, primitive or user-defined. Panic if nonexistent.
    TypeIndex retrieve_type(const std::string& name) const {
        auto result = retrieve_type_or_invalid(name);
        assert(result.has_value());
        return *result;
    }

    /// Convert a typeset into a vector of type indices
    std::vector<TypeIndex> typeset_to_indices(const ast::analysis::TypeSet& typeset) const {
        std::vector<TypeIndex> indices;
        for (const auto& ty : typeset) {
            indices.push_back(retrieve_type(ty.getName().toString()));
        }
        return indices;
    }

    /// Retrieve the details of an ident type
    const std::string& retrieve_ident(const TypeIndex& index) const {
        for (const auto& [key, val] : type_idents) {
            if (val == index) {
                return key;
            }
        }
        assert(false);
    }

    /// Retrieve the details of an ADT type
    const ADT& retrieve_adt(const TypeIndex& index) const {
        for (const auto& [key, val] : type_records) {
            if (val.first == index) {
                return val.second;
            }
        }
        assert(false);
    }

private:
    /// Create a new index
    TypeIndex new_index() {
        return TypeIndex(counter++);
    }

private:
    /// Retrieve a type by its name, primitive or user-defined. Returns invalid if non-exist
    std::optional<TypeIndex> retrieve_type_or_invalid(const std::string& name) const {
        // primitives
        if (name == BUILTIN_TYPE_NUMBER) {
            return type_number;
        }
        if (name == BUILTIN_TYPE_UNSIGNED) {
            return type_unsigned;
        }

        // user-defined
        const auto it_ident = type_idents.find(name);
        if (it_ident != type_idents.end()) {
            return it_ident->second;
        }
        const auto it_record = type_records.find(name);
        if (it_record != type_records.end()) {
            return it_record->second.first;
        }

        // unable to find
        return std::nullopt;
    }

    /// Checked registration of a new ident type
    void register_type_ident(std::string name) {
        assert(!retrieve_type_or_invalid(name).has_value());
        const auto [_, inserted] = type_idents.emplace(std::move(name), new_index());
        assert(inserted);
    }

    /// Checked registration of a set of mutually recursive record types
    void register_type_records(const std::set<const ast::analysis::Type*>& scc) {
        // only three cases possible given an SCC
        // - SCC has a single type that represents a plain record (non-recursive)
        // - SCC has a single type and is a self-inductive ADT
        // - SCC contains multiple types that are mutually recursive ADTs

        // finalize the ordering among the ADTs
        std::map<const ast::analysis::Type*, size_t> indices;
        std::vector<std::tuple<const ast::analysis::Type*, const ast::Type*>> ordered;
        for (auto type : scc) {
            auto it = adt_registry.find(type->getName().toString());
            assert(it != adt_registry.end());

            auto ty_idx = ordered.size();
            ordered.emplace_back(it->second);
            indices[type] = ty_idx;
        }

        // construct the ADT group
        std::vector<ADT> decls;
        for (auto [type_item, ast_item] : ordered) {
            if (auto type_record = dynamic_cast<const ast::analysis::RecordType*>(type_item)) {
                auto ast_record = dynamic_cast<const ast::RecordType*>(ast_item);

                // collect fields
                const auto& type_fields = type_record->getFields();
                const auto& ast_fields = ast_record->getFields();

                std::vector<ADTField> field_decls;
                for (unsigned i = 0; i < type_fields.size(); i++) {
                    const auto* field_type = type_fields[i];
                    auto existing_type = retrieve_type_or_invalid(field_type->getName().toString());
                    if (existing_type.has_value()) {
                        field_decls.emplace_back(ast_fields[i]->getName(), *existing_type);
                    } else {
                        auto it = indices.find(field_type);
                        assert(it != indices.end());
                        field_decls.emplace_back(ast_fields[i]->getName(), it->second);
                    }
                }

                // construct the default branch and the ADT
                ADTBranch default_branch("", field_decls);
                decls.emplace_back(type_record->getName().toString(), std::vector({default_branch}));
            } else if (auto type_adt = dynamic_cast<const ast::analysis::AlgebraicDataType*>(type_item)) {
                auto ast_adt = dynamic_cast<const ast::AlgebraicDataType*>(ast_item);

                // collect branches
                const auto& type_branches = type_adt->getBranches();
                const auto& ast_branches = ast_adt->getBranches();

                std::vector<ADTBranch> branch_decls;
                for (const auto& type_branch : type_branches) {
                    auto iter = std::find_if(
                            ast_branches.cbegin(), ast_branches.cend(), [type_branch](const auto ast_branch) {
                                return ast_branch->getBranchName() == type_branch.name;
                            });
                    const auto& ast_branch = *iter;

                    // collect fields
                    const auto& type_fields = type_branch.types;
                    const auto& ast_fields = ast_branch->getFields();

                    std::vector<ADTField> field_decls;
                    for (unsigned i = 0; i < type_fields.size(); i++) {
                        const auto* field_type = type_fields[i];
                        auto existing_type = retrieve_type_or_invalid(field_type->getName().toString());
                        if (existing_type.has_value()) {
                            field_decls.emplace_back(ast_fields[i]->getName(), *existing_type);
                        } else {
                            auto it = indices.find(field_type);
                            assert(it != indices.end());
                            field_decls.emplace_back(ast_fields[i]->getName(), it->second);
                        }
                    }

                    // construct the branch decl
                    branch_decls.emplace_back(type_branch.name.toString(), field_decls);
                }

                // construct the ADT
                decls.emplace_back(type_adt->getName().toString(), branch_decls);
            } else {
                assert(false);
            }
        }

        // build the ADT sorts
        std::vector<TypeIndex> fixup;
        for (const auto& decl : decls) {
            const auto [it, inserted] = type_records.emplace(decl.name, std::make_pair(new_index(), decl));
            assert(inserted);
            fixup.push_back(it->second.first);
        }

        // fixup the ADT unnamed types
        for (const auto& decl : decls) {
            auto [index, adt_old] = type_records.at(decl.name);
            type_records.erase(decl.name);

            auto adt_new = adt_old.fix_sort_index(fixup);
            const auto [_, inserted] = type_records.emplace(decl.name, std::make_pair(index, adt_new));
            assert(inserted);
        }

        // save the decls into registration sequence
        std::vector<std::pair<TypeIndex, ADT>> indexed_decls;
        for (unsigned i = 0; i < decls.size(); i++) {
            indexed_decls.emplace_back(fixup[i], decls[i]);
        }
        adt_sequence.emplace_back(indexed_decls);
    }

private:
    /// Check for more restrictive typing rules. Also perform two tasks
    /// - register type idents (uninterpreted types), and
    /// - maintain a registry for ADT types.
    void type_check(const ast::Program& program, const ast::analysis::TypeEnvironment& type_env) {
        for (const auto ast_type : program.getTypes()) {
            const auto& type = type_env.getType(*ast_type);
            assert(type.getName() == ast_type->getQualifiedName());

            // these types can never be user-defined
            auto type_const = dynamic_cast<const ast::analysis::ConstantType*>(&type);
            assert(type_const == nullptr);
            auto type_primitive = dynamic_cast<const ast::analysis::PrimitiveType*>(&type);
            assert(type_primitive == nullptr);

            // filter out invalid cases
            if (auto type_union = dynamic_cast<const ast::analysis::UnionType*>(&type)) {
                throw std::runtime_error("Union type is not permitted: " + type_union->getName().toString());
            }
            if (auto type_alias = dynamic_cast<const ast::analysis::AliasType*>(&type)) {
                throw std::runtime_error("Type alias is not permitted: " + type_alias->getName().toString());
            }

            // ident type (uninterpreted type)
            if (auto type_subset = dynamic_cast<const ast::analysis::SubsetType*>(&type)) {
                if (type_subset->getBaseType().getName().toString() != BUILTIN_TYPE_SYMBOL) {
                    throw std::runtime_error("Only the `symbol` type can be subset typed: " +
                                             type_subset->getName().toString());
                }

                // mark a direct subset type of `symbol` an ident type (i.e., uninterpreted type)
                register_type_ident(type_subset->getName().toString());
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
                const auto [_, inserted] = adt_registry.emplace(
                        type_record->getName().toString(), std::make_tuple(type_record, ast_record));
                assert(inserted);
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
                const auto [_, inserted] = adt_registry.emplace(
                        type_adt->getName().toString(), std::make_tuple(type_adt, ast_adt));
                assert(inserted);
            } else {
                throw std::runtime_error("Unknown user-defined type: " + type.getName().toString());
            }
        }
    }

    /// The ADTs can be mutually recursive, build a dependency graph to capture their dependencies
    Graph<const ast::analysis::Type*> build_adt_dep_graph() const {
        Graph<const ast::analysis::Type*> dep_graph;

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
                    if (!retrieve_type_or_invalid(field_type->getName().toString()).has_value()) {
                        auto it = adt_registry.find(field_type->getName().toString());
                        assert(it != adt_registry.end());
                        dep_graph.addEdge(type_record, std::get<0>(it->second));
                    }
                }
            } else if (auto type_adt = dynamic_cast<const ast::analysis::AlgebraicDataType*>(type)) {
                for (const auto& type_branch : type_adt->getBranches()) {
                    for (const auto field_type : type_branch.types) {
                        // add an edge for the ADT if we haven't registered the type somewhere
                        if (!retrieve_type_or_invalid(field_type->getName().toString()).has_value()) {
                            auto it = adt_registry.find(field_type->getName().toString());
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
};

}  // namespace souffle::smt