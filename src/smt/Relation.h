/************************************************************************
 *
 * @file Relation.h
 *
 * Hosts the relations appeared in the program
 *
 ***********************************************************************/

#pragma once

#include <map>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "smt/Typing.h"

namespace souffle::smt {

// forward declarations
class RelationRegistry;
class ClauseRegistry;
class Frontend;

/**
 * An index that uniquely identifies a relation
 */
class RelationIndex : public Index {
    friend RelationRegistry;

protected:
    explicit RelationIndex(size_t index_) : Index(index_) {}
};

/**
 * An information package about a relation
 */
struct RelationInfo {
public:
    const std::vector<std::pair<std::string, TypeIndex>> params;

public:
    explicit RelationInfo(std::vector<std::pair<std::string, TypeIndex>> params_)
            : params(std::move(params_)) {}
};

/**
 * A registry of relations appeared
 */
class RelationRegistry {
    friend Frontend;
    friend ClauseRegistry;

private:
    // counter
    size_t counter = 1;

    // environment
    const TypeRegistry& typeRegistry;

protected:
    // registry
    std::map<std::string, std::pair<RelationIndex, RelationInfo>> mapping{};

public:
    RelationRegistry(const ast::TranslationUnit& unit, const TypeRegistry& typeRegistry_)
            : typeRegistry(typeRegistry_) {
        const auto& program = unit.getProgram();

        // register relations
        for (const auto rel : program.getRelations()) {
            if (!rel->getFunctionalDependencies().empty()) {
                // the "choice" features is not supported yet
                throw std::runtime_error(
                        "Functional constraints not supported: " + rel->getQualifiedName().toString());
            }

            // collect parameters
            std::vector<std::pair<std::string, TypeIndex>> params;
            for (const auto attr : rel->getAttributes()) {
                auto type = typeRegistry.retrieve_type(attr->getTypeName().toString());
                params.emplace_back(attr->getName(), type);
            }
            register_relation(rel->getQualifiedName().toString(), params);
        }
    }

public:
    /// Retrieve a relation by its name. Panic if nonexistent.
    RelationIndex retrieve_relation(const std::string& name) const {
        auto result = retrieve_relation_or_invalid(name);
        assert(result.has_value());
        return *result;
    }

    /// Retrieve the details for a relation by its index.
    const RelationInfo& retrieve_details(const RelationIndex& index) const {
        for (const auto& [key, val] : mapping) {
            if (val.first == index) {
                return val.second;
            }
        }
        assert(false);
    }

private:
    /// Create a new index
    RelationIndex new_index() {
        return RelationIndex(counter++);
    }

private:
    /// Retrieve a relation or nullptr if non-exist
    std::optional<RelationIndex> retrieve_relation_or_invalid(const std::string& name) const {
        const auto it = mapping.find(name);
        if (it != mapping.end()) {
            return it->second.first;
        }
        return std::nullopt;
    }

    /// Register a relation to the registry
    void register_relation(std::string name, std::vector<std::pair<std::string, TypeIndex>> params) {
        assert(!retrieve_relation_or_invalid(name).has_value());
        const auto [_, inserted] =
                mapping.emplace(std::move(name), std::make_pair(new_index(), std::move(params)));
        assert(inserted);
    }
};

}  // namespace souffle::smt