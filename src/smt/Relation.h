/************************************************************************
 *
 * @file Relation.h
 *
 * Hosts the relations appeared in the program
 *
 ***********************************************************************/

#pragma once

#include "smt/Common.h"
#include "smt/Query.h"
#include "smt/Typing.h"

namespace souffle::smt {

// forward declarations
class RelationRegistry;

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
    const RelationIndex index;
    const std::string name;
    const std::vector<std::pair<std::string, TypeIndex>> params;

public:
    RelationInfo(
            RelationIndex index_, std::string name_, std::vector<std::pair<std::string, TypeIndex>> params_)
            : index(index_), name(std::move(name_)), params(std::move(params_)) {}
};

/**
 * A registry of relations appeared
 */
class RelationRegistry {
private:
    // counter
    size_t counter = 1;

    // environment
    const TypeRegistry& typeRegistry;
    const QueryRegistry& queryRegistry;

protected:
    // registry
    std::map<std::string, RelationInfo> mapping{};

public:
    RelationRegistry(const ast::TranslationUnit& unit, const TypeRegistry& typeRegistry_,
            const QueryRegistry& queryRegistry_)
            : typeRegistry(typeRegistry_), queryRegistry(queryRegistry_) {
#ifdef SMT_DEBUG
        std::cout << "[relation] analysis started" << std::endl;
#endif

        // register relations
        const auto& program = unit.getProgram();
        for (const auto rel : program.getRelations()) {
            if (!rel->getFunctionalDependencies().empty()) {
                // the "choice" features is not supported yet
                throw std::runtime_error(
                        "Functional constraints not supported: " + rel->getQualifiedName().toString());
            }

            auto rel_name = rel->getQualifiedName().toString();

            // ignore relations that are marked as queries
            if (queryRegistry.is_query(rel_name)) {
                continue;
            }

#ifdef SMT_DEBUG
            std::cout << "[relation] analyzing relation: " << rel_name << std::endl;
#endif

            // collect parameters
            std::vector<std::pair<std::string, TypeIndex>> params;
            for (const auto attr : rel->getAttributes()) {
                auto type = typeRegistry.retrieve_type(attr->getTypeName().toString());
                params.emplace_back(attr->getName(), type);
            }
            register_relation(std::move(rel_name), params);
        }

#ifdef SMT_DEBUG
        std::cout << "[relation] analysis completed" << std::endl;
#endif
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
            if (val.index == index) {
                return val;
            }
        }
        assert(false);
    }

    /// Expose all relations.
    const std::map<std::string, RelationInfo>& all_relations() const {
        return mapping;
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
            return it->second.index;
        }
        return std::nullopt;
    }

    /// Register a relation to the registry
    void register_relation(std::string name, std::vector<std::pair<std::string, TypeIndex>> params) {
        assert(!retrieve_relation_or_invalid(name).has_value());
        auto info = RelationInfo(new_index(), name, std::move(params));
        const auto [_, inserted] = mapping.emplace(std::move(name), info);
        assert(inserted);
    }
};

}  // namespace souffle::smt