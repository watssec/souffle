/************************************************************************
 *
 * @file Adapter.h
 *
 * Common types for SMT adaptors
 *
 ***********************************************************************/

#pragma once

#include <variant>
#include <vector>

#include "ast/QualifiedName.h"

namespace souffle::smt {

/**
 * Declaration of one field in one variant of an ADT
 */
template <typename SORT>
struct ADTField {
public:
    const std::string& name;
    const std::variant<const SORT*, size_t> type;

public:
    ADTField(const std::string& _name, const SORT* _type) : name(_name), type(_type) {}
    ADTField(const std::string& _name, size_t _type) : name(_name), type(_type) {}
};

/**
 * Declaration of one variant of an ADT
 */
template <typename SORT>
struct ADTBranch {
public:
    const ast::QualifiedName& name;
    const std::vector<ADTField<SORT>> fields;

public:
    ADTBranch(const ast::QualifiedName& _name, std::vector<ADTField<SORT>>&& _fields)
            : name(_name), fields(_fields) {}
};

/**
 * Declaration of one ADT
 */
template <typename SORT>
struct ADT {
public:
    const ast::QualifiedName& name;
    const std::vector<ADTBranch<SORT>> branches;

public:
    ADT(const ast::QualifiedName& _name, std::vector<ADTBranch<SORT>>&& _branches)
            : name(_name), branches(_branches) {}
};

}  // namespace souffle::smt