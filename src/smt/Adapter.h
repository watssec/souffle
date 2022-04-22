/************************************************************************
 *
 * @file Adapter.h
 *
 * Common types for SMT adaptors
 *
 ***********************************************************************/

#pragma once

#include <variant>

#include "ast/QualifiedName.h"

namespace souffle::smt {

/**
 * Declaration of one field in one variant of an ADT
 */
template <typename SORT>
struct ADTField {
public:
    const ast::QualifiedName& name;
    const std::variant<const SORT&, size_t> type;

public:
    ADTField(const ast::QualifiedName& _name, const SORT& _type) : name(_name), type(_type) {}
    ADTField(const ast::QualifiedName& _name, size_t _type) : name(_name), type(_type) {}
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
    ADTBranch(const ast::QualifiedName& _name, std::initializer_list<ADTField<SORT>> _fields)
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
    ADT(const ast::QualifiedName& _name, std::initializer_list<ADTBranch<SORT>> _branches)
            : name(_name), branches(_branches) {}
};

/**
 * Declaration of a group of mutually recursive ADTs
 */
template <typename SORT>
struct ADTGroup {
public:
    const std::vector<ADT<SORT>> adts;

public:
    explicit ADTGroup(std::initializer_list<ADTBranch<SORT>> _adts) : adts(_adts) {}
};

}  // namespace souffle::smt