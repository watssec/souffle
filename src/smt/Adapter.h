/************************************************************************
 *
 * @file Adapter.h
 *
 * Common types for SMT adaptors
 *
 ***********************************************************************/

#pragma once

#include <utility>
#include <variant>
#include <vector>

namespace souffle::smt {

/**
 * Declaration of one field in one variant of an ADT
 */
template <typename SORT>
struct ADTField {
public:
    const std::string name;
    const std::variant<const SORT*, size_t> type;

public:
    ADTField(std::string _name, const SORT* _type) : name(std::move(_name)), type(_type) {}
    ADTField(std::string _name, size_t _type) : name(std::move(_name)), type(_type) {}
};

/**
 * Declaration of one variant of an ADT
 */
template <typename SORT>
struct ADTBranch {
public:
    const std::string name;
    const std::vector<ADTField<SORT>> fields;

public:
    ADTBranch(std::string _name, std::vector<ADTField<SORT>>&& _fields)
            : name(std::move(_name)), fields(_fields) {}
};

/**
 * Declaration of one ADT
 */
template <typename SORT>
struct ADT {
public:
    const std::string name;
    const std::vector<ADTBranch<SORT>> branches;

public:
    ADT(std::string _name, std::vector<ADTBranch<SORT>>&& _branches)
            : name(std::move(_name)), branches(_branches) {}
};

}  // namespace souffle::smt