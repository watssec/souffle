#pragma once

#include <map>
#include <stdexcept>

#include <cvc5/cvc5.h>
#include <z3.h>

#include "ast/Program.h"
#include "ast/QualifiedName.h"
#include "ast/TranslationUnit.h"

namespace souffle::smt {

/**
 * A base class for AST to SMT conversion
 *
 * @tparam SORT: the `type` representation in SMT solver
 */
template <typename SORT>
class Translator {
protected:
    /// Types, including primitives and user-defined
    std::map<ast::QualifiedName, SORT> types;

protected:
    Translator() = default;

protected:
    /// Checked registration of types
    void register_type(ast::QualifiedName name, SORT sort) {
        if (!types.emplace(name, sort).second) {
            throw std::runtime_error("Type already registered: " + name.toString());
        }
    }

protected:
    /// Create primitive type: number
    virtual SORT create_type_number() = 0;

public:
    /// Convert the translation unit into an SMT context
    void convert(const ast::TranslationUnit& unit) {
        // prepare primitive types
        register_type(ast::QualifiedName("number"), create_type_number());

        // register user-defined types
        auto& program = unit.getProgram();
        for (const auto& type : program.getTypes()) {
            // TODO: implement it
            std::cout << *type << std::endl;
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
};

/**
 * A concrete AST to SMT translator based on CVC recursive function
 */
class TranslatorCVCRec : public TranslatorCVC {
public:
    TranslatorCVCRec() = default;
};

}  // namespace souffle::smt