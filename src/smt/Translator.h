#pragma once

#include <cvc5/cvc5.h>
#include <z3.h>

#include "ast/TranslationUnit.h"

namespace souffle::smt {

/**
 * A base class for AST to SMT conversion
 */
class Translator {
protected:
    Translator() = default; /* disables construction */

public:
    void convert(const ast::TranslationUnit& unit);
};

/**
 * An abstract AST to SMT translator based on Z3
 */
class TranslatorZ3 : public Translator {
protected:
    Z3_context ctx;

protected:
    explicit TranslatorZ3(Z3_config cfg);
    ~TranslatorZ3();
};

/**
 * A concrete AST to SMT translator based on Z3 MuZ facility
 */
class TranslatorZ3MuZ : public TranslatorZ3 {
public:
    TranslatorZ3MuZ();
    ~TranslatorZ3MuZ() = default;
};

/**
 * A concrete AST to SMT translator based on Z3 recursive function
 */
class TranslatorZ3Rec : public TranslatorZ3 {
public:
    TranslatorZ3Rec();
    ~TranslatorZ3Rec() = default;
};

}  // namespace souffle::smt