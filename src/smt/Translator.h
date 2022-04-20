#pragma once

#include "ast/TranslationUnit.h"

namespace souffle::smt {

class Translator {
protected:
    virtual void convert(const ast::TranslationUnit& translationUnit) = 0;
};

}  // namespace souffle::smt