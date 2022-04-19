#pragma once

#include "ast/TranslationUnit.h"

namespace souffle::smt {

class Translator {
public:
    void convert(const ast::TranslationUnit& translationUnit);
};

}  // namespace souffle::smt