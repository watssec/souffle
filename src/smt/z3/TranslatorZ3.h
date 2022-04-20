#pragma once

#include "smt/Translator.h"

namespace souffle::smt::z3 {

class TranslatorZ3 : Translator {
public:
    void convert(const ast::TranslationUnit& translationUnit) override;
};

}  // namespace souffle::smt::z3