#include "smt/z3/TranslatorMUZ.h"

namespace souffle::smt::z3 {

void TranslatorMUZ::convert(const ast::TranslationUnit& translationUnit) {
    Translator::convert(translationUnit);
}

}  // namespace souffle::smt::z3