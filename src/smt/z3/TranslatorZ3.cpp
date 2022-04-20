#include "smt/z3/TranslatorZ3.h"

namespace souffle::smt::z3 {

void TranslatorZ3::convert(const ast::TranslationUnit& translationUnit) {
    Translator::convert(translationUnit);
}

}  // namespace souffle::smt::z3