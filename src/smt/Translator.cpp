#include "smt/Translator.h"

#include "ast/Program.h"

#include <ostream>

namespace souffle::smt {

void Translator::convert(const ast::TranslationUnit& translationUnit) {
    auto& program = translationUnit.getProgram();
    for (auto type : program.getTypes()) {
        std::cout << *type << std::endl;
    }
}

}  // namespace souffle::smt
