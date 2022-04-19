#include "smt/Translator.h"
#include "ast/Program.h"

#include <ostream>

namespace souffle::smt {

void Translator::convert(const ast::TranslationUnit& translationUnit) {
    auto& program = translationUnit.getProgram();

    std::cout << "[" << program.getTypes().size() << " types]" << std::endl;
    for (auto type : program.getTypes()) {
        std::cout << *type << std::endl;
    }

    std::cout << "[" << program.getRelations().size() << " relations]" << std::endl;
    for (auto rel : program.getRelations()) {
        std::cout << *rel << std::endl;
    }

    std::cout << "[" << program.getClauses().size() << " clauses]" << std::endl;
    for (auto clause : program.getClauses()) {
        std::cout << *clause << std::endl;
    }
}

}  // namespace souffle::smt