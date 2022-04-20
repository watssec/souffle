#include "smt/Translator.h"
#include "ast/Program.h"
#include "ast/TranslationUnit.h"

namespace souffle::smt {

void Translator::convert(const ast::TranslationUnit& unit) {
    auto& program = unit.getProgram();

    // register types
    for (const auto& type : program.getTypes()) {
        // TODO: implement it
        std::cout << *type << std::endl;
    }
}

TranslatorZ3::TranslatorZ3(Z3_config cfg) : Translator() {
    ctx = Z3_mk_context(cfg);
    Z3_del_config(cfg);
}

TranslatorZ3::~TranslatorZ3() {
    Z3_del_context(ctx);
    ctx = nullptr;
}

namespace {
Z3_config config_for_z3_muz() {
    auto cfg = Z3_mk_config();
    return cfg;
}
}  // namespace

TranslatorZ3MuZ::TranslatorZ3MuZ() : TranslatorZ3(config_for_z3_muz()) {}

namespace {
Z3_config config_for_z3_rec() {
    auto cfg = Z3_mk_config();
    return cfg;
}
}  // namespace

TranslatorZ3Rec::TranslatorZ3Rec() : TranslatorZ3(config_for_z3_rec()) {}

}  // namespace souffle::smt