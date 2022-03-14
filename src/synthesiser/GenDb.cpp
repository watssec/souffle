/**
 * Souffle - A Datalog Compiler
 * Copyright (c) 2022, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file GenDb.cpp
 *
 * Implementation of the various C++ representations for classes, functions, etc.
 * and a way to produce C++ code.
 *
 ***********************************************************************/
#include "GenDb.h"
#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/FileUtil.h"
#include "souffle/utility/MiscUtil.h"
#include "souffle/utility/StreamUtil.h"
#include "souffle/utility/StringUtil.h"
#include "souffle/utility/json11.h"
#include "souffle/utility/tinyformat.h"
#include "synthesiser/GenDb.h"
#include <filesystem>
#include <list>
namespace fs = std::filesystem;

namespace souffle::synthesiser {

std::streambuf::int_type DelayableOutputStream::overflow(std::streambuf::int_type ch) {
    if (!current_stream) {
        pieces.emplace_back(std::nullopt, std::make_shared<std::stringstream>());
        current_stream = pieces.back().second;
    }
    current_stream->put(ch);
    return ch;
}
/** Return a piece of stream that will be included in the output only if the given condition is true when
 * this stream is flushed. */
std::shared_ptr<std::ostream> DelayableOutputStream::delayed_if(const bool& cond) {
    current_stream.reset();
    pieces.emplace_back(&cond, std::make_shared<std::stringstream>());
    return pieces.back().second;
}

std::shared_ptr<std::ostream> DelayableOutputStream::delayed() {
    current_stream.reset();
    pieces.emplace_back(std::nullopt, std::make_shared<std::stringstream>());
    return pieces.back().second;
}

/** */
void DelayableOutputStream::flushAll(std::ostream& os) {
    current_stream.reset();
    while (!pieces.empty()) {
        auto& piece = pieces.front();
        if ((!piece.first) || **piece.first) {
            os << piece.second->str();
        }
        pieces.pop_front();
    }
}

void Gen::addDependency(Gen& dep, bool def_only) {
    if (def_only) {
        dependencies.emplace(&dep);
    } else {
        decl_dependencies.emplace(&dep);
    }
}

void Gen::addInclude(std::string str, bool def_only) {
    if (def_only) {
        includes.emplace(str);
    } else {
        decl_includes.emplace(str);
    }
}

void GenFunction::setRetType(std::string ty) {
    retType = ty;
}

void GenFunction::setNextArg(std::string ty, std::string name, std::optional<std::string> defaultValue) {
    args.push_back(std::make_tuple(ty, name, defaultValue));
}

void GenFunction::setNextInitializer(std::string field, std::string value) {
    initializer.push_back(std::make_pair(field, value));
}

void GenFunction::declaration(std::ostream& o) {
    o << retType << " " << name << "(" << join(args, ",", [&](auto& out, const auto arg) {
        out << std::get<0>(arg) << " " << std::get<1>(arg);
        std::optional<std::string> defaultValue = std::get<2>(arg);
        if (defaultValue) {
            out << " = " << *defaultValue;
        }
    }) << ");";
}

void GenFunction::definition(std::ostream& o) {
    o << retType << " ";
    if (cl) {
        o << cl->getName() << "::";
    }
    o << name << "(" << join(args, ",", [&](auto& out, const auto arg) {
        out << std::get<0>(arg) << " " << std::get<1>(arg);
    }) << ")";
    if (isConstructor && initializer.size() > 0) {
        o << ":\n";
        o << join(initializer, ",\n",
                [&](auto& out, const auto arg) { out << arg.first << "(" << arg.second << ")"; });
    }
    o << "{\n";
    o << bodyStream.str();
    o << "}\n";
}

GenFunction& GenClass::addFunction(std::string name, Visibility v) {
    methods.push_back(mk<GenFunction>(name, this, v));
    return *methods.back();
}

void GenClass::addField(std::string type, std::string name, Visibility v, std::optional<std::string> init) {
    fields.push_back(std::make_tuple(name, type, v, init));
}

GenFunction& GenClass::addConstructor(Visibility v) {
    methods.push_back(mk<GenFunction>(getName(), this, v));
    GenFunction& m = *methods.back();
    m.setIsConstructor();
    return m;
}

void GenClass::declaration(std::ostream& o) {
    o << "namespace souffle {\n";

    o << "class " << name;
    if (inheritance.size() > 0) {
        o << ": " << join(inheritance, ", ", [&](auto& out, const auto arg) { out << arg; });
    }
    o << " {\n";
    DelayableOutputStream public_o;
    DelayableOutputStream private_o;
    public_o << "public:\n";
    private_o << "private:\n";

    for (auto& fn : methods) {
        auto& o = (fn->getVisibility() == Public) ? public_o : private_o;
        fn->declaration(o);
        o << "\n";
    }
    for (auto& [field, ty, v, init] : fields) {
        auto& o = (v == Public) ? public_o : private_o;
        o << ty << " " << field;
        if (init) {
            o << *init;
        }
        o << ";\n";
    }
    public_o.flushAll(o);
    private_o.flushAll(o);
    o << "};\n";
    o << "} // namespace souffle\n";
}

void GenClass::definition(std::ostream& o) {
    if (ignoreUnusedArgumentWarning) {
        o << "#ifdef _MSC_VER\n";
        o << "#pragma warning(disable: 4100)\n";
        o << "#endif // _MSC_VER\n";
    }
    o << "namespace souffle {\n";
    for (auto& fn : methods) {
        fn->definition(o);
        o << "\n";
    }
    o << "} // namespace souffle\n";

    if (ignoreUnusedArgumentWarning) {
        // restore unused argument warning
        o << "#ifdef _MSC_VER\n";
        o << "#pragma warning(default: 4100)\n";
        o << "#endif // _MSC_VER\n";
    }
}

GenClass& GenDb::getClass(std::string name) {
    classes.push_back(mk<GenClass>(name, this));
    GenClass& res = *classes.back();
    nameToGen[name] = &res;
    return res;
}

GenDatastructure& GenDb::getDatastructure(std::string name, std::optional<std::string> namespace_opt) {
    std::string fullName = name;
    if (namespace_opt) {
        fullName = *namespace_opt + "::" + name;
    }
    datastructures.push_back(mk<GenDatastructure>(name, namespace_opt, this));
    GenDatastructure& res = *datastructures.back();
    nameToGen[fullName] = &res;
    return res;
}

void GenDatastructure::declaration(std::ostream& o) {
    std::string ns = "souffle";
    if (namespace_name) {
        ns += "::" + *namespace_name;
    }
    o << "namespace " << ns << " {\n";
    o << declarationStream.str();
    o << "} // namespace " << ns << " \n";
}

void GenDatastructure::definition(std::ostream& o) {
    std::string ns = "souffle";
    if (namespace_name) {
        ns += "::" + *namespace_name;
    }
    o << "namespace " << ns << " {\n";
    o << definitionStream.str();
    o << "} // namespace " << ns << " \n";
}

fs::path GenDatastructure::fileBaseName() {
    fs::path p = name;
    if (namespace_name) {
        p = fs::path(*namespace_name);
    }
    return p;
}

void GenDb::emitSingleFile(std::ostream& o) {
    std::set<std::string> defines = globalDefines;
    std::set<std::string> includes = globalIncludes;
    auto add = [&](Gen& gen) {
        auto& inc = gen.getDeclIncludes();
        includes.insert(inc.begin(), inc.end());
        inc = gen.getIncludes();
        includes.insert(inc.begin(), inc.end());
    };

    for (auto& ds : datastructures) {
        add(*ds);
    }
    for (auto& cl : classes) {
        add(*cl);
    }

    for (auto& def : defines) {
        o << "#define " << def << "\n";
    }
    for (auto& inc : includes) {
        o << "#include " << inc << "\n";
    }
    o << "extern \"C\" {\n";
    o << externCStream.str();
    o << "}\n";
    for (auto& ds : datastructures) {
        ds->declaration(o);
        ds->definition(o);
    }
    std::size_t size = classes.size();
    for (std::size_t i = 1; i < size - 1; i++) {
        Own<GenClass>& cl = classes[i];
        cl->declaration(o);
        cl->definition(o);
    }
    classes.front()->declaration(o);
    classes.front()->definition(o);
    classes.back()->declaration(o);
    classes.back()->definition(o);
    o << hiddenHooksStream.str() << "\n";
}

void GenDb::emitMultipleFilesInDir(std::string dir) {
    fs::path rootDir = dir;
    fs::create_directories(rootDir);

    auto globalHeader = [&](std::ofstream& hpp) {
        for (auto& def : globalDefines) {
            hpp << "#define " << def << "\n";
        }
        for (auto& inc : globalIncludes) {
            hpp << "#include " << inc << "\n";
        }
    };

    auto genHeader = [&](std::ofstream& hpp, std::ofstream& cpp, Gen& gen) {
        hpp << "#pragma once\n";
        globalHeader(hpp);
        for (const std::string& inc : gen.getDeclIncludes()) {
            hpp << "#include " << inc << "\n";
        }
        for (Gen* dep : gen.getDeclDependencies()) {
            hpp << "#include " << dep->getHeader() << "\n";
        }
        for (const std::string& inc : gen.getIncludes()) {
            cpp << "#include " << inc << "\n";
        }
        for (Gen* dep : gen.getDependencies()) {
            cpp << "#include " << dep->getHeader() << "\n";
        }
        cpp << "#include " << gen.getHeader() << "\n";
    };

    for (auto& ds : datastructures) {
        std::ofstream hpp{rootDir / ds->fileBaseName().concat(".hpp")};
        std::ofstream cpp{rootDir / ds->fileBaseName().concat(".cpp")};
        genHeader(hpp, cpp, *ds);
        ds->declaration(hpp);
        ds->definition(cpp);
    }
    for (auto& cl : classes) {
        std::ofstream hpp{rootDir / cl->fileBaseName().concat(".hpp")};
        std::ofstream cpp{rootDir / cl->fileBaseName().concat(".cpp")};
        genHeader(hpp, cpp, *cl);
        cl->declaration(hpp);
        cl->definition(cpp);
        if (cl->isMain) {
            cpp << hiddenHooksStream.str() << "\n";
        }
    }
}

}  // namespace souffle::synthesiser