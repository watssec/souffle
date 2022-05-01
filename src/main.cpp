/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file main.cpp
 *
 * Main driver for Souffle
 *
 ***********************************************************************/

#include "Global.h"
#include "ast/Clause.h"
#include "ast/Node.h"
#include "ast/Program.h"
#include "ast/TranslationUnit.h"
#include "ast/analysis/PrecedenceGraph.h"
#include "ast/analysis/SCCGraph.h"
#include "ast/analysis/typesystem/Type.h"
#include "ast/transform/AddNullariesToAtomlessAggregates.h"
#include "ast/transform/ComponentChecker.h"
#include "ast/transform/ComponentInstantiation.h"
#include "ast/transform/Conditional.h"
#include "ast/transform/ExecutionPlanChecker.h"
#include "ast/transform/ExpandEqrels.h"
#include "ast/transform/Fixpoint.h"
#include "ast/transform/FoldAnonymousRecords.h"
#include "ast/transform/GroundWitnesses.h"
#include "ast/transform/GroundedTermsChecker.h"
#include "ast/transform/IOAttributes.h"
#include "ast/transform/IODefaults.h"
#include "ast/transform/InlineRelations.h"
#include "ast/transform/MagicSet.h"
#include "ast/transform/MaterializeAggregationQueries.h"
#include "ast/transform/MaterializeSingletonAggregation.h"
#include "ast/transform/MinimiseProgram.h"
#include "ast/transform/NameUnnamedVariables.h"
#include "ast/transform/NormaliseGenerators.h"
#include "ast/transform/PartitionBodyLiterals.h"
#include "ast/transform/Pipeline.h"
#include "ast/transform/PragmaChecker.h"
#include "ast/transform/ReduceExistentials.h"
#include "ast/transform/RemoveBooleanConstraints.h"
#include "ast/transform/RemoveEmptyRelations.h"
#include "ast/transform/RemoveRedundantRelations.h"
#include "ast/transform/RemoveRedundantSums.h"
#include "ast/transform/RemoveRelationCopies.h"
#include "ast/transform/ReplaceSingletonVariables.h"
#include "ast/transform/ResolveAliases.h"
#include "ast/transform/ResolveAnonymousRecordAliases.h"
#include "ast/transform/SemanticChecker.h"
#include "ast/transform/SimplifyAggregateTargetExpression.h"
#include "ast/transform/SubsumptionQualifier.h"
#include "ast/transform/UniqueAggregationVariables.h"
#include "ast2ram/TranslationStrategy.h"
#include "ast2ram/UnitTranslator.h"
#include "ast2ram/provenance/TranslationStrategy.h"
#include "ast2ram/provenance/UnitTranslator.h"
#include "ast2ram/seminaive/TranslationStrategy.h"
#include "ast2ram/seminaive/UnitTranslator.h"
#include "ast2ram/utility/TranslatorContext.h"
#include "config.h"
#include "interpreter/Engine.h"
#include "interpreter/ProgInterface.h"
#include "parser/ParserDriver.h"
#include "ram/Node.h"
#include "ram/Program.h"
#include "ram/TranslationUnit.h"
#include "ram/transform/CollapseFilters.h"
#include "ram/transform/Conditional.h"
#include "ram/transform/EliminateDuplicates.h"
#include "ram/transform/ExpandFilter.h"
#include "ram/transform/HoistAggregate.h"
#include "ram/transform/HoistConditions.h"
#include "ram/transform/IfConversion.h"
#include "ram/transform/IfExistsConversion.h"
#include "ram/transform/Loop.h"
#include "ram/transform/MakeIndex.h"
#include "ram/transform/Parallel.h"
#include "ram/transform/ReorderConditions.h"
#include "ram/transform/ReorderFilterBreak.h"
#include "ram/transform/ReportIndex.h"
#include "ram/transform/Sequence.h"
#include "ram/transform/Transformer.h"
#include "ram/transform/TupleId.h"
#include "reports/DebugReport.h"
#include "reports/ErrorReport.h"
#include "souffle/RamTypes.h"
#ifndef _MSC_VER
#include "souffle/profile/Tui.h"
#include "souffle/provenance/Explain.h"
#endif
#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/FileUtil.h"
#include "souffle/utility/MiscUtil.h"
#include "souffle/utility/StreamUtil.h"
#include "souffle/utility/StringUtil.h"
#include "souffle/utility/SubProcess.h"
#include "synthesiser/Synthesiser.h"
#include <cassert>
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <iomanip>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <thread>
#include <utility>
#include <vector>

namespace souffle {

/**
 * Executes a binary file.
 */
[[noreturn]] void executeBinaryAndExit(const std::string& binaryFilename) {
    assert(!binaryFilename.empty() && "binary filename cannot be blank");

    std::map<char const*, std::string> env;
    if (Global::config().has("library-dir")) {
        auto escapeLdPath = [](auto&& xs) { return escape(xs, {':', ' '}, "\\"); };
        auto ld_path = toString(join(
                map(Global::config().getMany("library-dir"), escapeLdPath), std::string(1, PATHdelimiter)));
#if defined(_MSC_VER)
        std::size_t l;
        std::wstring env_path(ld_path.length() + 1, L' ');
        ::mbstowcs_s(&l, env_path.data(), env_path.size(), ld_path.data(), ld_path.size());
        env_path.resize(l - 1);

        DWORD n = GetEnvironmentVariableW(L"PATH", nullptr, 0);
        if (n > 0) {
            // append path
            std::unique_ptr<wchar_t[]> orig(new wchar_t[n]);
            GetEnvironmentVariableW(L"PATH", orig.get(), n);
            env_path = env_path + L";" + std::wstring(orig.get());
        }
        SetEnvironmentVariableW(L"PATH", env_path.c_str());

#elif defined(__APPLE__)
        env["DYLD_LIBRARY_PATH"] = ld_path;
#else
        env["LD_LIBRARY_PATH"] = ld_path;
#endif
    }

    auto exit = execute(binaryFilename, {}, env);
    if (!exit) throw std::invalid_argument("failed to execute `" + binaryFilename + "`");

    if (!Global::config().has("dl-program")) {
        remove(binaryFilename.c_str());
        remove((binaryFilename + ".cpp").c_str());
    }

    std::exit(exit ? *exit : EXIT_FAILURE);
}

/**
 * Compiles the given source file to a binary file.
 */
void compileToBinary(const std::string& command, std::string_view sourceFilename) {
    std::vector<std::string> argv;

    argv.push_back(command);

    if (Global::config().has("swig")) {
        argv.push_back("-s");
        argv.push_back(Global::config().get("swig"));
    }

    for (auto&& path : Global::config().getMany("library-dir")) {
        // The first entry may be blank
        if (path.empty()) {
            continue;
        }
        argv.push_back(tfm::format("-L%s", path));
    }
    for (auto&& library : Global::config().getMany("libraries")) {
        // The first entry may be blank
        if (library.empty()) {
            continue;
        }
        argv.push_back(tfm::format("-l%s", library));
    }

    argv.push_back(std::string(sourceFilename));

#if defined(_MSC_VER)
    const char* interpreter = "python";
#else
    const char* interpreter = "python3";
#endif
    auto exit = execute(interpreter, argv);
    if (!exit) throw std::invalid_argument(tfm::format("unable to execute tool <python3 %s>", command));
    if (exit != 0)
        throw std::invalid_argument(tfm::format("failed to compile C++ source <%s>", sourceFilename));
}

int main(int argc, char** argv) {
    /* Time taking for overall runtime */
    auto souffle_start = std::chrono::high_resolution_clock::now();

    std::string versionFooter;

    /* have all to do with command line arguments in its own scope, as these are accessible through the global
     * configuration only */
    try {
        std::stringstream header;
        header << "============================================================================" << std::endl;
        header << "souffle -- A datalog engine." << std::endl;
        header << "Usage: souffle [OPTION] FILE." << std::endl;
        header << "----------------------------------------------------------------------------" << std::endl;
        header << "Options:" << std::endl;

        std::stringstream footer;
        footer << "----------------------------------------------------------------------------" << std::endl;
        footer << "Version: " << PACKAGE_VERSION << "" << std::endl;
        footer << "----------------------------------------------------------------------------" << std::endl;
        footer << "Copyright (c) 2016-22 The Souffle Developers." << std::endl;
        footer << "Copyright (c) 2013-16 Oracle and/or its affiliates." << std::endl;
        footer << "All rights reserved." << std::endl;
        footer << "============================================================================" << std::endl;

        versionFooter = footer.str();

        // command line options, the environment will be filled with the arguments passed to them, or
        // the empty string if they take none
        // main option, the datalog program itself, has an empty key
        std::vector<MainOption> options{{"", 0, "", "", false, ""},
                {"auto-schedule", 'a', "FILE", "", false,
                        "Use profile auto-schedule <FILE> for auto-scheduling."},
                {"fact-dir", 'F', "DIR", ".", false, "Specify directory for fact files."},
                {"include-dir", 'I', "DIR", ".", true, "Specify directory for include files."},
                {"output-dir", 'D', "DIR", ".", false,
                        "Specify directory for output files. If <DIR> is `-` then stdout is used."},
                {"jobs", 'j', "N", "1", false,
                        "Run interpreter/compiler in parallel using N threads, N=auto for system "
                        "default."},
                {"compile", 'c', "", "", false,
                        "Generate C++ source code, compile to a binary executable, then run this "
                        "executable."},
                {"generate", 'g', "FILE", "", false,
                        "Generate C++ source code for the given Datalog program and write it to "
                        "<FILE>. If <FILE> is `-` then stdout is used."},
                {"inline-exclude", '\x7', "RELATIONS", "", false,
                        "Prevent the given relations from being inlined. Overrides any `inline` qualifiers."},
                {"swig", 's', "LANG", "", false,
                        "Generate SWIG interface for given language. The values <LANG> accepts is java and "
                        "python. "},
                {"library-dir", 'L', "DIR", "", true, "Specify directory for library files."},
                {"libraries", 'l', "FILE", "", true, "Specify libraries."},
                {"no-warn", 'w', "", "", false, "Disable warnings."},
                {"magic-transform", 'm', "RELATIONS", "", false,
                        "Enable magic set transformation changes on the given relations, use '*' "
                        "for all."},
                {"magic-transform-exclude", '\x8', "RELATIONS", "", false,
                        "Disable magic set transformation changes on the given relations. Overrides "
                        "`magic-transform`. Implies `inline-exclude` for the given relations."},
                {"macro", 'M', "MACROS", "", false, "Set macro definitions for the pre-processor"},
                {"disable-transformers", 'z', "TRANSFORMERS", "", false,
                        "Disable the given AST transformers."},
                {"dl-program", 'o', "FILE", "", false,
                        "Generate C++ source code, written to <FILE>, and compile this to a "
                        "binary executable (without executing it)."},
                {"index-stats", '\x9', "", "", false, "Enable collection of index statistics"},
                {"live-profile", '\1', "", "", false, "Enable live profiling."},
                {"profile", 'p', "FILE", "", false, "Enable profiling, and write profile data to <FILE>."},
                {"profile-frequency", '\2', "", "", false, "Enable the frequency counter in the profiler."},
                {"debug-report", 'r', "FILE", "", false, "Write HTML debug report to <FILE>."},
                {"pragma", 'P', "OPTIONS", "", true, "Set pragma options."},
                {"provenance", 't', "[ none | explain | explore ]", "", false,
                        "Enable provenance instrumentation and interaction."},
                {"verbose", 'v', "", "", false, "Verbose output."},
                {"version", '\3', "", "", false, "Version."},
                {"show", '\4', "[ <see-list> ]", "", true,
                        "Print selected program information.\n"
                        "Modes:\n"
                        "\tinitial-ast\n"
                        "\tinitial-ram\n"
                        "\tparse-errors\n"
                        "\tprecedence-graph\n"
                        "\tprecedence-graph-text\n"
                        "\tscc-graph\n"
                        "\tscc-graph-text\n"
                        "\ttransformed-ast\n"
                        "\ttransformed-ram\n"
                        "\ttype-analysis"},
                {"parse-errors", '\5', "", "", false, "Show parsing errors, if any, then exit."},
                {"help", 'h', "", "", false, "Display this help message."},
                {"legacy", '\6', "", "", false, "Enable legacy support."},
                {"preprocessor", '\7', "CMD", "", false, "C preprocessor to use."}};
        Global::config().processArgs(argc, argv, header.str(), versionFooter, options);

        // ------ command line arguments -------------

        // Take in pragma options from the command line
        if (Global::config().has("pragma")) {
            ast::transform::PragmaChecker::Merger merger;

            for (auto&& option : Global::config().getMany("pragma")) {
                // TODO: escape sequences for `:` to allow `:` in a pragma key?
                std::size_t splitPoint = option.find(':');

                std::string optionName = option.substr(0, splitPoint);
                std::string optionValue = (splitPoint == std::string::npos)
                                                  ? ""
                                                  : option.substr(splitPoint + 1, option.length());

                merger(optionName, optionValue);
            }
        }

        /* for the version option, if given print the version text then exit */
        if (Global::config().has("version")) {
            std::cout << versionFooter << std::endl;
            return 0;
        }
        Global::config().set("version", PACKAGE_VERSION);

        /* for the help option, if given simply print the help text then exit */
        if (Global::config().has("help")) {
            std::cout << Global::config().help();
            return 0;
        }

        if (!Global::config().has("")) {
            std::cerr << "No datalog file specified.\n";
            return 0;
        }

        /* check that datalog program exists */
        if (!existFile(Global::config().get(""))) {
            throw std::runtime_error("cannot open file " + std::string(Global::config().get("")));
        }

        /* for the jobs option, to determine the number of threads used */
#ifdef _OPENMP
        if (isNumber(Global::config().get("jobs").c_str())) {
            if (std::stoi(Global::config().get("jobs")) < 1) {
                throw std::runtime_error("-j/--jobs may only be set to 'auto' or an integer greater than 0.");
            }
        } else {
            if (!Global::config().has("jobs", "auto")) {
                throw std::runtime_error("-j/--jobs may only be set to 'auto' or an integer greater than 0.");
            }
            // set jobs to zero to indicate the synthesiser and interpreter to use the system default.
            Global::config().set("jobs", "0");
        }
#else
        // Check that -j option has not been changed from the default
        if (Global::config().get("jobs") != "1" && !Global::config().has("no-warn")) {
            std::cerr << "\nThis installation of Souffle does not support concurrent jobs.\n";
        }
#endif

        /* if an output directory is given, check it exists */
        if (Global::config().has("output-dir") && !Global::config().has("output-dir", "-") &&
                !existDir(Global::config().get("output-dir")) &&
                !(Global::config().has("generate") ||
                        (Global::config().has("dl-program") && !Global::config().has("compile")))) {
            throw std::runtime_error(
                    "output directory " + Global::config().get("output-dir") + " does not exists");
        }

        /* verify all input directories exist (racey, but gives nicer error messages for common mistakes) */
        for (auto&& dir : Global::config().getMany("include-dir")) {
            if (!existDir(dir)) throw std::runtime_error("include directory `" + dir + "` does not exist");
        }

        /* collect all macro definitions for the pre-processor */
        if (Global::config().has("macro")) {
            std::string currentMacro = "";
            std::string allMacros = "";
            for (const char& ch : Global::config().get("macro")) {
                if (ch == ' ') {
                    allMacros += " -D";
                    allMacros += currentMacro;
                    currentMacro = "";
                } else {
                    currentMacro += ch;
                }
            }
            allMacros += " -D" + currentMacro;
            Global::config().set("macro", allMacros);
        }

        if (Global::config().has("live-profile") && !Global::config().has("profile")) {
            Global::config().set("profile");
        }

        /* if index-stats is set then check that the profiler is also set */
        if (Global::config().has("index-stats")) {
            if (!Global::config().has("profile"))
                throw std::runtime_error("must be profiling to collect index-stats");
        }

    } catch (std::exception& e) {
        std::cerr << e.what() << std::endl;
        exit(EXIT_FAILURE);
    }

    /**
     * Ensure that code generation is enabled if using SWIG interface option.
     */
    if (Global::config().has("swig") && !Global::config().has("generate")) {
        Global::config().set("generate", simpleName(Global::config().get("")));
    }

    // ------ start souffle -------------

    const std::string souffleExecutable = which(argv[0]);

    if (souffleExecutable.empty()) {
        throw std::runtime_error("failed to determine souffle executable path");
    }

    /* Create the pipe to establish a communication between cpp and souffle */

    std::string cmd;

    if (Global::config().has("preprocessor")) {
        cmd = Global::config().get("preprocessor");
    } else {
        cmd = which("mcpp");
        if (isExecutable(cmd)) {
            cmd += " -e utf8 -W0";
        } else {
            cmd = which("gcc");
            if (isExecutable(cmd)) {
                cmd += " -x c -E";
            } else {
                std::cerr << "failed to locate mcpp or gcc pre-processors\n";
                throw std::runtime_error("failed to locate mcpp or gcc pre-processors");
            }
        }
    }

    cmd += " " + toString(join(Global::config().getMany("include-dir"), " ",
                         [&](auto&& os, auto&& dir) { tfm::format(os, "-I \"%s\"", dir); }));

    if (Global::config().has("macro")) {
        cmd += " " + Global::config().get("macro");
    }
    // Add RamDomain size as a macro
    cmd += " -DRAM_DOMAIN_SIZE=" + std::to_string(RAM_DOMAIN_SIZE);
    cmd += " \"" + Global::config().get("") + "\"";
#if defined(_MSC_VER)
    // cl.exe prints the input file name on the standard error stream,
    // we must silent it in order to preserve an empty error output
    // because Souffle test-suite is sensible to error outputs.
    cmd += " 2> nul";
#endif
    FILE* in = popen(cmd.c_str(), "r");

    /* Time taking for parsing */
    auto parser_start = std::chrono::high_resolution_clock::now();

    // ------- parse program -------------

    // parse file
    ErrorReport errReport(Global::config().has("no-warn"));
    DebugReport debugReport;
    Own<ast::TranslationUnit> astTranslationUnit =
            ParserDriver::parseTranslationUnit("<stdin>", in, errReport, debugReport);

    // close input pipe
    int preprocessor_status = pclose(in);
    if (preprocessor_status == -1) {
        perror(nullptr);
        throw std::runtime_error("failed to close pre-processor pipe");
    } else if (preprocessor_status != 0) {
        std::cerr << "Pre-processors command failed with code " << preprocessor_status << ": '" << cmd
                  << "'\n";
        throw std::runtime_error("Pre-processor command failed");
    }

    /* Report run-time of the parser if verbose flag is set */
    if (Global::config().has("verbose")) {
        auto parser_end = std::chrono::high_resolution_clock::now();
        std::cout << "Parse time: " << std::chrono::duration<double>(parser_end - parser_start).count()
                  << "sec\n";
    }

    auto hasShowOpt = [&](auto&&... kind) { return (Global::config().has("show", kind) || ...); };

    // `--show parse-errors` is special in that it (ab?)used the return code to specify the # of errors.
    //  Other `--show` modes can be used in conjunction with each other.
    if (hasShowOpt("parse-errors")) {
        if (1 < Global::config().getMany("show").size()) {
            std::cerr << "WARNING: `--show parse-errors` inhibits other `--show` actions.\n";
        }

        std::cout << astTranslationUnit->getErrorReport();
        return static_cast<int>(astTranslationUnit->getErrorReport().getNumErrors());
    }

    // ------- check for parse errors -------------
    astTranslationUnit->getErrorReport().exitIfErrors();

    // ------- rewriting / optimizations -------------

    /* set up additional global options based on pragma declaratives */
    (mk<ast::transform::PragmaChecker>())->apply(*astTranslationUnit);

    if (hasShowOpt("initial-ast", "initial-datalog")) {
        std::cout << astTranslationUnit->getProgram() << std::endl;
        // no other show options specified -> bail, we're done.
        if (Global::config().getMany("show").size() == 1) return 0;
    }

    /* construct the transformation pipeline */

    // Equivalence pipeline
    auto equivalencePipeline =
            mk<ast::transform::PipelineTransformer>(mk<ast::transform::NameUnnamedVariablesTransformer>(),
                    mk<ast::transform::FixpointTransformer>(mk<ast::transform::MinimiseProgramTransformer>()),
                    mk<ast::transform::ReplaceSingletonVariablesTransformer>(),
                    mk<ast::transform::RemoveRelationCopiesTransformer>(),
                    mk<ast::transform::RemoveEmptyRelationsTransformer>(),
                    mk<ast::transform::RemoveRedundantRelationsTransformer>());

    // Magic-Set pipeline
    auto magicPipeline = mk<ast::transform::PipelineTransformer>(
            mk<ast::transform::ConditionalTransformer>(
                    Global::config().has("magic-transform"), mk<ast::transform::ExpandEqrelsTransformer>()),
            mk<ast::transform::MagicSetTransformer>(), mk<ast::transform::ResolveAliasesTransformer>(),
            mk<ast::transform::RemoveRelationCopiesTransformer>(),
            mk<ast::transform::RemoveEmptyRelationsTransformer>(),
            mk<ast::transform::RemoveRedundantRelationsTransformer>(), clone(equivalencePipeline));

    // Partitioning pipeline
    auto partitionPipeline =
            mk<ast::transform::PipelineTransformer>(mk<ast::transform::NameUnnamedVariablesTransformer>(),
                    mk<ast::transform::PartitionBodyLiteralsTransformer>(),
                    mk<ast::transform::ReplaceSingletonVariablesTransformer>());

    // Provenance pipeline
    auto provenancePipeline = mk<ast::transform::ConditionalTransformer>(Global::config().has("provenance"),
            mk<ast::transform::PipelineTransformer>(mk<ast::transform::ExpandEqrelsTransformer>(),
                    mk<ast::transform::NameUnnamedVariablesTransformer>()));

    // Main pipeline
    auto pipeline = mk<ast::transform::PipelineTransformer>(mk<ast::transform::ComponentChecker>(),
            mk<ast::transform::ComponentInstantiationTransformer>(),
            mk<ast::transform::IODefaultsTransformer>(),
            mk<ast::transform::SimplifyAggregateTargetExpressionTransformer>(),
            mk<ast::transform::UniqueAggregationVariablesTransformer>(),
            mk<ast::transform::FixpointTransformer>(mk<ast::transform::PipelineTransformer>(
                    mk<ast::transform::ResolveAnonymousRecordAliasesTransformer>(),
                    mk<ast::transform::FoldAnonymousRecords>())),
            mk<ast::transform::SubsumptionQualifierTransformer>(), mk<ast::transform::SemanticChecker>(),
            mk<ast::transform::GroundWitnessesTransformer>(),
            mk<ast::transform::UniqueAggregationVariablesTransformer>(),
            mk<ast::transform::MaterializeSingletonAggregationTransformer>(),
            mk<ast::transform::FixpointTransformer>(
                    mk<ast::transform::MaterializeAggregationQueriesTransformer>()),
            mk<ast::transform::RemoveRedundantSumsTransformer>(),
            mk<ast::transform::NormaliseGeneratorsTransformer>(),
            mk<ast::transform::ResolveAliasesTransformer>(),
            mk<ast::transform::RemoveBooleanConstraintsTransformer>(),
            mk<ast::transform::ResolveAliasesTransformer>(), mk<ast::transform::MinimiseProgramTransformer>(),
            mk<ast::transform::InlineUnmarkExcludedTransform>(),
            mk<ast::transform::InlineRelationsTransformer>(), mk<ast::transform::GroundedTermsChecker>(),
            mk<ast::transform::ResolveAliasesTransformer>(),
            mk<ast::transform::RemoveRedundantRelationsTransformer>(),
            mk<ast::transform::RemoveRelationCopiesTransformer>(),
            mk<ast::transform::RemoveEmptyRelationsTransformer>(),
            mk<ast::transform::ReplaceSingletonVariablesTransformer>(),
            mk<ast::transform::FixpointTransformer>(mk<ast::transform::PipelineTransformer>(
                    mk<ast::transform::ReduceExistentialsTransformer>(),
                    mk<ast::transform::RemoveRedundantRelationsTransformer>())),
            mk<ast::transform::RemoveRelationCopiesTransformer>(), std::move(partitionPipeline),
            std::move(equivalencePipeline), mk<ast::transform::RemoveRelationCopiesTransformer>(),
            std::move(magicPipeline), mk<ast::transform::RemoveEmptyRelationsTransformer>(),
            mk<ast::transform::AddNullariesToAtomlessAggregatesTransformer>(),
            mk<ast::transform::ExecutionPlanChecker>(), std::move(provenancePipeline),
            mk<ast::transform::IOAttributesTransformer>());

    // Disable unwanted transformations
    if (Global::config().has("disable-transformers")) {
        std::vector<std::string> givenTransformers =
                splitString(Global::config().get("disable-transformers"), ',');
        pipeline->disableTransformers(
                std::set<std::string>(givenTransformers.begin(), givenTransformers.end()));
    }

    // Set up the debug report if necessary
    if (Global::config().has("debug-report")) {
        auto parser_end = std::chrono::high_resolution_clock::now();
        std::stringstream ss;

        // Add current time
        std::time_t time = std::time(nullptr);
        ss << "Executed at ";
        ss << std::put_time(std::localtime(&time), "%F %T") << "\n";

        // Add config
        ss << "(\n";
        ss << join(Global::config().data(), ",\n", [](std::ostream& out, const auto& arg) {
            out << "  \"" << arg.first << "\" -> \"" << arg.second << '"';
        });
        ss << "\n)";

        debugReport.addSection("Configuration", "Configuration", ss.str());

        // Add parsing runtime
        std::string runtimeStr =
                "(" + std::to_string(std::chrono::duration<double>(parser_end - parser_start).count()) + "s)";
        debugReport.addSection("Parsing", "Parsing " + runtimeStr, "");

        pipeline->setDebugReport();
    }

    // Toggle pipeline verbosity
    pipeline->setVerbosity(Global::config().has("verbose"));

    // Apply all the transformations
    pipeline->apply(*astTranslationUnit);

    // Output the transformed datalog (support alias opt name of 'datalog')
    if (hasShowOpt("transformed-ast", "transformed-datalog")) {
        std::cout << astTranslationUnit->getProgram() << std::endl;
    }

    // Output the precedence graph in graphviz dot format
    if (hasShowOpt("precedence-graph")) {
        astTranslationUnit->getAnalysis<ast::analysis::PrecedenceGraphAnalysis>().printHTML(std::cout);
        std::cout << std::endl;
    }

    // Output the precedence graph in text format
    if (hasShowOpt("precedence-graph-text")) {
        astTranslationUnit->getAnalysis<ast::analysis::PrecedenceGraphAnalysis>().print(std::cout);
        std::cout << std::endl;
    }

    // Output the scc graph in graphviz dot format
    if (hasShowOpt("scc-graph")) {
        astTranslationUnit->getAnalysis<ast::analysis::SCCGraphAnalysis>().printHTML(std::cout);
        std::cout << std::endl;
    }

    // Output the scc graph in text format
    if (hasShowOpt("scc-graph-text")) {
        astTranslationUnit->getAnalysis<ast::analysis::SCCGraphAnalysis>().print(std::cout);
        std::cout << std::endl;
    }

    // Output the type analysis
    if (hasShowOpt("type-analysis")) {
        astTranslationUnit->getAnalysis<ast::analysis::TypeAnalysis>().print(std::cout);
        std::cout << std::endl;
    }

    // bail if we've nothing else left to show
    if (Global::config().has("show") && !hasShowOpt("initial-ram", "transformed-ram")) return 0;

    // ------- execution -------------
    /* translate AST to RAM */
    debugReport.startSection();
    auto translationStrategy =
            Global::config().has("provenance")
                    ? mk<ast2ram::TranslationStrategy, ast2ram::provenance::TranslationStrategy>()
                    : mk<ast2ram::TranslationStrategy, ast2ram::seminaive::TranslationStrategy>();
    auto unitTranslator = Own<ast2ram::UnitTranslator>(translationStrategy->createUnitTranslator());
    auto ramTranslationUnit = unitTranslator->translateUnit(*astTranslationUnit);
    debugReport.endSection("ast-to-ram", "Translate AST to RAM");

    if (hasShowOpt("initial-ram")) {
        std::cout << ramTranslationUnit->getProgram();
        // bail if we've nothing else left to show
        if (!hasShowOpt("transformed-ram")) return 0;
    }

    // Apply RAM transforms
    {
        using namespace ram::transform;
        Own<Transformer> ramTransform = mk<TransformerSequence>(
                mk<LoopTransformer>(mk<TransformerSequence>(mk<ExpandFilterTransformer>(),
                        mk<HoistConditionsTransformer>(), mk<MakeIndexTransformer>())),
                mk<IfConversionTransformer>(), mk<IfExistsConversionTransformer>(),
                mk<CollapseFiltersTransformer>(), mk<TupleIdTransformer>(),
                mk<LoopTransformer>(
                        mk<TransformerSequence>(mk<HoistAggregateTransformer>(), mk<TupleIdTransformer>())),
                mk<ExpandFilterTransformer>(), mk<HoistConditionsTransformer>(),
                mk<CollapseFiltersTransformer>(), mk<EliminateDuplicatesTransformer>(),
                mk<ReorderConditionsTransformer>(), mk<LoopTransformer>(mk<ReorderFilterBreak>()),
                mk<ConditionalTransformer>(
                        // job count of 0 means all cores are used.
                        []() -> bool { return std::stoi(Global::config().get("jobs")) != 1; },
                        mk<ParallelTransformer>()),
                mk<ReportIndexTransformer>());

        ramTransform->apply(*ramTranslationUnit);
    }

    if (ramTranslationUnit->getErrorReport().getNumIssues() != 0) {
        std::cerr << ramTranslationUnit->getErrorReport();
    }

    // Output the transformed RAM program and return
    if (hasShowOpt("transformed-ram")) {
        std::cout << ramTranslationUnit->getProgram();
        return 0;
    }

    const bool execute_mode = Global::config().has("compile");
    const bool compile_mode = Global::config().has("dl-program");
    const bool generate_mode = Global::config().has("generate");

    const bool must_interpret =
            !execute_mode && !compile_mode && !generate_mode && !Global::config().has("swig");
    const bool must_execute = execute_mode;
    const bool must_compile = must_execute || compile_mode || Global::config().has("swig");

    try {
        if (must_interpret) {
            // ------- interpreter -------------

            std::thread profiler;
            // Start up profiler if needed
            if (Global::config().has("live-profile")) {
#ifdef _MSC_VER
                throw("No live-profile on Windows\n.");
#else
                profiler = std::thread([]() { profile::Tui().runProf(); });
#endif
            }

            // configure and execute interpreter
            Own<interpreter::Engine> interpreter(mk<interpreter::Engine>(*ramTranslationUnit));
            interpreter->executeMain();
            // If the profiler was started, join back here once it exits.
            if (profiler.joinable()) {
                profiler.join();
            }
            if (Global::config().has("provenance")) {
#ifdef _MSC_VER
                throw("No explain/explore provenance on Windows\n.");
#else
                // only run explain interface if interpreted
                interpreter::ProgInterface interface(*interpreter);
                if (Global::config().get("provenance") == "explain") {
                    explain(interface, false);
                } else if (Global::config().get("provenance") == "explore") {
                    explain(interface, true);
                }
#endif
            }
        } else {
            // ------- compiler -------------
            // int jobs = std::stoi(Global::config().get("jobs"));
            // jobs = (jobs <= 0 ? MAX_THREADS : jobs);
            auto synthesiser =
                    mk<synthesiser::Synthesiser>(/*static_cast<std::size_t>(jobs),*/ *ramTranslationUnit);

            // Find the base filename for code generation and execution
            std::string baseFilename;
            if (compile_mode) {
                baseFilename = Global::config().get("dl-program");
            } else if (generate_mode) {
                baseFilename = Global::config().get("generate");

                // trim .cpp extension if it exists
                if (baseFilename.size() >= 4 && baseFilename.substr(baseFilename.size() - 4) == ".cpp") {
                    baseFilename = baseFilename.substr(0, baseFilename.size() - 4);
                }
            } else {
                baseFilename = tempFile();
            }

            if (baseName(baseFilename) == "/" || baseName(baseFilename) == ".") {
                baseFilename = tempFile();
            }

            std::string baseIdentifier = identifier(simpleName(baseFilename));
            std::string sourceFilename = baseFilename + ".cpp";

            bool withSharedLibrary;
            auto synthesisStart = std::chrono::high_resolution_clock::now();
            const bool emitToStdOut = Global::config().has("generate", "-");
            if (emitToStdOut)
                synthesiser->generateCode(std::cout, baseIdentifier, withSharedLibrary);
            else {
                std::ofstream os{sourceFilename};
                synthesiser->generateCode(os, baseIdentifier, withSharedLibrary);
                os.close();
            }
            if (Global::config().has("verbose")) {
                auto synthesisEnd = std::chrono::high_resolution_clock::now();
                std::cout << "Synthesis time: "
                          << std::chrono::duration<double>(synthesisEnd - synthesisStart).count() << "sec\n";
            }

            if (withSharedLibrary) {
                if (!Global::config().has("libraries")) {
                    Global::config().set("libraries", "functors");
                }
                if (!Global::config().has("library-dir")) {
                    Global::config().set("library-dir", ".");
                }
            }

            if (must_compile) {
                /* Fail if a souffle-compile executable is not found */
                const auto souffle_compile = findTool("souffle-compile.py", souffleExecutable, ".");
                if (!souffle_compile) throw std::runtime_error("failed to locate souffle-compile.py");

                auto t_bgn = std::chrono::high_resolution_clock::now();
                compileToBinary(*souffle_compile, sourceFilename);
                auto t_end = std::chrono::high_resolution_clock::now();

                if (Global::config().has("verbose")) {
                    std::cout << "Compilation time: " << std::chrono::duration<double>(t_end - t_bgn).count()
                              << "sec\n";
                }
            }

            // run compiled C++ program if requested.
            if (must_execute) {
                std::string binaryFilename = baseFilename;
#if defined(_MSC_VER)
                binaryFilename += ".exe";
#endif
                executeBinaryAndExit(binaryFilename);
            }
        }
    } catch (std::exception& e) {
        std::cerr << e.what() << std::endl;
        std::exit(EXIT_FAILURE);
    }

    /* Report overall run-time in verbose mode */
    if (Global::config().has("verbose")) {
        auto souffle_end = std::chrono::high_resolution_clock::now();
        std::cout << "Total time: " << std::chrono::duration<double>(souffle_end - souffle_start).count()
                  << "sec\n";
    }

    return 0;
}

}  // end of namespace souffle

int main(int argc, char** argv) {
    return souffle::main(argc, argv);
}
