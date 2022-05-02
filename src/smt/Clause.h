/************************************************************************
 *
 * @file Clause.h
 *
 * Hosts the clauses appeared in the program
 *
 ***********************************************************************/

#pragma once

#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include "smt/Relation.h"
#include "smt/Typing.h"

namespace souffle::smt {

// forward declarations
class ClauseInstantiation;
class ClauseAnalyzer;

/**
 * An index that uniquely identifies a term in a clause
 */
class TermIndex : public Index {
    friend ClauseAnalyzer;

protected:
    explicit TermIndex(size_t index_) : Index(index_) {}
};

/**
 * An information package about a term
 */
struct Term {
    friend ClauseAnalyzer;

public:
    const TermIndex index;
    const std::vector<TermIndex> children;

protected:
    explicit Term(TermIndex index_, std::vector<TermIndex> children_)
            : index(index_), children(std::move(children_)) {}

public:
    virtual ~Term() = default;
};

// leaf nodes

struct TermConstBool : public Term {
    friend ClauseAnalyzer;

public:
    const bool value;

protected:
    TermConstBool(TermIndex index_, bool value_) : Term(index_, {}), value(value_) {}
};

struct TermConstNumber : public Term {
    friend ClauseAnalyzer;

public:
    int64_t value;

protected:
    TermConstNumber(TermIndex index_, int64_t value_) : Term(index_, {}), value(value_) {}
};

struct TermConstUnsigned : public Term {
    friend ClauseAnalyzer;

public:
    uint64_t value;

protected:
    TermConstUnsigned(TermIndex index_, uint64_t value_) : Term(index_, {}), value(value_) {}
};

struct TermVarNamed : public Term {
    friend ClauseAnalyzer;

public:
    const std::string name;

protected:
    TermVarNamed(TermIndex index_, std::string name_) : Term(index_, {}), name(std::move(name_)) {}
};

struct TermVarUnnamed : public Term {
    friend ClauseAnalyzer;

public:
    const ast::UnnamedVariable* ptr;

protected:
    TermVarUnnamed(TermIndex index_, const ast::UnnamedVariable* ptr_) : Term(index_, {}), ptr(ptr_) {}
};

struct TermIdent : public Term {
    friend ClauseAnalyzer;

public:
    const std::optional<TypeIndex> type;
    const std::string value;

protected:
    TermIdent(TermIndex index_, std::string value_)
            : Term(index_, {}), type(std::nullopt), value(std::move(value_)) {}

    TermIdent(TermIndex index_, TypeIndex type_, std::string value_)
            : Term(index_, {}), type(type_), value(std::move(value_)) {}
};

// recursive nodes

struct TermOp : public Term {
    friend ClauseAnalyzer;

public:
    const FunctorOp op;

protected:
    TermOp(TermIndex index_, FunctorOp op_, std::vector<TermIndex> children_)
            : Term(index_, children_), op(op_) {}
};

struct TermCtor : public Term {
    friend ClauseAnalyzer;

public:
    const TypeIndex adt;
    const std::string branch;

protected:
    TermCtor(TermIndex index_, TypeIndex adt_, std::string branch_, std::vector<TermIndex> children_)
            : Term(index_, children_), adt(adt_), branch(std::move(branch_)) {}
};

struct TermAtom : public Term {
    friend ClauseAnalyzer;

public:
    const RelationIndex relation;

protected:
    TermAtom(TermIndex index_, RelationIndex relation_, std::vector<TermIndex> children_)
            : Term(index_, children_), relation(relation_) {}
};

struct TermNegation : public Term {
    friend ClauseAnalyzer;

protected:
    TermNegation(TermIndex index_, TermIndex atom) : Term(index_, {atom}) {}
};

struct TermConstraint : public Term {
    friend ClauseAnalyzer;

public:
    const BinaryConstraintOp op;

protected:
    TermConstraint(TermIndex index_, BinaryConstraintOp op_, TermIndex lhs, TermIndex rhs)
            : Term(index_, {lhs, rhs}), op(op_) {}
};

/**
 * An instantiation of one clause
 */
class ClauseInstantiation {
    friend ClauseAnalyzer;

public:
    // environment
    const std::map<std::string, TypeIndex> vars_named;
    const std::map<const ast::UnnamedVariable*, TypeIndex> vars_unnamed;
    const std::map<const ast::UnnamedVariable*, std::string> anon_names;

protected:
    ClauseInstantiation(std::map<std::string, TypeIndex> vars_named_,
            std::map<const ast::UnnamedVariable*, TypeIndex> vars_unnamed_)
            : vars_named(std::move(vars_named_)), vars_unnamed(std::move(vars_unnamed_)),
              anon_names(create_anon_names(vars_named, vars_unnamed)) {}

private:
    static std::map<const ast::UnnamedVariable*, std::string> create_anon_names(
            const std::map<std::string, TypeIndex>& vars_named,
            const std::map<const ast::UnnamedVariable*, TypeIndex>& vars_unnamed) {
        std::map<const ast::UnnamedVariable*, std::string> result;
        unsigned counter = 0;
        for (const auto& [key, val] : vars_unnamed) {
            std::string name = "$anon" + std::to_string(counter);
            auto it = vars_named.find(name);
            assert(it == vars_named.end());
            result.emplace(key, name);
        }
        return result;
    }
};

/**
 * Holds the sequence of AST reconstruction
 */
class ConstructionOrder {
    friend ClauseAnalyzer;

public:
    const std::vector<const Term*> head;
    const std::vector<std::vector<const Term*>> body;

protected:
    ConstructionOrder(std::vector<const Term*> head_, std::vector<std::vector<const Term*>> body_)
            : head(std::move(head_)), body(std::move(body_)) {}
};

/**
 * A registry of terms appeared in one clause
 */
class ClauseAnalyzer {
private:
    // environment
    const ast::analysis::TypeAnalysis& typing;
    const TypeRegistry& typeRegistry;
    const RelationRegistry& relationRegistry;

    // counter
    size_t counter = 1;

protected:
    // bounded variables
    std::map<std::string, ast::analysis::TypeSet> vars_named{};
    std::map<const ast::UnnamedVariable*, ast::analysis::TypeSet> vars_unnamed{};

    // term registry
    std::map<TermIndex, std::unique_ptr<Term>> terms;

    // structure
    TermIndex head{0};
    std::vector<TermIndex> body;

public:
    ClauseAnalyzer(const ast::Clause* clause, const ast::analysis::TypeAnalysis& typing_,
            const TypeRegistry& typeRegistry_, const RelationRegistry& relationRegistry_)
            : typing(typing_), typeRegistry(typeRegistry_), relationRegistry(relationRegistry_) {
        // the heavy lifting
        analyze_clause(clause);

        // fixup the missing type inference for idents (i.e., string constants)
        infer_ident_type_for_string_constant();

        // sanity check: a fact cannot have free variables (named or unnamed)
        if (body.empty()) {
            assert(vars_named.empty());
            assert(vars_unnamed.empty());
        }
    }

public:
    RelationIndex get_head() const {
        auto atom = dynamic_cast<const TermAtom*>(terms.at(head).get());
        return atom->relation;
    }

    std::vector<RelationIndex> get_deps() const {
        std::vector<RelationIndex> result;
        for (const auto& i : body) {
            if (auto atom = dynamic_cast<const TermAtom*>(terms.at(i).get())) {
                result.push_back(atom->relation);
            }
        }
        return result;
    }

    std::vector<ClauseInstantiation> create_instantiations() const {
        // build a set of clause registries based on types assigned to each variable
        std::map<std::string, std::vector<TypeIndex>> sorts_for_named_vars;
        for (const auto& [key, val] : vars_named) {
            sorts_for_named_vars.emplace(key, typeRegistry.typeset_to_indices(val));
        }
        auto combo_named = cartesian_distribute(sorts_for_named_vars);

        std::map<const ast::UnnamedVariable*, std::vector<TypeIndex>> sorts_for_unnamed_vars;
        for (const auto& [key, val] : vars_unnamed) {
            sorts_for_unnamed_vars.emplace(key, typeRegistry.typeset_to_indices(val));
        }
        auto combo_unnamed = cartesian_distribute(sorts_for_unnamed_vars);

        std::vector<ClauseInstantiation> registries;
        for (auto item_named : combo_named) {
            for (auto item_unnamed : combo_unnamed) {
                auto registry = ClauseInstantiation(item_named, item_unnamed);
                registries.push_back(registry);
            }
        }
        return registries;
    }

    ConstructionOrder create_sequence() const {
        const auto head_seq = visit_terms(head);
        std::vector<std::vector<const Term*>> body_seqs;
        for (const auto& item : body) {
            body_seqs.emplace_back(visit_terms(item));
        }
        return ConstructionOrder(head_seq, body_seqs);
    }

private:
    /// Create a new index
    TermIndex new_index() {
        return TermIndex(counter++);
    }

private:
    template <typename T, typename... ARGS>
    TermIndex register_term(ARGS... args) {
        auto index = new_index();
        terms.emplace(index, new T(index, args...));
        return index;
    }

private:
    TermIndex analyze_clause_argument(const ast::Argument* arg) {
        // rule: each argument must have a finite set of types, i.e., not the universal type
        auto typeset = typing.getTypes(arg);
        assert(!typeset.empty() && !typeset.isAll());

        // filter out unsupported cases
        if (dynamic_cast<const ast::TypeCast*>(arg)) {
            throw std::runtime_error("Type casts are not supported yet");
        }
        if (dynamic_cast<const ast::Aggregator*>(arg)) {
            throw std::runtime_error("Aggregators are not supported yet");
        }
        if (dynamic_cast<const ast::Counter*>(arg)) {
            throw std::runtime_error("Counters are not supported yet");
        }
        if (dynamic_cast<const ast::UserDefinedFunctor*>(arg)) {
            throw std::runtime_error("User-defined functors are not supported yet");
        }

        // filter out prohibited cases
        if (dynamic_cast<const ast::NilConstant*>(arg)) {
            throw std::runtime_error("Nil constant (for record construction) is prohibited");
        }

        // constants
        if (const auto arg_const_num = dynamic_cast<const ast::NumericConstant*>(arg)) {
            assert(typeset.size() == 1);
            const auto attrs = typing.getTypeAttributes(arg_const_num);
            assert(attrs.size() == 1);

            const auto num_type = typing.getPolymorphicNumericConstantType(*arg_const_num);
            switch (num_type) {
                case ast::NumericConstant::Type::Int: {
                    return register_term<TermConstNumber>(std::stoll(arg_const_num->getConstant()));
                }
                case ast::NumericConstant::Type::Uint: {
                    return register_term<TermConstUnsigned>(std::stoull(arg_const_num->getConstant()));
                }
                case ast::NumericConstant::Type::Float: {
                    throw new std::runtime_error("The float type is not supported");
                }
            }
        }
        if (const auto arg_const_str = dynamic_cast<const ast::StringConstant*>(arg)) {
            assert(typeset.size() == 1);
            const auto attrs = typing.getTypeAttributes(arg_const_str);
            assert(attrs.size() == 1);
            assert(*attrs.begin() == TypeAttribute::Symbol);

            return register_term<TermIdent>(arg_const_str->getConstant());
        }

        // variables
        if (const auto arg_var = dynamic_cast<const ast::Variable*>(arg)) {
            // implicit assert: all inferred types for variables should appear in the type registry as well
            auto converted = typeRegistry.typeset_to_indices(typeset);
            assert(converted.size() == typeset.size());

            // save it to variable registry if we haven't seen it yet
            auto it = vars_named.find(arg_var->getName());
            if (it == vars_named.end()) {
                vars_named.emplace(arg_var->getName(), typeset);
            } else {
                assert(it->second == typeset);
            }

            // save the term to registry
            return register_term<TermVarNamed>(arg_var->getName());
        }

        if (const auto arg_ignored = dynamic_cast<const ast::UnnamedVariable*>(arg)) {
            // implicit assert: all inferred types for variables should appear in the type registry as well
            auto converted = typeRegistry.typeset_to_indices(typeset);
            assert(converted.size() == typeset.size());

            // save it to variable registry
            auto const [_, inserted] = vars_unnamed.emplace(arg_ignored, typeset);
            assert(inserted);

            // save the term to registry
            return register_term<TermVarUnnamed>(arg_ignored);
        }

        // terms
        if (const auto arg_term = dynamic_cast<const ast::Term*>(arg)) {
            std::vector<TermIndex> child_terms;
            for (const auto sub_arg : arg_term->getArguments()) {
                child_terms.push_back(analyze_clause_argument(sub_arg));
            }

            // (intrinsic) functors
            if (const auto arg_functor = dynamic_cast<const ast::IntrinsicFunctor*>(arg)) {
                const auto op = typing.getPolymorphicOperator(*arg_functor);
                return register_term<TermOp>(op, child_terms);
            }

            // record ctor
            if (const auto arg_record = dynamic_cast<const ast::RecordInit*>(arg)) {
                assert(typeset.size() == 1);

                // rule: the constructed type should appear in the type registry as well
                auto index = typeRegistry.retrieve_type(typeset.begin()->getName().toString());
                const auto& adt_decl = typeRegistry.retrieve_adt(index);

                assert(adt_decl.branches.size() == 1);
                const auto& branch_decl = adt_decl.branches[0];
                assert(branch_decl.name == "");

                // cascade down to arguments
                const auto& sub_args = arg_term->getArguments();
                assert(branch_decl.fields.size() == sub_args.size());

                // register the term
                return register_term<TermCtor>(index, branch_decl.name, child_terms);
            }

            // branch ctor
            if (const auto arg_branch = dynamic_cast<const ast::BranchInit*>(arg)) {
                assert(typeset.size() == 1);
                // rule: the constructed type should appear in the type registry as well
                auto index = typeRegistry.retrieve_type(typeset.begin()->getName().toString());
                auto adt_decl = typeRegistry.retrieve_adt(index);

                // lookup the branch decl
                const ADTBranch* branch_decl = nullptr;
                for (auto const& adt_branch : adt_decl.branches) {
                    if (adt_branch.name == arg_branch->getBranchName().toString()) {
                        branch_decl = &adt_branch;
                    }
                }
                assert(branch_decl != nullptr);

                // cascade down to arguments
                const auto& sub_args = arg_term->getArguments();
                assert(branch_decl->fields.size() == sub_args.size());

                // register the term
                return register_term<TermCtor>(index, branch_decl->name, child_terms);
            }

            // catch all
            throw std::runtime_error("Unknown term type");
        }

        // catch all
        throw std::runtime_error("Unknown argument type");
    }

    TermIndex analyze_clause_atom(const ast::Atom* atom) {
        auto index = relationRegistry.retrieve_relation(atom->getQualifiedName().toString());

        // retrieve the type information for each argument
        const auto details = relationRegistry.retrieve_details(index);
        assert(details.domains.size() == atom->getArity());
        const auto args = atom->getArguments();
        assert(details.domains.size() == args.size());

        // iterate over args
        std::vector<TermIndex> child_terms;
        for (auto arg : atom->getArguments()) {
            child_terms.push_back(analyze_clause_argument(arg));
        }

        // register the atom as a term
        return register_term<TermAtom>(index, child_terms);
    }

    /// Analyze one clause, perform sanity checks while collecting information
    void analyze_clause(const ast::Clause* clause) {
        // head
        head = analyze_clause_atom(clause->getHead());

        // body
        for (const auto* literal : clause->getBodyLiterals()) {
            if (dynamic_cast<const ast::FunctionalConstraint*>(literal)) {
                throw std::runtime_error("Functional constraints not supported yet");
            }

            if (auto literal_bool = dynamic_cast<const ast::BooleanConstraint*>(literal)) {
                const auto term = register_term<TermConstBool>(literal_bool->isTrue());
                body.push_back(term);
                continue;
            }
            if (auto literal_bin = dynamic_cast<const ast::BinaryConstraint*>(literal)) {
                const auto lhs = analyze_clause_argument(literal_bin->getLHS());
                const auto rhs = analyze_clause_argument(literal_bin->getRHS());
                const auto op = typing.getPolymorphicOperator(*literal_bin);
                const auto term = register_term<TermConstraint>(op, lhs, rhs);
                body.push_back(term);
                continue;
            }
            if (auto literal_atom = dynamic_cast<const ast::Atom*>(literal)) {
                const auto term = analyze_clause_atom(literal_atom);
                body.push_back(term);
                continue;
            }
            if (auto literal_negation = dynamic_cast<const ast::Negation*>(literal)) {
                const auto sub = analyze_clause_atom(literal_negation->getAtom());
                const auto term = register_term<TermNegation>(sub);
                body.push_back(term);
                continue;
            }

            // we should have covered all literal types
            throw std::runtime_error("Unknown literal type");
        }
    }

private:
    void reverse_link_terms_recursive(const TermIndex& index, std::map<TermIndex, TermIndex>& links) const {
        const auto& term = terms.at(index);
        for (const auto& child : term->children) {
            reverse_link_terms_recursive(child, links);
            const auto& [_, inserted] = links.emplace(child, index);
            assert(inserted);
        }
    }

    std::map<TermIndex, TermIndex> reverse_link_terms() const {
        std::map<TermIndex, TermIndex> links;
        reverse_link_terms_recursive(head, links);
        for (const auto& index : body) {
            reverse_link_terms_recursive(index, links);
        }
        return links;
    }

    /// The type analysis of Souffle does not fully infer the type for string constants.
    /// This function tries to infer the identifier type for a string constant.
    void infer_ident_type_for_string_constant() {
        const auto parents = reverse_link_terms();

        // infer types
        std::map<TermIndex, TypeIndex> term_ident_types;
        for (auto& [key, val] : terms) {
            if (dynamic_cast<TermIdent*>(val.get())) {
                // retrieve the parent type
                const auto parent = terms.at(parents.at(key)).get();

                if (auto term_atom = dynamic_cast<const TermAtom*>(parent)) {
                    unsigned pos = 0;
                    for (const auto& child : term_atom->children) {
                        if (child != key) {
                            pos++;
                            continue;
                        }

                        // found the child, retrieve its type declaration
                        const auto& details = relationRegistry.retrieve_details(term_atom->relation);
                        const auto& ident_name = typeRegistry.retrieve_ident(details.domains[pos]);
                        auto ident_type = typeRegistry.retrieve_type(ident_name);

                        // host the information in another dat structure
                        const auto& [_, inserted] = term_ident_types.emplace(key, ident_type);
                        assert(inserted);
                        break;
                    }

                    // must be able to find
                    assert(pos != term_atom->children.size());
                    continue;
                }

                // currently, the parent must be an atom type
                throw std::runtime_error("Unexpected term type for a TermIdent");
            }
        }

        // fixup the terms
        for (const auto& [key, val] : term_ident_types) {
            auto old_term = dynamic_cast<const TermIdent*>(terms.at(key).get());
            auto new_term = new TermIdent(old_term->index, val, old_term->value);
            terms.erase(key);
            terms.emplace(key, new_term);
        }
    }

private:
    void visit_terms_recursive(const TermIndex& index, std::vector<const Term*>& sequence) const {
        const auto& term = terms.at(index);
        for (const auto& child : term->children) {
            visit_terms_recursive(child, sequence);
        }
        sequence.push_back(term.get());
    }

    std::vector<const Term*> visit_terms(const TermIndex& index) const {
        std::vector<const Term*> sequence;
        visit_terms_recursive(index, sequence);
        return sequence;
    }
};

/**
 * A registry of clauses appeared in the whole program
 */
class ClauseRegistry {
    friend Frontend;

private:
    // environment
    const TypeRegistry& typeRegistry;
    const RelationRegistry& relationRegistry;

protected:
    // registry
    std::map<RelationIndex, std::vector<ClauseAnalyzer>> mapping{};

    // relation registration sequence
    std::list<SCC<RelationIndex>> sequence{};

public:
    ClauseRegistry(const ast::TranslationUnit& unit, const TypeRegistry& typeRegistry_,
            const RelationRegistry& relationRegistry_)
            : typeRegistry(typeRegistry_), relationRegistry(relationRegistry_) {
        // add rules and their dependencies
        Graph<RelationIndex> dep_graph;

        const auto& program = unit.getProgram();
        const auto& type_analysis = unit.getAnalysis<ast::analysis::TypeAnalysis>();
        for (const auto clause : program.getClauses()) {
            const auto relation =
                    relationRegistry.retrieve_relation(clause->getHead()->getQualifiedName().toString());
            mapping[relation].emplace_back(clause, type_analysis, typeRegistry, relationRegistry);
        }

        // populate the nodes in the dep graph
        for (const auto& [_, val] : relationRegistry.mapping) {
            dep_graph.addNode(val.first);
        }

        // populate the edges in the dep graph
        for (const auto& [_, val] : mapping) {
            for (const auto& analyzer : val) {
                const auto head = analyzer.get_head();
                for (const auto& dep : analyzer.get_deps()) {
                    dep_graph.addEdge(head, dep);
                }
            }
        }
        sequence = dep_graph.deriveSCC();
    }
};

}  // namespace souffle::smt