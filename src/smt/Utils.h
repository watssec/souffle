/************************************************************************
 *
 * @file Utils.h
 *
 * Utilities used in the SMT translation process.
 *
 ***********************************************************************/

#pragma once

#include <list>
#include <map>
#include <set>
#include <stack>
#include <vector>

namespace souffle::smt {

/// Implementation of the Tarjan's SCC algorithm
namespace algorithm::tarjan {

struct noncopyable {
    noncopyable() = default;
    noncopyable(const noncopyable&) = delete;
    noncopyable& operator=(const noncopyable&) = delete;
};

template <typename T>
class runner;

template <typename T>
class vertex : private noncopyable {
    friend runner<T>;

public:
    explicit vertex(const T& t) : data_(t) {}
    void add_neighbour(vertex* v) {
        neighbours_.push_back(v);
    }
    void add_neighbours(const std::initializer_list<vertex*>& vs) {
        neighbours_.insert(neighbours_.end(), vs);
    }
    const T& get_data() {
        return data_;
    }

private:
    T data_;
    int index_ = -1;
    int lowlink_ = -1;
    bool on_stack_ = false;
    std::vector<vertex*> neighbours_;
};

template <typename T>
class graph : private noncopyable {
    friend runner<T>;

public:
    vertex<T>* add_vertex(const T& t) {
        vertexes_.emplace_back(t);
        return &vertexes_.back();
    }

private:
    std::list<vertex<T>> vertexes_;
};

template <typename T>
class runner : private noncopyable {
public:
    using component = std::vector<vertex<T>*>;
    std::list<component> run(graph<T>& graph) {
        index_ = 0;
        stack_.clear();
        strongly_connected_.clear();
        for (auto& v : graph.vertexes_) {
            if (v.index_ == -1) strong_connect(&v);
        }
        return strongly_connected_;
    }

private:
    void strong_connect(vertex<T>* v) {
        v->index_ = index_;
        v->lowlink_ = index_;
        ++index_;
        stack_.push_back(v);
        v->on_stack_ = true;
        for (auto w : v->neighbours_) {
            if (w->index_ == -1) {
                strong_connect(w);
                v->lowlink_ = std::min(v->lowlink_, w->lowlink_);
            } else if (w->on_stack_) {
                v->lowlink_ = std::min(v->lowlink_, w->index_);
            }
        }
        if (v->lowlink_ == v->index_) {
            strongly_connected_.push_back(component());
            component& c = strongly_connected_.back();
            for (;;) {
                auto w = stack_.back();
                stack_.pop_back();
                w->on_stack_ = false;
                c.push_back(w);
                if (w == v) break;
            }
        }
    }

private:
    int index_ = 0;
    std::list<vertex<T>*> stack_;
    std::list<component> strongly_connected_;
};

}  // namespace algorithm::tarjan

template <typename T>
class Graph {
protected:
    // graph representation in adjacency list
    std::map<const T*, std::set<const T*>> graph;

public:  // Graph construction
    void addNode(const T* key) {
        const auto it = graph.find(key);
        assert(it == graph.end());
        graph[key] = {};
    }

    void addEdge(const T* src, const T* dst) {
        const auto it_src = graph.find(src);
        assert(it_src != graph.end());
        const auto it_dst = graph.find(dst);
        assert(it_dst != graph.end());
        graph[src].insert(dst);
    }

    std::vector<const T*> allNodes() {
        std::vector<const T*> result;
        for (const auto [key, _] : graph) {
            result.push_back(key);
        }
        return result;
    }

public:  // SCC details
};

}  // namespace souffle::smt