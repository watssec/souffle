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

public:
    void add_neighbour(vertex* v) {
        neighbours_.push_back(v);
    }

    const T& get_data() {
        return data_;
    }

private:
    const T data_;
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
class SCC {
public:
    const std::set<T> nodes;
    const bool is_cyclic;

public:
    SCC(std::set<T> nodes_, bool is_cyclic_) : nodes(std::move(nodes_)), is_cyclic(is_cyclic_) {}
};

template <typename T>
class Graph {
protected:
    // graph representation in adjacency list
    std::map<T, std::set<T>> graph;

public:  // Graph construction
    void addNode(T key) {
        const auto it = graph.find(key);
        assert(it == graph.end());
        graph[key] = {};
    }

    void addEdge(T src, T dst) {
        const auto it_src = graph.find(src);
        assert(it_src != graph.end());
        const auto it_dst = graph.find(dst);
        assert(it_dst != graph.end());
        graph[src].insert(dst);
    }

public:  // SCC
    std::list<SCC<T>> deriveSCC() const {
        algorithm::tarjan::graph<T> g;
        std::map<T, algorithm::tarjan::vertex<T>*> v;
        for (const auto [key, _] : graph) {
            v[key] = g.add_vertex(key);
        }
        for (const auto [key, neighbors] : graph) {
            for (const auto n : neighbors) {
                v[key]->add_neighbour(v[n]);
            }
        }

        std::list<SCC<T>> result;
        algorithm::tarjan::runner<T> tarjan;
        for (auto&& component : tarjan.run(g)) {
            std::set<T> scc;
            for (auto node : component) {
                scc.insert(node->get_data());
            }
            bool is_cyclic = scc.size() != 1 || graph.at(*scc.begin()).empty();
            result.emplace_back(scc, is_cyclic);
        }
        return result;
    }
};

template <typename T>
std::vector<std::vector<T>> cartesian_product(const std::vector<std::vector<T>>& v) {
    std::vector<std::vector<T>> s = {{}};
    for (const auto& u : v) {
        std::vector<std::vector<T>> r;
        for (const auto& x : s) {
            for (const auto y : u) {
                r.push_back(x);
                r.back().push_back(y);
            }
        }
        s = move(r);
    }
    return s;
}

template <typename K, typename V>
std::vector<std::map<K, V>> cartesian_distribute(const std::map<K, std::vector<V>>& table) {
    std::vector<K> vec_keys;
    std::vector<std::vector<V>> vec_vals;
    for (const auto& [key, val] : table) {
        vec_vals.emplace_back(val);
        vec_keys.emplace_back(key);
    }

    std::vector<std::map<K, V>> result;
    for (auto item : cartesian_product(vec_vals)) {
        assert(vec_keys.size() == item.size());
        std::map<K, V> instance;
        for (unsigned i = 0; i < vec_keys.size(); i++) {
            instance.emplace(vec_keys[i], item[i]);
        }
        result.push_back(instance);
    }
    return result;
}

/**
 * An index that uniquely identifies something
 */
class Index {
private:
    size_t index;

protected:
    explicit Index(size_t index_) : index(index_) {}

protected:
    friend bool operator==(const Index& lhs, const Index& rhs) {
        return lhs.index == rhs.index;
    }
    friend bool operator!=(const Index& lhs, const Index& rhs) {
        return lhs.index != rhs.index;
    }
    friend bool operator>(const Index& lhs, const Index& rhs) {
        return lhs.index > rhs.index;
    }
    friend bool operator<(const Index& lhs, const Index& rhs) {
        return lhs.index < rhs.index;
    }
};

}  // namespace souffle::smt