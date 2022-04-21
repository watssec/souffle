/************************************************************************
 *
 * @file Utils.h
 *
 * Utilities used in the SMT translation process.
 *
 ***********************************************************************/

#pragma once

#include <map>
#include <set>
#include <stack>
#include <vector>

namespace souffle::smt {

template <typename T>
class Graph {
protected:
    // graph representation in adjacency list
    std::map<const T*, std::set<const T*>> graph;

public:
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
};

}  // namespace souffle::smt