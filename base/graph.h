#ifndef ICARUS_BASE_GRAPH_H
#define ICARUS_BASE_GRAPH_H

#include <queue>
#include <unordered_map>
#include <unordered_set>

namespace base {

template <typename T>
struct Graph {
  void add_node(const T& node) { adj_lists_[node]; };

  void add_edge(const T& from, const T& to) {
    adj_lists_[to];
    adj_lists_[from].insert(to);
  }

  std::unordered_set<T> const& at(T const& t) const { return adj_lists_.at(t); }

  template <typename Fn>
  void topologically(Fn&& fn) const {
    auto adj_list = adj_lists_;

    base::Graph<T> reverse;
    for (auto const& [key, vals] : adj_list) {
      reverse.add_node(key);
      for (auto const& val : vals) { reverse.add_edge(val, key); }
    }

    std::queue<T> empty_keys;
    for (auto& [key, vals] : adj_list) {
      if (vals.empty()) { empty_keys.push(key); }
    }

    while (!empty_keys.empty()) {
      auto const& key = empty_keys.front();
      fn(key);
      for (auto val : reverse.adj_lists_.at(key)) {
        auto& adjs = adj_list.at(val);
        adjs.erase(key);
        if (adjs.empty()) { empty_keys.push(val); }
      }
      reverse.adj_lists_.erase(key);
      adj_list.erase(key);
      empty_keys.pop();
    }
  }

  std::unordered_set<T> sink_deps(T const& t) {
    std::unordered_set<T> results;
    insert_sink_deps(t, &results);
    return results;
  }

  size_t num_nodes() const { return adj_lists_.size(); }

 private:
  void insert_sink_deps(T const& t, std::unordered_set<T>* results) {
    for (auto const& n : adj_lists_.at(t)) {
      if (adj_lists_.at(n).empty()) {
        results->insert(n);
      } else {
        insert_sink_deps(n, results);
      }
    }
  }

  // adjacency lists will be typically short, so probably better to use a flat
  // map structure.
  std::unordered_map<T, std::unordered_set<T>> adj_lists_;
};

}  // namespace base

#endif  // ICARUS_BASE_GRAPH
