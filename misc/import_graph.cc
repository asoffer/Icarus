#include "import_graph.h"

#include <queue>

std::pair<std::filesystem::path const *, bool> ImportGraph::node(
    std::filesystem::path const &p) {
  std::error_code ec;
  auto canonical_path = std::filesystem::canonical(p, ec);
  if (ec) { return std::pair(nullptr, false); }
  auto[iter, newly_inserted] = all_paths_.insert(canonical_path);
  return std::pair(&*iter, newly_inserted);
}

bool ImportGraph::AddDependency(std::filesystem::path const *dependee,
                                std::filesystem::path const *depender) {
  if (depender == dependee) { return false; }
  std::unordered_set<std::filesystem::path const *> handled;
  std::queue<std::filesystem::path const *> to_process;
  to_process.push(dependee);
  while (!to_process.empty()) {
    auto elem = to_process.front();
    if (elem == depender) { return false; }
    auto[iter, newly_inserted] = handled.insert(elem);
    to_process.pop();
    if (newly_inserted) {
      auto deps_iter = import_deps_.find(elem);
      if (deps_iter != import_deps_.end()) {
        for (auto path : deps_iter->second) { to_process.push(path); }
      }
    }
  }
  import_deps_[depender].insert(dependee);
  return true;
}
