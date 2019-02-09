#ifndef ICARUS_IMPORT_GRAPH_H
#define ICARUS_IMPORT_GRAPH_H

#include <filesystem>
#include <unordered_set>
#include <unordered_map>

struct ImportGraph {
 public:
  std::pair<std::filesystem::path const *, bool> node(
      std::filesystem::path const &p);

  bool AddDependency(std::filesystem::path const *dependee,
                     std::filesystem::path const *depender);
 private:
  struct PathHasher {
    size_t operator()(std::filesystem::path const &p) const {
      return std::filesystem::hash_value(p);
    }
  };

  std::unordered_set<std::filesystem::path, PathHasher> all_paths_;
  std::unordered_map<std::filesystem::path const *,
                      std::unordered_set<std::filesystem::path const *>>
      import_deps_;
};

#endif  // ICARUS_IMPORT_GRAPH_H
