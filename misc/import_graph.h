#ifndef ICARUS_IMPORT_GRAPH_H
#define ICARUS_IMPORT_GRAPH_H

#include <filesystem>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"

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

  absl::flat_hash_set<std::filesystem::path, PathHasher> all_paths_;
  absl::flat_hash_map<std::filesystem::path const *,
                      absl::flat_hash_set<std::filesystem::path const *>>
      import_deps_;
};

#endif  // ICARUS_IMPORT_GRAPH_H
