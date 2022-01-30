#ifndef ICARUS_FRONTEND_SOURCE_INDEXER_H
#define ICARUS_FRONTEND_SOURCE_INDEXER_H

#include <memory>
#include <string>
#include <string_view>

#include "absl/container/flat_hash_map.h"
#include "ir/value/module_id.h"

namespace frontend {

// `SourceIndexer` holds source text along with lazily computed data about the
// source (e.g., offsets of line numbers). Source is held in such a way that,
// once inserted, the contents have a stable address and can be referenced for
// the lifetime of the `SourceIndexer`.
struct SourceIndexer {
  std::string_view insert(ir::ModuleId module, std::string content) {
    auto [iter, inserted] = source_.try_emplace(module);
    if (inserted) {
      iter->second          = std::make_unique<Entry>();
      iter->second->content = std::move(content);
    }
    return iter->second->content;
  }

 private:
  // TODO: Implementation-wise, it's actually rather unfortunate that have the
  // unique_ptr and the string-indirection. Almost all uses of std::string for
  // the source content will not fit in the small-string-optimizated space and
  // therefore already have buffers that are safe to reference even as `source_`
  // is added to. The structure we really want here is std::unique_ptr<char[]>,
  // but that puts unfortunate burden on the API itself.
  struct Entry {
    std::string content;
  };
  absl::flat_hash_map<ir::ModuleId, std::unique_ptr<Entry>> source_;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_SOURCE_INDEXER_H
