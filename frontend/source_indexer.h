#ifndef ICARUS_FRONTEND_SOURCE_INDEXER_H
#define ICARUS_FRONTEND_SOURCE_INDEXER_H

#include <memory>
#include <span>
#include <string>
#include <string_view>

#include "absl/container/flat_hash_map.h"
#include "data_types/module_id.h"

namespace frontend {

// `SourceIndexer` holds source text along with lazily computed data about the
// source (e.g., offsets of line numbers). Source is held in such a way that,
// once inserted, the contents have a stable address and can be referenced for
// the lifetime of the `SourceIndexer`.
struct SourceIndexer {
  // If `module` is not already present in the indexer, inserts `module` and
  // associates `content` with it. Regardless, the returned `std::string_view`
  // is a view of the content associated with `module`. The returned
  // `std::string_view` is valid for the lifetime of `*this`.
  std::string_view insert(data_types::ModuleId module, std::string &&content);
  std::string_view insert(data_types::ModuleId module, std::string_view content);
  std::string_view insert(data_types::ModuleId module, char const *content) {
    return insert(module, std::string_view(content));
  }

  struct Entry {
    Entry(Entry const &)            = delete;
    Entry(Entry &&)                 = delete;
    Entry &operator=(Entry const &) = delete;
    Entry &operator=(Entry &&)      = delete;

    explicit Entry(data_types::ModuleId module, std::string content)
        : module_(module), content_(std::move(content)) {}

    data_types::ModuleId module_id() const { return module_; }

    // Returns a stable view of the file contents.
    std::string_view content() const { return content_; }

    // Returns a view of line number `n` (one-indexed), excluding any newline or
    // null-terminator.
    std::string_view line(size_t n);

    // Returns a (one-indexed) line number corresponding to the line containing
    // the character pointed to by `p`.
    size_t line_containing(char const *p);

    // Returns a pair of (one-indexed) line numbers corresponding to the
    // half-open range of lines that contain `content`.
    std::pair<size_t, size_t> lines_containing(std::string_view content);

   private:
    // Returns a span of offsets into `this->content()` determining the indices
    // in `content_` of the first character of each line of text.
    std::span<size_t const> line_starts();

    std::optional<std::vector<size_t>> line_terminators_;
    const data_types::ModuleId module_;
    const std::string content_;
  };
  // Given a `std::string_view` referring to content in one of the source texts
  // managed by this indexer, returns the entry for the source containing this
  // range of content.
  Entry &EntryFor(std::string_view subcontent);

 private:
  // TODO: Implementation-wise, it's actually rather unfortunate that have the
  // unique_ptr and the string-indirection in `Entry`. Almost all uses of
  // std::string for the source content will not fit in the
  // small-string-optimizated space and therefore already have buffers that are
  // safe to reference even as `source_` is added to. The structure we really
  // want here is std::unique_ptr<char[]>, but that puts unfortunate burden on
  // the API itself.
  absl::flat_hash_map<data_types::ModuleId, std::unique_ptr<Entry>> source_;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_SOURCE_INDEXER_H
