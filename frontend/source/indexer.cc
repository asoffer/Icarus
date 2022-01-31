#include "frontend/source/indexer.h"

#include "base/debug.h"

namespace frontend {

std::string_view SourceIndexer::insert(ir::ModuleId module,
                                       std::string&& content) {
  auto [iter, inserted] = source_.try_emplace(module);
  if (inserted) { iter->second = std::make_unique<Entry>(std::move(content)); }
  return iter->second->content();
}

std::string_view SourceIndexer::insert(ir::ModuleId module,
                                       std::string_view content) {
  auto [iter, inserted] = source_.try_emplace(module);
  if (inserted) {
    iter->second = std::make_unique<Entry>(std::string(content));
  }
  return iter->second->content();
}

SourceIndexer::Entry &SourceIndexer::EntryFor(std::string_view subcontent) {
  // Note: There are subtleties in the implementation of this function due to
  // the fact that comparing pointers in C++ is undefined in many circumstances
  // one would not naievely expect. Effectively what we would like to do is
  // compare the pointer `subcontent.data()` to the extents of each file content
  // string, and return the first (only) entry for which that
  // `subcontent.data()` lies between the extents. However, comparing pointers
  // from separate allocations is undefined behavior so doing so directly is not
  // portable.
  //
  // However, `std::less<char const *>` is guaranteed to provide a total
  // ordering on pointers even when pointers come from separate allocations.
  // Simply replacing `<` with `std::less<char const *>` avoids any potential
  // undefined behavior, but it still does not quite give us what we need. it
  // could be that `std::less<char const *>` claims the subcontent lies between
  // the file content extents even the file content does not contain the given
  // subcontent. Though possible in theory, this does not happen on any
  // implementation we are aware of.
  char const *q = subcontent.data();
  for (auto const &[id, entry] : source_) {
    if (not std::less<char const *>{}(q, entry->content().begin()) and
        not std::less<char const *>{}(entry->content().end(), q)) {
      return *entry;
    }
  }
  UNREACHABLE();
}

absl::Span<size_t const> SourceIndexer::Entry::line_starts() {
  if (line_terminators_) { return *line_terminators_; }
  line_terminators_.emplace(1, 0);

  for (size_t i = 1; i < content().size(); ++i) {
    // TODO: The lexer already covers `IsVerticalWhitespace` and we should share
    // that implementation.
    if (content_[i] == '\n' or content_[i] == '\r') {
      line_terminators_->emplace_back(i + 1);
    }
  }
  line_terminators_->emplace_back(content_.size() + 1);
  return *line_terminators_;
}

std::string_view SourceIndexer::Entry::line(size_t n) {
  ASSERT(n > 0);
  absl::Span line_start_indices = line_starts();
  size_t start_index            = line_start_indices[n - 1];
  size_t end_index              = line_start_indices[n];
  return std::string_view(content_.data() + start_index,
                          end_index - start_index - 1);
}

size_t SourceIndexer::Entry::line_containing(char const *p) {
  absl::Span line_start_indices = line_starts();
  auto iter                     = std::upper_bound(line_start_indices.begin(),
                               line_start_indices.end(), p - content_.data());
  return std::distance(line_start_indices.begin(), iter);
}

std::pair<size_t, size_t> SourceIndexer::Entry::lines_containing(
    std::string_view s) {
  absl::Span line_start_indices = line_starts();
  auto iter1                    = std::upper_bound(line_start_indices.begin(),
                                line_start_indices.end(), s.data() - content_.data());
  auto iter2 = std::upper_bound(iter1, line_start_indices.end(),
                                s.data() + s.size() - 1 - content_.data());
  return std::pair(std::distance(line_start_indices.begin(), iter1),
                   std::distance(line_start_indices.begin(), iter2) + 1);
}

}  // namespace frontend
