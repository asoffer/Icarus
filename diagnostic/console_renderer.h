#ifndef ICARUS_DIAGNOSTIC_CONSOLE_RENDERER_H
#define ICARUS_DIAGNOSTIC_CONSOLE_RENDERER_H

#include <cstdio>
#include <type_traits>

#include "diagnostic/message.h"
#include "frontend/source/indexer.h"

namespace diagnostic {

struct ConsoleRenderer {
  // Assumes the file is already open.
  constexpr explicit ConsoleRenderer(std::FILE* out,
                                     frontend::SourceIndexer* source_indexer)
      : source_indexer_(*ASSERT_NOT_NULL(source_indexer)), out_(out) {}

  void AddError(frontend::SourceBuffer const* source,
                DiagnosticMessage const& diag) {
    Add(source, Category::Error, diag);
  }

  void Add(frontend::SourceBuffer const* source, Category cat,
           DiagnosticMessage const& diag);
  void Flush();

 private:
  void WriteSourceQuote(SourceQuote const& quote);

  bool has_data_ = false;
  frontend::SourceIndexer& source_indexer_;
  std::FILE* out_;
};

}  // namespace diagnostic

#endif  // ICARUS_DIAGNOSTIC_CONSOLE_RENDERER_H
