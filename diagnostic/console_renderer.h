#ifndef ICARUS_DIAGNOSTIC_CONSOLE_RENDERER_H
#define ICARUS_DIAGNOSTIC_CONSOLE_RENDERER_H

#include "diagnostic/message.h"
#include "frontend/source_indexer.h"
#include "nth/io/file.h"

namespace diagnostic {

struct ConsoleRenderer {
  // Assumes the file is already open.
  constexpr explicit ConsoleRenderer(nth::file& file,
                                     frontend::SourceIndexer& source_indexer)
      : source_indexer_(source_indexer), file_(file) {}

  void AddError(DiagnosticMessage const& diag) { Add(Category::Error, diag); }

  void Add(Category cat, DiagnosticMessage const& diag);
  void Flush();

 private:
  void WriteSourceQuote(SourceQuote const& quote);

  bool has_data_ = false;
  frontend::SourceIndexer& source_indexer_;
  nth::file& file_;
};

}  // namespace diagnostic

#endif  // ICARUS_DIAGNOSTIC_CONSOLE_RENDERER_H
