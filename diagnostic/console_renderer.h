#ifndef ICARUS_DIAGNOSTIC_CONSOLE_RENDERER_H
#define ICARUS_DIAGNOSTIC_CONSOLE_RENDERER_H

#include <cstdio>
#include <type_traits>

#include "diagnostic/message.h"

namespace diagnostic {

struct ConsoleRenderer {
  // Assumes the file is already open.
  constexpr explicit ConsoleRenderer(std::FILE* out) : out_(out) {}

  void AddError(std::string_view filename, frontend::SourceBuffer const& source,
                DiagnosticMessage const& diag) {
    Add(filename, source, Category::Error, diag);
  }

  void Add(std::string_view filename, frontend::SourceBuffer const& source,
           Category cat, DiagnosticMessage const& diag);
  void Flush();

 private:
  void WriteSourceQuote(frontend::SourceBuffer const& buffer,
                        SourceQuote const& quote);

  bool has_data_ = false;
  std::FILE* out_;
};

}  // namespace diagnostic

#endif  // ICARUS_DIAGNOSTIC_CONSOLE_RENDERER_H
