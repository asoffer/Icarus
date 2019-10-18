#ifndef ICARUS_DIAGNOSTIC_CONSOLE_RENDERER_H
#define ICARUS_DIAGNOSTIC_CONSOLE_RENDERER_H

#include <cstdio>
#include <type_traits>

#include "diagnostic/diagnostic.h"

namespace diagnostic {

struct ConsoleRenderer {
  // Assumes the file is already open.
  constexpr explicit ConsoleRenderer(std::FILE* out) : out_(out) {}

  void AddError(Diagnostic const& diag) { Add(Category::Error, diag); }

  void Add(Category cat, Diagnostic const& diag) {
    has_data_ = true;
    diag.for_each_component([&](auto const& component) {
      using T = std::decay_t<decltype(component)>;
      if constexpr (std::is_same_v<T, Text>) {
        std::fputs(component.c_str(), out_);
      } else if constexpr (std::is_same_v<T, List>) {
        for (std::string const& item : component.items()) {
          std::fprintf(out_, "  * %s", item.c_str());
        }
      } else {
        WriteSourceQuote(component);
      }
      std::fputs("\n\n", out_);
    });
  }

  void Flush();

 private:
  void WriteSourceQuote(SourceQuote const& quote);

  bool has_data_ = false;
  std::FILE* out_;
};

}  // namespace diagnostic

#endif  // ICARUS_DIAGNOSTIC_CONSOLE_RENDERER_H
