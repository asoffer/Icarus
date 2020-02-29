#include "console_renderer.h"

#include "base/meta.h"

namespace diagnostic {

namespace {
inline int NumDigits(frontend::LineNum line) {
  auto n = line.value;
  if (n == 0) { return 1; }
  int counter = 0;
  while (n != 0) {
    n /= 10;
    ++counter;
  }
  return counter;
}

static absl::flat_hash_map<frontend::Source const *, std::vector<std::string>>
    lines;
std::string const &LoadLine(frontend::Source const *src,
                            frontend::LineNum line) {
  auto iter = lines.find(src);
  if (iter == lines.end()) {
    iter = lines.emplace(src, src->LoadLines()).first;
  }
  return iter->second[line.value];
}

}  // namespace

void ConsoleRenderer::Flush() {
  if (not has_data_) { return; }
  has_data_ = false;
  std::fputs("\n\n", out_);
  std::fflush(out_);
}

void ConsoleRenderer::WriteSourceQuote(frontend::Source const *source,
                                       SourceQuote const &quote) {
  int border_alignment = NumDigits(quote.lines.endpoints_.back() - 1) + 2;
  for (base::Interval<frontend::LineNum> line_range : quote.lines) {
    for (frontend::LineNum line = line_range.begin(); line != line_range.end();
         ++line) {
      std::fprintf(out_, "\033[97;1m%*d | \033[0m%s\n",  //
                   border_alignment, line.value,         //
                   LoadLine(ASSERT_NOT_NULL(source), line).c_str());
    }
  }
}

void ConsoleRenderer::Add(frontend::Source const *source, Category cat,
                          DiagnosticMessage const &diag) {
  has_data_ = true;
  diag.for_each_component([&](auto const &component) {
    using T = std::decay_t<decltype(component)>;
    if constexpr (std::is_same_v<T, Text>) {
      std::fputs(component.c_str(), out_);
    } else if constexpr (std::is_same_v<T, List>) {
      for (std::string const &item : component.items()) {
        std::fprintf(out_, "  * %s", item.c_str());
      }
    } else if constexpr (std::is_same_v<T, SourceQuote>) {
      WriteSourceQuote(source, component);
    } else {
      static_assert(base::always_false<T>());
    }
    std::fputs("\n\n", out_);
  });
}

void ConsoleRenderer::Add(Category cat, DiagnosticMessage const &diag) {
  has_data_ = true;
  diag.for_each_component([&](auto const &component) {
    using T = std::decay_t<decltype(component)>;
    if constexpr (std::is_same_v<T, Text>) {
      std::fputs(component.c_str(), out_);
    } else if constexpr (std::is_same_v<T, List>) {
      for (std::string const &item : component.items()) {
        std::fprintf(out_, "  * %s", item.c_str());
      }
    } else if constexpr (std::is_same_v<T, SourceQuote>) {
      WriteSourceQuote(component.source, component);
    } else {
      static_assert(base::always_false<T>());
    }
    std::fputs("\n\n", out_);
  });
}

}  // namespace diagnostic
