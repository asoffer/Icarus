#include "console_renderer.h"

#include <optional>
#include <string_view>

#include "absl/strings/str_format.h"
#include "base/interval.h"
#include "base/macros.h"
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

std::optional<std::string_view> LoadLine(frontend::Source const *src,
                                         frontend::LineNum line) {
  auto iter = lines.find(src);
  if (iter == lines.end()) {
    iter = lines.emplace(src, src->LoadLines()).first;
  }

  if (line.value >= iter->second.size()) { return std::nullopt; }
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
  ASSERT(source != nullptr);
  if (quote.lines.empty()) {
    // TODO: Ensure that this is impossible.
    std::fputs("Internal Error: SourceQuote is empty\n", out_);
    return;
  }

  auto highlight_iter   = quote.highlights.begin();
  bool inside_highlight = false;
  std::optional<frontend::SourceLoc> next_highlight_change;
  if (highlight_iter != quote.highlights.end()) {
    next_highlight_change = highlight_iter->range.begin();
  }

  int border_alignment = NumDigits(quote.lines.endpoints_.back() - 1) + 2;
  frontend::LineNum prev_line_num = (*quote.lines.begin()).begin();
  for (base::Interval<frontend::LineNum> line_range : quote.lines) {
    // If there's only one line between two intervals, we might as well print
    // it. Otherwise we add some ellipsis.
    switch (line_range.begin().value - prev_line_num.value) {
      case 0:
      case 1: break;
      case 2: {
        ASSIGN_OR(std::abort(),  //
                  auto line_str, LoadLine(source, prev_line_num + 1));
        absl::FPrintF(out_, "\033[97;1m%*d | \033[0m%s\n", border_alignment,
                      prev_line_num.value + 1, line_str);
      } break;
      default: {
        absl::FPrintF(out_, "\033[97;1m%*s.. | \033[0m\n", border_alignment - 2,
                      "");
      } break;
    }

    for (frontend::LineNum line = line_range.begin(); line != line_range.end();
         ++line) {
      absl::FPrintF(out_, "\033[97;1m%*d | ", border_alignment, line.value);

      auto set_highlight = [&] {
        if (inside_highlight) {
          absl::FPrintF(out_, "\033[3%d;1m",
                        static_cast<int>(highlight_iter->style.color));
        } else {
          absl::FPrintF(out_, "\033[0m");
        }
      };

      set_highlight();

      ASSIGN_OR(continue, auto line_str, LoadLine(source, line));

      if (next_highlight_change and next_highlight_change->line_num > line) {
        absl::FPrintF(out_, "%s", line_str);
        continue;
      }

      if (next_highlight_change) {
        ASSERT(next_highlight_change->line_num == line);
      }

      frontend::Offset off{0};
      while (next_highlight_change and
             next_highlight_change->line_num == line) {
        absl::FPrintF(
            out_, "%s",
            line_str.substr(off.value,
                            next_highlight_change->offset.value - off.value));

        off = next_highlight_change->offset;
        if (inside_highlight) {
          inside_highlight = false;
          ++highlight_iter;
          if (highlight_iter == quote.highlights.end()) {
            next_highlight_change = std::nullopt;
          } else {
            next_highlight_change = highlight_iter->range.begin();
          }
        } else {
          inside_highlight = true;
          if (highlight_iter == quote.highlights.end()) {
            next_highlight_change = std::nullopt;
          } else {
            next_highlight_change = highlight_iter->range.begin();
          }
        }
        set_highlight();
      }
      absl::FPrintF(out_, "%s\n", line_str.substr(off.value));
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
        absl::FPrintF(out_, "  * %s\n", item);
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
        absl::FPrintF(out_, "  * %s", item);
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
