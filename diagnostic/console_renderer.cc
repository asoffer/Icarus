#include "console_renderer.h"

#include <cstdio>
#include <optional>
#include <string_view>

#include "absl/strings/str_format.h"
#include "base/interval.h"
#include "base/meta.h"

namespace diagnostic {

// namespace {
// inline int NumDigits(frontend::LineNum line) {
//   auto n = line.value;
//   if (n == 0) { return 1; }
//   int counter = 0;
//   while (n != 0) {
//     n /= 10;
//     ++counter;
//   }
//   return counter;
// }
//
// }  // namespace

void ConsoleRenderer::Flush() {
  if (not has_data_) { return; }
  has_data_ = false;
  std::fputs("\n\n", out_);
  std::fflush(out_);
}

void ConsoleRenderer::WriteSourceQuote(SourceQuote const &quote) {
  ASSERT(not quote.highlights.empty());

  // TODO: We should just keep them sorted. In a flat_set or something.
  auto highlights = quote.highlights;
  std::sort(highlights.begin(), highlights.end(),
            [](auto const &l, auto const &r) {
              return std::less<char const *>{}(l.range.data(), r.range.data());
            });

  for (auto const &highlight : highlights) {
    auto &entry       = source_indexer_.EntryFor(highlight.range);
    auto [start, end] = entry.lines_containing(highlight.range);
    // TODO: get the filename for this module id from the ModuleMap.
    auto mid = entry.module_id();
    for (size_t line_number = start; line_number < end; ++line_number) {
      absl::FPrintF(out_, "\033[97;1m%s:%d | \033[0m%s\n", mid, line_number,
                    entry.line(line_number));
    }
  }

#if 0

  auto highlight_iter   = quote.highlights.begin();
  bool inside_highlight = false;
  std::optional<frontend::SourceLoc> next_highlight_change;
  if (highlight_iter != quote.highlights.end()) {
    next_highlight_change = highlight_iter->range.begin();
  }

  frontend::LineNum last_line = quote.lines.endpoints_.back() - 1;
  int border_alignment        = NumDigits(last_line) + 2;

  frontend::LineNum prev_line_num = (*quote.lines.begin()).begin();
  for (base::Interval<frontend::LineNum> line_range : quote.lines) {
    // If there's only one line between two intervals, we might as well print
    // it. Otherwise we add some ellipsis.
    switch (line_range.begin().value - prev_line_num.value) {
      case 0:
      case 1: break;
      case 2: {
        absl::FPrintF(out_, "\033[97;1m%*d | \033[0m%s\n", border_alignment,
                      (prev_line_num + 1).value,
                      quote.source->line(prev_line_num + 1));
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

      std::string_view line_str = quote.source->line(line);

      if (next_highlight_change and
          quote.source->line_number(*next_highlight_change) > line) {
        absl::FPrintF(out_, "%s", line_str);
        continue;
      }

      if (next_highlight_change) {
        ASSERT(quote.source->line_number(*next_highlight_change) == line);
      }

      frontend::Offset off{0};
      while (next_highlight_change and
             quote.source->line_number(*next_highlight_change) == line and
             off.value < line_str.length()) {
        frontend::Offset change_offset =
            quote.source->offset_in_line(*next_highlight_change);

        absl::FPrintF(out_, "%s",
                      line_str.substr(off.value, (change_offset - off).value));

        off = change_offset;
        if (inside_highlight) {
          inside_highlight = false;
          ++highlight_iter;
          if (highlight_iter == quote.highlights.end()) {
            next_highlight_change = std::nullopt;
          } else {
            next_highlight_change = highlight_iter->range.end();
          }
        } else {
          inside_highlight = true;
          if (highlight_iter == quote.highlights.end()) {
            next_highlight_change = std::nullopt;
          } else {
            next_highlight_change = highlight_iter->range.end();
          }
        }
        set_highlight();
      }
      if (off.value < line_str.length()) {
        absl::FPrintF(out_, "%s", line_str.substr(off.value));
      }
    }
  }
  // Ensure that any following messages are on a separate line, even if the last
  // line of the source quote didn't include a newline.
  if (not quote.source->line(last_line).ends_with('\n'))
    absl::FPrintF(out_, "\n");
#endif
}

void ConsoleRenderer::Add(Category cat, DiagnosticMessage const &diag) {
  has_data_ = true;

  // TODO: Source file name.
  std::fputs("\033[31;1mError\033[0m:\n", out_);

  diag.for_each_component([&](auto const &component) {
    constexpr auto type = nth::type<std::decay_t<decltype(component)>>;
    if constexpr (type == nth::type<Text>) {
      std::fputs(component.c_str(), out_);
      std::fputs("\n\n", out_);
    } else if constexpr (type == nth::type<List>) {
      for (std::string const &item : component.items()) {
        absl::FPrintF(out_, "  * %s\n", item);
      }
    } else if constexpr (type == nth::type<SourceQuote>) {
      WriteSourceQuote(component);
    } else {
      static_assert(base::always_false(type));
    }
  });
  std::fputs("\n", out_);
}

}  // namespace diagnostic
