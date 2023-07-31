#include "diagnostic/console_renderer.h"

#include <cstdio>
#include <optional>
#include <string_view>

#include "absl/strings/str_format.h"
#include "nth/meta/sequence.h"
#include "nth/meta/type.h"

namespace diagnostic {

namespace {
inline int NumDigits(size_t n) {
  if (n == 0) { return 1; }
  int counter = 0;
  while (n != 0) {
    n /= 10;
    ++counter;
  }
  return counter;
}

}  // namespace

void ConsoleRenderer::Flush() {
  if (not has_data_) { return; }
  has_data_ = false;
  std::fputs("\n\n", out_);
  std::fflush(out_);
}

void ConsoleRenderer::WriteSourceQuote(SourceQuote const &quote) {
  NTH_ASSERT(not quote.highlights.empty());

  // TODO: We should just keep them sorted. In a flat_set or something.
  auto highlights = quote.highlights;
  std::sort(highlights.begin(), highlights.end(),
            [](auto const &l, auto const &r) {
              return std::less<char const *>{}(l.range.data(), r.range.data());
            });

  for (auto const &highlight : highlights) {
    auto &entry       = source_indexer_.EntryFor(highlight.range);
    auto [start, end] = entry.lines_containing(highlight.range);
    std::string text_width_str, number_width_str;
    size_t number_width = NumDigits(end - 1);
    for (size_t i = 0; i < 80; ++i) { text_width_str += "\u2500"; }
    for (size_t i = 0; i < number_width; ++i) { number_width_str += "\u2500"; }
    absl::FPrintF(out_, "    \u256d%s\u252c%s\u256e\n", number_width_str,
                  text_width_str);
    // TODO: get the filename for this module id from the ModuleMap.
    for (size_t line_number = start; line_number < end; ++line_number) {
      absl::FPrintF(out_, "    \u2502%*d\u2502%s%*s\u2502\n", number_width,
                    line_number, entry.line(line_number),
                    80 - entry.line(line_number).size(), "");
    }

    absl::FPrintF(out_, "    \u2570%s\u2534%s\u256f\n", number_width_str,
                  text_width_str);
  }
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
      static_assert(type.dependent(false));
    }
  });
  std::fputs("\n", out_);
}

}  // namespace diagnostic
