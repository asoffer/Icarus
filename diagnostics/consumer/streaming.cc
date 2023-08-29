#include "diagnostics/consumer/streaming.h"

#include "absl/strings/str_format.h"
#include "nth/debug/debug.h"
#include "nth/strings/text.h"

namespace ic::diag {
namespace {

constexpr std::string_view HorizontalBorder =
    "──────────────────────────────────────────────────────────────────────────"
    "──────";

void DrawLineRange(DiagnosticConsumer &consumer, uint32_t indentation,
                   uint32_t start, uint32_t end) {
  absl::FPrintF(stderr, "%*s╭───┬%s╮\n", 4 + indentation, "", HorizontalBorder);

  for (uint32_t n = start; n < end; ++n) {
    std::string_view line_text = consumer.Line(n);
    absl::FPrintF(stderr, "%*s│ \033[97m%d\033[0m │ %s%*s│\033[0m\n",
                  4 + indentation, "", n, line_text,
                  79 - indentation - line_text.size(), "");
  }

  absl::FPrintF(stderr, "%*s╰───┴%s╯\n", 4 + indentation, "", HorizontalBorder);
}

}  // namespace

void StreamingConsumer::Start(MessageComponent const &component) {
  indentation_ += 2;
}

void StreamingConsumer::Process(MessageComponent const &component) {
  if (auto const *t = component.As<Header>()) {
    absl::FPrintF(stderr, "%*s\033[91mError:\033[0m ", indentation_, "");
  } else if (auto const *t = component.As<Text>()) {
    std::string_view text = t->text();
    while (not text.empty()) {
      absl::FPrintF(stderr, "%*s%s\n", indentation_, "",
                    nth::GreedyLineBreak(text, 80 - indentation_,
                                         nth::text_encoding::ascii));
    }
  } else if (auto const *q = component.As<SourceQuote>()) {
    auto [line, column] = LineAndColumn(q->token());

    DrawLineRange(*this, indentation_, std::max<uint32_t>(line - 1, 1), line + 1);
  } else {
    NTH_UNIMPLEMENTED();
  }
}

void StreamingConsumer::Complete(MessageComponent const &component) {
  indentation_ -= 2;
}

}  // namespace ic
