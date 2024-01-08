#include "diagnostics/consumer/streaming.h"

#include "absl/strings/str_format.h"
#include "nth/debug/debug.h"
#include "nth/strings/text.h"

namespace ic::diag {
namespace {

constexpr std::string_view HorizontalBorder =
    "──────────────────────────────────────────────────────────────────────────"
    "──────";

constexpr uint32_t Digits(uint32_t n) {
  uint32_t digits = 0;
  while (n != 0) {
    ++digits;
    n /= 10;
  }
  return digits;
}

void DrawLineRange(DiagnosticConsumer &consumer, uint32_t indentation,
                   uint32_t start, uint32_t end) {
  constexpr size_t width   = 2 * 10 + 3;
  char digit_border[width] = {};
  char *p                  = digit_border;
  uint32_t digits          = Digits(end - 1);
  for (uint32_t i = 0; i < digits + 2; ++i) {
    std::memcpy(p, "─", std::strlen("─"));
    p += std::strlen("─");
  }
  *p = '\0';

  absl::FPrintF(stderr, "%*s╭%s┬%s╮\n", 4 + indentation, "", digit_border,
                HorizontalBorder);

  for (uint32_t n = start; n < end; ++n) {
    std::string_view line_text = consumer.Line(n);
    absl::FPrintF(stderr, "%*s│ \033[97m%*d\033[0m │ %s%*s│\033[0m\n",
                  4 + indentation, "", digits, n, line_text,
                  79 - indentation - line_text.size(), "");
  }

  absl::FPrintF(stderr, "%*s╰%s┴%s╯\n", 4 + indentation, "", digit_border,
                HorizontalBorder);
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

    uint32_t start = std::max<uint32_t>(line - 1, 1);
    uint32_t end   = std::min<uint32_t>(line + 2, lines() + 1);
    DrawLineRange(*this, indentation_, start, end);
  } else {
    NTH_UNIMPLEMENTED();
  }
}

void StreamingConsumer::Complete(MessageComponent const &component) {
  indentation_ -= 2;
}

}  // namespace ic
