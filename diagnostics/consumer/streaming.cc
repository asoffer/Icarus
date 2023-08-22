#include "diagnostics/consumer/streaming.h"

#include "absl/strings/str_format.h"
#include "nth/debug/debug.h"
#include "nth/strings/text.h"

namespace ic::diag {

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
  } else {
    NTH_UNIMPLEMENTED();
  }
}

void StreamingConsumer::Complete(MessageComponent const &component) {
  indentation_ -= 2;
}

}  // namespace ic
