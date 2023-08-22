#include "diagnostics/consumer/tty.h"

#include <sys/ioctl.h>
#include <unistd.h>

#include <csignal>

#include "absl/strings/str_format.h"
#include "common/errno.h"
#include "nth/debug/debug.h"
#include "nth/strings/text.h"

namespace ic::diag {
namespace {

static std::atomic<uint64_t> window_change_epoch;

void HandleWindowChange(int) {
  window_change_epoch.fetch_add(1, std::memory_order::relaxed);
}

}  // namespace

int TtyConsumer::Columns() {
  uint64_t epoch = window_change_epoch.load(std::memory_order::relaxed);
  if (epoch != window_change_epoch_) {
    epoch = window_change_epoch_;
    winsize w;
    // Ignore failures that happen during signal processing.
    errno_resetter e;
    ioctl(STDIN_FILENO, TIOCGWINSZ, &w);
    columns_ = w.ws_col;
  }
  return columns_;
}

std::optional<TtyConsumer> TtyConsumer::Make(std::string_view source) {
  errno_resetter e;
  winsize w;
  if (ioctl(STDIN_FILENO, TIOCGWINSZ, &w) != 0) { return std::nullopt; }
  uint64_t columns = w.ws_col;

  if (std::signal(SIGWINCH, HandleWindowChange) == SIG_ERR) {
    return std::nullopt;
  }

  return TtyConsumer(source, columns);
}

TtyConsumer::TtyConsumer(std::string_view source, uint64_t columns)
    : DiagnosticConsumer(source), columns_(columns) {}

void TtyConsumer::Start(MessageComponent const &component) {
  indentation_ += 2;
}

void TtyConsumer::Process(MessageComponent const &component) {
  if (auto const *t = component.As<Header>()) {
    absl::FPrintF(stderr, "%*s\033[91mError:\033[0m ", indentation_, "");
  } else if (auto const *t = component.As<Text>()) {
    std::string_view text = t->text();
    while (not text.empty()) {
      absl::FPrintF(stderr, "%*s%s\n", indentation_, "",
                    nth::GreedyLineBreak(text, Columns() - indentation_,
                                         nth::text_encoding::ascii));
    }
  } else {
    NTH_UNIMPLEMENTED();
  }
}

void TtyConsumer::Complete(MessageComponent const &component) {
  indentation_ -= 2;
}

}  // namespace ic
