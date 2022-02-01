#ifndef ICARUS_DIAGNOSTIC_CONSUMER_BUFFERING_H
#define ICARUS_DIAGNOSTIC_CONSUMER_BUFFERING_H

#include <cstdio>

#include "base/debug.h"
#include "diagnostic/console_renderer.h"
#include "diagnostic/consumer/consumer.h"
#include "diagnostic/message.h"

namespace diagnostic {

struct BufferingConsumer : DiagnosticConsumer {
  explicit BufferingConsumer(DiagnosticConsumer* c) : consumer_(*c) {}

  void ConsumeImpl(std::string_view category, std::string_view name,
                   DiagnosticMessage&& diag) override {
    buffer_.push_back({
        .category = category,
        .name     = name,
        .message  = std::move(diag),
    });
  }

  void drop() { buffer_.clear(); }

  void flush() {
    for (auto& [category, name, diag] : buffer_) {
      consumer_.ConsumeImpl(category, name, std::move(diag));
    }
    drop();
  }

  size_t size() const { return buffer_.size(); }
  bool empty() const { return buffer_.empty(); }

  std::vector<ConsumedMessage> extract() && {
    return std::exchange(buffer_, {});
  }

 private:
  std::vector<ConsumedMessage> buffer_;
  DiagnosticConsumer& consumer_;
};

}  // namespace diagnostic
#endif  // ICARUS_DIAGNOSTIC_CONSUMER_BUFFERING_H
