#ifndef ICARUS_DIAGNOSTIC_CONSUMER_TRACKING_H
#define ICARUS_DIAGNOSTIC_CONSUMER_TRACKING_H

#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "diagnostic/consumer/consumer.h"

namespace diagnostic {

// A DiagnosticConsumer which tracks the category and name of all diagnostics it
// consumes.
struct TrackingConsumer : DiagnosticConsumer {
  void ConsumeImpl(std::string_view category, std::string_view name,
                   DiagnosticMessage&&) override {
    diagnostics_.emplace_back(category, name);
  }

  std::span<std::pair<std::string, std::string> const> diagnostics() const {
    return diagnostics_;
  }

  void clear() { diagnostics_.clear(); }

 private:
  // TODO: These could be string_view, but currently GoogleTest doesn't support
  // pretty-printing std::string_view, so we're using strings to make the test
  // error output readable.
  std::vector<std::pair<std::string, std::string>> diagnostics_;
};

}  // namespace diagnostic

#endif  // ICARUS_DIAGNOSTIC_CONSUMER_TRACKING_H
