#ifndef ICARUS_DIAGNOSTICS_CONSUMER_STREAMING_H
#define ICARUS_DIAGNOSTICS_CONSUMER_STREAMING_H

#include <cstdint>
#include <optional>
#include <string_view>

#include "diagnostics/consumer/consumer.h"
#include "diagnostics/message.h"

namespace ic::diag {

struct StreamingConsumer : DiagnosticConsumer {
  using DiagnosticConsumer::DiagnosticConsumer;

  ~StreamingConsumer() = default;

  void Start(MessageComponent const &component) override;
  void Process(MessageComponent const &component) override;
  void Complete(MessageComponent const &component) override;

 private:
  uint64_t indentation_ = -2;
};

}  // namespace ic::diag

#endif  // ICARUS_DIAGNOSTICS_CONSUMER_STREAMING_H
