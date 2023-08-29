#ifndef ICARUS_DIAGNOSTICS_CONSUMER_NULL_H
#define ICARUS_DIAGNOSTICS_CONSUMER_NULL_H

#include "diagnostics/consumer/consumer.h"
#include "diagnostics/message.h"

namespace ic::diag {

struct NullConsumer : DiagnosticConsumer {
  ~NullConsumer() = default;

  void Start(MessageComponent const &component) override {}
  void Process(MessageComponent const &component) override {}
  void Complete(MessageComponent const &component) override {}
};

}  // namespace ic::diag

#endif  // ICARUS_DIAGNOSTICS_CONSUMER_NULL_H
