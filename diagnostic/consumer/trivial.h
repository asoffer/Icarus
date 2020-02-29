#ifndef ICARUS_DIAGNOSTIC_CONSUMER_TRIVIAL_H
#define ICARUS_DIAGNOSTIC_CONSUMER_TRIVIAL_H

#include "diagnostic/consumer/consumer.h"
#include "diagnostic/message.h"

namespace diagnostic {

struct TrivialConsumer : DiagnosticConsumer {
  explicit TrivialConsumer() : DiagnosticConsumer(nullptr) {}
  ~TrivialConsumer() override {}

 protected:
  void ConsumeImpl(DiagnosticMessage&& diag) override {}
};

}  // namespace diagnostic

#endif  // ICARUS_DIAGNOSTIC_CONSUMER_TRIVIAL_H
