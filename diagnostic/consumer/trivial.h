#ifndef ICARUS_DIAGNOSTIC_CONSUMER_TRIVIAL_H
#define ICARUS_DIAGNOSTIC_CONSUMER_TRIVIAL_H

#include "diagnostic/message.h"
#include "diagnostic/consumer/consumer.h"

namespace diagnostic {

struct TrivialConsumer : DiagnosticConsumer {
  ~TrivialConsumer() override {}

 protected:
  void ConsumeImpl(DiagnosticMessage&& diag) override {}
};

}  // namespace diagnostic

#endif  // ICARUS_DIAGNOSTIC_CONSUMER_TRIVIAL_H
