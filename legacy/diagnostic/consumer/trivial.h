#ifndef ICARUS_DIAGNOSTIC_CONSUMER_TRIVIAL_H
#define ICARUS_DIAGNOSTIC_CONSUMER_TRIVIAL_H

#include "diagnostic/consumer/consumer.h"
#include "diagnostic/message.h"

namespace diagnostic {

struct TrivialConsumer : DiagnosticConsumer {
 protected:
  void ConsumeImpl(std::string_view category, std::string_view name,
                   DiagnosticMessage&& diag) override {}
};

}  // namespace diagnostic

#endif  // ICARUS_DIAGNOSTIC_CONSUMER_TRIVIAL_H
