#ifndef ICARUS_DIAGNOSTIC_CONSUMER_CONSUMER_H
#define ICARUS_DIAGNOSTIC_CONSUMER_CONSUMER_H

#include "diagnostic/diagnostic.h"

namespace diagnostic {

struct DiagnosticConsumer {
  virtual ~DiagnosticConsumer() {}
  virtual void Consume(Diagnostic&& diag) = 0;
};

}  // namespace diagnostic

#endif  // ICARUS_DIAGNOSTIC_CONSUMER_CONSUMER_H
