#ifndef ICARUS_DIAGNOSTIC_CONSUMER_STREAMING_H
#define ICARUS_DIAGNOSTIC_CONSUMER_STREAMING_H

#include <cstdio>

#include "diagnostic/console_renderer.h"
#include "diagnostic/consumer/consumer.h"
#include "diagnostic/diagnostic.h"

namespace diagnostic {

struct StreamingConsumer : DiagnosticConsumer {
  explicit StreamingConsumer(std::FILE* file) : renderer_(file) {}
  ~StreamingConsumer() override {}
  void Consume(Diagnostic&& d) override { renderer_.AddError(d); }

 private:
  ConsoleRenderer renderer_;
};

}  // namespace diagnostic
#endif  // ICARUS_DIAGNOSTIC_CONSUMER_STREAMING_H
