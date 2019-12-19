#ifndef ICARUS_DIAGNOSTIC_CONSUMER_STREAMING_H
#define ICARUS_DIAGNOSTIC_CONSUMER_STREAMING_H

#include <cstdio>

#include "diagnostic/console_renderer.h"
#include "diagnostic/consumer/consumer.h"
#include "diagnostic/message.h"

namespace diagnostic {

struct StreamingConsumer : DiagnosticConsumer {
  explicit StreamingConsumer(std::FILE* file) : renderer_(file) {}
  ~StreamingConsumer() override {}
  void ConsumeImpl(DiagnosticMessage&& d) override {
    renderer_.AddError(source_, d);
  }

 private:
  frontend::Source* source_;
  ConsoleRenderer renderer_;
};

}  // namespace diagnostic
#endif  // ICARUS_DIAGNOSTIC_CONSUMER_STREAMING_H
