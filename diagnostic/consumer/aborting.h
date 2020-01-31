#ifndef ICARUS_DIAGNOSTIC_CONSUMER_ABORTING_H
#define ICARUS_DIAGNOSTIC_CONSUMER_ABORTING_H

#include <cstdio>

#include "diagnostic/console_renderer.h"
#include "diagnostic/consumer/consumer.h"
#include "diagnostic/message.h"

namespace diagnostic {

struct AbortingConsumer : DiagnosticConsumer {
  explicit AbortingConsumer() : renderer_(stderr) {}
  ~AbortingConsumer() override {}
  void ConsumeImpl(DiagnosticMessage&& d) override {
    renderer_.AddError(source_, d);
    std::abort();
  }

 private:
  frontend::Source* source_;
  ConsoleRenderer renderer_;
};

}  // namespace diagnostic
#endif  // ICARUS_DIAGNOSTIC_CONSUMER_ABORTING_H
