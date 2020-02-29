#ifndef ICARUS_DIAGNOSTIC_CONSUMER_STREAMING_H
#define ICARUS_DIAGNOSTIC_CONSUMER_STREAMING_H

#include <cstdio>

#include "base/debug.h"
#include "diagnostic/console_renderer.h"
#include "diagnostic/consumer/consumer.h"
#include "diagnostic/message.h"
#include "frontend/source/source.h"

namespace diagnostic {

struct StreamingConsumer : DiagnosticConsumer {
  explicit StreamingConsumer(std::FILE* file, frontend::Source const* src)
      : DiagnosticConsumer(ASSERT_NOT_NULL(src)), renderer_(file) {}
  ~StreamingConsumer() override {}
  void ConsumeImpl(DiagnosticMessage&& d) override {
    renderer_.AddError(source(), d);
  }

 private:
  ConsoleRenderer renderer_;
};

}  // namespace diagnostic
#endif  // ICARUS_DIAGNOSTIC_CONSUMER_STREAMING_H
