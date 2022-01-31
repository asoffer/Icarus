#ifndef ICARUS_DIAGNOSTIC_CONSUMER_STREAMING_H
#define ICARUS_DIAGNOSTIC_CONSUMER_STREAMING_H

#include <cstdio>

#include "base/debug.h"
#include "diagnostic/console_renderer.h"
#include "diagnostic/consumer/consumer.h"
#include "diagnostic/message.h"
#include "frontend/source/indexer.h"

namespace diagnostic {

struct StreamingConsumer : DiagnosticConsumer {
  explicit StreamingConsumer(std::FILE* file,
                             frontend::SourceIndexer* source_indexer)
      : renderer_(file, source_indexer) {}
  ~StreamingConsumer() override {}

  void ConsumeImpl(std::string_view category, std::string_view name,
                   DiagnosticMessage&& diag) override {
    renderer_.AddError(source(), diag);
  }

 private:
  ConsoleRenderer renderer_;
};

}  // namespace diagnostic
#endif  // ICARUS_DIAGNOSTIC_CONSUMER_STREAMING_H
