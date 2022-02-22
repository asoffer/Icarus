#ifndef ICARUS_DIAGNOSTIC_CONSUMER_JSON_H
#define ICARUS_DIAGNOSTIC_CONSUMER_JSON_H

#include <cstdio>

#include "diagnostic/consumer/consumer.h"
#include "diagnostic/message.h"
#include "frontend/source_indexer.h"
#include "nlohmann/json.hpp"

namespace diagnostic {

struct JsonConsumer : DiagnosticConsumer {
  explicit JsonConsumer(frontend::SourceIndexer& source_indexer,
                        nlohmann::json& json)
      : json_(json), source_indexer_(source_indexer) {}

  ~JsonConsumer() override {}

  void ConsumeImpl(std::string_view category, std::string_view name,
                   DiagnosticMessage&& diag) override;

 private:
  nlohmann::json& json_;
  frontend::SourceIndexer& source_indexer_;
};

}  // namespace diagnostic
#endif  // ICARUS_DIAGNOSTIC_CONSUMER_JSON_H
