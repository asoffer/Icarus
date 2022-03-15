#ifndef ICARUS_COMPILER_FLAGS_H
#define ICARUS_COMPILER_FLAGS_H

#include <memory>
#include <string>

#include "absl/flags/flag.h"
#include "absl/status/statusor.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/source_indexer.h"

namespace compiler {

absl::StatusOr<std::unique_ptr<diagnostic::DiagnosticConsumer>>
DiagnosticConsumerFromFlag(absl::Flag<std::string> const& f,
                           frontend::SourceIndexer& source_indexer);

}  // namespace compiler

#endif  // ICARUS_COMPILER_FLAGS_H
