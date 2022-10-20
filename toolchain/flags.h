#ifndef ICARUS_TOOLCHAIN_FLAGS_H
#define ICARUS_TOOLCHAIN_FLAGS_H

#include <memory>
#include <string>

#include "absl/flags/flag.h"
#include "absl/status/statusor.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/source_indexer.h"

namespace toolchain {

void InitializeFlags(std::string_view program_usage,
                     bool (*filter)(std::string_view));

absl::StatusOr<std::unique_ptr<diagnostic::DiagnosticConsumer>>
DiagnosticConsumerFromFlag(absl::Flag<std::string> const& f,
                           frontend::SourceIndexer& source_indexer);

}  // namespace toolchain

#endif  // ICARUS_TOOLCHAIN_FLAGS_H
