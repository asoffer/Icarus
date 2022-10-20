#include "toolchain/flags.h"

#include "absl/flags/flag.h"
#include "absl/flags/usage.h"
#include "absl/flags/usage_config.h"
#include "diagnostic/consumer/json.h"
#include "diagnostic/consumer/streaming.h"

namespace toolchain {

void InitializeFlags(std::string_view program_usage,
                     bool (*filter)(std::string_view)) {
  absl::FlagsUsageConfig flag_config;
  flag_config.contains_helpshort_flags = filter;
  flag_config.contains_help_flags      = filter;
  absl::SetFlagsUsageConfig(flag_config);
  absl::SetProgramUsageMessage("Icarus compiler");
}

absl::StatusOr<std::unique_ptr<diagnostic::DiagnosticConsumer>>
DiagnosticConsumerFromFlag(absl::Flag<std::string> const& f,
                           frontend::SourceIndexer& source_indexer) {
  std::string diagnostics = absl::GetFlag(f);
  if (diagnostics == "console") {
    return std::make_unique<diagnostic::StreamingConsumer>(stderr,
                                                           &source_indexer);
  } else if (diagnostics == "json") {
    return std::make_unique<diagnostic::JsonConsumer>(stderr, source_indexer);
  } else {
    return absl::InvalidArgumentError(absl::StrFormat(
        "Invalid value for --diagnostics flag '%s'. Valid values "
        "are 'console' or 'json'\n",
        diagnostics));
  }
}

}  // namespace toolchain
