#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "absl/flags/usage.h"
#include "absl/flags/usage_config.h"
#include "absl/strings/match.h"
#include "absl/strings/string_view.h"
#include "ast/node.h"
#include "base/no_destructor.h"
#include "diagnostic/consumer/streaming.h"
#include "format/token_extractor.h"
#include "frontend/parse.h"
#include "frontend/source/file_name.h"
#include "frontend/source/string.h"

ABSL_FLAG(bool, in_place, false, "Update file in-place.");

namespace format {
int FormatFile(frontend::FileName const &file) {
  frontend::StringSource src("3 + abc");
  diagnostic::StreamingConsumer diag(stderr, &src);
  auto stmts = frontend::Parse(src, diag);
  TokenExtractor visitor;
  for (auto const &stmt : stmts) { visitor.Visit(stmt.get()); }
  return 0;
}
}  // namespace format

bool HelpFilter(absl::string_view module) {
  return absl::EndsWith(module, "/main.cc");
}

int main(int argc, char *argv[]) {
  absl::FlagsUsageConfig flag_config;
  flag_config.contains_helpshort_flags = &HelpFilter;
  flag_config.contains_help_flags      = &HelpFilter;
  absl::SetFlagsUsageConfig(flag_config);
  absl::SetProgramUsageMessage("the Icarus formatter.");
  std::vector<char *> args = absl::ParseCommandLine(argc, argv);
  absl::InitializeSymbolizer(args[0]);
  absl::FailureSignalHandlerOptions opts;
  absl::InstallFailureSignalHandler(opts);

  if (args.size() < 2) {
    std::cerr << "Missing required positional argument: source file"
              << std::endl;
    return 1;
  }
  if (args.size() > 2) {
    std::cerr << "Too many positional arguments." << std::endl;
    return 1;
  }
  return format::FormatFile(frontend::FileName(args[1]));
}
