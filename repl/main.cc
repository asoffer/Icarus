#include <dlfcn.h>

#include <memory>
#include <vector>

#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "absl/flags/usage.h"
#include "absl/flags/usage_config.h"
#include "absl/strings/match.h"
#include "absl/strings/str_split.h"
#include "absl/strings/string_view.h"
#include "base/log.h"
#include "diagnostic/consumer/streaming.h"
#include "repl/module.h"
#include "repl/source.h"

ABSL_FLAG(std::vector<std::string>, log, {},
          "Comma-separated list of log keys");
ABSL_FLAG(std::string, link, "",
          "Library to be dynamically loaded by the compiler to be used "
          "at compile-time. Libraries will not be unloaded.");

bool HelpFilter(absl::string_view module) {
  return absl::EndsWith(module, "/main.cc");
}

int main(int argc, char *argv[]) {
  absl::FlagsUsageConfig flag_config;
  flag_config.contains_helpshort_flags = &HelpFilter;
  flag_config.contains_help_flags      = &HelpFilter;
  absl::SetFlagsUsageConfig(flag_config);
  absl::SetProgramUsageMessage("the Icarus read-eval-print loop.");
  std::vector<char *> args = absl::ParseCommandLine(argc, argv);
  absl::InitializeSymbolizer(args[0]);
  absl::FailureSignalHandlerOptions opts;
  absl::InstallFailureSignalHandler(opts);

  std::vector<std::string> log_keys = absl::GetFlag(FLAGS_log);
  for (absl::string_view key : log_keys) { base::EnableLogging(key); }

  if (std::string lib = absl::GetFlag(FLAGS_link); not lib.empty()) {
    ASSERT_NOT_NULL(dlopen(lib.c_str(), RTLD_LAZY));
  }

  if (args.size() != 1) {
    std::cerr << "Positional arguments are not supported." << std::endl;
    return 1;
  }
  std::puts("Icarus REPL");
  repl::Source source(&std::cin, &std::cout);
  diagnostic::StreamingConsumer diag(stderr, &source);

  module::FileImporter<LibraryModule> importer;
  repl::Module mod;
  // TODO: Handle parse failures
  while (true) {
    mod.AppendNodes(frontend::Parse(source, diag), diag, importer);
  }
  return 0;
}