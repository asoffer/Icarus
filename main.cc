#include <vector>

#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "absl/strings/str_split.h"
#include "base/log.h"
#include "frontend/source/file_name.h"
#include "init/cli.h"

namespace debug {
extern bool parser;
extern bool validation;
extern bool optimize_ir;
}  // namespace debug

int RunRepl();
int RunCompiler(frontend::FileName const &);

void cli::Usage() {
  Flag("help") << "Show usage information."
               << []() { execute = cli::ShowUsage; };

#if defined(ICARUS_DEBUG)
  Flag("debug-parser") << "Step through the parser step-by-step for debugging."
                       << [](bool b = false) { debug::parser = b; };

  Flag("opt-ir") << "Opmitize intermediate representation"
                 << [](bool b = false) { debug::optimize_ir = b; };

  Flag("log") << "Comma-separated list of log keys" << [](char const *keys) {
    for (std::string_view key : absl::StrSplit(keys, ',')) {
      base::EnableLogging(key);
    }
  };
#endif  // defined(ICARUS_DEBUG)

  static char const *file;
  HandleOther = [](char const *arg) { file = arg; };

  Flag("repl", "r") << "Run the read-eval-print-loop." << [](bool b = false) {
    if (not execute) {
      execute =
          (b ? RunRepl : [] { return RunCompiler(frontend::FileName(file)); });
    }
  };
}

int main(int argc, char *argv[]) {
  absl::InitializeSymbolizer(argv[0]);
  absl::FailureSignalHandlerOptions opts;
  absl::InstallFailureSignalHandler(opts);
  return cli::ParseAndRun(argc, argv);
}
