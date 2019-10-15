#include <vector>

#include "absl/strings/str_split.h"
#include "base/log.h"
#include "init/cli.h"
#include "init/signal.h"

namespace debug {
extern bool parser;
extern bool validation;
extern bool optimize_ir;
}  // namespace debug

extern std::vector<std::string> files;

int RunRepl();
int RunCompiler();

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

  Flag("repl", "r") << "Run the read-eval-print-loop." << [](bool b = false) {
    if (not execute) { execute = (b ? RunRepl : RunCompiler); }
  };

  HandleOther = [](char const *arg) { files.emplace_back(arg); };
}

int main(int argc, char *argv[]) {
  init::InstallSignalHandlers();
  return cli::ParseAndRun(argc, argv);
}
