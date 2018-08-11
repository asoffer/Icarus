#include "base/container/vector.h"
#include "frontend/source.h"
#include "init/cli.h"
#include "init/signal.h"
#include "run/run.h"

namespace debug {
bool parser     = false;
bool validation = false;
}  // namespace debug

extern base::vector<frontend::Source::Name> files;

void cli::Usage() {
  Flag("help") << "Show usage information."
               << []() { execute = cli::ShowUsage; };

#ifdef DBG
  Flag("debug-parser") << "Step through the parser step-by-step for debugging."
                       << [](bool b = false) { debug::parser = b; };

  Flag("validation", "v") << "Whether or not to do function pre/post-condition "
                             "validation at compile-time."
                          << [](bool b = true) { debug::validation = b; };
#endif

  Flag("repl", "r") << "Run the read-eval-print-loop." << [](bool b = false) {
    if (!execute) { execute = (b ? RunRepl : RunCompiler); }
  };

  HandleOther = [](const char *arg) { files.emplace_back(arg); };
}

int main(int argc, char *argv[]) {
  init::InstallSignalHandlers();
  return cli::ParseAndRun(argc, argv);
}
