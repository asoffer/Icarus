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

#ifdef ICARUS_USE_LLVM
namespace backend {
extern char const *output_file;
}  // namespace backend
#endif

void cli::Usage() {
  Flag("help") << "Show usage information."
               << []() { execute = cli::ShowUsage; };

#ifdef DBG
  Flag("debug-parser") << "Step through the parser step-by-step for debugging."
                       << [](bool b = false) { debug::parser = b; };

  Flag("debug-validation", "v") << "Step through validator for debugging."
                                << [](bool b = false) { debug::validation = b; };
#endif

#ifdef ICARUS_USE_LLVM
  Flag("output") << "The name of the output file to write."
                 << [](char const *out = "a.out") { backend::output_file = out; };
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
