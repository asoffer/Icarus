#include <vector>
#include "init/cli.h"
#include "init/signal.h"

namespace debug {
extern bool parser;
extern bool validation;
}  // namespace debug


namespace feature {
extern bool loose_casting;
}  // namespace feature

extern std::vector<std::string> files;

int RunRepl();
int RunCompiler();

#ifdef ICARUS_USE_LLVM
namespace backend {
extern char const *output_file;
}  // namespace backend

#endif

void cli::Usage() {
  Flag("help") << "Show usage information."
               << []() { execute = cli::ShowUsage; };

  Flag("loose-casting", "l")
      << "Allow casting between any integral types or between floating-point "
         "types, or from integral to floating-point, even if it results in a "
         "loss of precision."
      << [](bool b = false) { feature::loose_casting = b; };

#ifdef DBG
  Flag("debug-parser") << "Step through the parser step-by-step for debugging."
                       << [](bool b = false) { debug::parser = b; };

  Flag("debug-validation", "v") << "Step through validator for debugging." <<
      [](bool b = false) { debug::validation = b; };
#endif

#ifdef ICARUS_USE_LLVM
  Flag("output") << "The name of the output file to write." <<
      [](char const *out = "a.out") { backend::output_file = out; };
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
