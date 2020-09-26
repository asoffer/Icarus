#include <dlfcn.h>
#include <memory>
#include <vector>

#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "absl/strings/str_split.h"
#include "base/log.h"
#include "diagnostic/consumer/streaming.h"
#include "init/cli.h"
#include "repl/module.h"
#include "repl/source.h"

void cli::Usage() {
  execute = [] {
    std::puts("Icarus REPL");
    repl::Source source(&std::cin, &std::cout);
    diagnostic::StreamingConsumer diag(stderr, &source);
    repl::Module mod;
    // TODO: Handle parse failures
    LOG("", "got here");
    while (true) { mod.ProcessFromSource(&source, diag); }
    return 0;
  };

  Flag("help") << "Show usage information."
               << []() { execute = cli::ShowUsage; };

#if defined(ICARUS_DEBUG)
  Flag("log") << "Comma-separated list of log keys" << [](char const *keys) {
    for (std::string_view key : absl::StrSplit(keys, ',')) {
      base::EnableLogging(key);
    }
  };
#endif  // defined(ICARUS_DEBUG)

  Flag("link") << "Library to be dynamically loaded by the compiler to be used "
                  "at compile-time. Libraries will not be unloaded."
               << [](char const *lib) {
                    static_cast<void>(ASSERT_NOT_NULL(dlopen(lib, RTLD_LAZY)));
                  };

  HandleOther = [](char const *arg) {};
}

int main(int argc, char *argv[]) {
  absl::InitializeSymbolizer(argv[0]);
  absl::FailureSignalHandlerOptions opts;
  absl::InstallFailureSignalHandler(opts);
  return cli::ParseAndRun(argc, argv);
}
