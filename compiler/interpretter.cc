#include <dlfcn.h>

#include <memory>
#include <string>
#include <vector>

#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "absl/flags/usage.h"
#include "absl/flags/usage_config.h"
#include "absl/strings/str_split.h"
#include "base/expected.h"
#include "base/log.h"
#include "base/no_destructor.h"
#include "base/untyped_buffer.h"
#include "compiler/compiler.h"
#include "compiler/executable_module.h"
#include "compiler/module.h"
#include "diagnostic/consumer/streaming.h"
#include "frontend/parse.h"
#include "frontend/source/file_name.h"
#include "frontend/source/shared.h"
#include "ir/compiled_fn.h"
#include "ir/interpretter/evaluate.h"
#include "module/module.h"
#include "opt/opt.h"

ABSL_FLAG(std::vector<std::string>, log, {},
          "Comma-separated list of log keys");
ABSL_FLAG(std::string, link, "",
          "Library to be dynamically loaded by the compiler to be used "
          "at compile-time. Libraries will not be unloaded.");
#if defined(ICARUS_DEBUG)
ABSL_FLAG(bool, debug_parser, false,
          "Step through the parser step-by-step for debugging.");
ABSL_FLAG(bool, opt_ir, false, "Optimize intermediate representation.");
#endif  // defined(ICARUS_DEBUG)

namespace debug {
extern bool parser;
extern bool validation;
}  // namespace debug

namespace compiler {
namespace {

int Interpret(frontend::FileName const &file_name) {
  diagnostic::StreamingConsumer diag(stderr, frontend::SharedSource());
  auto canonical_file_name = frontend::CanonicalFileName::Make(file_name);
  auto maybe_file_src      = frontend::FileSource::Make(canonical_file_name);
  if (not maybe_file_src) {
    diag.Consume(frontend::MissingModule{.source    = canonical_file_name,
                                         .requestor = "",
                                         .reason = stringify(maybe_file_src)});
    return 1;
  }

  auto *src = &*maybe_file_src;
  diag      = diagnostic::StreamingConsumer(stderr, src);
  compiler::ExecutableModule exec_mod;
  exec_mod.AppendNodes(frontend::Parse(*src, diag), diag);
  if (diag.num_consumed() != 0) { return 1; }
  auto &main_fn = exec_mod.main();

  // TODO All the functions? In all the modules?
  if (absl::GetFlag(FLAGS_opt_ir)) { opt::RunAllOptimizations(&main_fn); }
  main_fn.WriteByteCode<interpretter::instruction_set_t>();
  interpretter::Execute(
      &main_fn, base::untyped_buffer::MakeFull(main_fn.num_regs() * 16), {});

  return 0;
}

}  // namespace
}  // namespace compiler

bool HelpFilter(absl::string_view module) {
  return absl::EndsWith(module, "interpretter.cc");
}

int main(int argc, char *argv[]) {
  absl::FlagsUsageConfig flag_config;
  flag_config.contains_helpshort_flags = &HelpFilter;
  flag_config.contains_help_flags      = &HelpFilter;
  absl::SetFlagsUsageConfig(flag_config);
  absl::SetProgramUsageMessage("Icarus interpreter");
  std::vector<char *> args = absl::ParseCommandLine(argc, argv);
  absl::InitializeSymbolizer(args[0]);
  absl::FailureSignalHandlerOptions opts;
  absl::InstallFailureSignalHandler(opts);

  for (absl::string_view key : absl::GetFlag(FLAGS_log)) {
    base::EnableLogging(key);
  }
  if (std::string lib = absl::GetFlag(FLAGS_link); not lib.empty()) {
    ASSERT_NOT_NULL(dlopen(lib.c_str(), RTLD_LAZY));
  }
  debug::parser = absl::GetFlag(FLAGS_debug_parser);

  if (args.size() < 2) {
    std::cerr << "Missing required positional argument: source file"
              << std::endl;
    return 1;
  }
  if (args.size() > 2) {
    std::cerr << "Too many positional arguments." << std::endl;
    return 1;
  }
  return compiler::Interpret(frontend::FileName(args[1]));
}
