#include <dlfcn.h>
#include <iostream>
#include <memory>
#include <vector>

#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "absl/strings/escaping.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_format.h"
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
#include "init/cli.h"
#include "ir/compiled_fn.h"
#include "ir/interpretter/execution_context.h"
#include "module/module.h"
#include "opt/opt.h"

namespace {
bool optimize_ir = false;

int DumpControlFlowGraph(frontend::FileName const &file_name) {
  diagnostic::StreamingConsumer diag(stderr, frontend::SharedSource());
  auto canonical_file_name = frontend::CanonicalFileName::Make(file_name);
  auto maybe_file_src      = frontend::FileSource::Make(canonical_file_name);
  if (not maybe_file_src) {
    diag.Consume(frontend::MissingModule{
        .source    = canonical_file_name,
        .requestor = "",
    });
    return 1;
  }

  auto *src = &*maybe_file_src;
  diag = diagnostic::StreamingConsumer(stderr, src);
  compiler::ExecutableModule exec_mod;
  exec_mod.AppendNodes(frontend::Parse(*src, diag), diag);
  if (diag.num_consumed() != 0) { return 1; }
  auto &main_fn = exec_mod.main();

  if (optimize_ir) { opt::RunAllOptimizations(&main_fn); }

  std::string output =
      "digraph {\n"
      "  node [shape=record];\n";
  for (auto const *block : main_fn.blocks()) {
    absl::StrAppendFormat(&output, "  \"%p\" [label=\"", block);
    for (auto const& inst : block->instructions()) {
      absl::StrAppendFormat(&output, "%s\\n",
                            absl::CEscape(inst.to_string()));
    }
    absl::StrAppend(&output, "\"]\n");
  }

  for (auto const *block : main_fn.blocks()) {
    block->jump().Visit([&](auto j) {
      constexpr auto type = base::meta<std::decay_t<decltype(j)>>;
      if constexpr (type == base::meta<ir::JumpCmd::RetJump>) {
        if (block->incoming().empty()) { return; }
        absl::StrAppendFormat(&output, "  \"%p\" -> return;\n", block);
      } else {
        if constexpr (type == base::meta<ir::JumpCmd::UncondJump>) {
          absl::StrAppendFormat(&output, "  \"%p\" -> \"%p\";\n", block,
                                j.block);
        } else if constexpr (type == base::meta<ir::JumpCmd::CondJump>) {
          absl::StrAppendFormat(&output,
                                "  \"%p\" -> \"%p\";\n"
                                "  \"%p\" -> \"%p\";\n",
                                block, j.true_block, block, j.false_block);
        } else if constexpr (type == base::meta<ir::JumpCmd::UnreachableJump>) {
          absl::StrAppendFormat(&output, "  \"%p\" -> unreachable;\n", block);
        } else {
          absl::StrAppendFormat(&output, "  \"%p\" -> choose;\n", block);
        }
      }
    });
  }
  std::cout << output << "}";

  return 0;
}

}  // namespace

void cli::Usage() {
  static base::NoDestructor<frontend::FileName> file;
  execute = [] { return DumpControlFlowGraph(*file); };

  Flag("help") << "Show usage information."
               << []() { execute = cli::ShowUsage; };

#if defined(ICARUS_DEBUG)
  Flag("opt-ir") << "Opmitize intermediate representation"
                 << [](bool b = false) { optimize_ir = b; };

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

  HandleOther = [](char const *arg) { *file = frontend::FileName(arg); };
}

int main(int argc, char *argv[]) {
  absl::InitializeSymbolizer(argv[0]);
  absl::FailureSignalHandlerOptions opts;
  absl::InstallFailureSignalHandler(opts);
  return cli::ParseAndRun(argc, argv);
}
