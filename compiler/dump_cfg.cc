#include <dlfcn.h>

#include <cstdlib>
#include <iostream>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "absl/flags/usage.h"
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
#include "ir/compiled_fn.h"
#include "ir/interpreter/execution_context.h"
#include "module/module.h"
#include "opt/opt.h"

ABSL_FLAG(std::vector<std::string>, log, {},
          "Comma-separated list of log keys");
ABSL_FLAG(std::string, link, "",
          "Library to be dynamically loaded by the compiler to be used "
          "at compile-time. Libraries will not be unloaded.");
ABSL_FLAG(bool, opt_ir, false, "Optimize intermediate representation.");
ABSL_FLAG(std::vector<std::string>, module_paths, {},
          "Comma-separated list of paths to search when importing modules. "
          "Defaults to $ICARUS_MODULE_PATH.");

namespace {

void DumpControlFlowGraph(ir::CompiledFn const *fn, std::ostream &output) {
  absl::Format(&output,
               "  subgraph cluster_fn%u {\n"
               "  fontname = monospace\n"
               "  label = \"NativeFn(fn = 0x%x)\";\n",
               reinterpret_cast<uintptr_t>(fn),
               reinterpret_cast<uintptr_t>(fn));
  absl::flat_hash_map<uintptr_t, std::vector<ir::BasicBlock const *>> clusters;
  for (auto const *block : fn->blocks()) {
    clusters[block->debug().cluster_index].push_back(block);
  }

  constexpr auto style_for_jump = [](ir::JumpCmd::Kind k) -> char const * {
    switch (k) {
      case ir::JumpCmd::Kind::Unreachable:
        return "style=filled fillcolor=tomato";
      case ir::JumpCmd::Kind::Return: return "style=filled fillcolor=gray";
      default: return "";
    }
  };

  output << "    node [shape=record];\n";
  for (auto const &[index, cluster] : clusters) {
    if (index != 0) {
      absl::Format(&output,
                   "  subgraph cluster_%d {\n"
                   "  label = \"\";\n"
                   "  style=filled;\ncolor=lightgray;\n",
                   index);
    }

    for (auto const *block : cluster) {
      std::string header = block->debug().header.empty()
                               ? ""
                               : absl::StrCat(block->debug().header, "|");
      absl::Format(&output, "  \"%016p\" [%s fontname=monospace label=\"{%s",
                   block, style_for_jump(block->jump().kind()), header);
      for (auto const &inst : block->instructions()) {
        output << absl::CEscape(inst.to_string()) << "\\l";
      }
      block->jump().Visit([&](auto j) {
        constexpr auto type = base::meta<std::decay_t<decltype(j)>>;
        if constexpr (type == base::meta<ir::JumpCmd::CondJump>) {
          output << "cond-jump: " << j.reg;
        } else if constexpr (type == base::meta<ir::JumpCmd::ChooseJump>) {
          output << "choose";
        }
      });

      output << "}\"]\n";
    }
    if (index != 0) { output << "}\n\n"; }
  }

  for (auto const *block : fn->blocks()) {
    block->jump().Visit([&](auto j) {
      constexpr auto type = base::meta<std::decay_t<decltype(j)>>;
      if constexpr (type == base::meta<ir::JumpCmd::UncondJump>) {
        absl::Format(&output, "  \"%016p\" -> \"%016p\";\n", block, j.block);
      } else if constexpr (type == base::meta<ir::JumpCmd::CondJump>) {
        absl::Format(&output,
                     "  \"%016p\" -> \"%016p\" [label=true];\n"
                     "  \"%016p\" -> \"%016p\" [label=false];\n",
                     block, j.true_block, block, j.false_block);
      }
    });
  }

  output << "}\n";
}

int DumpControlFlowGraph(frontend::FileName const &file_name,
                         std::ostream &output) {
  diagnostic::StreamingConsumer diag(stderr, frontend::SharedSource());
  auto canonical_file_name = frontend::CanonicalFileName::Make(file_name);
  auto maybe_file_src      = frontend::FileSource::Make(canonical_file_name);
  if (not maybe_file_src) {
    diag.Consume(frontend::MissingModule{
        .source    = canonical_file_name,
        .requestor = "",
        .reason    = stringify(maybe_file_src),
    });
    return 1;
  }

  auto *src = &*maybe_file_src;
  diag      = diagnostic::StreamingConsumer(stderr, src);
  module::FileImporter<compiler::LibraryModule> importer;
  importer.module_lookup_paths = absl::GetFlag(FLAGS_module_paths);
  compiler::ExecutableModule exec_mod;
  exec_mod.AppendNodes(frontend::Parse(*src, diag), diag, importer);
  if (diag.num_consumed() != 0) { return 1; }
  auto &main_fn = exec_mod.main();

  if (absl::GetFlag(FLAGS_opt_ir)) { opt::RunAllOptimizations(&main_fn); }

  output << "digraph {\n";
  DumpControlFlowGraph(&main_fn, output);
  exec_mod.context().ForEachCompiledFn(
      [&](ir::CompiledFn const *f) { DumpControlFlowGraph(f, output); });
  output << "}";

  return 0;
}

}  // namespace

int main(int argc, char *argv[]) {
  // Provide a new default for --module_paths.
  if (char *const env_str = std::getenv("ICARUS_MODULE_PATH")) {
    absl::SetFlag(&FLAGS_module_paths, absl::StrSplit(env_str, ':'));
  }
  absl::SetProgramUsageMessage("the Icarus control flow graph dumper.");
  std::vector<char *> args = absl::ParseCommandLine(argc, argv);
  absl::InitializeSymbolizer(args[0]);
  absl::FailureSignalHandlerOptions opts;
  absl::InstallFailureSignalHandler(opts);

  std::vector<std::string> log_keys = absl::GetFlag(FLAGS_log);
  for (std::string_view key : log_keys) { base::EnableLogging(key); }

  if (std::string lib = absl::GetFlag(FLAGS_link); not lib.empty()) {
    ASSERT_NOT_NULL(dlopen(lib.c_str(), RTLD_LAZY));
  }

  if (args.size() < 2) {
    std::cerr << "Missing required positional argument: source file"
              << std::endl;
    return 1;
  }
  if (args.size() > 2) {
    std::cerr << "Too many positional arguments." << std::endl;
    return 1;
  }
  return DumpControlFlowGraph(frontend::FileName(args[1]), std::cout);
}
