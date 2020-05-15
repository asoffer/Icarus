#include <dlfcn.h>
#include <memory>
#include <vector>

#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "absl/strings/str_split.h"
#include "base/expected.h"
#include "base/log.h"
#include "base/untyped_buffer.h"
#include "compiler/compiler.h"
#include "compiler/executable_module.h"
#include "compiler/module.h"
#include "diagnostic/errors.h"
#include "frontend/parse.h"
#include "frontend/source/file_name.h"
#include "frontend/source/repl.h"
#include "frontend/source/shared.h"
#include "init/cli.h"
#include "interpretter/execute.h"
#include "ir/compiled_fn.h"
#include "module/module.h"
#include "opt/opt.h"

namespace debug {
extern bool parser;
extern bool validation;
extern bool optimize_ir;
}  // namespace debug

namespace compiler {
namespace {

void ReplEval(ast::Expression const *expr, Compiler *compiler) {
  // TODO is nullptr for module okay here?
  ir::CompiledFn fn(type::Func({}, {}), {});
  ICARUS_SCOPE(ir::SetCurrent(&fn)) {
    ir::GetBuilder().CurrentBlock() = fn.entry();

    // TODO support multiple values computed simultaneously?
    auto expr_val = compiler->EmitValue(expr);
    if (compiler->diag().num_consumed() != 0) { return; }
    // TODO compiler->CompleteDeferredBodies();
    auto *expr_type = compiler->type_of(expr);
    if (expr_type != type::Void()) { NOT_YET(); }
    compiler->builder().ReturnJump();
  }

  interpretter::ExecutionContext ctx;
  interpretter::Execute(&fn, base::untyped_buffer(0), {}, &ctx);
}

struct ReplModule : public CompiledModule {
  ~ReplModule() override {}

  void ProcessNodes(base::PtrSpan<ast::Node const> nodes,
                    diagnostic::DiagnosticConsumer &diag) override {
    Compiler compiler({
        .builder             = ir::GetBuilder(),
        .data                = data(),
        .diagnostic_consumer = diag,
    });
    for (ast::Node const *node : nodes) {
      if (node->is<ast::Declaration>()) {
        auto *decl = &node->as<ast::Declaration>();

        {
          compiler.VerifyType(decl);
          compiler.EmitValue(decl);
          // TODO compiler.CompleteDeferredBodies();
          if (compiler.diag().num_consumed() != 0) { return; }
        }

      } else if (node->is<ast::Expression>()) {
        auto *expr = &node->as<ast::Expression>();
        ReplEval(expr, &compiler);
        fprintf(stderr, "\n");
      } else {
        NOT_YET(*node);
      }
    }
  }
};

int Repl() {
  std::puts("Icarus REPL");

  auto *repl = frontend::Source::Make<frontend::ReplSource>();
  diagnostic::StreamingConsumer diag(stderr, repl);
  ReplModule mod;

  // TODO Parse can fail.
  while (true) { mod.ProcessFromSource(repl, diag); }
}

}  // namespace
}  // namespace compiler

void cli::Usage() {
  execute = compiler::Repl;

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
