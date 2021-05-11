#ifndef ICARUS_COMPILER_EXECUTABLE_MODULE_H
#define ICARUS_COMPILER_EXECUTABLE_MODULE_H

#include "compiler/compiler.h"
#include "compiler/library_module.h"
#include "compiler/module.h"
#include "diagnostic/consumer/consumer.h"
#include "ir/compiled_fn.h"
#include "module/importer.h"

namespace compiler {

struct ExecutableModule : CompiledModule {
  explicit ExecutableModule() {}
  ~ExecutableModule() override {}

  ir::CompiledFn &main() { return main_; }
  ir::CompiledFn const &main() const { return main_; }

 protected:
  void ProcessNodes(base::PtrSpan<ast::Node const> nodes,
                    diagnostic::DiagnosticConsumer &diag,
                    module::Importer &importer) override {
    Compiler c({
        .data                = context(this),
        .diagnostic_consumer = diag,
        .importer            = importer,
    });

    // TODO: It's conceivable that there's an early return from the implicit
    // main, so it'd be nice to have some mechanism for handling this. That
    // being said, we don't want to add a fake ast node for the implicit main
    // because it's not actually present.
    for (ast::Node const *node : nodes) { context(this).TrackJumps(node); }
    c.VerifyAll(nodes);
    if (diag.num_consumed() > 0 or has_error_in_dependent_module()) {
      CompilationComplete();
      return;
    }

    c.ProcessExecutableBody(nodes, &main());
    CompilationComplete();
  }

 private:
  ir::CompiledFn main_ = ir::CompiledFn(type::Func({}, {}), {});
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_EXECUTABLE_MODULE_H
