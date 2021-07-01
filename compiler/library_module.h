#ifndef ICARUS_COMPILER_LIBRARY_MODULE_H
#define ICARUS_COMPILER_LIBRARY_MODULE_H

#include "absl/functional/function_ref.h"
#include "compiler/compiler.h"
#include "compiler/module.h"
#include "diagnostic/consumer/consumer.h"
#include "ir/value/module_id.h"
#include "module/importer.h"

namespace compiler {
struct LibraryModule : CompiledModule {
  explicit LibraryModule() {}
  ~LibraryModule() override {}

 protected:
  void ProcessNodes(base::PtrSpan<ast::Node const> nodes,
                    diagnostic::DiagnosticConsumer &diag,
                    module::Importer &importer) override {
    ParsingComplete();

    Compiler c({
        .data                = context(this),
        .diagnostic_consumer = diagnostic_consumer(),
        .importer            = importer,
    });

    c.VerifyAll(nodes);
    if (diagnostic_consumer().num_consumed() > 0 or
        has_error_in_dependent_module()) {
      CompilationComplete();
      return;
    }

    base::untyped_buffer buffer;
    for (ast::Node const *node : nodes) { c.EmitToBuffer(node, buffer); }
    c.CompleteDeferredBodies();

    CompilationComplete();
  }
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_LIBRARY_MODULE_H
