#ifndef ICARUS_COMPILER_LIBRARY_MODULE_H
#define ICARUS_COMPILER_LIBRARY_MODULE_H

#include <thread>

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
    ExportsComplete();

    Compiler c({
        .data                = context(),
        .diagnostic_consumer = diag,
        .importer            = importer,
    });

    c.VerifyAll(nodes);
    if (c.diag().num_consumed() > 0) {
      CompilationComplete();
      return;
    }

    for (ast::Node const *node : nodes) { c.EmitValue(node); }
    c.CompleteDeferredBodies();

    CompilationComplete();
  }
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_LIBRARY_MODULE_H
