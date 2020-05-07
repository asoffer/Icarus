#ifndef ICARUS_COMPILER_LIBRARY_MODULE_H
#define ICARUS_COMPILER_LIBRARY_MODULE_H

#include "compiler/compiler.h"
#include "compiler/extract_jumps.h"
#include "compiler/module.h"
#include "diagnostic/consumer/consumer.h"

namespace compiler {
struct LibraryModule : CompiledModule {
  explicit LibraryModule() {}
  ~LibraryModule() override {}

 protected:
  void ProcessNodes(base::PtrSpan<ast::Node const> nodes,
                    diagnostic::DiagnosticConsumer &diag) override {
    Compiler c({
        .builder             = ir::GetBuilder(),
        .data                = data(),
        .diagnostic_consumer = diag,
    });

    for (ast::Node const *node : nodes) {
      ExtractJumps(&c.data().extraction_map_, node);
    }
    for (ast::Node const *node : nodes) { c.Visit(node, VerifyTypeTag{}); }
    if (c.diag().num_consumed() > 0) { return; }

    for (ast::Node const *node : nodes) { c.Visit(node, EmitValueTag{}); }
    c.CompleteDeferredBodies();
  }
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_LIBRARY_MODULE_H
