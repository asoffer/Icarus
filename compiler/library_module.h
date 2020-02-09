#ifndef ICARUS_COMPILER_LIBRARY_MODULE_H
#define ICARUS_COMPILER_LIBRARY_MODULE_H

#include "compiler/compiler.h"
#include "compiler/extract_jumps.h"
#include "compiler/module.h"
#include "diagnostic/consumer/streaming.h"

namespace compiler {
struct LibraryModule : CompiledModule {
  explicit LibraryModule() {}
  ~LibraryModule() override {}

 protected:
  void ProcessNodes(base::PtrSpan<ast::Node const> nodes) override {
    diagnostic::StreamingConsumer consumer(stderr);
    compiler::Compiler c(this, consumer);
    for (ast::Node const *node : nodes) {
      ExtractJumps(&c.data_.extraction_map_, node);
    }
    for (ast::Node const *node : nodes) { c.Visit(node, VerifyTypeTag{}); }
    if (c.diag().num_consumed() > 0) { return; }

    for (ast::Node const *node : nodes) { c.Visit(node, EmitValueTag{}); }
    c.CompleteDeferredBodies();
  }
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_LIBRARY_MODULE_H
