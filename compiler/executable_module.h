#ifndef ICARUS_COMPILER_EXECUTABLE_MODULE_H
#define ICARUS_COMPILER_EXECUTABLE_MODULE_H

#include "compiler/compiler.h"
#include "compiler/emit_function_call_infrastructure.h"
#include "compiler/extract_jumps.h"
#include "compiler/module.h"
#include "diagnostic/consumer/consumer.h"
#include "ir/compiled_fn.h"

namespace compiler {

struct ExecutableModule : CompiledModule {
  explicit ExecutableModule() {}
  ~ExecutableModule() override {}

  ir::CompiledFn &main() { return main_; }
  ir::CompiledFn const &main() const { return main_; }

 protected:
  void ProcessNodes(base::PtrSpan<ast::Node const> nodes,
                    diagnostic::DiagnosticConsumer &diag) override {
    Compiler c({.builder             = ir::GetBuilder(),
                .data                = data(),
                .diagnostic_consumer = diag});

    for (ast::Node const *node : nodes) {
      ExtractJumps(&c.data().extraction_map_, node);
    }

    c.VerifyAll(nodes);
    if (diag.num_consumed() > 0) { return; }

    ProcessExecutableBody(&c, nodes, &main());
  }

 private:
  ir::CompiledFn main_ = ir::CompiledFn(type::Func({}, {}), {});
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_EXECUTABLE_MODULE_H
