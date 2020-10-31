#ifndef ICARUS_COMPILER_EXECUTABLE_MODULE_H
#define ICARUS_COMPILER_EXECUTABLE_MODULE_H

#include "compiler/compiler.h"
#include "compiler/emit_function_call_infrastructure.h"
#include "compiler/extract_jumps.h"
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
                    diagnostic::DiagnosticConsumer &diag) override {
    module::FileImporter<LibraryModule> importer;
    Compiler c({
        .data                = context(),
        .diagnostic_consumer = diag,
        .importer            = importer,
    });

    for (ast::Node const *node : nodes) {
      ExtractJumps(&c.context().extraction_map_, node);
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
