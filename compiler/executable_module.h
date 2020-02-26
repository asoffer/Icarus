#ifndef ICARUS_COMPILER_EXECUTABLE_MODULE_H
#define ICARUS_COMPILER_EXECUTABLE_MODULE_H

#include "compiler/compiler.h"
#include "compiler/emit_function_call_infrastructure.h"
#include "compiler/extract_jumps.h"
#include "compiler/module.h"
#include "diagnostic/consumer/streaming.h"
#include "ir/compiled_fn.h"

namespace compiler {

struct ExecutableModule : CompiledModule {
  explicit ExecutableModule() {}
  ~ExecutableModule() override {}

  ir::CompiledFn *main() { return &main_; }

  // TODO hide this
  void set_main(ir::CompiledFn *main_fn) {}

 protected:
  void ProcessNodes(base::PtrSpan<ast::Node const> nodes) override {
    diagnostic::StreamingConsumer consumer(stderr);
    compiler::Compiler c(this, consumer);

    for (ast::Node const *node : nodes) {
      ExtractJumps(&c.data_.extraction_map_, node);
    }

    // Do one pass of verification over constant declarations. Then come
    // back a second time to handle the remaining.
    // TODO this may be necessary in library modules too.
    std::vector<ast::Node const *> deferred;
    for (ast::Node const *node : nodes) {
      if (auto const *decl = node->if_as<ast::Declaration>()) {
        if (decl->flags() & ast::Declaration::f_IsConst) {
          c.Visit(decl, VerifyTypeTag{});
          continue;
        }
      }
      deferred.push_back(node);
    }

    for (ast::Node const *node : deferred) { c.Visit(node, VerifyTypeTag{}); }

    if (consumer.num_consumed() > 0) { return; }

    ProcessExecutableBody(&c, nodes, main());
  }

 private:
  ir::CompiledFn main_ = ir::CompiledFn(type::Func({}, {}), {});
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_EXECUTABLE_MODULE_H
