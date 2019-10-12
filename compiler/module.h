#ifndef ICARUS_COMPILER_MODULE_H
#define ICARUS_COMPILER_MODULE_H

#include "ast/ast_fwd.h"
#include "base/ptr_span.h"
#include "module/module.h"

namespace compiler {

struct Compiler;

struct CompiledModule : module::ExtendedModule<CompiledModule> {
  void ProcessNewNodes(base::PtrSpan<ast::Node const> nodes);
  Compiler* compiler_ = nullptr;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_MODULE_H
