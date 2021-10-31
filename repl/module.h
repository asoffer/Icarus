#ifndef ICARUS_REPL_MODULE_H
#define ICARUS_REPL_MODULE_H

#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/module.h"

namespace repl {

struct Module : compiler::CompiledModule {
  ~Module() override {}
};

}  // namespace repl

#endif  // ICARUS_REPL_MODULE_H
