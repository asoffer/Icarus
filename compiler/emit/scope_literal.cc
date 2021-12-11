#include <vector>

#include "ast/ast.h"
#include "compiler/compiler.h"
#include "ir/value/reg_or.h"
#include "type/type.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::ScopeLiteral const *node,
                            ir::PartialResultBuffer &out) {
  NOT_YET();
}

}  // namespace compiler
